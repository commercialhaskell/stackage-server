{-# LANGUAGE OverloadedStrings #-}

-- | Lists the package page similar to Hackage.

module Handler.Package where

import           Data.Char
import           Data.Slug
import           Data.Tag
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (addUTCTime)
import           Database.Esqueleto ((^.), (&&.), Value (Value))
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import           Formatting
import           Import
import           Text.Email.Validate

-- | Page metadata package.
getPackageR :: PackageName -> Handler Html
getPackageR pn = do
    let maxSnaps = 10
        haddocksLink ident version =
            HaddockR ident [concat [toPathPiece pn, "-", toPathPiece version]]
    muid <- maybeAuthId
    (mnightly, mlts, downloads, recentDownloads, nLikes, liked,
      Entity _ metadata, revdeps', mdocs) <- runDB $ do
        mnightly <- getNightly pn
        mlts <- getLts pn
        nLikes <- count [LikePackage ==. pn]
        let getLiked uid = (>0) <$> count [LikePackage ==. pn, LikeVoter ==. uid]
        liked <- maybe (return False) getLiked muid
        downloads <- count [DownloadPackage ==. pn]
        now' <- liftIO getCurrentTime
        let nowMinus30 = addUTCTime (-30 * 24 * 60 * 60) now'
        recentDownloads <- count [DownloadPackage ==. pn, DownloadTimestamp >=. nowMinus30]
        metadata <- getBy404 (UniqueMetadata pn)
        revdeps' <- reverseDeps pn
        mdocsent <- selectFirst [DocsName ==. pn] [Desc DocsUploaded]
        mdocs <- forM mdocsent $ \(Entity docsid (Docs _ version _)) -> (,)
            <$> pure version
            <*> (map entityVal <$>
                 selectList [ModuleDocs ==. docsid] [Asc ModuleName])
        return ( mnightly
               , mlts
               , downloads
               , recentDownloads
               , nLikes
               , liked
               , metadata
               , revdeps'
               , mdocs
               )

    myTags <- maybe (return []) (runDB . user'sTagsOf pn) muid
    tags <- fmap (map (\(v,count) -> (v,count,any (==v) myTags)))
                 (runDB (packageTags pn))

    let likeTitle = if liked
                       then "You liked this!"
                       else "I like this!" :: Text

    let homepage = case T.strip (metadataHomepage metadata) of
                     x | null x -> Nothing
                       | otherwise -> Just x
        synopsis = metadataSynopsis metadata
        deps = enumerate (metadataDeps metadata)
        revdeps = enumerate revdeps'
        authors = enumerate (parseIdentitiesLiberally (metadataAuthor metadata))
        maintainers = let ms = enumerate (parseIdentitiesLiberally (metadataMaintainer metadata))
                      in if ms == authors
                            then []
                            else ms
    defaultLayout $ do
        setTitle $ toHtml pn
        $(combineStylesheets 'StaticR
            [ css_font_awesome_min_css
            ])
        $(widgetFile "package")
  where enumerate = zip [0::Int ..]
        reformat (Value version, Value title, Value ident, Value hasHaddocks) =
            (version,fromMaybe title (stripPrefix "Stackage build for " title),ident,hasHaddocks)

-- | Get tags of the given package.
packageTags :: PackageName -> YesodDB App [(Slug,Int)]
packageTags pn =
    fmap (map boilerplate)
         (E.select
              (E.from (\(t `E.LeftOuterJoin` bt) -> do
                 E.on $ t E.^. TagTag E.==. bt E.^. BannedTagTag
                 E.where_
                     $ (t ^. TagPackage E.==. E.val pn) E.&&.
                       (E.isNothing $ E.just $ bt E.^. BannedTagTag)
                 E.groupBy (t ^. TagTag)
                 E.orderBy [E.asc (t ^. TagTag)]
                 return (t ^. TagTag,E.count (t ^. TagTag)))))
  where boilerplate (E.Value a,E.Value b) = (a,b)

-- | Get tags of the package by the user.
user'sTagsOf :: PackageName -> UserId -> YesodDB App [Slug]
user'sTagsOf pn uid =
    fmap (map (\(E.Value v) -> v))
         (E.select
              (E.from (\t ->
                  do E.where_ (t ^. TagPackage E.==. E.val pn E.&&.
                               t ^. TagVoter E.==. E.val uid)
                     E.orderBy [E.asc (t ^. TagTag)]
                     return (t ^. TagTag))))

-- | Get reverse dependencies of a package.
reverseDeps :: PackageName -> YesodDB App [PackageName]
reverseDeps pn = fmap (map boilerplate) $ E.select $ E.from $ \dep -> do
    E.where_ $ dep ^. DependencyDep E.==. E.val pn
    E.orderBy [E.asc $ dep ^. DependencyUser]
    return $ dep ^. DependencyUser
  where boilerplate (E.Value e) = e

-- | Get the latest nightly snapshot for the given package.
getNightly :: PackageName -> YesodDB App (Maybe (Day, Text, Version, SnapSlug))
getNightly pn =
  fmap (fmap boilerplate . listToMaybe)
       (E.select (E.from query))
  where boilerplate (E.Value a,E.Value b,E.Value c,E.Value d) =
          (a,b,c,d)
        query (p,n,s) =
          do E.where_ ((p ^. PackageName' E.==. E.val pn) E.&&.
                       (p ^. PackageStackage E.==. n ^. NightlyStackage) E.&&.
                       (s ^. StackageId E.==. n ^. NightlyStackage))
             E.orderBy [E.desc (n ^. NightlyDay)]
             return (n ^. NightlyDay
                    ,n ^. NightlyGhcVersion
                    ,p ^. PackageVersion
                    ,s ^. StackageSlug)

-- | Get the latest LTS snapshot for the given package.
getLts :: PackageName -> YesodDB App (Maybe (Int,Int,Version,SnapSlug))
getLts pn =
  fmap (fmap boilerplate . listToMaybe)
       (E.select (E.from query))
  where boilerplate (E.Value a,Value b,Value c,Value d) =
          (a,b,c,d)
        query (p,n,s) =
          do E.where_ ((p ^. PackageName' E.==. E.val pn) E.&&.
                       (p ^. PackageStackage E.==. n ^. LtsStackage) E.&&.
                       (s ^. StackageId E.==. n ^. LtsStackage))
             E.orderBy [E.desc (n ^. LtsMajor),E.desc (n ^. LtsMinor)]
             return (n ^. LtsMajor
                    ,n ^. LtsMinor
                    ,p ^. PackageVersion
                    ,s ^. StackageSlug)

-- | An identifier specified in a package. Because this field has
-- quite liberal requirements, we often encounter various forms. A
-- name, a name and email, just an email, or maybe nothing at all.
data Identifier
  = EmailOnly !EmailAddress -- ^ An email only e.g. jones@example.com
  | Contact !Text
            !EmailAddress -- ^ A contact syntax, e.g. Dave Jones <jones@example.com>
  | PlainText !Text -- ^ Couldn't parse anything sensible, leaving as-is.
  deriving (Show,Eq)

-- | An author/maintainer field may contain a comma-separated list of
-- identifiers. It may be the case that a person's name is written as
-- "Einstein, Albert", but we only parse commas when there's an
-- accompanying email, so that would be:
--
--  Einstein, Albert <emc2@gmail.com>, Isaac Newton <falling@apple.com>
--
-- Whereas
--
-- Einstein, Albert, Isaac Newton
--
-- Will just be left alone. It's an imprecise parsing because the
-- input is wide open, but it's better than nothing:
--
-- λ> parseIdentitiesLiberally "Chris Done, Dave Jones <chrisdone@gmail.com>, Einstein, Albert, Isaac Newton, Michael Snoyman <michael@snoyman.com>"
-- [PlainText "Chris Done"
-- ,Contact "Dave Jones" "chrisdone@gmail.com"
-- ,PlainText "Einstein, Albert, Isaac Newton"
-- ,Contact "Michael Snoyman" "michael@snoyman.com"]
--
-- I think that is quite a predictable and reasonable result.
--
parseIdentitiesLiberally :: Text -> [Identifier]
parseIdentitiesLiberally =
  filter (not . empty) .
  map strip .
  concatPlains .
  map parseChunk .
  T.split (== ',')
  where empty (PlainText e) = T.null e
        empty _ = False
        strip (PlainText t) = PlainText (T.strip t)
        strip x = x
        concatPlains = go
          where go (PlainText x:PlainText y:xs) =
                  go (PlainText (x <> "," <> y) :
                      xs)
                go (x:xs) = x : go xs
                go [] = []

-- | Try to parse a chunk into an identifier.
--
-- 1. First tries to parse an \"email@domain.com\".
-- 2. Then tries to parse a \"Foo <email@domain.com>\".
-- 3. Finally gives up and returns a plain text.
--
-- λ> parseChunk "foo@example.com"
-- EmailOnly "foo@example.com"
-- λ> parseChunk "Dave Jones <dave@jones.com>"
-- Contact "Dave Jones" "dave@jones.com"
-- λ> parseChunk "<x>"
-- PlainText "<x>"
-- λ> parseChunk "Hello!"
-- PlainText "Hello!"
--
parseChunk :: Text -> Identifier
parseChunk chunk =
  case emailAddress (T.encodeUtf8 (T.strip chunk)) of
    Just email -> EmailOnly email
    Nothing ->
      case T.stripPrefix
             ">"
             (T.dropWhile isSpace
                          (T.reverse chunk)) of
        Just rest ->
          case T.span (/= '<') rest of
            (T.reverse -> emailStr,this) ->
              case T.stripPrefix "< " this of
                Just (T.reverse -> name) ->
                  case emailAddress (T.encodeUtf8 (T.strip emailStr)) of
                    Just email ->
                      Contact (T.strip name) email
                    _ -> plain
                _ -> plain
        _ -> plain
  where plain = PlainText chunk

-- | Render email to text.
renderEmail :: EmailAddress -> Text
renderEmail = T.decodeUtf8 . toByteString

-- | Format a number with commas nicely.
formatNum :: Int -> Text
formatNum = sformat commas

postPackageLikeR :: PackageName -> Handler ()
postPackageLikeR packageName = maybeAuthId >>= \muid -> case muid of
    Nothing -> return ()
    Just uid -> runDB $ P.insert_ $ Like packageName uid

postPackageUnlikeR :: PackageName -> Handler ()
postPackageUnlikeR name = maybeAuthId >>= \muid -> case muid of
    Nothing -> return ()
    Just uid -> runDB $ P.deleteWhere [LikePackage ==. name, LikeVoter ==. uid]

postPackageTagR :: PackageName -> Handler ()
postPackageTagR packageName =
  maybeAuthId >>=
  \muid ->
    case muid of
      Nothing -> return ()
      Just uid ->
        do mtag <- lookupPostParam "slug"
           case mtag of
             Just tag ->
               do slug <- mkTag tag
                  void (runDB (P.insert (Tag packageName slug uid)))
             Nothing -> error "Need a slug"

postPackageUntagR :: PackageName -> Handler ()
postPackageUntagR packageName =
  maybeAuthId >>=
  \muid ->
    case muid of
      Nothing -> return ()
      Just uid ->
        do mtag <- lookupPostParam "slug"
           case mtag of
             Just tag ->
               do slug <- mkTag tag
                  void (runDB (P.deleteWhere
                                 [TagPackage ==. packageName
                                 ,TagTag ==. slug
                                 ,TagVoter ==. uid]))
             Nothing -> error "Need a slug"

getPackageSnapshotsR :: PackageName -> Handler Html
getPackageSnapshotsR pn =
  do let haddocksLink ident version =
             HaddockR ident [concat [toPathPiece pn, "-", toPathPiece version]]
     muid <- maybeAuthId
     snapshots <- (runDB .
                   fmap (map reformat) .
                   E.select . E.from)
                    (\(p,s) ->
                       do E.where_ $
                            (p ^. PackageStackage E.==. s ^. StackageId) &&.
                            (p ^. PackageName' E.==. E.val pn)
                          E.orderBy [E.desc $ s ^. StackageUploaded]
                          return
                            (p ^. PackageVersion
                            ,s ^. StackageTitle
                            ,s ^. StackageSlug
                            ,s ^. StackageHasHaddocks))
     defaultLayout
       (do setTitle ("Packages for " >> toHtml pn)
           $(combineStylesheets 'StaticR
                                [css_font_awesome_min_css])
           $(widgetFile "package-snapshots"))
  where reformat (Value version,Value title,Value ident,Value hasHaddocks) =
          (version
          ,fromMaybe title (stripPrefix "Stackage build for " title)
          ,ident
          ,hasHaddocks)
