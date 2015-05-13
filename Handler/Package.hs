{-# LANGUAGE OverloadedStrings #-}

-- | Lists the package page similar to Hackage.

module Handler.Package
    ( getPackageR
    , getPackageSnapshotsR
    , postPackageLikeR
    , postPackageUnlikeR
    , postPackageTagR
    , postPackageUntagR
    , packagePage
    ) where

import           Data.Char
import           Data.Slug
import           Data.Tag
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT

import           Database.Esqueleto ((^.), (&&.), Value (Value))
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import           Formatting
import           Import
import qualified Text.Blaze.Html.Renderer.Text as LT
import           Text.Email.Validate
import Stackage.Database

-- | Page metadata package.
getPackageR :: PackageName -> Handler Html
getPackageR = packagePage Nothing

packagePage :: Maybe (SnapName, Version)
            -> PackageName
            -> Handler Html
packagePage mversion pname = do
    let pname' = toPathPiece pname
    (deprecated, inFavourOf) <- getDeprecated pname'
    latests <- getLatests pname'
    render <- getUrlRender
    muid <- maybeAuthId
    (nLikes, liked) <- runDB $ do
        nLikes <- count [LikePackage ==. pname]
        let getLiked uid = (>0) <$> count [LikePackage ==. pname, LikeVoter ==. uid]
        liked <- maybe (return False) getLiked muid

        return (nLikes, liked)
    deps' <- getDeps pname'
    revdeps' <- getRevDeps pname'
    Entity _ package <- getPackage pname' >>= maybe notFound return

    mdocs <-
        case mversion of
            Just (sname, version) -> do
                ms <- getPackageModules sname pname'
                return $ Just (sname, toPathPiece version, ms)
            Nothing ->
                case latests of
                    li:_ -> do
                        ms <- getPackageModules (liSnapName li) pname'
                        return $ Just (liSnapName li, liVersion li, ms)
                    [] -> return Nothing

    let ixInFavourOf = zip [0::Int ..] inFavourOf
        displayedVersion = maybe (packageLatest package) (toPathPiece . snd) mversion

    myTags <- maybe (return []) (runDB . user'sTagsOf pname) muid
    tags <- fmap (map (\(v,count') -> (v,count',any (==v) myTags)))
                 (runDB (packageTags pname))

    let likeTitle = if liked
                       then "You liked this!"
                       else "I like this!" :: Text

    let homepage = case T.strip (packageHomepage package) of
                     x | null x -> Nothing
                       | otherwise -> Just x
        synopsis = packageSynopsis package
        deps = enumerate deps'
        revdeps = enumerate revdeps'
        authors = enumerate (parseIdentitiesLiberally (packageAuthor package))
        maintainers = let ms = enumerate (parseIdentitiesLiberally (packageMaintainer package))
                      in if ms == authors
                            then []
                            else ms
    defaultLayout $ do
        setTitle $ toHtml pname
        $(combineStylesheets 'StaticR
            [ css_font_awesome_min_css
            ])
        let pn = pname
            toPkgVer x y = concat [x, "-", y]
        $(widgetFile "package")
  where enumerate = zip [0::Int ..]

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
getPackageSnapshotsR pn = error "getPackageSnapshotsR"
{-
  do let haddocksLink ident version =
             HaddockR ident [concat [toPathPiece pn, "-", toPathPiece version]]
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
          -}
