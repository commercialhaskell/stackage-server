{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Foundation where

import ClassyPrelude.Yesod
import Data.WebsiteContent
import Settings
import Settings.StaticFiles
import Stackage.Database
import Text.Blaze
import Text.Hamlet (hamletFile)
import Types
import Yesod.AtomFeed
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.GitRepo
import Yesod.GitRev (GitRev)
import qualified RIO

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings           :: !AppSettings
    , appStatic             :: !Static -- ^ Settings for static file serving.
    , appHttpManager        :: !Manager
    , appLogger             :: !Logger
    , appLogFunc            :: !RIO.LogFunc
    , appWebsiteContent     :: !(GitRepo WebsiteContent)
    , appStackageDatabase   :: !StackageDatabase
    , appLatestStackMatcher :: !(IO (Text -> Maybe Text))
    -- ^ Give a pattern, get a URL
    , appMirrorStatus       :: !(IO (Status, WidgetFor App ()))
    , appGetHoogleDB        :: !(SnapName -> IO (Maybe FilePath))
    , appGitRev             :: !GitRev
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

defaultLayoutNoContainer :: Widget -> Handler Html
defaultLayoutNoContainer = defaultLayoutWithContainer False

defaultLayoutWithContainer :: Bool -> Widget -> Handler Html
defaultLayoutWithContainer insideContainer widget = do
    mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    cur <- getCurrentRoute
    pc <- widgetToPageContent $ do
        $(combineStylesheets 'StaticR
            [ css_normalize_css
            , css_bootstrap_css
            , css_bootstrap_responsive_css
            ])
        $((combineScripts 'StaticR
                          [ js_jquery_js
                          , js_bootstrap_js
                          ]))
        atomLink FeedR "Recent Stackage snapshots"
        $(widgetFile "default-layout")

    mcurr <- getCurrentRoute
    let notHome = mcurr /= Just HomeR

    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing   -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = return Nothing

    defaultLayout = defaultLayoutWithContainer True

    {- MSS 2018-06-21 Not worrying about broken cabal-install anymore

    -- Ideally we would just have an approot that always includes https, and
    -- redirect users from non-SSL to SSL connections. However, cabal-install
    -- is broken, and does not support TLS. Therefore, we *don't* force the
    -- redirect.
    --
    -- Nonetheless, we want to keep generated links as https:// links. The
    -- problem is that sometimes CORS kicks in and breaks a static resource
    -- when loading from a non-secure page. So we have this ugly hack: whenever
    -- the destination is a static file, don't include the scheme or hostname.
    urlRenderOverride y route@StaticR{} =
        Just $ uncurry (joinPath y "") $ renderRoute route
    urlRenderOverride _ _ = Nothing
    -}

    {- Temporarily disable to allow for horizontal scaling
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs
    -}

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO _ "CLEANUP" _ = pure False
    shouldLogIO app _source level = pure $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    maximumContentLength _ _ = Just 2000000

instance ToMarkup (Route App) where
    toMarkup c =
        case c of
          AllSnapshotsR{} -> "Snapshots"
          BlogHomeR       -> "Blog"
          _               -> ""

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

instance GetStackageDatabase App Handler where
    getStackageDatabase = appStackageDatabase <$> getYesod
    getLogFunc = appLogFunc <$> getYesod

instance GetStackageDatabase App (WidgetFor App) where
    getStackageDatabase = appStackageDatabase <$> getYesod
    getLogFunc = appLogFunc <$> getYesod
