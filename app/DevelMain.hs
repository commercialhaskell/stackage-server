{-# LANGUAGE ImplicitPrelude #-}

-- | Devel web server.
--
-- > :l DevelMain
-- > DevelMain.update
--
-- To start/restart the server.

module DevelMain where

import Application (App, withFoundationDev, makeApplication)

import Control.Concurrent
import Foreign.Store
import Network.Wai.Handler.Warp
import Yesod
import Data.IORef


data Command = Run (IO ())
             | Stop

newtype Devel = Devel (Store (IORef (App -> IO Application)))

-- | Start the web server.
main :: IO Devel
main = do
    c <- newChan
    ref <- newIORef makeApplication
    tid <-
        forkIO $
        withFoundationDev $ \settings foundation ->
            runSettings
                settings
                (\req cont -> do
                     mkApp <- readIORef ref
                     application <- mkApp foundation
                     application req cont)
    _ <- newStore tid
    ref' <- newStore ref
    _ <- newStore c
    return $ Devel ref'

-- | Update the server, start it if not running.
update :: IO Devel
update =
  do m <- lookupStore 1
     case m of
       Nothing -> main
       Just store ->
         do ref <- readStore store
            c <- readStore (Store 2)
            writeChan c ()
            writeIORef ref makeApplication
            return $ Devel store
