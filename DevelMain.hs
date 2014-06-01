{-# LANGUAGE ImplicitPrelude #-}

-- | Devel web server.
--
-- > :l DevelMain
-- > DevelMain.update
--
-- To start/restart the server.

module DevelMain where

import Application (getApplicationDev)

import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp
import Yesod
import Yesod.Static

-- | Start the web server.
main :: IO (Store (IORef Application))
main =
  do s <- static "static"
     c <- newChan
     (port,app) <- getApplicationDev True
     ref <- newIORef app
     tid <- forkIO
              (runSettings
                 (setPort port defaultSettings)
                 (\req ->
                    do handler <- readIORef ref
                       handler req))
     _ <- newStore tid
     ref' <- newStore ref
     _ <- newStore c
     return ref'

-- | Update the server, start it if not running.
update :: IO (Store (IORef Application))
update =
  do m <- lookupStore 1
     case m of
       Nothing -> main
       Just store ->
         do ref <- readStore store
            c <- readStore (Store 2)
            writeChan c ()
            s <- static "static"
            (port,app) <- getApplicationDev True
            writeIORef ref app
            return store
