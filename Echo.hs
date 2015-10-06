-- | A quick and dirty way to echo a printf-style debugging message to
-- a file from anywhere.
--
-- To use from Emacs, run `tail -f /tmp/echo` with M-x grep. You can
-- rename the buffer to *echo* or something. The grep-mode buffer has
-- handy up/down keybindings that will open the file location for you
-- and it supports results coming in live. So it's a perfect way to
-- browse printf-style debugging logs.

module Echo where

import Control.Concurrent.MVar
import Control.Monad.Trans      (MonadIO(..))
import Data.Time
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Prelude
import System.IO.Unsafe

-- | God forgive me for my sins.
echoV :: MVar ()
echoV = unsafePerformIO (newMVar ())
{-# NOINLINE echoV #-}

-- | Echo something.
echo :: Q Exp
echo = [|write $(location >>= liftLoc) |]

-- | Grab the filename and line/col.
liftLoc :: Loc -> Q Exp
liftLoc (Loc filename _pkg _mod (line, _) _) =
    [|($(lift filename)
      ,$(lift line))|]

-- | Thread-safely (probably) write to the log.
write :: (MonadIO m) => (FilePath,Int) -> String -> m ()
write (file,line) it =
    liftIO (withMVar echoV (const (loggit)))
  where loggit =
            do now <- getCurrentTime
               appendFile "/tmp/echo" (loc ++ ": " ++ fmt now ++ " " ++ it ++ "\n")
        loc = file ++ ":" ++ show line
        fmt = formatTime defaultTimeLocale "%T%Q"

clear :: IO ()
clear = writeFile "/tmp/echo" ""
