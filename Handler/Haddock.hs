module Handler.Haddock
    ( getHaddockR
    ) where

import           Control.Concurrent (forkIO)
import           Crypto.Hash (Digest, SHA1)
import           Crypto.Hash.Conduit (sinkHash)
import           Data.Aeson (withObject)
import qualified Data.ByteString.Base16 as B16
import           Data.Byteable (toBytes)
import           Data.Conduit.Zlib (gzip)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Filesystem (isDirectory, createTree, isFile, rename, removeFile, removeDirectory)
import qualified Filesystem.Path.CurrentOS as F
import           Import
import           Network.Mime (defaultMimeLookup)
import           System.IO (IOMode (ReadMode), withBinaryFile)
import           System.IO.Temp (withTempFile)
import           System.Posix.Files (createLink)
import Stackage.Database

getHaddockR :: SnapName -> [Text] -> Handler ()
getHaddockR slug rest = redirect $ concat
    $ "http://haddock.stackage.org/"
    : toPathPiece slug
    : map (cons '/') rest
