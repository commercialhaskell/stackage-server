{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Options.Applicative
import RIO
import RIO.List as L
import RIO.Text as T
import Stackage.Database.Cron
import Stackage.Database.Github

readText :: ReadM T.Text
readText = T.pack <$> str

readLogLevel :: ReadM LogLevel
readLogLevel =
    maybeReader $ \case
        "debug" -> Just LevelDebug
        "info" -> Just LevelInfo
        "warn" -> Just LevelWarn
        "error" -> Just LevelError
        _ -> Nothing

readGithubRepo :: ReadM GithubRepo
readGithubRepo =
    maybeReader $ \str' ->
        case L.span (/= '/') str' of
            (grAccount, '/':grName)
                | not (L.null grName) -> Just GithubRepo {..}
            _ -> Nothing

optsParser :: Parser StackageCronOptions
optsParser =
    StackageCronOptions <$>
    switch
        (long "force-update" <> short 'f' <>
         help
             "Initiate a force update, where all snapshots will be updated regardless if \
             \their yaml files from stackage-snapshots repo have been updated or not.") <*>
    option
        readText
        (long "download-bucket" <> value haddockBucketName <> metavar "DOWNLOAD_BUCKET" <>
         help
             ("S3 Bucket name where things like haddock and current hoogle files should \
              \be downloaded from. Default is: " <>
              T.unpack haddockBucketName)) <*>
    option
        readText
        (long "upload-bucket" <> value haddockBucketName <> metavar "UPLOAD_BUCKET" <>
         help
             ("S3 Bucket where hoogle db and snapshots.json file will be uploaded to. Default is: " <>
              T.unpack haddockBucketName)) <*>
    switch
        (long "do-not-upload" <>
         help "Stop from hoogle db and snapshots.json from being generated and uploaded") <*>
    option
        readLogLevel
        (long "log-level" <> metavar "LOG_LEVEL" <> short 'l' <> value LevelInfo <>
         help "Verbosity level (debug|info|warn|error). Default level is 'info'.") <*>
    option
        readGithubRepo
        (long "snapshots-repo" <> metavar "SNAPSHOTS_REPO" <>
         value (GithubRepo repoAccount repoName) <>
         help
             ("Github repository with snapshot files. Default level is '" ++
              repoAccount ++ "/" ++ repoName ++ "'.")) <*>
    switch (long "report-progress" <> help "Report how many packages has been loaded.") <*>
    switch
        (long "cache-cabal-files" <>
         help
             ("Improve performance by caching parsed cabal files" ++
              " at expense of higher memory consumption"))
  where
    repoAccount = "commercialhaskell"
    repoName = "stackage-snapshots"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    opts <-
        execParser $
        info
            (optsParser <*
             abortOption (ShowHelpText Nothing) (long "help" <> short 'h' <> help "Display this message."))
            (header "stackage-cron - Keep stackage.org up to date" <>
             progDesc
                 "Uses github.com/commercialhaskell/stackage-snapshots repository as a source \
                 \for keeping stackage.org up to date. Amongst other things are: update of hoogle db\
                 \and it's upload to S3 bucket, use stackage-content for global-hints" <>
             fullDesc)
    stackageServerCron opts
