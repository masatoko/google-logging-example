{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Exception
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.Google as Google
import Network.Google.Auth
import Network.Google.Logging
import System.IO (stdout)

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Show.Pretty (ppShow)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified System.Environment as Env

import qualified Network.Google.Compute.Metadata as Metadata

pprint :: Show a => a -> IO ()
pprint =
  putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

getEnv :: IO (Text, Text, Text, Text, Text)
getEnv = do
  -- Name of log should be something like:
  --   projects/project-id/logs/my-app
  --
  -- MonitoredResource should contain to simulate Google's fluentd:
  --   labels: {
  --     project_id:  "project-id"
  --     instance_id:  "7937682100289506354"
  --     zone:  "us-central1-a"
  --   }
  --
  manager <- newManager tlsManagerSettings
  projectId <- Metadata.getProjectId manager
  description <- Metadata.getDescription manager
  hostname <- Metadata.getHostname manager
  instanceId <- Metadata.getInstanceId manager
  zone <- Metadata.getZone manager
  return (projectId, description, hostname, instanceId, zone)

logMsg :: Text -> Text -> IO (Rs EntriesWrite)
logMsg logId msg = do
  (projectId, description, hostname, instanceId, zone) <- getEnv
  print ("projectId" :: Text, projectId)
  print ("description" :: Text, description)
  print ("hostname" :: Text, hostname)
  print ("instanceId" :: Text, instanceId)
  print ("zone" :: Text, zone)

  lgr <- newLogger Google.Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ loggingWriteScope)

  -- Try to pull out credentials to see what they've been set to.
  let store = view envStore env
  auth <- retrieveAuthFromStore store
  let credentials = _credentials auth
  case credentials of
    FromMetadata serviceId -> print ("FromMetadata" :: Text, serviceId)
    FromClient _a _b -> print ("FromClient" :: Text)
    FromAccount serviceAccount -> print ("FromAccount" :: Text, serviceAccount)
    FromUser user -> print ("FromUser" :: Text, user)

  let
    entry = logEntry & leTextPayload ?~ msg
                     & leSeverity ?~ Info -- Network.Google.Logging - LogEntrySeverity
    entries = [entry]
    logName = "projects/" <> projectId <> "/logs/" <> logId
    resourceLabels = monitoredResourceLabels $ HM.fromList
      [ ("project_id", projectId)
      , ("instance_id", instanceId)
      , ("zone", zone)
      ]
    resource = monitoredResource -- Required
      & mrType ?~ "gce_instance"
      & mrLabels ?~ resourceLabels
    labels = writeLogEntriesRequestLabels $ HM.fromList -- Optional
      [ ("key", "value")
      ]

  runResourceT . runGoogle env $
    send
      (entriesWrite
        (writeLogEntriesRequest
          & wlerEntries .~ entries
          & wlerLogName ?~ logName
          & wlerResource ?~ resource
          & wlerLabels ?~ labels
        )
      )

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [logId, msg] ->
      handle (\e -> pprint ("exception" :: Text, e :: SomeException)) $ do
        putStrLn "Writing log message..."
        result <- logMsg (T.pack logId) (T.pack msg)
        pprint ("result" :: Text, result)
    _ -> putStrLn "usage: google-log log-id message"
