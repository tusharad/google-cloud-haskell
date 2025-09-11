module Logging (runApp) where

import Google.Cloud.Logging.Logs

runApp :: IO ()
runApp = do
  let projectId = "haskread"
  eLogs <- listLogs (Projects projectId) defaultListLogsOps
  print eLogs
  eSettings <- getSettings (Projects projectId)
  print eSettings
