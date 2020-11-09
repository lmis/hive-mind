{-# LANGUAGE TemplateHaskell #-}
module InteractiveCommand
  ( InteractiveCommand
  , start
  , restart
  , hIn
  , hOut
  , hErr
  , hProc
  , startupTimeout
  , command
  )
where

import           System.Process                 ( ProcessHandle
                                                , runInteractiveCommand
                                                , cleanupProcess
                                                )
import           System.IO                      ( Handle
                                                , hSetBinaryMode
                                                , hGetLine
                                                , hWaitForInput
                                                )
import           Control.Monad                  ( void )
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                )

data InteractiveCommand = InteractiveCommand {
  _command :: !String
 ,_startupTimeout :: !Int
 ,_hIn :: Handle
 ,_hOut :: Handle
 ,_hErr :: Handle
 ,_hProc :: ProcessHandle
}
makeLenses ''InteractiveCommand

start :: Int -> String -> IO InteractiveCommand
start startupTimeout' command' = do
  (hIn', hOut', hErr', hProc') <- runInteractiveCommand command'
  _                            <- hSetBinaryMode hIn' False
  _                            <- hSetBinaryMode hOut' False
  ready                        <- hWaitForInput hOut' startupTimeout'
  if ready then void $ hGetLine hOut' else error $ command' ++ " start time-out"

  return InteractiveCommand { _command        = command'
                            , _startupTimeout = startupTimeout'
                            , _hIn            = hIn'
                            , _hOut           = hOut'
                            , _hErr           = hErr'
                            , _hProc          = hProc'
                            }

kill :: InteractiveCommand -> IO ()
kill cmd = cleanupProcess
  (Just $ cmd ^. hIn, Just $ cmd ^. hOut, Just $ cmd ^. hErr, cmd ^. hProc)

restart :: InteractiveCommand -> IO InteractiveCommand
restart p = do
  kill p
  start (p ^. startupTimeout) $ p ^. command


