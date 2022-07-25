module Shell.Run where

import           Control.Exception  ( throwIO )
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Shell.Types
import           System.Directory   ( getCurrentDirectory, getHomeDirectory, setCurrentDirectory )
import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )
import           System.IO          ( Handle, hFlush, hPutStr, stderr, stdin, stdout )
import           System.Process hiding (runCommand)
import qualified System.Process     as P hiding (runCommand)

runCd :: [T.Text] -> IO ()
runCd []   = getHomeDirectory >>= setCurrentDirectory
runCd [fp] = setCurrentDirectory (T.unpack fp)
runCd _    = throwIO CdTooManyArgs

runCommand :: Program -> Args -> Handle -> Handle -> IO ()
runCommand cmd args input output = do
  let process = (P.proc (T.unpack cmd) (map T.unpack args)) { std_out = UseHandle output, std_in = UseHandle input }
  (_, _, _, h) <- createProcess process
  exitCode <- waitForProcess h
  pure ()

runAction :: Handle -> Handle -> Action -> IO ()
runAction input output (Command cmd args) = do
  case cmd of
    "cd" -> runCd args
    _ -> runCommand cmd args input output

runAction input output (Pipe from to)     = do
  (reader, writer) <- createPipe
  runAction input writer from
  runAction reader output to
