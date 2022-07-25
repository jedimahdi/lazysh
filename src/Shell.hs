module Shell where

import           Control.Exception ( SomeException, catch )
import           Error.Diagnose    ( defaultStyle, printDiagnostic )
import           Shell.Parser
import           Shell.Prompt
import           Shell.Run
import           System.IO         ( hPrint, stderr, stdin, stdout )

shell :: IO ()
shell = do
  line <- prompt
  case parseAction line of
    Right action -> runAction stdin stdout action `catch` \(e :: SomeException) -> hPrint stderr e
    Left diag    -> printDiagnostic stderr True True 4 defaultStyle diag
  shell
