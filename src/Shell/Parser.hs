module Shell.Parser where

import qualified Data.Text                        as T
import           Data.Void                        ( Void )
import           Error.Diagnose
    ( Diagnostic, addFile, defaultStyle, printDiagnostic, stderr )
import           Error.Diagnose.Compat.Megaparsec ( HasHints, errorDiagnosticFromBundle, hints )
import           Shell.Types
import           Text.Megaparsec                  ( (<|>) )
import qualified Text.Megaparsec                  as M
import qualified Text.Megaparsec.Char             as M

type Parser = M.Parsec Void T.Text

commandParser :: Parser Action
commandParser = do
  cmd <- M.space *> M.some M.letterChar <* M.space
  args <- M.many (M.some (M.char '-' <|> M.letterChar <|> M.digitChar) <* M.space)
  pure $ Command (T.pack cmd) (map T.pack args)

barParser :: Parser Char
barParser = M.space *> M.char '|'

pipeParser :: Parser Action
pipeParser = do
  first <- commandParser
  rest <- M.many (barParser *> commandParser)
  pure $ foldl Pipe first rest

actionParser :: Parser Action
actionParser = do
  pipeParser <* M.eof

instance HasHints Void msg where
  hints _ = mempty

parseAction :: T.Text -> Either (Diagnostic T.Text) Action
parseAction line
  = case M.runParser actionParser filename line of
      Left bundle ->
        let diag :: Diagnostic T.Text = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
        in Left $ addFile diag filename (T.unpack line)
      Right res -> Right res
  where
    filename = "<interactive>"

