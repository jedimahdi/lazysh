module Shell.Prompt where

import           Data.Foldable      ( traverse_ )
import qualified Data.List          as List
import           Data.Maybe         ( fromMaybe )
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Directory   ( getCurrentDirectory, getHomeDirectory )
import           System.Environment ( lookupEnv )
import           System.IO          ( hFlush, stdout )

data PromptItem
  = Chunk String
  | CurrentDirectory
  | CurrentDirectoryFromHome
  deriving (Eq, Show)

type Prompt = [PromptItem]

parsePrompt :: String -> Prompt
parsePrompt "" = []
parsePrompt ('%' : cs) = case cs of
  ('d' : s) -> CurrentDirectory : parsePrompt s
  ('~' : s) -> CurrentDirectoryFromHome : parsePrompt s
  _         -> Chunk "%" : parsePrompt cs
parsePrompt s               = let (beforePrecent, afterPrecent) = span (/= '%') s
  in Chunk beforePrecent : parsePrompt afterPrecent

prompt :: IO T.Text
prompt = showPrompt >> TIO.getLine
  where
    showPrompt :: IO ()
    showPrompt = do
      p <- fromMaybe "%~ $ " <$> lookupEnv "MY_PROMPT"
      traverse_ showPromptItem $ parsePrompt p
      hFlush stdout

    showPromptItem :: PromptItem -> IO ()
    showPromptItem (Chunk s)                = putStr s
    showPromptItem CurrentDirectory         = do
      dir <- getCurrentDirectory
      putStr dir
    showPromptItem CurrentDirectoryFromHome = do
      dir <- getCurrentDirectory
      home <- getHomeDirectory
      putStr $ "~" ++ (dir List.\\ home)
