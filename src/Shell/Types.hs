module Shell.Types where

import           Control.Exception ( Exception )
import           Data.Text         ( Text )

data PromptItem
  = Chunk String
  | CurrentDirectory
  | CurrentDirectoryFromHome
  deriving (Eq, Show)

type Prompt = [PromptItem]

type Program = Text

type Args = [Text]

data DirectoryPath
  = GoHome
  | Chdir FilePath
  deriving (Eq, Show)

data Action
  = Command Program Args
  | Pipe Action Action
  deriving (Eq, Show)

data RunException = CdTooManyArgs

instance Show RunException where
  show CdTooManyArgs = "cd: too many arguments"

instance Exception RunException
