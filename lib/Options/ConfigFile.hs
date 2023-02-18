module Options.ConfigFile
  (
  ) where

import Options.Parse (EnigmaSpec(..), EnigmaSpecOpt(..))
import Control.Exception (Exception, throwIO)

import Data.Either (either)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

readConfig :: FilePath -> IO EnigmaSpec
readConfig cf = do
  es <- T.readFile cf >>= pure . parseConfig
  either throwIO pure es

instance Exception ConfigError
data ConfigError = ConfigError
  deriving Show

-- EnigmaSpecO :: EnigmaSpecOpt -> EnigmaSpec
-- data EnigmaSpecOpt = EnigmaSpecOpt
--   { rotorNumbers    :: Rots
--   , reflectorNumber :: SerialNumber
--   , stateNumber     :: StateNumber
--   } deriving Show
parseConfig :: Text -> Either ConfigError EnigmaSpec
parseConfig = undefined
