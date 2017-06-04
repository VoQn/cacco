module Main where

import           Control.Monad         (when)

import           System.Console.GetOpt
import           System.Environment

import           Cacco.REPL
import           Lib

data Flag = Version deriving (Eq, Show)

options :: [OptDescr Flag]
options = [
    Option ['V', '?'] ["version"] (NoArg Version) "show version number"
  ]

main :: IO ()
main = do
    argv <- getArgs
    case getOpt Permute options argv of
        ([], [], []) -> replLoop
        (fs, _, [])  -> when (Version `elem` fs) printVersion
        (_, _, err)  -> ioError (userError (concat err ++ usageInfo header options))
  where
    header = "Usage: ic [OPTION...] "
