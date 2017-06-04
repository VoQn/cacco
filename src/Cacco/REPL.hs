module Cacco.REPL where

import qualified Data.Text                as T

import           Control.Monad.Trans
import           System.Console.Haskeline

import           Cacco.Parser

process :: String -> IO ()
process line = do
  let res = parseTopLevel [] $ T.pack line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

replLoop :: IO ()
replLoop = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "cacco> "
      case minput of
        Nothing    -> outputStrLn "Bye."
        Just input -> liftIO (process input) >> loop
