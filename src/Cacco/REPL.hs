module Cacco.REPL where

import qualified Data.Text                as T

import           Control.Monad.Trans
import           System.Console.Haskeline

import           Cacco.Eval               (eval)
import           Cacco.Parser
import           Cacco.Val                (pretty)

process :: String -> IO ()
process line = do
  let res = parseTopLevel [] $ T.pack line
  case res of
    Left err -> print err
    Right ex -> (`mapM_` ex) $ \e ->
      case eval e [] of
        Left err  -> print err
        Right val -> putStrLn $ pretty val

replLoop :: IO ()
replLoop = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "cacco> "
      case minput of
        Nothing    -> outputStrLn "Bye."
        Just input -> liftIO (process input) >> loop
