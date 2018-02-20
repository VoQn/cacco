module Cacco.REPL where

import qualified Data.Text                as T

import           Control.Monad.Trans
import           System.Console.Haskeline

import           Cacco.Core               (builtin)
import           Cacco.Env                (Env (..))
import qualified Cacco.Env                as Env
import           Cacco.Eval               (eval)
import           Cacco.Syntax.Location    (Location)
import           Cacco.Syntax.Parser      (parseTopLevel)
import           Cacco.Val                (Val, pretty)

prelude :: Env (Val Location)
prelude = Env.initEnv { symbols = builtin }

process :: String -> IO ()
process line = do
  let res = parseTopLevel [] $ T.pack line
  case res of
    Left err -> print err
    Right ex -> (`mapM_` ex) $ \e ->
      case eval prelude e of
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
