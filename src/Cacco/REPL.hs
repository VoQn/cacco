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

process :: Env (Val Location) -> String -> IO (Env (Val Location))
process env line = case parseTopLevel [] $ T.pack line of
    Left  err   -> print err >> return env
    Right exprs -> go env exprs
  where
    go env [] = return env
    go env (e:es) = do
      let (result, env') = eval env e
      case result of
        Left err  -> print err >> return env'
        Right val -> putStrLn (pretty val) >> go env' es

replLoop :: IO ()
replLoop = runInputT defaultSettings $ loop prelude
  where
    loop env = do
      minput <- getInputLine "cacco> "
      case minput of
        Nothing    -> outputStrLn "Bye."
        Just input -> liftIO (process env input) >>= loop
