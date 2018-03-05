module Cacco.REPL where

import qualified Data.Text                as T

import           Control.Monad.Trans
import           System.Console.Haskeline

import           Cacco.Core               (builtin)
import           Cacco.Env                (Env, initAmb, localScope)
import           Cacco.Eval               (eval)
import           Cacco.Syntax.Expr        (AstF)
import           Cacco.Syntax.Location    (Location)
import           Cacco.Syntax.Parser      (parseTopLevel)
import           Cacco.Val                (Val, pretty)
import           Data.Ann                 (Ann)

prelude :: Env (Val Location)
prelude = initAmb { localScope = builtin }

evalPrint :: Env (Val Location) -> [Ann Location AstF] -> IO (Env (Val Location))
evalPrint env [] = return env
evalPrint env (expr:rest) = do
    let (result, env') = eval expr env
    case result of
      Left err  -> print err >> return env'
      Right val -> putStrLn (pretty val) >> evalPrint env' rest

process :: String -> Env (Val Location) -> IO (Env (Val Location))
process line env = case parseTopLevel [] $ T.pack line of
    Left  err   -> print err >> return env
    Right exprs -> evalPrint env exprs

loop :: MonadException m => Env (Val Location) -> InputT m ()
loop env = do
    minput <- getInputLine "('-')> "
    case minput of
      Nothing    -> outputStrLn "Bye."
      Just input -> liftIO (process input env) >>= loop

replLoop :: IO ()
replLoop = runInputT defaultSettings $ loop prelude
