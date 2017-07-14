

module Main where

import AST
import Exec
import Lexer
import Parser

import Control.Monad.State
import System.Environment (getArgs)
import System.Exit
import Text.Parsec.Prim


main :: IO ()
main = do files <- getArgs
          mapM_ runFile files


runFile :: String -> IO ()
runFile file = do contents <- readFile file
                  case runParser program () file (scan contents) of
                    Left err -> error (show err)
                    Right prog -> exec prog
