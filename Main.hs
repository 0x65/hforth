module Main where

import Control.Monad.State
import Control.Monad.Error

import Forth
import Primitives

run :: VirtualMachine -> IO ()
run vm = do
    (result, newState) <- runStateT (runErrorT execute) vm
    case result of
        Left err  -> putStrLn $ "Error: " ++ err
        _         -> return () 
    run newState


main :: IO ()
main = run emptyVM { dictionary = primitives }
