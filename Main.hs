module Main where

import Control.Monad.State
import Control.Monad.Error

import Forth
import Primitives

run :: VirtualMachine -> IO ()
run vm = do
    (result, newState) <- runStateT ((runErrorT . modeFunc . mode) vm) vm
    print $ newState
    case result of
        Left err  -> putStrLn $ "Error: " ++ err
        _         -> return () 
    run newState


main :: IO ()
main = run emptyVM { dictionary = primitives }
