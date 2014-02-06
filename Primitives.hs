module Primitives where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.IO.Class

import System.Exit

import Forth

primitives :: [(String, Forth ())]
primitives = [ ("+", binaryOp (+))
             , ("-", binaryOp (-))
             , ("*", binaryOp (*))
             , ("/", binaryOp div)
             , ("=", comparisonOp (==))
             , ("BYE", liftIO exitSuccess)
             , (".", liftIO . print =<< pop)
             , (".S", liftIO . print . stack =<< get)
             , ("IF", throwError "Interpreting a compile-only word")
             , ("ELSE", throwError "Interpreting a compile-only word")
             , ("THEN", throwError "Interpreting a compile-only word")
             , (";", throwError "Interpreting a compile-only word")
             , (":", beginCompile)
             ]


binaryOp :: (Val -> Val -> Val) -> Forth ()
binaryOp f = do
    x <- pop
    y <- pop
    push (f y x)


comparisonOp :: (Val -> Val -> Bool) -> Forth ()
comparisonOp f = binaryOp $ \x y -> (toInteger . fromEnum) (f x y)


beginCompile :: Forth ()
beginCompile = get >>= \vm -> case (mode vm) of
    Interpret -> put vm { mode = Compile }
    _         -> throwError "Illegal use of :"
