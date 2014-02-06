module Forth where

import System.IO
import System.Exit

import Data.Char
import Data.List
import Data.Traversable
import Data.Maybe

import Text.Read (readMaybe)

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

type Val = Integer

data VirtualMachine = VirtualMachine {
      stack :: [Val]
    , dictionary :: [(String, Forth ())]
    -- rstack :: [Val]
    , buffer :: String
    , mode :: VMMode
}

data VMMode = Interpret
            | Compile
    deriving (Show, Eq)

instance Show VirtualMachine where
    show vm = "VM " ++
              "<Stack=" ++ show (stack vm) ++ "> " ++
              "<Buffer=" ++ show (buffer vm) ++ "> " ++
              "<Dictionary=" ++ show ((map fst . dictionary) vm) ++ "> " ++
              "<Mode=" ++ show (mode vm) ++ "> "

type Forth a = ErrorT String (StateT VirtualMachine IO) a

data Expr = Literal Val
          | Word String
     deriving (Show, Eq)


emptyVM :: VirtualMachine
emptyVM = VirtualMachine { stack = [], buffer = "", mode = Interpret, dictionary = [] }


modeFunc :: VMMode -> Forth ()
modeFunc Interpret = interpret
modeFunc Compile   = compile


push :: Val -> Forth ()
push x = get >>= \vm -> put vm { stack = x:stack vm }


pop :: Forth Val
pop = get >>= \vm -> case stack vm of
    []      -> throwError "Pop from an empty stack"
    (x:xs)  -> put vm { stack = xs } >> return x


readNextString :: Forth (Maybe String)
readNextString = get >>= \vm -> if (null . buffer) vm
    then do eof <- liftIO isEOF
            if eof then return Nothing
                   else do x <- liftIO getLine
                           put vm { buffer = map toUpper x }
                           readNextString
    else do let (ex, rest) = break isSpace $ (dropWhile isSpace . buffer) vm
            put vm { buffer = rest }
            return $ Just ex


getNextExpr :: Forth (Maybe Expr)
getNextExpr = readNextString >>= traverse makeExpr


makeExpr :: String -> Forth Expr
makeExpr s = get >>= \vm -> case lookupWord s (dictionary vm) of
    Just _  -> return $ Word s
    Nothing -> case readMaybe s of
        Just n  -> return $ Literal n
        Nothing -> throwError $ "Unrecognized word " ++ s


lookupWord :: String -> [(String, Forth ())] -> Maybe (Forth ())
lookupWord w d = fmap snd $ find ((w ==) . fst) d


interpret :: Forth ()
interpret = getNextExpr >>= \expr -> case expr of
    Just (Literal n) -> push n
    Just (Word w)    -> interpretWord w
    Nothing          -> liftIO exitSuccess


interpretWord :: String -> Forth ()
interpretWord w = get >>= \vm -> fromMaybe 
    (throwError $ "Unrecognized word " ++ w)
    (lookupWord w (dictionary vm))


compile :: Forth ()
compile = do
        s <- readNextString
        case s of
            Nothing   -> liftIO exitSuccess
            Just name -> do action <- accumulate (return ())
                            vm <- get
                            put vm { dictionary = (name, action):(dictionary vm), mode = Interpret }
    where accumulate a = getNextExpr >>= \expr -> case expr of
            Nothing          -> liftIO exitSuccess 
            Just (Word ";")  -> return a
            Just (Word w)    -> accumulate (a >> interpretWord w)
            Just (Literal n) -> accumulate (a >> push n)
