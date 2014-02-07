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


push :: Val -> Forth ()
push x = modify $ \vm -> vm { stack = x:stack vm }


pop :: Forth Val
pop = get >>= \vm -> case stack vm of
    []      -> throwError "Pop from an empty stack"
    (x:xs)  -> put vm { stack = xs } >> return x


readNextString :: Forth String
readNextString = get >>= \vm -> if (null . buffer) vm
    then do eof <- liftIO isEOF
            if eof then liftIO exitSuccess
                   else do x <- liftIO getLine
                           put vm { buffer = map toUpper x }
                           readNextString
    else do let (ex, rest) = break isSpace $ (dropWhile isSpace . buffer) vm
            put vm { buffer = rest }
            return ex


getNextExpr :: Forth Expr
getNextExpr = makeExpr =<< readNextString


makeExpr :: String -> Forth Expr
makeExpr s = get >>= \vm -> case lookupWord s (dictionary vm) of
    Just _  -> return $ Word s
    Nothing -> case readMaybe s of
        Just n  -> return $ Literal n
        Nothing -> throwError $ "Unrecognized word " ++ s


lookupWord :: String -> [(String, Forth ())] -> Maybe (Forth ())
lookupWord w d = fmap snd $ find ((w ==) . fst) d


interpret :: Forth ()
interpret = getNextExpr >>= defaultAction


interpretWord :: String -> Forth ()
interpretWord w = get >>= \vm -> fromMaybe 
    (throwError $ "Unrecognized word " ++ w)
    (lookupWord w (dictionary vm))


interpretIf :: Forth () -> Forth () -> Forth ()
interpretIf tb fb = pop >>= \x -> if x /= 0 then tb else fb


compile :: Forth ()
compile = do
        name <- readNextString
        action <- accumulate (return ())
        modify $ \vm -> vm { dictionary = (name, action):dictionary vm, mode = Interpret }
    where accumulate a = getNextExpr >>= \expr -> case expr of
            Word ";"  -> return a
            Word "IF" -> compileIf >>= \i -> accumulate (a >> i)
            e         -> accumulate (a >> defaultAction e)


compileIf :: Forth (Forth ())
compileIf = accumulate (return (), return ()) True
    where accumulate a q = getNextExpr >>= \expr -> case expr of
            Word "IF"   -> compileIf >>= \i -> accumulate (choiceApply a (>> i) q) q
            Word "THEN" -> return $ uncurry interpretIf a
            Word "ELSE" -> accumulate a False
            e           -> accumulate (choiceApply a (>> defaultAction e) q) q


defaultAction :: Expr -> Forth ()
defaultAction (Word w)    = interpretWord w
defaultAction (Literal n) = push n


choiceApply :: (a, a) -> (a -> a) -> Bool -> (a, a)
choiceApply (x, y) f True  = (f x, y)
choiceApply (x, y) f False = (x, f y)


execute :: Forth ()
execute = get >>= \vm -> case mode vm of
    Interpret -> interpret
    Compile   -> compile
