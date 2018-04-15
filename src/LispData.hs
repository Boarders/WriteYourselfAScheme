{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
module LispData where

import           Data.Typeable
import qualified Data.Text as T (Text(), unwords, pack, unpack)
import qualified Data.Map as Map
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Monoid ((<>))

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc Env
  | Nil
  | Bool Bool
  deriving (Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal}

type Env = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT Env IO a}
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader Env
           , MonadIO)

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = case val of
  Atom atom        -> atom
  List vals        -> "(" <> (T.unwords $ fmap showVal vals) <> ")"
  Number int       -> T.pack $ show int
  String str       -> str
  Fun _            -> "(internal function)"
  Lambda _ _       -> "(lambda function)"
  Nil              -> "Nil"
  Bool bool        -> case bool of
                        True  -> "#t"
                        False -> "#f"
