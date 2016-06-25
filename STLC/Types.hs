{-# LANGUAGE DataKinds #-}

module STLC.Types where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List

type Name = String

data Term = B Bool
          | N Integer
          | V Name
          | Ann  :. Term
          | Term :@ Term deriving (Eq)

instance Show Term where
  show (B b) = show b
  show (N n) = show n
  show (V v) = v
  show (a :. t) = "(\\" ++ show a ++ " -> " ++ show t ++ ")"

data Type = TBool | TNat
          | Type :-> Type
          | Hole deriving Eq

data Ann = Name ::: Type deriving (Eq)

instance Show Type where
  show TBool = "bool"
  show TNat = "nat"
  show (a :-> b) = "(" ++ show a ++ "->" ++ show b ++ ")"
  show Hole = "_"

infixr :->

instance Show Ann where
  show (t ::: ty) = t ++ ":" ++ show ty 

infixr :::

type Context = [Ann]

type TypeResult = Either TypeError Type

data TypeError = TError String Term deriving (Show) -- error result and term involved

typeOf :: Context -> Term -> TypeResult

typeOf _ (B _)   = Right TBool
typeOf _ (N _)   = Right TNat
typeOf cxt (V n) = case find (\(n' ::: ty) -> n' == n) cxt of
  Nothing -> Left (TError "variable not in context" (V n))
  Just (_ ::: ty) -> Right ty
typeOf cxt ((n ::: ty) :. term) = do
  ety <- typeOf ((n ::: ty) : cxt) term
  return (ty :-> ety)
typeOf cxt (e1 :@ e2) = do
  e1ty <- typeOf cxt e1
  e2ty <- typeOf cxt e2
  case e1ty of
    (a :-> b) -> if e2ty == a then return b else fail "bad application"
    _ -> fail "bad application"

cxt :: Context
cxt = [
       "add" ::: TNat :-> TNat :-> TNat, "sub" ::: TNat :-> TNat :-> TNat,
       "mul" ::: TNat :-> TNat :-> TNat, "if" ::: TBool :-> TBool :-> TBool,
       "and" ::: TBool :-> TBool :-> TBool, "or" ::: TBool :-> TBool :-> TBool
      ]
