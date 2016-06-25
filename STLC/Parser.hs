module STLC.Parser where

import STLC.Types
import Text.Parsec
import Data.Char
import Text.Parsec.Char

forbidden :: String
forbidden = "\\ \n\t().:->"

whitespace = many (oneOf " \n\t")

name :: Parsec String () Term
name = do
  c <- many1 (noneOf forbidden)
  return (V c)

num :: Parsec String () Term
num = do
  c <- many1 digit
  let c' = zipWith (*) (map digitToInt $ reverse c) [ n ^ 10 | n <- [0..] ]
  return (N $ toInteger $ sum c')

bool :: Parsec String () Term
bool = (string "true" *> pure (B True)) <|> (string "false" *> pure (B False))

lambda :: Parsec String () Term
lambda = do
  whitespace *> char '\\'
  (V var) <- whitespace *> name
  whitespace *> char ':'
  t <- whitespace *> ty
  whitespace *> string "->" <* whitespace
  e <- term
  return ((var ::: t) :. e)

basety :: Parsec String () Type
basety = (string "nat" *> pure TNat) <|> (string "bool" *> pure TBool)

ty :: Parsec String () Type
ty = (char '(' *> ty <* whitespace <* char ')')  <|> do 
        x <- basety 
        pure x

baseterm :: Parsec String () Term
baseterm = (lambda <|> name) <|> (char '(' *> term <* char ')') <|> num <|> bool

term :: Parsec String () Term
term = do
  (t:ts) <- many1 (whitespace *> baseterm <* whitespace)
  return (foldr (:@) t ts)
