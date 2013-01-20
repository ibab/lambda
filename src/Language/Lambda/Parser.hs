{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Lambda.Parser (parseLambda) where

import Language.Lambda
import Text.Parsec

lexpr = try term
    <|> (try $ between (char '(') (char ')') application)
    <|> abstraction
    <?> "lambda expression"

term = do
  var <- variable
  return (Term var)

variable = do
  result <- many1 (noneOf "()λ\\,. \n\t")
  return (Var result)

application = do
  skipMany space
  exprs <- lexpr `endBy` (many space)
  skipMany space
  return (foldl1 (:$) exprs)

commaSep = do
  skipMany space
  char ','
  skipMany space

abstraction = do
  oneOf "λ\\"
  skipMany space
  vars <- variable `sepBy` commaSep
  skipMany space
  char '.'
  skipMany space
  body <- application
  return (foldr (:->) body vars)

parseLambda = parse application "(lambda)"
