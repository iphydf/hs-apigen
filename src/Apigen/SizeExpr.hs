{-# LANGUAGE OverloadedStrings #-}
-- | C size expressions for buffer sizes, such as
-- @max(width, abs(ystride)) * height@.
module Apigen.SizeExpr
    ( SizeExpr (..)
    , parseSizeExpr
    , sizeExprVars
    ) where

import           Data.Char (isAlphaNum, isDigit, isSpace)
import           Data.Text (Text)
import qualified Data.Text as Text

data SizeExpr
    = SizeNum Integer
    | SizeVar Text
    | SizeOp Char SizeExpr SizeExpr  -- ^ @+@, @-@, @*@ or @/@
    | SizeCall Text [SizeExpr]       -- ^ @max@ or @abs@
    deriving (Show, Eq)

sizeExprVars :: SizeExpr -> [Text]
sizeExprVars e = case e of
    SizeNum _     -> []
    SizeVar v     -> [v]
    SizeOp _ a b  -> sizeExprVars a ++ sizeExprVars b
    SizeCall _ as -> concatMap sizeExprVars as

data Token = TNum Integer | TIdent Text | TSym Char
    deriving (Show, Eq)

parseSizeExpr :: Text -> Maybe SizeExpr
parseSizeExpr text = do
    tokens <- tokenize text
    case sum' tokens of
        Just (e, []) -> Just e
        _            -> Nothing

tokenize :: Text -> Maybe [Token]
tokenize t = case Text.uncons t of
    Nothing -> Just []
    Just (c, rest)
        | isSpace c -> tokenize rest
        | isDigit c ->
            let (digits, after) = Text.span isDigit t
            in (TNum (read (Text.unpack digits)) :) <$> tokenize after
        | isAlphaNum c || c == '_' ->
            let (ident, after) = Text.span (\x -> isAlphaNum x || x == '_') t
            in (TIdent ident :) <$> tokenize after
        | c `elem` ("+-*/()," :: String) -> (TSym c :) <$> tokenize rest
        | otherwise -> Nothing

-- Left-associative chains of the operators of one precedence level.
chain :: [Char] -> ([Token] -> Maybe (SizeExpr, [Token])) -> [Token] -> Maybe (SizeExpr, [Token])
chain ops operand tokens = operand tokens >>= uncurry go
  where
    go lhs (TSym op : rest) | op `elem` ops = do
        (rhs, after) <- operand rest
        go (SizeOp op lhs rhs) after
    go lhs rest = Just (lhs, rest)

sum' :: [Token] -> Maybe (SizeExpr, [Token])
sum' = chain "+-" product'

product' :: [Token] -> Maybe (SizeExpr, [Token])
product' = chain "*/" atom

atom :: [Token] -> Maybe (SizeExpr, [Token])
atom tokens = case tokens of
    TNum n : rest -> Just (SizeNum n, rest)
    TIdent f : TSym '(' : rest -> do
        (args, after) <- arguments rest
        Just (SizeCall f args, after)
    TIdent v : rest -> Just (SizeVar v, rest)
    TSym '(' : rest -> do
        (e, after) <- sum' rest
        case after of
            TSym ')' : after' -> Just (e, after')
            _                 -> Nothing
    _ -> Nothing
  where
    arguments ts = do
        (e, after) <- sum' ts
        case after of
            TSym ',' : rest -> do
                (es, after') <- arguments rest
                Just (e : es, after')
            TSym ')' : rest -> Just ([e], rest)
            _ -> Nothing
