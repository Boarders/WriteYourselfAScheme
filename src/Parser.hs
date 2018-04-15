{-# LANGUAGE TypeApplications #-}
module Parser where

import           Control.Applicative ((*>), (<*))
import           Data.Functor.Identity (Identity(runIdentity))

import           Text.Parsec
    ((<|>), oneOf, letter, digit, many, char, many1, noneOf, sepBy, try, eof, ParseError, parse)
import           Text.Parsec.Text (Parser)
import           Text.Parsec.Expr()
import qualified Text.Parsec.Token as Tok
    (GenTokenParser(..), GenLanguageDef (..), makeTokenParser, whiteSpace)
import qualified Text.Parsec.Language as Lang (emptyDef)
import qualified Data.Text as T (Text(), pack, unpack)

import LispData


lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Tok.identStart      = letter <|> oneOf "-+/*=|&<>"
  , Tok.identLetter     = digit <|> letter <|> oneOf "?+=|&-/"
  , Tok.reservedOpNames = ["'", "\""]
  }

Tok.TokenParser
  { Tok.parens     = lexParens
  , Tok.identifier = lexIdentifier } = lexer

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

-- Parsers for Lisp data constructors

parseAtom :: Parser LispVal
parseAtom = Atom.(T.pack) <$> lexIdentifier

parseText :: Parser LispVal
parseText = fmap (String.(T.pack)) $
              reservedOp "\"" *>
              (many1 $ (noneOf "\""))
              <*  reservedOp "\""

parsePosNumber :: Parser LispVal
parsePosNumber =  Number.(read @Integer) <$> (many digit)

parseNegNumber = Number . negate . read @Integer <$> (
                   char '-' *>
                   many digit)

parseNumber :: Parser LispVal
parseNumber = (parsePosNumber <|> try parseNegNumber)

separators :: Parser Char
separators = char ' ' <|> char '\n'

parseProgram :: Parser LispVal
parseProgram = undefined

parseSExp :: Parser LispVal
parseSExp = List <$> lexParens (parseExpr `sepBy` separators)

parseQuote :: Parser LispVal
parseQuote = (\val -> List [Atom "quote", val]) <$> (
               (reservedOp "\'") *>
               parseExpr )

parseReserved :: Parser LispVal
parseReserved =
      (reservedOp "Nil" *> return Nil)
  <|> (reservedOp "#t"  *> return (Bool True))
  <|> (reservedOp "#f"  *> return (Bool False))

parseExpr :: Parser LispVal
parseExpr =
      parseReserved
  <|> parseNumber
  <|> parseAtom
  <|> parseText
  <|> parseQuote
  <|> parseSExp

parseFile :: Parser a -> Parser a
parseFile p =
  (Tok.whiteSpace lexer) *> p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (parseFile parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (parseFile parseExpr) "<file>"
