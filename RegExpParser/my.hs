import System.IO
import Control.Monad
import Data.Either
import qualified Text.Parsec.Char as PC
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Term = Literal Char
          | Sequence [Term]
          | Anything
          | Repeat Term
          | Parens Term
          | Choice Term Term
          | Error
  deriving ( Show )

data RegExp = Normal Char            -- ^ A character that is not in "()*|."
            | Any                   -- ^ Any charater
            | ZeroOrMore RegExp     -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp   -- ^ A choice between 2 regexps
            | Str [RegExp]          -- ^ A sequence of regexps.
            deriving (Show, Eq)





term :: Parser Term
term = buildExpressionParser ops atom where
    ops = [[Postfix (char '*' >> return Repeat)               ]
          ,[Infix   (return sequence)               AssocRight]
          ,[Infix   (char '|' >> return choice)     AssocRight]]
    --atom = msum [ alphaNum , parens term ]
    atom = liftM Literal (noneOf "()*|.")
            <|> (char '.' >> return Anything)
            <|> (liftM Parens $ parens term)


    choice (Choice _ _) _ = Error
    choice _ (Choice _ _) = Error
    choice l r = Choice  l r
    sequence a b = Sequence $ seqTerms a ++ seqTerms b

    seqTerms (Sequence ts) = ts
    seqTerms t = [t]

    parens = between (char '(') (char ')')




parseTerm :: String -> Maybe Term
parseTerm s
    | isLeft e = Nothing
    | otherwise = case e of (Right r) -> Just r
    where e =  parse (term <* eof) "" s

fromTerm :: Term -> Maybe RegExp
fromTerm t
    | hasError t = Nothing
    | otherwise = Just (ctrRegex t)
    where ctrRegex Anything = Any
          ctrRegex (Literal x) = Normal x
          ctrRegex (Sequence s) = Str (map ctrRegex s)
          ctrRegex (Repeat s) = ZeroOrMore (ctrRegex s)
          ctrRegex (Choice l r) = Or (ctrRegex l) (ctrRegex r)
          ctrRegex (Parens x) = ctrRegex x
          hasError Error = True
          hasError (Sequence s) = any hasError s
          hasError (Choice l r) = hasError l || hasError r
          hasError (Repeat s) = hasError s
          hasError (Literal _) = False
          hasError Anything = False
          hasError (Parens x) = hasError x


parseRegExp :: String -> Maybe RegExp
parseRegExp r = parseTerm r >>= fromTerm


