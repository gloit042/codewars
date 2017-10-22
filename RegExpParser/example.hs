module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative ((<$>),(<*>),(<*),(*>))

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any character
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)


parseRegExp :: String -> Maybe RegExp
parseRegExp s = helper $ readP_to_S regExp s
  where
    helper rs = case [a | (a,"") <- rs] of
      []    -> Nothing
      (a:_) -> Just a

regExp :: ReadP RegExp
regExp = strExp +++ (Or <$> (strExp <* char '|') <*> strExp)

strExp :: ReadP RegExp
strExp = charExp
     +++ (Str <$> many1 charExp)

charExp :: ReadP RegExp
charExp = atom +++ (ZeroOrMore <$> atom <* char '*')

atom :: ReadP RegExp
atom = (Normal <$> satisfy (not . flip elem "()*|."))
   +++ (char '.' *> return Any)
   +++ (char '(' *> regExp <* char ')')
