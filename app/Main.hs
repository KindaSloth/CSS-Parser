module Main where

import Text.Parsec
import Data.List (isPrefixOf)
import qualified Data.Functor.Identity

data Selector = GenericSelector String
              | ClassSelector String
              deriving(Eq, Show)

data Rule = Rule String String
    deriving(Eq, Show)

data CssAst = Ruleset [Selector] [Rule]
            | MediaQuery Rule [CssAst]
            deriving(Eq, Show)

rulesetParser :: Parsec String () CssAst
rulesetParser = do
    selectors <- spaces *> (spaces *> (many (noneOf ['\n', '{', ' ', ',']) `sepBy1` (char ',' <* spaces)) <* spaces) <* spaces
    body <- between (char '{') (char '}') (spaces *> (ruleParser `sepBy1` spaces) <* spaces)
    pure $ Ruleset (map chooseSelector selectors) body

ruleParser :: Parsec String () Rule
ruleParser = do
    key   <- spaces *> many (noneOf ['\n', ' ', ':', ',', ';', '}', ')']) <* spaces
    _     <- spaces *> char ':' <* spaces
    value <- spaces *> many (noneOf ['\n', ' ', ':', ',', ';', '}', ')']) <* spaces
    _     <- spaces *> char ';' <* spaces
    pure $ Rule key value

ruleMediaQueryParser :: Parsec String () Rule
ruleMediaQueryParser = do
    key   <- spaces *> many (noneOf ['\n', ' ', ':', ',', ';', '}', ')']) <* spaces
    _     <- spaces *> char ':' <* spaces
    value <- spaces *> many (noneOf ['\n', ' ', ':', ',', ';', '}', ')']) <* spaces
    pure $ Rule key value

chooseSelector :: String -> Selector
chooseSelector selector = if "." `isPrefixOf` selector
    then ClassSelector selector
    else GenericSelector selector

mediaQueryParser :: Parsec String () CssAst
mediaQueryParser = do
    query <- spaces *> string "@media" *> between (spaces *> char '(') (char ')' <* spaces) (spaces *> ruleMediaQueryParser <* spaces) <* spaces
    body <- between (char '{') (char '}') (spaces *> many parser <* spaces)
    pure $ MediaQuery query body

parser :: ParsecT String () Data.Functor.Identity.Identity CssAst
parser = try rulesetParser <|> try mediaQueryParser

parseAll :: Parsec String () [CssAst]
parseAll = spaces *> (parser `sepBy1` spaces) <* spaces

test :: FilePath -> IO (Either ParseError [CssAst])
test file = do
    input <- readFile file
    pure $ parse parseAll "unknown" input

main :: IO ()
main = putStrLn "Hello, Haskell!"
