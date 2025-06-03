module Main (main) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text qualified as T

-- import Text.ParserCombinators.ReadP (skipSpaces)

main :: IO ()
main = do
    case t of
        Right str -> putStr str
        Left str -> putStr str

t :: Either String String
t = parseOnly example tmp

example :: Parser String
example =
    string "code:"
        *> ( formatCode
                <$> takeWhile1 (/= '\n')
                <*> (stripLeadingTabs <$> takeWhile1 (const True))
           )
  where
    formatCode :: T.Text -> T.Text -> String
    formatCode lang code = T.unpack $ "```" `T.append` lang `T.append` code `T.append` "```\n"

    stripLeadingTabs :: T.Text -> T.Text
    stripLeadingTabs text = T.unlines $ map processLine (T.lines text)
      where
        processLine :: T.Text -> T.Text
        processLine line = case T.uncons line of
            Just (_, tailText) -> tailText
            Nothing -> T.empty

tmp :: T.Text
tmp = "code:haskell\n tmp = 6\n tmp2 = 6\n     where\n"