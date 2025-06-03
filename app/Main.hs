module Main (main) where

import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text qualified as T

main :: IO ()
main = do
    case t of
        Right str -> putStr str
        Left err -> putStrLn $ "Parse error: " ++ err

t :: Either String String
t = parseOnly parseCustomCodeBlock tmp

-- | Parses the custom "code:" block format and converts it to Markdown.
parseCustomCodeBlock :: Parser String
parseCustomCodeBlock = do
    _ <- string "code:"
    -- Language name: non-space, non-newline characters
    lang <- takeWhile1 (\c -> not (isSpace c) && c /= '\n')
    -- Consume the rest of the header line (optional description)
    _ <- takeTill (== '\n')
    _ <- char '\n' -- Consume the newline character after the header

    -- The rest of the input is the code block content
    codeBlockRaw <- takeText

    let processedCode = stripLeadingSpacePerLine codeBlockRaw
    return $ formatToMarkdown lang processedCode

-- | Removes a single leading space from each line of the input Text.
stripLeadingSpacePerLine :: T.Text -> T.Text
stripLeadingSpacePerLine inputText = T.unlines $ map processLine (T.lines inputText)
  where
    processLine :: T.Text -> T.Text
    processLine line = case T.uncons line of
        Just (' ', rest) -> rest -- If line starts with a space, remove it
        _ -> line -- Otherwise, keep the line as is

-- | Formats the language and code into a Markdown fenced code block.
formatToMarkdown :: T.Text -> T.Text -> String
formatToMarkdown lang code = T.unpack $ T.concat ["```", lang, "\n", code, "```\n"]

tmp :: T.Text
tmp = "code:haskell\n tmp = 6\n tmp2 = 6\n     where\n"