{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative (optional) -- many' and optional are useful
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text qualified as T
import Text.RawString.QQ

main :: IO ()
main = do
    case t of
        Right str -> putStr str
        Left err -> putStrLn $ "Parse error: " ++ err

t :: Either String String
t = parseOnly parseCustomCodeBlock myRawString

{- | Parses lines that start with at least one space, collecting them.
The parsing stops when a line does not start with a space, or EOF is reached.
Each collected line includes its original leading space(s).
-}
parseIndentedCodeContent :: Parser T.Text
parseIndentedCodeContent = do
    let lineStartingWithSpace = do
            mc <- peekChar -- Check the next char without consuming
            case mc of
                Just ' ' -> do
                    -- If it's a space
                    -- Consume the whole line including the leading space(s)
                    line <- takeTill (== '\n')
                    -- Consume the newline, if present. Handles EOF correctly for the last line.
                    _ <- optional (char '\n')
                    return line
                _ -> fail "not an indented line" -- This failure will stop `many'`
    linesList <- many' lineStartingWithSpace
    return $ T.unlines linesList

-- | Parses the custom "code:" block format and converts it to Markdown.
parseCustomCodeBlock :: Parser String
parseCustomCodeBlock = do
    _ <- string "code:"
    lang <- takeWhile1 (\c -> not (isSpace c) && c /= '\n')
    _ <- takeTill (== '\n')
    _ <- char '\n' -- Consume the newline character after the header
    codeBlockRaw <- parseIndentedCodeContent

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

myRawString :: T.Text
myRawString =
    [r|code:json
 "Demo": {
     "type": "stdio",
     "command": "uv",
     "args": [
         "run",
         "--with",
         "mcp[cli]",
         "mcp",
         "run",
         "/home/myubuntu/program/mcp-in-vscode/server.py"
     ],
     "env": {}
|]