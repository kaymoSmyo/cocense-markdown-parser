module Main (main) where

import AST
import Data.Text (Text, append)
import Test.HUnit

parseScrapbox :: a
parseScrapbox = undefined
renderMarkdown :: a
renderMarkdown = undefined

-- ヘルパー関数
testParseAndRender :: String -> Text -> Document -> Test
testParseAndRender name input expected =
    TestLabel name $
        TestList
            [ TestCase $ assertEqual (name ++ " - parse") expected (parseScrapbox input)
            , TestCase $ assertEqual (name ++ " - render") input (renderMarkdown expected)
            ]

-- インラインテキスト要素のテスト
inlineTextTests :: Test
inlineTextTests =
    TestList
        [ testParseAndRender
            "plain text"
            "This is plain text"
            (Document [Paragraph [PlainText "This is plain text"]])
        , testParseAndRender
            "mixed plain texts"
            "First line\nSecond line"
            ( Document
                [ Paragraph [PlainText "First line"]
                , Paragraph [PlainText "Second line"]
                ]
            )
        ]

-- 装飾付きインライン要素のテスト
inlineDecoratedTests :: Test
inlineDecoratedTests =
    TestList
        [ testParseAndRender
            "bold - single asterisk"
            "[* bold]"
            (Document [Paragraph [Bold 1 "bold"]])
        , testParseAndRender
            "bold - double asterisk"
            "[** very bold]"
            (Document [Paragraph [Bold 2 "very bold"]])
        , testParseAndRender
            "italic"
            "[/ italic]"
            (Document [Paragraph [Italic "italic"]])
        , testParseAndRender
            "code span"
            "`code`"
            (Document [Paragraph [CodeSpan "code"]])
        ]

-- リンク関連要素のテスト
linkTests :: Test
linkTests =
    TestList
        [ testParseAndRender
            "simple link"
            "[link]"
            (Document [Paragraph [Link "link" Nothing]])
        , testParseAndRender
            "link with text"
            "[url text]"
            (Document [Paragraph [Link "url" (Just "text")]])
        , testParseAndRender
            "image"
            "[https://example.com/image.png]"
            (Document [Paragraph [Image "https://example.com/image.png" Nothing]])
        , testParseAndRender
            "image with alt text"
            "[https://example.com/image.png alt text]"
            (Document [Paragraph [Image "https://example.com/image.png" (Just "alt text")]])
        ]

-- []記法のテスト
refTextTest :: Test
refTextTest =
    TestList
        [ testParseAndRender
            "ref text"
            "[reference]"
            (Document [Paragraph [RefText "reference"]])
        , testParseAndRender
            "multiple ref texts"
            "[ref1][ref2]"
            (Document [Paragraph [RefText "ref1", RefText "ref2"]])
        , testParseAndRender
            "nest ref text"
            "[ref [nest ref]]"
            (Document [Paragraph [RefText "ref [nest ref]"]])
        ]

-- 特殊インライン要素のテスト
specialInlineTests :: Test
specialInlineTests =
    TestList
        [ testParseAndRender
            "math"
            "[$ E = mc^2]"
            (Document [Paragraph [Math "E = mc^2"]])
        , testParseAndRender
            "hashtag"
            "#tag"
            (Document [Paragraph [HashTag "tag"]])
        ]

-- ブロックレベル要素のテスト
blockTests :: Test
blockTests =
    TestList
        [ testParseAndRender
            "code block without language"
            "code:haskell\n main = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        , testParseAndRender
            "code block with language"
            "code:haskell\n main = putStrLn \"Hello\""
            (Document [CodeBlock "haskell" "main = putStrLn \"Hello\""])
        , testParseAndRender
            "blank line"
            "\n"
            (Document [BlankLine])
        , testParseAndRender
            "code block with tab indent"
            "code:haskell\n\tmain = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        , testParseAndRender
            "code block with space indent"
            "code:haskell\n main = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        , testParseAndRender
            "code block with full-width space indent"
            "code:haskell\n　main = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        ]

-- リスト要素のテスト
listTests :: Test
listTests =
    TestList
        [ testParseAndRender
            "single list item"
            " item"
            (Document [UListItem 1 [Paragraph [PlainText "item"]]])
        , testParseAndRender
            "multiple list items"
            " item1\n item2"
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 1 [Paragraph [PlainText "item2"]]
                ]
            )
        , testParseAndRender
            "nested list items"
            " item1\n  item2\n   item3"
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 2 [Paragraph [PlainText "item2"]]
                , UListItem 3 [Paragraph [PlainText "item3"]]
                ]
            )
        , testParseAndRender
            "list items with mixed indentation"
            " item1\n\titem2\n　item3" -- スペース、タブ、全角スペース
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 1 [Paragraph [PlainText "item2"]]
                , UListItem 1 [Paragraph [PlainText "item3"]]
                ]
            )
        , testParseAndRender
            "nested list with mixed indentation"
            " item1\n\t\titem2\n　　　item3" -- レベル1、2、3をそれぞれ異なる種類のインデント
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 2 [Paragraph [PlainText "item2"]]
                , UListItem 3 [Paragraph [PlainText "item3"]]
                ]
            )
        , testParseAndRender
            "list with irregular mixed indentation"
            " item1\n 　item2\n　 \titem3" -- 半角スペース+全角スペース、全角スペース+半角スペース+タブの混在
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 2 [Paragraph [PlainText "item2"]]
                , UListItem 3 [Paragraph [PlainText "item3"]]
                ]
            )
        ]

-- 複合的なドキュメントのテスト
complexDocumentTests :: Test
complexDocumentTests =
    TestList
        [ testParseAndRender
            "mixed elements"
            ( "Title\n"
                `append` " [* bold] and [/ italic]"
                `append` "\tcode:haskell"
                `append` "　 main = pure ()"
                `append` " #tag"
            )
            ( Document
                [ Paragraph [PlainText "Title"]
                , UListItem
                    1
                    [ Paragraph
                        [ Bold 1 "bold"
                        , PlainText " and "
                        , Italic "italic"
                        ]
                    , CodeBlock "haskell" " main = pure ()"
                    , Paragraph [HashTag "tag"]
                    ]
                ]
            )
        ]

-- メインのテストスイート
tests :: Test
tests =
    TestList
        [ TestLabel "Inline Text Tests" inlineTextTests
        , TestLabel "Inline Decorated Tests" inlineDecoratedTests
        , TestLabel "Link Tests" linkTests
        , TestLabel "Special Inline Tests" specialInlineTests
        , TestLabel "Block Tests" blockTests
        , TestLabel "List Tests" listTests
        , TestLabel "Complex Document Tests" complexDocumentTests
        , TestLabel "[] Tests" refTextTest
        ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then putStrLn "All tests passed!"
        else error "Some tests failed!"
