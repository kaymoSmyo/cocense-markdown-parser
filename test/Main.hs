module Main (main) where

import Control.Lens
import AST (
    Block (
        BlankLine,
        CodeBlock,
        Paragraph,
        Quotation,
        _code,
        _indent,
        _lang,
        _line,
        _quaLine
    ),
    Document (..),
    Inline (
        Bold,
        CodeSpan,
        CommandLine,
        CrossOut,
        HashTag,
        Icon,
        Image,
        Italic,
        Link,
        Math,
        PageLink,
        PlainText,
        _boldLevel,
        _boldText,
        _imageURL,
        _italicBoldLevel,
        _italicText,
        _link,
        _linkLabel,
        _linkedURL
    ),
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Test.HUnit

parseScrapbox :: a
parseScrapbox = undefined
renderMarkdown :: a
renderMarkdown = undefined

-- ヘルパー関数
testParseCocense :: String -> Text -> Document -> Test
testParseCocense name input expected =
    TestLabel name $
        TestCase $
            assertEqual (name ++ " - parse") expected (parseScrapbox input)

-- インラインテキスト要素のテスト
inlineTextTests :: Test
inlineTextTests =
    TestList
        [ testParseCocense
            "plain text"
            "This is plain text"
            (Document [Paragraph{_indent = 0, _line = [PlainText "This is plain text"]}])
        , testParseCocense
            "mixed plain texts"
            "First line\nSecond line"
            ( Document
                [ Paragraph{_indent = 0, _line = [PlainText "First line"]}
                , Paragraph{_indent = 0, _line = [PlainText "Second line"]}
                ]
            )
        ]

-- 装飾付きインライン要素のテスト
inlineDecoratedTests :: Test
inlineDecoratedTests =
    TestList
        [ testParseCocense
            "bold - single asterisk"
            "[* bold]"
            (Document [Paragraph{_indent = 0, _line = [Bold{_boldLevel = 1, _boldText = "bold"}]}])
        , testParseCocense
            "bold - double asterisk"
            "[** very bold]"
            (Document [Paragraph{_indent = 0, _line = [Bold{_boldLevel = 2, _boldText = "very bold"}]}])
        , testParseCocense
            "italic"
            "[/ italic]"
            (Document [Paragraph{_indent = 0, _line = [Italic{_italicBoldLevel = 0, _italicText = "italic"}]}])
        , testParseCocense
            "code span"
            "`code`"
            (Document [Paragraph{_indent = 0, _line = [CodeSpan "code"]}])
        ]

-- -- リンク関連要素のテスト
linkTests :: Test
linkTests =
    TestList
        [ testParseCocense
            "simple link"
            "[link]"
            (Document [Paragraph [Link "link" Nothing]])
        , testParseCocense
            "link with text"
            "[url text]"
            (Document [Paragraph [Link "url" (Just "text")]])
        , testParseCocense
            "image"
            "[https://example.com/image.png]"
            (Document [Paragraph [Image "https://example.com/image.png" Nothing]])
        , testParseCocense
            "image with alt text"
            "[https://example.com/image.png alt text]"
            (Document [Paragraph [Image "https://example.com/image.png" (Just "alt text")]])

        ]

-- []記法のテスト
pageLinkTest :: Test
pageLinkTest =
    TestList
        [ testParseCocense
            "ref text"
            "[reference]"
            (Document [Paragraph [RefText "reference"]])
        , testParseCocense
            "multiple ref texts"
            "[ref1][ref2]"
            (Document [Paragraph [RefText "ref1", RefText "ref2"]])
        , testParseCocense
            "nest ref text"
            "[ref [nest ref]]"
            (Document [Paragraph [RefText "ref [nest ref]"]])
        ]

-- 特殊インライン要素のテスト
specialInlineTests :: Test
specialInlineTests =
    TestList
        [ testParseCocense
            "math"
            "[$ E = mc^2]"
            (Document [Paragraph [Math "E = mc^2"]])
        , testParseCocense
            "hashtag"
            "#tag"
            (Document [Paragraph [HashTag "tag"]])
        ]

-- ブロックレベル要素のテスト
blockTests :: Test
blockTests =
    TestList
        [ testParseCocense
            "code block without language"
            "code:haskell\n main = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        , testParseCocense
            "code block with language"
            "code:haskell\n main = putStrLn \"Hello\""
            (Document [CodeBlock "haskell" "main = putStrLn \"Hello\""])
        , testParseCocense
            "blank line"
            "\n"
            (Document [BlankLine])
        , testParseCocense
            "code block with tab indent"
            "code:haskell\n\tmain = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        , testParseCocense
            "code block with space indent"
            "code:haskell\n main = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        , testParseCocense
            "code block with full-width space indent"
            "code:haskell\n　main = pure ()"
            (Document [CodeBlock "haskell" "main = pure ()"])
        ]

-- リスト要素のテスト
listTests :: Test
listTests =
    TestList
        [ testParseCocense
            "single list item"
            " item"
            (Document [UListItem 1 [Paragraph [PlainText "item"]]])
        , testParseCocense
            "multiple list items"
            " item1\n item2"
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 1 [Paragraph [PlainText "item2"]]
                ]
            )
        , testParseCocense
            "nested list items"
            " item1\n  item2\n   item3"
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 2 [Paragraph [PlainText "item2"]]
                , UListItem 3 [Paragraph [PlainText "item3"]]
                ]
            )
        , testParseCocense
            "list items with mixed indentation"
            " item1\n\titem2\n　item3" -- スペース、タブ、全角スペース
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 1 [Paragraph [PlainText "item2"]]
                , UListItem 1 [Paragraph [PlainText "item3"]]
                ]
            )
        , testParseCocense
            "nested list with mixed indentation"
            " item1\n\t\titem2\n　　　item3" -- レベル1、2、3をそれぞれ異なる種類のインデント
            ( Document
                [ UListItem 1 [Paragraph [PlainText "item1"]]
                , UListItem 2 [Paragraph [PlainText "item2"]]
                , UListItem 3 [Paragraph [PlainText "item3"]]
                ]
            )
        , testParseCocense
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
        [ testParseCocense
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

-- コマンドラインのテスト
commandLineTests :: Test
commandLineTests = 
    TestList
        [ testParseCocense
            "command line with dollar"
            "$ ls -la"
            (Document [Paragraph{_indent = 0, _line = [CommandLine "ls -la"]}])
        , testParseCocense
            "command line with percent"
            "% npm install"
            (Document [Paragraph{_indent = 0, _line = [CommandLine "npm install"]}])
        ]

-- 打消し線のテスト
crossOutTests :: Test
crossOutTests =
    TestList
        [ testParseCocense
            "cross out text"
            "[- deleted text]"
            (Document [Paragraph{_indent = 0, _line = [CrossOut "deleted text"]}])
        ]

-- アイコンのテスト
iconTests :: Test
iconTests =
    TestList
        [ testParseCocense
            "icon"
            "[icon.png]"
            (Document [Paragraph{_indent = 0, _line = [Icon "icon.png"]}])
        ]

-- クォーテーションのテスト
quotationTests :: Test
quotationTests =
    TestList
        [ testParseCocense
            "simple quotation"
            "> quoted text"
            (Document [Quotation [PlainText "quoted text"]])
        ]

-- tests の更新
tests :: Test
tests = TestList []

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then putStrLn "All tests passed!"
        else error "Some tests failed!"
