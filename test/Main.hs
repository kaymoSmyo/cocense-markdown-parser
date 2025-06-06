module Main (main) where

import AST
import Control.Lens
import Data.Text (Text, append)
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
            "plain text with newline"
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
            (Document [Paragraph{_indent = 0, _line = [Link{_link = "link", _linkLabel = Nothing}]}])
        , testParseCocense
            "link with text"
            "[url text]"
            (Document [Paragraph{_indent = 0, _line = [Link{_link = "url", _linkLabel = Just "text"}]}])
        , testParseCocense
            "image with URL only"
            "[https://example.com/image.png]"
            (Document [Paragraph{_indent = 0, _line = [Image{_imageURL = "https://example.com/image.png", _linkedURL = ""}]}])
        , testParseCocense
            "image with URL and linkedURL"
            "[https://example.com/image.png alt text]"
            (Document [Paragraph{_indent = 0, _line = [Image{_imageURL = "https://example.com/image.png", _linkedURL = "alt text"}]}])

        ]

-- []記法のテスト
pageLinkTest :: Test
pageLinkTest =
    TestList
        [ testParseCocense
            "page link"
            "[reference]"
            (Document [Paragraph{_indent = 0, _line = [PageLink "reference"]}])
        , testParseCocense
            "multiple page links"
            "[ref1][ref2]"
            (Document [Paragraph{_indent = 0, _line = [PageLink "ref1", PageLink "ref2"]}])
        , testParseCocense
            "page link with nested brackets in text"
            "[ref [nest ref]]"
            (Document [Paragraph{_indent = 0, _line = [PageLink "ref [nest ref]"]}])
        ]

-- 特殊インライン要素のテスト
specialInlineTests :: Test
specialInlineTests =
    TestList
        [ testParseCocense
            "math"
            "[$ E = mc^2]"
            (Document [Paragraph{_indent = 0, _line = [Math "E = mc^2"]}])
        , testParseCocense
            "hashtag"
            "#tag"
            (Document [Paragraph{_indent = 0, _line = [HashTag "tag"]}])
        ]

-- ブロックレベル要素のテスト
blockTests :: Test
blockTests =
    TestList
        [ testParseCocense
            "code block with unspecified language"
            "code:haskell\n main = pure ()"
            (Document [CodeBlock{_indent = 0, _lang = "haskell", _code = "main = pure ()"}])
        , testParseCocense
            "code block with specified language"
            "code:haskell\n main = putStrLn \"Hello\""
            (Document [CodeBlock{_indent = 0, _lang = "haskell", _code = "main = putStrLn \"Hello\""}])
        , testParseCocense
            "blank line"
            "\n"
            (Document [BlankLine])
        , testParseCocense
            "code block with tab-indented content line"
            "code:haskell\n\tmain = pure ()"
            (Document [CodeBlock{_indent = 0, _lang = "haskell", _code = "main = pure ()"}])
        , testParseCocense
            "code block with space-indented content line"
            "code:haskell\n main = pure ()"
            (Document [CodeBlock{_indent = 0, _lang = "haskell", _code = "main = pure ()"}])
        , testParseCocense
            "code block with full-width space-indented content line"
            "code:haskell\n　main = pure ()"
            (Document [CodeBlock{_indent = 0, _lang = "haskell", _code = "main = pure ()"}])
        ]

-- リスト要素のテスト
listTests :: Test
listTests =
    TestList
        [ testParseCocense
            "single list item"
            " item"
            (Document [Paragraph{_indent = 1, _line = [PlainText "item"]}])
        , testParseCocense
            "multiple list items"
            " item1\n item2"
            ( Document
                [ Paragraph{_indent = 1, _line = [PlainText "item1"]}
                , Paragraph{_indent = 1, _line = [PlainText "item2"]}
                ]
            )
        , testParseCocense
            "nested list items"
            " item1\n  item2\n   item3"
            ( Document
                [ Paragraph{_indent = 1, _line = [PlainText "item1"]}
                , Paragraph{_indent = 2, _line = [PlainText "item2"]}
                , Paragraph{_indent = 3, _line = [PlainText "item3"]}
                ]
            )
        , testParseCocense
            "list items with mixed indentation"
            " item1\n\titem2\n　item3" -- スペース、タブ、全角スペース
            ( Document
                [ Paragraph{_indent = 1, _line = [PlainText "item1"]}
                , Paragraph{_indent = 1, _line = [PlainText "item2"]}
                , Paragraph{_indent = 1, _line = [PlainText "item3"]}
                ]
            )
        , testParseCocense
            "nested list with mixed indentation"
            " item1\n\t\titem2\n　　　item3" -- レベル1、2、3をそれぞれ異なる種類のインデント
            ( Document
                [ Paragraph{_indent = 1, _line = [PlainText "item1"]}
                , Paragraph{_indent = 2, _line = [PlainText "item2"]}
                , Paragraph{_indent = 3, _line = [PlainText "item3"]}
                ]
            )
        , testParseCocense
            "list with irregular mixed indentation"
            " item1\n 　item2\n　 \titem3" -- 半角スペース+全角スペース、全角スペース+半角スペース+タブの混在
            ( Document
                [ Paragraph{_indent = 1, _line = [PlainText "item1"]}
                , Paragraph{_indent = 2, _line = [PlainText "item2"]}
                , Paragraph{_indent = 3, _line = [PlainText "item3"]}
                ]
            )
        ]

-- 複合的なドキュメントのテスト
complexDocumentTests :: Test
complexDocumentTests =
    TestList
        [ testParseCocense
            "complex document with mixed elements"
            ( "Title\n"
                `append` " [* bold] and [/ italic]"
                `append` "\tcode:haskell"
                `append` "　 main = pure ()"
                `append` " #tag"
            )
            ( Document
                [ Paragraph{_indent = 0, _line = [PlainText "Title"]} -- Assuming Title is not indented
                , Paragraph{_indent = 1, _line = [ Bold{_boldLevel = 1, _boldText = "bold"}, PlainText " and ", Italic{_italicBoldLevel = 0, _italicText = "italic"}]} -- Assuming this line is indented by 1
                , CodeBlock{_indent = 1, _lang = "haskell", _code = " main = pure ()"} -- Assuming CodeBlock is part of the list item, hence indent 1. This might need further clarification based on parser logic.
                , Paragraph{_indent = 1, _line = [HashTag "tag"]} -- Assuming this line is indented by 1
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
            (Document [Quotation{_indent = 0, _quaLine = Paragraph{_indent = 0, _line = [PlainText "quoted text"]}}])
        , testParseCocense
            "quotation with indented paragraph"
            ">  indented quoted text"
            (Document [Quotation{_indent = 0, _quaLine = Paragraph{_indent = 1, _line = [PlainText "indented quoted text"]}}])
        , testParseCocense
            "multiple consecutive quotation lines"
            "> line1\n> line2"
            ( Document
                [ Quotation{_indent = 0, _quaLine = Paragraph{_indent = 0, _line = [PlainText "line1"]}}
                , Quotation{_indent = 0, _quaLine = Paragraph{_indent = 0, _line = [PlainText "line2"]}}
                ]
            )
        ]

-- tests の更新
tests :: Test
tests =
    TestList
        [ inlineTextTests
        , inlineDecoratedTests
        , linkTests
        , pageLinkTest
        , specialInlineTests
        , blockTests
        , listTests
        , complexDocumentTests
        , commandLineTests
        , crossOutTests
        , iconTests
        , quotationTests
        ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then putStrLn "All tests passed!"
        else error "Some tests failed!"
