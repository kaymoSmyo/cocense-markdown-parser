module InlineTest (inlineTextTest) where

import AST
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

parseCosense :: Text -> InlineElem
parseCosense _ = undefined

{--
テストリスト

[Enhance [page]]
[Enhance #tag]
[[bold]]

--}

-- インライン要素のテスト
inlineTextTest :: Spec
inlineTextTest = describe "InlineElem Tests" $ do
    describe "Enhace" $ do
        it "bold" $ do
            parseCosense "[** abc]" `shouldBe` Enhaced [Bold 2] (Plain "abc")
        it "italic" $ do
            parseCosense "[/ abc]" `shouldBe` Enhaced [Italic] (Plain "abc")
        it "crossout" $ do
            parseCosense "[- abc]" `shouldBe` Enhaced [CrossOut] (Plain "abc")
        it "bold italic crossout" $ do
            parseCosense "[*/- abc]"
                `shouldBe` Enhaced [Bold 1, Italic, CrossOut] (Plain "abc")

    describe "Plain" $ do
        it "plain text" $ do
            parseCosense "abc" `shouldBe` Plain "abc"

    describe "Link" $ do
        it "page link" $ do
            parseCosense "[Page]" `shouldBe` Link "Page"

    describe "HashTag" $ do
        it "hashtag" $ do
            parseCosense "#tag" `shouldBe` HashTag "tag"

    describe "URL" $ do
        it "url only" $ do
            parseCosense "[https://example.com]"
                `shouldBe` URL "https://example.com" Nothing
        it "url with label" $ do
            parseCosense "[https://example.com Example]"
                `shouldBe` URL "https://example.com" (Just "Example")

    describe "Tex" $ do
        it "tex" $ do
            parseCosense "[$ e^{i\\theta} = \\cos \\theta + i \\sin \\theta]"
                `shouldBe` Tex "e^{i\\theta} = \\cos \\theta + i \\sin \\theta"

    describe "Code" $ do
        it "code span" $ do
            parseCosense "`code`" `shouldBe` Code "code"
