module AST (Document (..), Block (..), Inline (..)) where

import Data.Text (Text)

-- | ドキュメント全体を表すAST
newtype Document = Document [Block]
    deriving (Show, Eq)

-- | ブロックレベルの要素
data Block
    = -- | 段落 (インライン要素のリスト) 改行までの文を意味する
      Paragraph [Inline]
    | -- | コードブロック (言語指定と内容)
      CodeBlock Text Text
    | -- | リストアイテム (インデントレベルと子ブロック)
      UListItem Word [Block]
    | -- | 空行
      -- 他のブロック要素 (見出し、引用など) はScrapboxの記法に合わせて追加
      BlankLine
    deriving (Show, Eq)

-- | インラインレベルの要素
data Inline
    = -- | 装飾のないテキスト
      PlainText Text
    | -- | 太字 [* ] * は複数個
      Bold Word Text
    | -- | イタリック [/ ]
      Italic Text
    | -- | コードスパン ``
      CodeSpan Text
    | -- | リンク (リンク先と表示テキスト)
      Link Text (Maybe Text)
    | -- | 数式 [$ ]
      Math Text
    | -- | ハッシュタグ #
      HashTag Text
    | -- | 画像 (画像URLと表示テキスト)
      -- 他のインライン要素 (打ち消し線、下線など) はScrapboxの記法に合わせて追加
      Image Text (Maybe Text)
    | -- | []記法
      RefText Text
    deriving (Show, Eq)

example1 :: Document
example1 =
    Document
        [ UListItem
            1
            [ Paragraph
                [ PlainText "aiue"
                , Link "aiue" Nothing -- [aiue]
                , CodeSpan "code" -- `code`
                , Bold 1 "aiue" -- [* aiue]
                ]
            ]
        ]

-- 一つインデントされ、リストアイテムになったコードブロック
example2 :: Document
example2 =
    Document
        [ UListItem
            1
            [ CodeBlock (Just "haskel") "tmp = 0\ntmp2 = 0"
            ]
        ]