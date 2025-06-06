module AST (Document (..), Block (..), Inline (..)) where

import Control.Lens
import Data.Text (Text)

-- | ドキュメント全体を表すAST
newtype Document = Document [Block]
    deriving (Show, Eq)

-- | ブロックレベルの要素 インデントの量も持つ
data Block
    = -- | 段落 (インライン要素のリスト) 改行までの文を意味する
      Paragraph {_indent :: Word, _line :: [Inline]}
    | -- | コードブロック (言語指定と内容)
      CodeBlock {_indent :: Word, _lang :: Text, _code :: Text}
    | -- | 引用 > コードブロックが来ることはない
      Quotation {_indent :: Word, _quaLine :: Block}
    | -- | 空行
      BlankLine
    deriving (Show, Eq)

data Inline
    = -- | 装飾のないテキスト
      PlainText Text
    | -- | 太字 [[]]もしくは、[* ] * は複数個の可能性
      Bold {_boldLevel :: Word, _boldText :: Text}
    | -- | イタリック [/ ] [/*]
      Italic {_italicBoldLevel :: Word, _italicText :: Text}
    | -- | コードスパン ``
      CodeSpan Text
    | -- | リンク (リンク先と表示テキスト)
      Link {_link :: Text, _linkLabel :: Maybe Text}
    | -- | 数式 [$ ]
      Math Text
    | -- | ハッシュタグ #
      HashTag Text
    | -- | 画像 (画像URLと表示テキスト)
      -- 他のインライン要素 (打ち消し線、下線など) はScrapboxの記法に合わせて追加
      Image {_imageURL :: Text, _linkedURL :: Text}
    | -- | []記法
      PageLink Text
    | -- | コマンドライン $ もしくは %以降
      CommandLine Text
    | -- | 打消し線 [- ]
      CrossOut Text
    | -- | アイコン
      Icon Text
    deriving (Show, Eq)
makeLenses ''Inline
makeLenses ''Block

-- 　aiue[aiue]`aiue`[* aiue]
example1 :: Document
example1 =
    Document
        [ Paragraph
            { _indent = 1
            , _line =
                [ PlainText "aiue"
                , Link{_link = "aiue", _linkLabel = Nothing}
                , CodeSpan "code"
                , Bold{_boldLevel = 1, _boldText = "aiue"}
                ]
            }
        ]

-- 一つインデントされ、リストアイテムになったコードブロック
example2 :: Document
example2 =
    Document
        [ CodeBlock{_indent = 1, _lang = "haskell", _code = "tmp = 0\ntmp2 = 0"}
        ]
