module AST where

import Data.Text (Text)

{--
基本方針

情報が多い状態から少ない状態にはできるが、その逆は難しい
そのため、なるべくcosenseの記法をASTに翻訳する
その後、mdに変換するときに、mdの記法にないものをmdの記法に合わせる

--}
{--
ブロック要素は複数行をもつ

一行はインライン複数のインライン要素の集まり
    行の種類
        コマンドライン
        引用
        プレーン
インライン要素は[]の中で表記される
    インライン要素
        リンク
            Enhance (Link "aiu")([- [aiu]])はcosenseでok
            Link (Enhance "aiu")([[- aiu]])は"[Enhance "aiu"]"になる ただ[]で囲まれているだけ
        [ 強調
        , 斜体
        , 打消し
        ]
            これらは順番を入れ替えても結果は同じ
            修飾の一つ以上の組合せと考える
        二重大括弧での強調
        URL
            前 or 後ろに名前を付けることも可能
            Enhanceでリンクの文字の加工も可能
        ハッシュタグ
            Enhanceで修飾可能
        Tex
        コード

--}

data Enhace
    = Bold Word
    | Italic
    | CrossOut
    deriving (Show, Eq)
data InlineElem
    = Enhaced [Enhace] InlineElem
    | Plain Text
    | Link Text
    | HashTag Text
    | URL Text (Maybe Text)
    | Tex Text
    | Code Text
    deriving (Show, Eq)

-- -- | ドキュメント全体を表すAST
-- newtype Document = Document [Block]
--     deriving (Show, Eq)

-- -- | ブロックレベルの要素
-- data Block
--     = -- | 段落 (インライン要素のリスト) 改行までの文を意味する
--       Paragraph [Inline]
--     | -- | コードブロック (言語指定と内容)
--       CodeBlock Text Text
--     | -- | リストアイテム (インデントレベルと子ブロック)
--       UListItem Word [Block]
--     | -- | 空行
--       -- 他のブロック要素 (見出し、引用など) はScrapboxの記法に合わせて追加
--       BlankLine
--     deriving (Show, Eq)
