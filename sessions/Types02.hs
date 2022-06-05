{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Key = String
type Value = String
type Name = String
type Content = String

data Attribute = Attribute Key Value | None deriving (Show, Read)
data Tag = Tag Name | AttrTag Name [Attribute] deriving (Show, Read)
data Element = Element Tag Content deriving (Show, Read)
data Document = Node [Document] | Leaf Element deriving (Show, Read)

attrStr :: Attribute -> [Char]
attrStr (Attribute key value) = " " ++ key ++ "='" ++ value ++ "'"

attrsStr :: Foldable t => t Attribute -> [Char]
attrsStr = foldr(\x acc -> attrStr x ++ acc) []

open :: Tag -> [Char]
open (Tag name) = "<" ++ name ++ ">"
open (AttrTag name attrs) = "<" ++ name ++ attrsStr attrs ++ ">"

close :: Tag -> [Char]
close (Tag name) = "</" ++ name ++ ">"

block :: Tag -> [Char]
block (AttrTag name attrs) = "<" ++ name ++ attrsStr attrs ++ " />"

element :: Element -> [Char]
element (Element tag content) = open tag ++ content ++ close tag

node :: Document -> [Char]
node (Leaf el) = element el
node (Node docs) = foldr(\x acc -> node x ++ acc) [] docs

-- >>> node (Node [(Leaf (Element (Tag "div") "hello world")), (Leaf (Element (Tag "div") "hello world"))])
-- "<div>hello world</div><div>hello world</div>"
