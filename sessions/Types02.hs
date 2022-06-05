{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Key = String
type Value = String
type Name = String
type Content = String

data Attribute = Attribute Key Value | None deriving (Show, Read)
data Tag = Tag Name | AttrTag Name [Attribute] deriving (Show, Read)
data Document = Document [Document] | Node Tag Document | Element Tag Content deriving (Show, Read)

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

-- element :: Element -> [Char]
-- element (Element tag content) = open tag ++ content ++ close tag

doc :: Document -> [Char]
doc (Element tag content) = open tag ++ content ++ close tag
doc (Node tag document) = open tag ++ doc document ++ close tag
doc (Document documents) = foldr(\x acc -> doc x ++ acc) [] documents

-- >>> doc (Node (Tag "div") (Document [(Element (Tag "div") "hello world"), (Element (Tag "div") "hello world")]))
-- "<div><div>hello world</div><div>hello world</div></div>"
