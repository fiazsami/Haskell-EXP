type Name = String
type Value = String
type Content = String

data Attribute = Attr Name Value | BoolAttr Name deriving (Show, Read)
data Tag = Tag Name | TagAttr Name [Attribute] deriving (Show, Read)
data Document = Group [Document] | HTML Tag [Document] | Element Tag Content deriving (Show, Read)

attr :: Attribute -> [Char]
attr (Attr name value) = " " ++ name ++ "='" ++ value ++ "'"
attr (BoolAttr name) = " " ++ name

attrs :: Foldable t => t Attribute -> [Char]
attrs = foldr(\x acc -> attr x ++ acc) []

open :: Tag -> [Char]
open (Tag name) = "<" ++ name ++ ">"
open (TagAttr name attributes) = "<" ++ name ++ attrs attributes ++ ">"

close :: Tag -> [Char]
close (Tag name) = "</" ++ name ++ ">"
close (TagAttr name _) = "</" ++ name ++ ">"

block :: Tag -> [Char]
block (Tag name) = "<" ++ name ++  " />"
block (TagAttr name attributes) = "<" ++ name ++ attrs attributes ++ " />"

doc :: Document -> [Char]
doc (Element tag content) = open tag ++ content ++ close tag
doc (HTML tag documents) = open tag ++ doc (Group documents) ++ close tag
doc (Group documents) = foldr(\x acc -> doc x ++ acc) [] documents
