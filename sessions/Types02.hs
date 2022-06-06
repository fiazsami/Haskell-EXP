type Name = String
type Value = String
type Content = String


dtdHTML :: [Char]
dtdHTML = "<!DOCTYPE html>"


data Attribute = Attribute Name Value | BoolAttr Name deriving (Show, Read)
data Tag = Tag Name | AttrTag Name [Attribute] deriving (Show, Read)
data Document = HTML Tag [Document] | Element Tag Content deriving (Show, Read)


wrap :: [a] -> [a] -> [a] -> [a]
wrap w1 w2 item = w1 ++ item ++ w2
wrOpen :: [Char] -> [Char]
wrOpen = wrap "<" ">"
wrClose :: [Char] -> [Char]
wrClose = wrap "</" ">"
wrBlock :: [Char] -> [Char]
wrBlock = wrap "<" " />"
wrName :: [Char] -> [Char]
wrName = wrap " " "="
wrValue :: [Char] -> [Char]
wrValue = wrap "\"" "\""



attr :: Attribute -> [Char]
attr (Attribute name value) = wrName name ++ wrValue value
attr (BoolAttr name) = " " ++ name

attrs :: Foldable t => t Attribute -> [Char]
attrs = foldr(\x acc -> attr x ++ acc) []

open :: Tag -> [Char]
open (Tag name) = wrOpen name
open (AttrTag name attributes) =  wrOpen name ++ attrs attributes 

close :: Tag -> [Char]
close (Tag name) = wrClose name
close (AttrTag name _) = wrClose name

block :: Tag -> [Char]
block (Tag name) = wrBlock name
block (AttrTag name attributes) = wrBlock name ++ attrs attributes

doc :: Document -> [Char]
doc (Element tag content) = open tag ++ content ++ close tag
doc (HTML tag documents) = open tag ++ foldr(\x acc -> doc x ++ acc) [] documents ++ close tag



-- >>> doc (HTML (AttrTag "section" [Attribute "onclick" "clicked()", Attribute "style" "width:300px"]) [(Element (Tag "p") "hello world"), (Element (Tag "p") "hello world")])
-- "<section> onclick=\"clicked()\" style=\"width:300px\"<p>hello world</p><p>hello world</p></section>"
