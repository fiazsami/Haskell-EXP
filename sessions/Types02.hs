
type Key = String
type Value = String
type Name = String
type Content = String

data Attribute = Attribute Key Value deriving (Show, Read)
data Tag = Tag Name | AttrTag Name [Attribute] deriving (Show, Read)
data Document = Document [Document] | HTML Tag [Document] | Element Tag Content deriving (Show, Read)

attr :: Attribute -> [Char]
attr (Attribute key value) = " " ++ key ++ "='" ++ value ++ "'"

attrs :: Foldable t => t Attribute -> [Char]
attrs = foldr(\x acc -> attr x ++ acc) []

open :: Tag -> [Char]
open (Tag name) = "<" ++ name ++ ">"
open (AttrTag name attributes) = "<" ++ name ++ attrs attributes ++ ">"

close :: Tag -> [Char]
close (Tag name) = "</" ++ name ++ ">"
close (AttrTag name _) = "</" ++ name ++ ">"

block :: Tag -> [Char]
block (Tag name) = "<" ++ name ++  " />"
block (AttrTag name attributes) = "<" ++ name ++ attrs attributes ++ " />"

doc :: Document -> [Char]
doc (Element tag content) = open tag ++ content ++ close tag
doc (HTML tag documents) = open tag ++ foldr(\x acc -> doc x ++ acc) [] documents ++ close tag
doc (Document documents) = foldr(\x acc -> doc x ++ acc) [] documents

-- >>> doc (Document [(Element (Tag "div") "hello world"), (Element (Tag "div") "hello world")])
-- "<div>hello world</div><div>hello world</div>"

-- >>> doc (HTML (AttrTag "section" [Attribute "onclick" "clicked()", Attribute "style" "width:300px"]) [(Element (Tag "p") "hello world"), (Element (Tag "p") "hello world")])
-- "<section onclick='clicked()' style='width:300px'><p>hello world</p><p>hello world</p></section>"
