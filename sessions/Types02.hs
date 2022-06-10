type Name = String
type Value = String
type Content = String

data Attribute = Attr Name Value | BoolAttr Name deriving (Show, Read)
data Tag = Tag Name | TagAttr Name [Attribute] deriving (Show, Read)
data HTML = Group [HTML] | Elements Tag [HTML] | Element Tag Content | Single Tag deriving (Show, Read)

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

html :: HTML -> [Char]
html (Single tag) = open tag
html (Element tag content) = open tag ++ content ++ close tag
html (Elements tag elements) = open tag ++ html (Group elements) ++ close tag
html (Group group) = foldr(\x acc -> html x ++ acc) [] group

-- >>> html (Elements (TagAttr "html" [Attr "lang" "en"]) [(Group [(Elements (Tag "head") [(Single (TagAttr "meta" [Attr "charset" "UTF-8"])), (Single (TagAttr "meta" [Attr "http-equiv" "X-UA-Compatible"])), (Element (Tag "title") "Hello World")]), Element (Tag "body") ""])])