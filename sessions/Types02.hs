type Name = String
type Value = String
type Content = String

data Attribute = Attrs [Attribute] | Attr Name Value | BoolAttr Name deriving (Show, Read)
data Tag = Tag Name | TagAttr Name Attribute deriving (Show, Read)
data HTML = HTML [HTML] | Elements Tag [HTML] | Element Tag Content | Single Tag deriving (Show, Read)

attr :: Attribute -> [Char]
attr (Attrs attrs) = foldr(\x acc -> attr x ++ acc) [] attrs
attr (Attr name value) = " " ++ name ++ "='" ++ value ++ "'"
attr (BoolAttr name) = " " ++ name

open :: Tag -> [Char]
open (Tag name) = "<" ++ name ++ ">"
open (TagAttr name attributes) = "<" ++ name ++ attr attributes ++ ">"

close :: Tag -> [Char]
close (Tag name) = "</" ++ name ++ ">"
close (TagAttr name _) = "</" ++ name ++ ">"

html :: HTML -> [Char]
html (Single tag) = open tag
html (Element tag content) = open tag ++ content ++ close tag
html (Elements tag elements) = open tag ++ html (HTML elements) ++ close tag
html (HTML elements) = foldr(\x acc -> html x ++ acc) [] elements


input :: HTML 
input =
    Elements (TagAttr "html" (Attr "lang" "en")) [
            HTML [
                Elements (Tag "head") [
                    Single (TagAttr "meta" (Attrs [Attr "charset" "UTF-8"])), 
                    Single (TagAttr "meta" (Attrs [Attr "http-equiv" "X-UA-Compatible"])), 
                    Element (Tag "title") "Hello World"
                ], 
                Element (Tag "body") ""
        ]
    ]


-- >>> html input
-- "<html lang='en'><head><meta charset='UTF-8'><meta http-equiv='X-UA-Compatible'><title>Hello World</title></head><body></body></html>"


