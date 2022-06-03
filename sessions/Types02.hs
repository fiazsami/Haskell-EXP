{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Tag = Open String | Close String deriving (Show, Read)
data Element = Element Tag [Element] | Content String deriving (Show, Read)


open'p = Open "p"
close'p = Close "P"

html = Element (Open "p") [Content "hello"]

openTag :: Tag -> [Char]
openTag (Open t) = "<" ++ t ++ ">"
closeTag :: Tag -> [Char]
closeTag (Close t) = "</" ++ t ++ ">"



element :: Element -> [Char]
element (Element open@(Open tag) [Content content]) = openTag open ++ content ++ closeTag (Close tag)

-- >>> element html
-- "<p>hello</p>"
