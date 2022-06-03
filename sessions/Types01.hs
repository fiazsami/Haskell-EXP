{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Atrribute = Something String String

data Style = Style String String deriving (Show, Read)
data Tag = OpenStyle String [Style] | Open String | Close String deriving (Show, Read) 


styles :: [Style]
styles = [width "200px", height "400px", bgColor "red", Style "display" "inline-block"]

target :: Tag
target = OpenStyle "p" styles


styleAttr :: [Style] -> [Char]
styleAttr [] = ""
styleAttr styles = " styles='" ++ (foldr (\(Style k v) acc -> (k ++ ":" ++ v ++ ";") ++ acc) [] styles) ++ "'"

openTag :: Tag -> [Char]
openTag (OpenStyle tag styles) = "<" ++ tag ++ styleAttr styles ++ ">"

closeTag :: Tag -> [Char]
closeTag (Close tag) = "</" ++ tag ++ ">"

element :: Tag -> [Char] -> [Char]
element open@(OpenStyle tag styles) content = openTag open ++ content ++ closeTag (Close tag)

-- >>> element target "buying opportunity today!"
-- "<p styles='width:200px;height:400px;background-color:red;display:inline-block;'>buying opportunity today!</p>"

width :: String -> Style
width = Style "width"

height :: String -> Style
height = Style "height"

bgColor :: String -> Style
bgColor = Style "background-color"

