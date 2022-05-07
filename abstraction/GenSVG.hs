-- SVG PROOF OF CONCEPT

import System.IO ()

writeTo :: FilePath -> String -> IO ()
writeTo path html = do
    writeFile path html

output :: String -> IO ()
output  = writeTo "output/svg.html"

linkAttrs :: Foldable t => t ([Char], [Char]) -> [Char]
linkAttrs = link ""
    where
        link a list = foldl (\a x -> " " ++ toAttr x ++ a) a list

toAttr :: ([Char], [Char]) -> [Char]
toAttr (k,v) = k ++ "='" ++ v ++ "'"

openTag :: [Char] -> [([Char], [Char])] -> [Char]
openTag t a = "<" ++ t ++ linkAttrs a ++ ">"
closeTag :: [Char] -> [Char]
closeTag t = "</" ++ t ++ ">"

element :: [Char] -> [([Char], [Char])] -> [Char] -> [Char]
element t a c = openTag t a ++ c ++ closeTag t
block :: [Char] -> [([Char], [Char])] -> [Char]
block t a = "<" ++ t ++ linkAttrs a ++ " />"

attr :: a -> b -> (a, b)
attr k v = (k, v)

svgDoc :: [Char] -> [Char]
svgDoc = element "svg" [
        attr "version" "1.1",
        attr "width" "300",
        attr "height" "200",
        attr "xlmns" "http://ww.w3.org/2000/svg"]


redRect :: [Char]
redRect = block "rect" [
    attr "width" "100%",
    attr "height" "100%",
    attr "fill" "red"]

greenCircle :: [Char]
greenCircle = block "circle" [
    attr "cx" "150",
    attr "cy" "100",
    attr "r" "80",
    attr "fill" "green" ]

svgText :: [Char]
svgText = element "text" [
    attr "x" "150",
    attr "y" "120",
    attr "font-size" "60",
    attr "text-anchor" "middle",
    attr "fill" "white"] "YES"

-- >>> output (svgDoc (redRect ++ greenCircle ++ svgText))



