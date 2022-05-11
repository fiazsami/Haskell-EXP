
styleAttr s = " style='" ++ s ++ "'"

openTag t s = "<" ++ t ++ styleAttr s ++ ">"

closeTag t = "</" ++ t ++ ">"

element t s c = openTag t s ++ c ++ closeTag t


extractTag = pTag ""
    where
        pTag name [] = reverse name
        pTag name (x:xs) = 
            if x == '<' || x == '/' || x == '>'
            then pTag name xs
            else pTag (x : name) xs

parseTag = toks [] []
    where
        toks cur res [] = reverse (addItem cur res)
        toks cur res (x:xs) =
            if x == ' '
            then toks [] (addItem cur res) xs
            else toks (x:cur) res xs
        addItem c r = reverse c:r

parseElement = pIter [] []
    where
        pIter cur res [] = reverse res
        pIter cur res (x:xs) =
            if x == '>' || x == '<' || x == '/'
            then pIter [] (addItem cur res) xs
            else pIter (x:cur) res xs
        addItem [] r = r
        addItem c r = parseTag (reverse c) : r

-- >>> parseElement (element "div" "bgcolor:blue" "this is some lengty text!!!")
-- [["div","style='bgcolor:blue'"],["this","is","some","lengty","text!!!"],["div"]]
