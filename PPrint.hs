module Doc.PPrint where


import Doc.DocLike


class DocLike d => PPrint d a  where 
    pprint ::  a -> d

    parPprint ::  a -> d
    parPprint  = parens . pprint

    pplist    ::  [a] -> d
    pplist    xs = brackets (hcat (punctuate comma (map pprint xs)))

    pptuple   ::  [a] -> d
    pptuple   xs = parens (hcat (punctuate comma (map pprint xs)))
    
    
instance PPrint d a => PPrint d [a] where
    pprint  = pplist

instance DocLike d => PPrint d Char where
  pprint  = char
  pplist  = text

instance DocLike d => PPrint d Integer where
  pprint  = tshow

instance DocLike d => PPrint d Int where
  pprint  = tshow

instance DocLike d => PPrint d Float where
  pprint  = tshow

instance DocLike d => PPrint d Double where
  pprint  = tshow

instance DocLike d => PPrint d () where
    pprint () = text "()"

instance (PPrint d a, PPrint d b) => PPrint d (a,b) where
  pprint (x,y) = parens (hsep [pprint x <> comma, pprint y])

instance (PPrint d a, PPrint d b, PPrint d c) => PPrint d (a,b,c) where
  pprint (x,y,z) = parens (hsep [pprint x <> comma,
                                pprint y <> comma,
                                pprint z])


