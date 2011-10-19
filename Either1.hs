data Either1 s1 s2 n = L (s1 n) | R (s2 n)
    deriving(Eq,Ord)

either1 :: (t1 t3 -> t) -> (t2 t3 -> t) -> Either1 t1 t2 t3 -> t
either1 l _ (L x) = l x
either1 _ r (R x) = r x

bimap1
  :: (t t2 -> s1 n)
     -> (t1 t2 -> s2 n)
     -> Either1 t t1 t2
     -> Either1 s1 s2 n
bimap1 l r = either1 (L . l) (R . r)

