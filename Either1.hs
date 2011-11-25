module Either1 where

data Either1 s1 s2 n = Left1 (s1 n) | Right1 (s2 n)
    deriving(Eq,Ord)


either1 :: (t1 t3 -> t) -> (t2 t3 -> t) -> Either1 t1 t2 t3 -> t
either1 l _ (Left1 x) = l x
either1 _ r (Right1 x) = r x

bimap1
  :: (t t2 -> s1 n)
     -> (t1 t2 -> s2 n)
     -> Either1 t t1 t2
     -> Either1 s1 s2 n
bimap1 l r = either1 (Left1 . l) (Right1 . r)

toEither1 :: Either (s1 n) (s2 n) -> Either1 s1 s2 n
toEither1 = either Left1 Right1 

fromEither1 :: Either1 t1 t2 t3 -> Either (t1 t3) (t2 t3)
fromEither1 = either1 Left Right
