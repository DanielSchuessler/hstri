{-# LANGUAGE BangPatterns, NoMonomorphismRestriction, RecordWildCards, ViewPatterns #-}
module DD where
import Control.Exception
import Test.QuickCheck(Arbitrary(..),Gen)
import Control.Applicative
import Data.Packed.Matrix
import Data.Packed.Vector as Vector
import System.Random
import System.Process
import System.Exit
import Test.QuickCheck.Gen
import Data.Vect.Double.Base(Vec3(..))
import qualified Data.Vect.Double as Vect
import Numeric.Container
import Numeric.LinearAlgebra
import Debug.Trace



data VRepr = VRepr {
    vrepr_verts :: [Vector Double],
    vrepr_rays :: [Vector Double]
}
    deriving (Eq,Show)

vRepr verts rays = 
    assert (not (null (verts ++ rays))) $
    assert (allEqualOn dim (verts ++ rays)) $
    VRepr verts rays
    
instance Arbitrary VRepr where
    arbitrary = do
        dum <- arbitrary :: Gen [()]
        let d = 1+length dum
        
        (lo,lo') <- elements [(listOf1,listOf),(listOf,listOf1)]
        vRepr <$> lo (fromList <$> vectorOf d arbitrary) <*> lo' (fromList <$> vectorOf d arbitrary)

renderVRepr opts VRepr{..} =
        let
            n = length vrepr_verts
            s = length vrepr_rays
            d = dim (head (vrepr_verts++vrepr_rays))

            lins = "V-representation" :
                   "begin" :
                   unwords [ show (n+s), show (d+1), "real" ] :
                   [ unwords ("1" : (show <$> toList x)) | x <- vrepr_verts ] ++
                   [ unwords ("0" : (show <$> toList x)) | x <- vrepr_rays ] ++
                   [ "end" ] ++
                   opts

        in
            unlines lins

            
readVRepr :: String -> VRepr
readVRepr (lines -> lins) = vRepr vrepr_verts vrepr_rays
    where
        (_:_:lins1) = dropWhile (/="begin") lins 

        (vrepr_verts,vrepr_rays) = foldr c (error "expected 'end' line") lins1

        c "end" _ = ([],[])
        c lin (vs,rs) = case words lin of
                             "1":lin' -> (fromList (fmap read lin') : vs, rs)
                             "0":lin' -> (vs, fromList (fmap read lin') : rs)
                             _ -> error ("Invalid line: "++show lin)


-- | Multiplying @m@ on th left must map old coordinates to new coordinates
transformVRepr :: Matrix Double -> VRepr -> VRepr
transformVRepr m VRepr{..} = VRepr (fmap (m <>) vrepr_verts) (fmap (m <>) vrepr_rays)

translateVRepr t VRepr{..} = VRepr (fmap (t+) vrepr_verts) vrepr_rays

prop_vrepr x = readVRepr (renderVRepr [] x) == x

-- | @A x <= b@
data HRepr = HRepr {
    -- | Row-major
    hrepr_A :: Matrix Double,
    hrepr_b :: Vector Double
}
    deriving (Eq,Show)

allEqualOn f _A = null _A || (all ((== (f (head _A))) . f) (tail _A))

hRepr _A b = 
    assert (rows _A > 0) $
    assert (rows _A == dim b) $
    HRepr _A b

instance Arbitrary HRepr where
    arbitrary = do
        dum <- arbitrary :: Gen [()]
        let d = 1+length dum
        m <- choose (1,3*d)
        hRepr 
            <$> (fromLists <$> vectorOf m (vectorOf d arbitrary)) 
            <*> (fromList <$> vectorOf m arbitrary)

renderHRepr opts HRepr{..} =
        let
            m = rows hrepr_A
            d = cols hrepr_A

            lins = "H-representation" :
                   "begin" :
                   unwords [ show m, show (d+1), "real" ] :
                   [ unwords (show b : (show . negate <$> toList x)) 
                        | (b,x) <- zip (toList hrepr_b) (toRows hrepr_A) ] ++
                   [ "end" ] ++ 
                   opts

        in
            unlines lins

            
readHRepr :: String -> HRepr
readHRepr (lines -> lins) = 
        if null hrepr_A 
           then error ("Zero rows; full input was:\n"++unlines lins)
           else hRepr (fromRows hrepr_A) (fromList hrepr_b)
    where
        (_:_:lins1) = dropWhile (/="begin") lins 

        (hrepr_b,hrepr_A) = foldr c (error "expected 'end' line") lins1

        c "end" _ = ([],[])
        c lin (b,_A) = case words lin of
                             w0:lin' -> (read w0 : b, fromList (fmap (negate . read) lin') : _A)
                             _ -> error ("Invalid line: "++show lin)


transformHRepr :: Matrix Double -> HRepr -> HRepr
transformHRepr m (HRepr _A b) = HRepr (_A <> m) b

translateHRepr t (HRepr _A b) = HRepr _A (b - (_A <> t))
    
prop_hrepr x = readHRepr (renderHRepr [] x) == x


vrepr_extension = "ext"
hrepr_extension = "ine"

getTempBase :: IO [Char]
getTempBase = do
    i <- randomIO :: IO Int
    return ("/tmp/MathUtil"++show (abs i)++".")

cdd
  :: ([String] -> src -> String, [Char])
     -> (String -> tar, [Char]) -> src -> IO tar
cdd (render,source_extension) (readIt,target_extension)  v = do
    tb <- getTempBase
    putStrLn ("Temp filename stem: "++tb)
    writeFile (tb ++ source_extension) (render [] v)
    ec <- rawSystem "cddf+" [tb ++ source_extension]
    case ec of
         ExitSuccess -> readIt <$> readFile (tb ++ target_extension)
         _ -> error ("cddf+ failed: "++show ec)


homogenify v = Vector.join [v, fromList [1]] 

addEqualities eqs h = 
    intersectHReprs h 
        (hRepr (fromRows $ fst <$> ineqs) (fromList $ snd <$> ineqs))
  where
    ineqs = do
        eq@(l,r) <- eqs
        [ eq, (-l,-r) ] 

vreprToHRepr :: VRepr -> IO HRepr
--vreprToHRepr v | not (null (vrepr_rays v)) = error "Can't handle rays" 
vreprToHRepr !vrepr = 
    let

        (v0:verts) | null (vrepr_verts vrepr) = error "no vertices"
                   | otherwise = vrepr_verts $ vrepr

        d = dim v0

        diffverts = fmap (subtract v0) verts
        diffvertmatrix = let it = fromColumns diffverts in trace ("Translated vertices:\n"++show it) it

        (u,svs) = leftSV diffvertmatrix 

        u' = trans u
        u'' = takeRows (dim svs) u'

        -- y = x-v0
        -- z = u' y

        -- Az <= b
        -- Au'y <= b

        (transformTo,transformBack) 
            | dim svs == d = trace "Full-dimensional" (id,id)
            | otherwise = 
                    trace ("Singular - transforming the translated vertices with matrix:\n"
                            ++show u
                            ++"\n\tand then keeping the first "++show (dim svs)++" components")
                    (transformVRepr u'' . translateVRepr (-v0),
                     translateHRepr (-v0) . addEqualities eqs . transformHRepr u'')
        
        eqs = [ (v_orth_image,0) | v_orth_image <- toColumns (dropColumns (dim svs) u) ]


    in 
        do
            hrepr <- cdd (renderVRepr,vrepr_extension) (readHRepr,hrepr_extension) (transformTo vrepr)
            return (transformBack hrepr)

hreprToVRepr :: HRepr -> IO VRepr
hreprToVRepr = cdd (renderHRepr,hrepr_extension) (readVRepr,vrepr_extension)


test = do
    v <- unGen arbitrary <$> newStdGen <*> pure 10
    vreprToHRepr v

intersectHReprs :: HRepr -> HRepr -> HRepr
intersectHReprs (HRepr a1 b1) (HRepr a2 b2) = HRepr (fromBlocks [[a1],[a2]]) (Vector.join [b1,b2]) 

vec3sToVRepr :: [Vec3] -> VRepr
vec3sToVRepr vecs = vRepr [ fromList [x,y,z] | Vec3 x y z <- vecs ] []

vreprToVec3s :: VRepr -> [Vec3]
vreprToVec3s (VRepr vs rs) = 
    assert (null rs) $
    [ Vec3 x y z | xyz <- vs, let [x,y,z] = toList xyz ]

intersectConvexPolyhedra :: [Vec3] -> [Vec3] -> IO [Vec3]
intersectConvexPolyhedra p1 p2 = do
    let toH = vreprToHRepr . vec3sToVRepr
    h1 <- toH p1
    h2 <- toH p2
    vreprToVec3s <$> hreprToVRepr (intersectHReprs h1 h2)


--degen = fromColumns . fmap fromList $ [ [1,1,0], [2,2,0], [0,1,1], [1,2,1] ] 



--     let
--         vars1 = [ Left i | (_,i) <- zip p1 [0::Int ..] ]
--         vars2 = [ Right i | (_,i) <- zip p2 [0::Int ..] ]
--         vars = vars1 ++ vars2 
--                
-- 
--         lp = do
--             mapM_ (flip setVarKind ContVar) vars 
--             mapM_ (flip varGeq 0) vars
--             leqTo (varSum vars) 1 
--             mapM_ (\proj ->
--                         equal 
--                             (linCombination [ (proj p, v) | (v,p) <- zip vars1 p1]) 
--                             (linCombination [ (proj p, v) | (v,p) <- zip vars2 p2]))
--                   [_1,_2,_3]
-- 
--     in
--         quickSolveLP lp

