{-# LANGUAGE StandaloneDeriving, TemplateHaskell, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE TypeFamilies, FlexibleInstances, BangPatterns #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}

module MathUtil where


import Collections
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.AdditiveGroup
import Data.Complex
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.Vect.Double hiding(Vector)
import qualified Data.Vect.Double as Vect
import Data.Vect.Double.Instances
import Data.Vect.Double.Util.Dim4
import Data.VectorSpace
import Data.Word
import PrettyUtil
import QuickCheckUtil
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.Random
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import TupleTH
import qualified Data.Foldable as Fold
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import ZeroDefaultMap
import qualified Data.DList as DL
import Data.Char
import Debug.Trace
import THUtil
import qualified Data.Set as S
import Control.Monad.ST.Safe
import Data.Vector(Vector)
import HomogenousTuples
import Control.Arrow((&&&))
import qualified Data.List as L

anyOrth :: Vec3 -> Vec3
anyOrth (Vec3 0 y z) = Vec3 0 (-z) y
anyOrth (Vec3 x y _) = Vec3 (-y) x 0

stereograph :: Vec4 -> Vec3
stereograph (Vec4 x0 x1 x2 x3) = Vec3 x1 x2 x3 &* (recip (1+x0))

stereograph' :: Double -> Vec4 -> Vec3
stereograph' t (Vec4 y0 y1 y2 y3) = 
    let 
        Vec2 y0' y3' = rotMatrix2 t *. Vec2 y0 y3

    in stereograph (Vec4 y0' y1 y2 y3')

c2_to_r4 :: Complex Double -> Complex Double -> Vec4
c2_to_r4 ((:+) !a !b) ((:+) !c !d) = Vec4 a b c d 


gen4 :: Monad m => m Double -> m Vec4
gen4 g = liftM4 Vec4 g g g g

arb01 :: (Num a, Random a) => Gen a
arb01 = frequency [(10,return 0),(30,choose (0,1)) ] 

arbsimp :: Gen Vec4
arbsimp = do
    xs <- gen4 arb01 `suchThat` (/= Vect.zero)
    return (xs &/ (xs `dotprod` 1))
    

(&/) :: Vect.Vector v => v -> Double -> v
v &/ r = v &* recip r
    

{-# INLINABLE to3Sphere #-}
to3Sphere ::  Vec4 -> Vec4
to3Sphere (Vec4 x0 x1 x2 x3) = c2_to_r4 g1 g2  
    where
        x01 = x0+x1
        x23 = x2+x3
        h = pi * (1+x01-x23) / 4
        g1 | x01 == 0 = 0
           | otherwise = sin h `cScalarMul` cis (2*pi*x0/x01) 

        g2 | x23 == 0 = 0
           | otherwise = cos h `cScalarMul` cis (2*pi*x2/x23) 

-- instance AbelianGroup (Complex Double) where
--     zero = 0
--     (&+) = (+)
--     (&-) = (-)
--     neg = negate
-- 
-- instance Vector (Complex Double) where
--     mapVec f (a :+ b) = f a :+ f b

cScalarMul :: Double -> Complex Double -> Complex Double
cScalarMul r (a :+ b) = r*a :+ r*b

withOrigin :: AbelianGroup b => b -> (b -> b) -> b -> b
withOrigin o f = (&+ o) . f . (&- o)



-- | 'vec3Z' * @pointZTo z'@ = @z'@ 
pointZTo :: Vec3 -> Mat3
pointZTo z' = Mat3 x' y' z'
    where
                x' = normalize (anyOrth z')
                y' = normalize (crossprod x' z')



-- prop_g1 = forAll arbsimp (\x -> let y = to3Sphere x
--                                 in abs (1-normsqr y) < 1E-14) 
                                
matrixApproxEq :: MatrixNorms m => m -> m -> Bool
matrixApproxEq m1 m2 = matrixDistance m1 m2 < 1E-10




ratioToIntegral :: Integral a => Ratio a -> Maybe a
ratioToIntegral r = guard (denominator r == 1) >> Just (numerator r)
                  
lcms :: (Integral b, Fold.Foldable t) => t b -> b
lcms = Fold.foldr lcm 1

denomLcms :: (Integral b, Fold.Foldable t) => t (Ratio b) -> b
denomLcms = Fold.foldr (\x r -> lcm (denominator x) r) 1

data ERT r =
      SwapRows {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | AddRowToRow {- dst -} {-# UNPACK #-} !Int !r {-# UNPACK #-} !Int 
    | ScaleRow !r {-# UNPACK #-} !Int

    deriving Show

swapRows :: Int -> Int -> ERT r
swapRows = SwapRows
addRowToRow :: Int -> r -> Int -> ERT r
addRowToRow i_dst r i_src = assert (i_dst /= i_src) $ AddRowToRow i_dst r i_src
scaleRow :: Num r => r -> Int -> ERT r
scaleRow r = assert (r/=0) $ ScaleRow r

instance NFData r => NFData (ERT r) where
    rnf SwapRows{} = ()
    rnf (AddRowToRow _ r _) = rnf r `seq` ()
    rnf (ScaleRow r _) = rnf r `seq` ()

instance Show r => Pretty (ERT r) where prettyPrec = prettyPrecFromShow

applyERT :: (Num r, VG.Vector v r) => ERT r -> Vector (v r) -> Vector (v r) 
applyERT (SwapRows i i') !mtx
    | i == i' =        mtx
    | otherwise = V.modify (\mmtx -> VM.swap mmtx i i') mtx

applyERT (AddRowToRow i_dst r i_src) !mtx
    | r == 0 = mtx
    | otherwise = 
        mtx V.// [  ( i_dst
                        , VG.zipWith (\x y -> x + r*y) 
                            (mtx V.! i_dst) 
                            (mtx V.! i_src)
                        )
                 ]

applyERT (ScaleRow r i) !mtx
    | r == 1 = mtx
    | otherwise = mtx V.// [(i, scaleV r (mtx V.! i))]


applyERTs
  :: (Num r, VG.Vector v r) =>
     [ERT r] -> Vector (v r) -> Vector (v r)
applyERTs erts mtx = L.foldl' (flip applyERT) mtx erts

scaleV :: (Num b, VG.Vector v b) => b -> v b -> v b
scaleV r v = VG.map (r *) v

(@@>) ::  VG.Vector v a => Vector (v a) -> (Int, Int) -> a
(@@>) mtx_ (i, j) = (mtx_ V.! i) VG.! j
infix 9 @@>


rows, cols :: (VG.Vector v r) => Vector (v r) -> Int
rows = V.length
cols = VG.length . V.head 

type BMatrix r = Vector (Vector r)
type UMatrix r = Vector (VU.Vector r)


topNonzero
  :: (Num r, VG.Vector v r) =>
     Vector (v r) -> Int -> Int -> Maybe Int
topNonzero mtx i j = L.find (\i' -> (mtx @@> (i',j)) /= 0) [i..rows mtx-1]

-- | 
--
-- * The 'ERT's returned must be applied to the input matrix /from left to right/ to get the matrix in echelon form
--
-- * The third component of the result is the list of pivot column indices, descending
toSomeEchelon
  :: (Fractional r, VG.Vector v r) =>
     Bool -> Vector (v r) -> (Vector (v r), [ERT r], [Int])
toSomeEchelon doReduce mtx0 =
    let
        r = rows mtx0
        c = cols mtx0


        loop !i !j !mtx !erts !pivotColumnIndices
            | i == r || j ==c = (mtx,DL.toList erts,DL.toList pivotColumnIndices)
            | otherwise =

                case topNonzero mtx i j of

                     Nothing -> loop i (j+1) mtx erts pivotColumnIndices
                     Just pivotRow ->

                        let     recip_pivot = recip (mtx @@> (pivotRow,j))
                        in let  swapERT = swapRows i pivotRow
                        in let  mtx' = applyERT swapERT mtx 
                        in let  scaleRowERT = scaleRow recip_pivot i
                        in let  rowClearERT i' =
                                    addRowToRow 
                                            i' 
                                            (-(mtx' @@> (i',j))) 
                                            i 

                        in let  i's = (if doReduce then ([0..i-1]++) else id) [i+1..r-1]
                        in let  mtx'' = applyERT scaleRowERT mtx'
                        in let  mtx''' = L.foldl' (\_r i' -> applyERT (rowClearERT i') _r) mtx'' i's 
                        in let  rowClearERTs = DL.fromList (map rowClearERT i's)
                        in let  newERTs =
                                    (return swapERT) `mappend`
                                    (return scaleRowERT) `mappend`
                                    rowClearERTs
                                
                                        
                        in 
                            loop 
                                (i+1) 
                                (j+1) 
                                mtx'''
                                (erts `mappend` newERTs)
                                (return j `mappend` pivotColumnIndices)


    in
        loop 0 0 mtx0 mempty mempty
    

toEchelon
  :: (Fractional r, VG.Vector v r) =>
     Vector (v r) -> (Vector (v r), [ERT r], [Int])
toEchelon = toSomeEchelon False
toReducedEchelon
  :: (Fractional r, VG.Vector v r) =>
     Vector (v r) -> (Vector (v r), [ERT r], [Int])
toReducedEchelon = toSomeEchelon True

arbMatrix :: Int -> Int -> Gen (PrettyMatrix (BMatrix Rational))
arbMatrix r c = (PrettyMatrix . V.fromList . map V.fromList) 
    <$> vectorOf r (vectorOf c s)

  where
    s = fromIntegral <$> choose (-4,4::Int)

arbSquareMatrix :: Int -> Gen (PrettyMatrix (BMatrix Rational))
arbSquareMatrix = join arbMatrix

intSqrt :: Int -> Int
intSqrt = round . (id :: Double -> Double) . sqrt . fromIntegral


arbMatrix' :: Gen (PrettyMatrix (BMatrix Rational))
arbMatrix' = sized (\n -> join (join (liftM2 arbMatrix) (choose (1,intSqrt n))))

prop_toEchelon :: Property
prop_toEchelon = 
    forAll arbMatrix' 
        (\(PrettyMatrix mtx) -> case toReducedEchelon mtx of
                      (mtx',erts,_) ->
                          printTestCase ("mtx' =\n"++prettyMatrix mtx') $
                          printTestCase ("erts =\n"++show erts) $
                            mtx' .=. applyERTs erts mtx
                            .&&.
                            isEchelon mtx')
        

adjacentsSatisfy
  :: Fold.Foldable f => (t -> t -> Bool) -> f t -> Bool
adjacentsSatisfy rel = go . Fold.toList
    where
        go (x0:xs@(x1:_)) = rel x0 x1 && go xs
        go _ = True

isEchelon :: (Num a, VG.Vector v a) => Vector (v a) -> Bool
isEchelon mtx = adjacentsSatisfy rel (V.map leftmostNonzero mtx) 

    where
        leftmostNonzero = VG.findIndex (/= 0)

        rel Nothing x = isNothing x
        rel (Just _) Nothing = True
        rel (Just x) (Just y) = x < y

prop_isEchelon :: Property
prop_isEchelon = expectFailure (forAll arbMatrix' (isEchelon . unPrettyMatrix))

qc_MathUtil :: IO Bool
qc_MathUtil = $quickCheckAll

        
makeMatrixIntegral
  :: (Integral b, VG.Vector v (Ratio b), VG.Vector v b) =>
     Vector (v (Ratio b)) -> Vector (v b)
makeMatrixIntegral = V.map makeVecIntegral

makeVecIntegral
  :: (Integral b, VG.Vector v (Ratio b), VG.Vector v b) =>
     v (Ratio b) -> v b
makeVecIntegral ro =
    let
        l = VG.foldl' lcm 1 . VG.map denominator $ ro

        f = fromMaybe (assert False undefined)
                . ratioToIntegral
    in
        VG.map (f . (fromIntegral l*)) ro

-- instance Integral i => AdditiveGroup (Ratio i) where
--     zeroV = 0
--     (^+^) = (+)
--     negateV = negate


symbolicSolution
  :: (Fractional r,
      Ord variable,
      Ord r,
      Show variable,
      VG.Vector v r,
      Pretty r) =>
     Vector (v r)
     -> (Int -> variable)
     -> ((Vector (v r), [ERT r], [Int]),
         Vector (ZeroDefaultMap variable r))
symbolicSolution mtx0 mkVar = 
    case toReducedEchelon mtx0 of
      echelon@(mtx',_,pcis) -> (echelon, symbolicSolutionForEchelon mtx' pcis mkVar)

symbolicSolutionForEchelon
  :: forall r variable v. (Fractional r, Ord variable, VG.Vector v r, Ord r, Show variable, Pretty r) =>
     Vector (v r) -> [Int] -> (Int -> variable) -> Vector (ZeroDefaultMap variable r)
symbolicSolutionForEchelon mtx' [] mkVar = 
            V.generate (cols mtx') (flip zdm_singleton 1 . mkVar)


symbolicSolutionForEchelon mtx' pivotColumnIndices mkVar = V.create creat 
            where
                lastNonzeroRow = length pivotColumnIndices - 1
                isPivot = flip S.member (S.fromList pivotColumnIndices) 

                creat :: forall s. ST s (VM.MVector s (ZeroDefaultMap variable r)) 
                creat = do
                    v <- VM.new (cols mtx')
                    let
                        _write :: Int -> ZeroDefaultMap variable r -> ST s ()
                        _write j y = 
--                            $(traceExps "_write" ['j,'y]) 
                            
                            (VM.write v j y)

                        setVarFree j' = _write j' (zdm_singleton (mkVar j') 1)

                    let
                        loop i j pcis0
                          | j == -1 = 
--                                 $(traceExps "loop/done" ['i,'pcis0])
                                (return ())
                          | i == -1 =
                                forM_ [0..j] setVarFree
                                
                                
                          | (jp:pcis) <- pcis0 = do

                            
                            forM_ [jp+1..j] setVarFree
                                                    





                            let _sum = zdm_fromAssocs _sumAssocs
                                _sumAssocs = 
                                        mapMaybe (\jj ->
                                            let
                                                a = mtx' @@> (i,jj)
                                            in if isPivot jj
                                               then $(assrt [|a==0|] ['jj,'a]) Nothing
                                               else Just (mkVar jj, a))
                                            
                                            [jp+1..cols mtx'-1]

                                pivotCoeff = mtx' @@> (i,jp) 

                            _write jp  
                                ( negate (recip pivotCoeff)
                                    *^ _sum ) 

                            loop (i-1) (jp-1) pcis
                                
                          | otherwise = 
                          
                                    error ("loop/impossible " ++ $(showExps ['i,'j,'pcis0]))



                    loop 
                        lastNonzeroRow 
                        (cols mtx'-1) 
                        pivotColumnIndices

                    return v



dotprodWith
  :: (Num a, VG.Vector v a1, VG.Vector v b, VG.Vector v a) =>
     (a1 -> b -> a) -> v a1 -> v b -> a
dotprodWith f x y = VG.sum (VG.zipWith f x y) 

mulMVwith
  :: (Num a, VG.Vector v a1, VG.Vector v b, VG.Vector v a) =>
     (a1 -> b -> a) -> Vector (v a1) -> v b -> Vector a
mulMVwith f mtx v = V.generate (rows mtx) (\i -> dotprodWith f (mtx V.! i) v)

pro_symSol :: Property
pro_symSol = forAll arbMatrix' (\(PrettyMatrix mtx) -> 
    case symbolicSolution mtx (variable . return . chr . (+ ord 'a')) of
            ((mtx',_,_),s) ->
                printTestCase ("mtx' =\n"++prettyMatrix mtx') $
                printTestCase (unlines ("s =":
                                    map show (VG.toList s)++[";"])) $

                    mulMVwith (*^) mtx s .=. V.replicate (rows mtx) zeroV)



-- | Returns a tetrahedration of a three-sided prism, and the newly created edges and (inner triangles)
tetrahedrate3Prism
  :: (t, t, t)
     -> (t, t, t) -> ([(t, t, t, t)], [(t, t)], [(t, t, t)])
tetrahedrate3Prism (a,b,c) (a',b',c') =
    ( [ list4 a b c c', list4 a b b' c', list4 a a' b' c' ] 
    , [(a,c'),(b,c'),(a,b')]
    , [(a,b,c'),(a,b',c')])



incircleRadius :: Floating a => a -> a -> a -> a
incircleRadius a b c =
    let s = (a+b+c) / 2
    in sqrt ((s-a)*(s-b)*(s-c)/s)


type UnitInterval = Double
type Standard2Simplex = Vec2



-- | Triangulation of the triangle with corners @(0,0),(0,steps),(steps,0)@, using isosceles triangles with scele length 1
triangulateTriangle
  :: (Enum t, Num t) => t -> ([(t, t)], [((t, t), (t, t), (t, t))])
triangulateTriangle steps = (verts,ts)
    where
        m = steps-1

        verts = [(u,v) | u <- [0..m], v <- [0..m-u]]

        ts = 
                       [ ((u,v)
                         ,(u+1,v)
                         ,(u,v+1))
                         
                            | u <- [0..m-1], v <- [ 0..m-1-u ] ]  
                        ++
                       [ ((u,v)                             
                         ,(u,v+1)                           
                         ,(u-1,v+1))                          
                                                                
                            | u <- [1..m-1], v <- [ 0..m-1-u ] ]


bary2 :: Vect.Vector v => (v, v) -> v
bary2 (x, y) = (x &+ y) &/ 2

bary3 :: Vect.Vector v => (v, v, v) -> v
bary3 (x, y, z) = (x &+ y &+ z) &/ 3


hillClimb :: Ord b => (a -> [a]) -> (a -> b) -> a -> a
hillClimb successors badness initial = go (initial,badness initial)
    where
        go (x,curBadness) =
           case L.find ((<curBadness) . snd) .  map (id &&& badness) . successors $ x of
                Nothing -> x
                Just suc -> go suc

