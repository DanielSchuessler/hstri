{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell, BangPatterns, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Math.GaussElim where

import Control.Exception
import Control.DeepSeq
import PrettyUtil.Matrix
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Vector(Vector) 
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed(Unbox)
import qualified Data.DList as DL
import Data.Monoid
import qualified Data.Foldable as Fold
import Data.Foldable(Foldable)
import THUtil
import Math.SparseVector
import qualified Data.Set as S
import Data.Set(Set)
import Control.Monad.ST.Safe
import Control.Monad
import Data.Maybe
import Data.VectorSpace
import Control.Failure
import PrettyUtil


data ERT r =
      SwapRows {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | AddRowToRow {- dst -} {-# UNPACK #-} !Int !r {-# UNPACK #-} !Int 
    | ScaleRow !r {-# UNPACK #-} !Int

    deriving Show

swapRows :: Int -> Int -> ERT r
swapRows = SwapRows
addRowToRow :: Int -> r -> Int -> ERT r
addRowToRow i_dst r i_src = assert (i_dst /= i_src) $ AddRowToRow i_dst r i_src
scaleRow :: (Eq r, Num r) => r -> Int -> ERT r
scaleRow r = assert (r/=0) $ ScaleRow r

instance NFData r => NFData (ERT r) where
    rnf SwapRows{} = ()
    rnf (AddRowToRow _ r _) = rnf r `seq` ()
    rnf (ScaleRow r _) = rnf r `seq` ()

instance Show r => Pretty (ERT r) where prettyPrec = prettyPrecFromShow

class ApplyERT a r | a -> r where
    addMultiply :: r -> a r -> a r -> a r
    scale :: r -> a r -> a r

instance (Eq r, Num r) => ApplyERT V.Vector r where
    addMultiply r xs ys =  VG.zipWith (\x y -> x + r * y) xs ys
    scale = VG.map . (*)

newtype WrappedScalar r = WrapScalar { unwrapScalar :: r }

deriving instance VGM.MVector VU.MVector r => VGM.MVector VU.MVector (WrappedScalar r)
deriving instance VG.Vector VU.Vector r => VG.Vector VU.Vector (WrappedScalar r)
deriving instance Unbox r => Unbox (WrappedScalar r)

instance (Eq r, Num r) => ApplyERT WrappedScalar r where 
    addMultiply r (WrapScalar x) (WrapScalar y) = WrapScalar (x + r*y)
    scale r = WrapScalar . (r *) . unwrapScalar

applyERT
  :: (Eq r, Num r, VG.Vector v (a r), ApplyERT a r) =>
     ERT r -> v (a r) -> v (a r)
applyERT (SwapRows i i') !mtx
    | i == i' =        mtx
    | otherwise = VG.modify (\mmtx -> VGM.swap mmtx i i') mtx

applyERT (AddRowToRow i_dst r i_src) !mtx
    | r == 0 = mtx
    | otherwise = 
        mtx VG.// [  ( i_dst
                        , addMultiply r
                            ($(debugIndex) mtx i_dst) 
                            ($(debugIndex) mtx i_src)
                        )
                 ]

applyERT (ScaleRow r i) !mtx
    | r == 1 = mtx
    | otherwise = mtx VG.// [(i, scale r ($(debugIndex) mtx i))]


applyERTs
  :: (Eq r, Num r, VG.Vector v (a r), ApplyERT a r) =>
     [ERT r] -> v (a r) -> v (a r)
applyERTs erts mtx = L.foldl' (flip applyERT) mtx erts

scaleV :: (Num b, VG.Vector v b) => b -> v b -> v b
scaleV r v = VG.map (r *) v

(@@>) ::  
    (
       PrettyScalar a,
        
    VG.Vector v a) => Vector (v a) -> (Int, Int) -> a
(@@>) mtx_ (i, j) = 


    ($(debugIndex) ($(debugIndex) mtx_ i) j)
infix 9 @@>


rows, cols :: (VG.Vector v r) => Vector (v r) -> Int
rows = V.length

cols m | V.null m = error ("cols: empty matrix") 
       | otherwise = VG.length . V.head $ m 

type BMatrix r = Vector (Vector r)
type UMatrix r = Vector (VU.Vector r)


-- | @topNonzero mtx i j@ returns the smallest row index >= @i@ in column @j@ which is nonzero, or @Nothing@ if they're all zero.
topNonzero
  :: (Eq r, Num r, VG.Vector v r, PrettyScalar r) =>
     Vector (v r) -> Int -> Int -> Maybe Int
topNonzero mtx i j = L.find (\i' -> (mtx @@> (i',j)) /= 0) [i..rows mtx-1]

-- | 
--
-- * The 'ERT's returned must be applied to the input matrix /from left to right/ to get the matrix in echelon form
--
-- * The third component of the result is the list of pivot column indices, descending
toSomeEchelon
  :: (Eq r, PrettyScalar r, Fractional r, VG.Vector v r, ApplyERT v r) =>
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
  :: (Eq r, Fractional r, VG.Vector v r, PrettyScalar r, ApplyERT v r) =>
     Vector (v r) -> (Vector (v r), [ERT r], [Int])
toEchelon = toSomeEchelon False
toReducedEchelon
  :: (Eq r, Fractional r, VG.Vector v r, PrettyScalar r, ApplyERT v r) =>
     Vector (v r) -> (Vector (v r), [ERT r], [Int])
toReducedEchelon = toSomeEchelon True


        

adjacentsSatisfy
  :: Fold.Foldable f => (t -> t -> Bool) -> f t -> Bool
adjacentsSatisfy rel = go . Fold.toList
    where
        go (x0:xs@(x1:_)) = rel x0 x1 && go xs
        go _ = True

symbolicSolution
  :: (Fractional r,
      Ord variable,
      Ord r,
      Show variable,
      VG.Vector v r,
      ApplyERT v r,
      PrettyScalar r,
      Pretty r) =>
     Vector (v r)
     -> (Int -> variable)
     -> ((Vector (v r), [ERT r], [Int]),
         Vector (SparseVector variable r))
symbolicSolution mtx0 mkVar = 
    case toReducedEchelon mtx0 of
      echelon@(mtx',_,pcis) -> (echelon, symbolicSolutionForEchelon mtx' pcis mkVar)

symbolicSolutionForEchelon
  :: forall r variable v. (PrettyScalar r, Fractional r, Ord variable, VG.Vector v r, Ord r, Show variable, Pretty r) =>
     Vector (v r) -> [Int] -> (Int -> variable) -> Vector (SparseVector variable r)
symbolicSolutionForEchelon mtx' [] mkVar = 
            V.generate (cols mtx') (flip sparse_singleton 1 . mkVar)


symbolicSolutionForEchelon mtx' pivotColumnIndices mkVar = V.create creat 
            where
                lastNonzeroRow = length pivotColumnIndices - 1
                isPivot = flip S.member (S.fromList pivotColumnIndices) 

                creat :: forall s. ST s (VM.MVector s (SparseVector variable r)) 
                creat = do
                    v <- VM.new (cols mtx')
                    let
                        _write :: Int -> SparseVector variable r -> ST s ()
                        _write j y = 
                            
                            (VM.write v j y)

                        setVarFree j' = _write j' (sparse_singleton (mkVar j') 1)

                    let
                        loop i j pcis0
                          | j == -1 = 
                                (return ())
                          | i == -1 =
                                forM_ [0..j] setVarFree
                                
                                
                          | (jp:pcis) <- pcis0 = do

                            
                            forM_ [jp+1..j] setVarFree
                                                    





                            let _sum = sparse_fromAssocs _sumAssocs
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
dotprodWith f x y = 
    assert (VG.length x == VG.length y) $

    VG.sum (VG.zipWith f x y) 

mulMVwith
  :: (Num a, VG.Vector v a1, VG.Vector v b, VG.Vector v a) =>
     (a1 -> b -> a) -> Vector (v a1) -> v b -> Vector a
mulMVwith f mtx v = 
    assert
        (let c = cols mtx
             n = VG.length v
         in if c == n
               then True
               else trace ("mulMVwith: Incompatible dimensions: "
                            ++show (rows mtx)++"x"++show c++" vs. "++show n) False) $


    V.generate (rows mtx) (\i -> dotprodWith f ($(debugIndex) mtx i) v)

solve
  :: (Eq r, Fractional r,
      Show (v r),
      PrettyScalar r,
      VG.Vector v (WrappedScalar r),
      VG.Vector v Int,
      VG.Vector v r,
      ApplyERT v r,
      Failure String m) =>
     Vector (v r) -> v r -> m (v r)
solve mtx rhs = 
    let
        (echelonMtx,erts,pivotColumnIndices) = toEchelon mtx
        rhs' = applyERTsV erts rhs
    in
        backsubst echelonMtx pivotColumnIndices rhs'


applyERTsV
  :: (Eq r, Num r, VG.Vector v r, VG.Vector v (WrappedScalar r)) =>
     [ERT r] -> v r -> v r
applyERTsV erts = VG.map unwrapScalar . applyERTs erts . VG.map WrapScalar

prettyVector' :: VG.Vector v a => v a -> PrettyVector [a]
prettyVector' = PrettyVector . VG.toList
prettyMatrix'
  :: VG.Vector v a => Vector (v a) -> PrettyMatrix (Vector [a])
prettyMatrix' = PrettyMatrix . V.map VG.toList 

backsubst
  :: forall m v r. 
     (Eq r, Num r,
      PrettyScalar r,
      VG.Vector v Int,
      VG.Vector v r,
      Failure String m) =>
     Vector (v r) -> [Int] -> v r -> m (v r)
backsubst echelonMtx (VG.fromList -> VG.reverse -> pivotColumnIndices) rhs =

    assert (rows_ == VG.length rhs) $
    assert (rk <= rows_) $

        
    if (rows_==0)
        then return VG.empty -- arbitarily consider height-0 matrices as width-0
        else assert (VG.all (< cols_) pivotColumnIndices) $
           if extraRowsOK 
           then return result
           else failure ("backsubst "
                        ++showsPrec 11 (prettyMatrix' echelonMtx) ""++" _ "
                        ++showsPrec 11 (prettyVector' rhs) ""
                        ++": no solution")

 where
        rows_ = rows echelonMtx
        cols_ = cols echelonMtx
        rk = VG.length pivotColumnIndices

        diagMtx = VG.map (flip backpermute' pivotColumnIndices) (VG.take rk echelonMtx)

        result :: v r
        result = {-# SCC "backsubst/result" #-} 
        
            VG.create (do

            res <- VGM.replicate cols_ 0

            forM_ [rk-1,rk-2..0]

                (\i -> 

                    assert (if diagMtx @@> (i,i) == 1
                               then True
                               else trace ("(echelonMtx,diagMtx,pivotColumnIndices,i) = "
                                        ++show (prettyMatrix' echelonMtx
                                               ,prettyMatrix' diagMtx
                                               ,prettyVector' pivotColumnIndices
                                               ,i)) False) $

                        VGM.write res ($(debugIndex) pivotColumnIndices i) =<<
                            do
                                xs <- VG.mapM 
                                            (\j -> do 
                                                a <- VGM.read res ($(debugIndex) pivotColumnIndices j)
                                                return (a * (diagMtx @@> (i,j)))) 
                                            (V.enumFromN (i+1) (rk-1-i))
                                return 
                                    ($(debugIndex) rhs i - VG.sum xs)
                )
            
            return res

         )


        extraRowsOK =
            VG.all (==0)
                (VG.slice rk (rows_ - rk) rhs)



backpermute' :: (VG.Vector v Int, VG.Vector v a) => v a -> v Int -> v a
backpermute' v vi = VG.map ($(debugIndex) v) vi
