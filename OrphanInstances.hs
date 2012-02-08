{-# LANGUAGE ViewPatterns, TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module OrphanInstances() where

import Control.DeepSeq
import Data.Colour.SRGB as SRGB(RGB(..),toSRGB,Colour) 
import Data.Graph.Inductive.Graph(labEdges)
import Data.Graph.Inductive.Graph(labNodes)
import Data.Graph.Inductive.Tree(Gr)
import Data.GraphViz.Attributes
import Data.GraphViz.Types
import Data.Semigroup
import GraphUtil
import PrettyUtil
import Test.QuickCheck
import qualified Data.Graph.Inductive.PatriciaTree as Pat
import qualified Data.Vect.Double as Vect
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen
import Control.DeepSeq.TH
import qualified Data.Set as S
import qualified Data.Map as M
import Util
import Data.Vect.Double.Instances() -- Eq
import Language.Haskell.TH.Syntax



instance Pretty Attribute where
    prettyPrec = prettyPrecFromShow

instance Pretty n => Pretty (DotEdge n) where
    prettyPrec prec (DotEdge a b c) = 
        prettyPrecApp prec
            (text "DotEdge")
            [anyPretty a, anyPretty b, anyPretty c]

instance Semigroup Doc where 
    (<>) = (Leijen.<>)


-- | Matrix multiplication
instance Monoid Vect.Mat3 where 
    mappend = (Vect..*.)
    mempty = Vect.idmtx

-- | Matrix multiplication
instance Monoid Vect.Mat4 where 
    mappend = (Vect..*.)
    mempty = Vect.idmtx

-- | Matrix multiplication
instance Monoid Vect.Proj4 where 
    mappend = (Vect..*.)
    mempty = Vect.idmtx

instance Semigroup Vect.Mat2 where (<>) = (Vect..*.)
instance Semigroup Vect.Mat3 where (<>) = (Vect..*.)
instance Semigroup Vect.Mat4 where (<>) = (Vect..*.)
instance Semigroup Vect.Proj3 where (<>) = (Vect..*.)
instance Semigroup Vect.Proj4 where (<>) = (Vect..*.)
instance Semigroup Vect.Ortho2 where (<>) = (Vect..*.)
instance Semigroup Vect.Ortho3 where (<>) = (Vect..*.)
instance Semigroup Vect.Ortho4 where (<>) = (Vect..*.)

instance (Pretty n, Pretty e) => Pretty (Gr n e) where
    pretty = prettyGraph

instance (Pretty n, Pretty e) => Pretty (Pat.Gr n e) where
    pretty = prettyGraph
    
    
    

instance Arbitrary Vect.Vec4 where
    arbitrary = liftM4join4 Vect.Vec4 arbitrary


deriving instance Ord Vect.Vec2
deriving instance Ord Vect.Vec3
deriving instance Ord Vect.Vec4

instance (NFData n, NFData e) => NFData (Pat.Gr n e) where
    rnf g = rnf (labNodes g, labEdges g)

instance (Floating a, Ord a, NFData a) => NFData (Colour a) where
    rnf c = case toSRGB c of 
                 SRGB.RGB r g b -> rnf (r,g,b)

$(

    let Name _ (NameG _ pkg _) = ''S.Set
    in

        if pkgString pkg >= "containers-0.4.2.1"
        then return []
        else concatMapM id
                    [ deriveNFData ''S.Set
                    , deriveNFData ''M.Map
                    ])
