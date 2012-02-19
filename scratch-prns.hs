{-# LANGUAGE TemplateHaskell #-}
import ConcreteNormal.PreRenderable
import R3Immersions
import CheckAdmissibility
import Blender
import Triangulation
import ConcreteNormal
import NormalSurfaceBasic
import PreRenderable
import Numeric.AD.Vector
import DisjointUnion
import FileLocation
import Simplicial.DeltaSet2
import TriangulationCxtObject


ba = mkBlenderable normalSurfaceStyle pr_ 
     `disjointUnion`
     mkBlenderable 
        pseudomanifoldStyle 
        (pr_mapDs gds2 $ 
            mkPreRenderableFromTetImmersions (Just . getTetImm) $undef tr)


tr = mkTriangulation 1 []

cns :: ConcreteNSurface
cns = toConcrete (toAdmissible stdCoordSys tr (0 ./ ntA))

getTetImm = const (GTetE 3 (standardTet . unitToStd3))

pr_ = prnsFromTetImmersions getTetImm (gds2 cns)

scene
  :: Scene
       (GenericDeltaSet2
          (DJSimp DIM2 NTriOrNQuadHalf TTriangle))
scene = defaultScene $ disableHelpLines ba


main = testBlender scene
