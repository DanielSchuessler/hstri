import Blender
import TriangulationCxtObject
import NormalDisc
import INormalDisc
import StandardCoordinates
import Triangulation
import EdgeCentered
import Data.Vect.Double
import PreRenderable
import PrettyUtil
import DisjointUnion
import ConcreteNormal.PreRenderable
import Control.Monad
import ExampleTriangulations

main = do
    fess <- getFundamentalEdgeSurfaces tr_octahedron   

    forM_ fess (\fes ->

        testBlender
            (defaultScene 
                (transformCoords ((diag (Vec3 1 1 2) :: Mat3) Data.Vect.Double.*.) 
                    (pesudomanifoldStye (toPreRenderable spqwc)
                        `disjointUnion`
                    normalSurfaceStyle (standardCoordinatesToPreRenderable spqwc fes))))


        )
