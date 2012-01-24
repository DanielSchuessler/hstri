module Codec.Rga.Writer where
import Triangulation
import Codec.Rga.Util


gluingsToRga :: Triangulation -> TIndex -> RgaGluingsForTet 
gluingsToRga tr tet_dom =
        map4 f allIndex4'
  where
    f i_dom = 
        fmap g (lookupGluingOfITriangle tr glDom_)

      where
        v_dom = index4ToVertex i_dom 

        glDom_ = tet_dom ./ triangleByDualVertex v_dom

        g glCod_ = (tet_cod, tupleFromFun4 h)
          where
            tet_cod = getTIndex $ glCod_
            v_cod = oTriangleDualVertex . unI $ glCod_

            h (index4ToVertex -> v) = vertexToIndex4
                    (if v == v_dom
                        then v_cod
                        else gluingMap (glDom_,glCod_) v)

