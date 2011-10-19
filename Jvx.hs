toJVX :: forall a. (Ord (Vertex a), Show (Vertex a), DeltaSet a) => a -> (Vertex a -> Vec3) -> ByteString
toJVX a coords = 
    xrender $
    doc defaultDocInfo { 
        docInfo_docType = Just 
            "<!DOCTYPE jvx-model SYSTEM \"http://www.javaview.de/rsrc/jvx.dtd\">"
        } $
    xelem "jvx-model" $
    xelem "geometries" $
    xelem "geometry" $
    xelems $
        [
            xelem "pointSet"
                (xattrs [ xattr "dim" "3", xattr "point" "show" ] <#> 
                xelems [
                    xelem "points" $
                        xelems [ xelem "p" 
                                    ( -- xattr "tag" (show v) <#>
                                    
                                     xtextRaw (printf "%f %f %f" x y z :: String)) 
                                | v <- vertices a, let Vec3 x y z = coords v ]
                ]
                )
        ,   xelem "lineSet"
                (xattrs [ xattr "line" "show" ] <#>
                xelems [
                    xelem "lines" $
                        xelems [ xelem "l" $ xtextRaw 
                                            (printf "%d %d"
                                             (pointIndexOf v1) 
                                             (pointIndexOf v2)
                                                :: String 
                                             )
                                | e <- edges a, 
                                  let v1 = faceMap a NZero 0 e,
                                  let v2 = faceMap a NZero 1 e
                                    
                                      
                                ]




--         ,   xelem "faceSet"
--                 (xattrs [ xattr "line" "show" ] <#>
--                 xelems [
--                     xelem "lines" $
--                         xelems [ xelem "l" $ xtextRaw 
--                                             (printf "%d %d"
--                                              (pointIndexOf v1) 
--                                              (pointIndexOf v2)
--                                                 :: String 
--                                              )
--                                 | e <- edges a, 
--                                   let v1 = faceMap a NZero 0 e,
--                                   let v2 = faceMap a NZero 1 e
--                                     
--                                       
--                                 ]




                ]


                )
        ]

  where
    verts = vertices a
    pointIndices :: Map (Vertex a) Int
    pointIndices = Map.fromList (Prelude.zip verts [0..]) 
    pointIndexOf v = pointIndices ! v
    showPointIndexOf :: Vertex a -> String 
    showPointIndexOf = show . pointIndexOf


toJVX' = (B.putStrLn .) . toJVX

writeJVX a coords file = B.writeFile file (toJVX a coords)

