import HsTri

tr = tr_5

main =
    testBlender
    . defaultScene
    . fromSpqwc
    . spqwc_twoTetBipyramid tr
