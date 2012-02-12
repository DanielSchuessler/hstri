import Data.BitVector.Adaptive
import Data.Proxy

n = 100

main = print (withBitVectorType n 
            (\p -> zipWith 
                    (==) 
                    [bvAllBut n i `asProxyTypeOf` p | i <- [0..n-1]] 
                    (repeat ( bvFull n )))) 
