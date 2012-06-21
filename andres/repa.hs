import Data.Array.Repa
import Data.Array.Repa.Operators.Traversal

ex64 = fromFunction (Z :. 25 :: DIM1) (\(Z :. e) -> let x = e + 1 in x*x)
ex64' = computeUnboxedS ex64
ex65 = extent $ fromListUnboxed (Z:.2:.2:.2::DIM3) ([1..8] :: [Int])
ex65' = fromListUnboxed (Z:.2:.2:.2::DIM3) ([1..8] :: [Int]) ! (Z:.0:.1:.0)

rev :: (Source r e) => Array r DIM1 e -> Array D DIM1 e
rev arr = backpermute (extent arr)
                      (\(Z :. i) -> let (Z :. sz) = extent arr
                                    in (Z :. (sz - i - 1)))
                      arr

-- revex = computeUnboxedS ex64' :: Array U DIM1 Int

cartesian :: (Source r1 a, Source r2 b) => (a -> b -> c) -> Array r1 DIM1 a ->
             Array r2 DIM1 b -> Array D DIM2 c

cartesian f arr1 arr2 = unsafeTraverse2 arr1
                                        arr2
                                        (\ (Z :. a) (Z :. b) -> (Z :. a :. b))
                                        (\ g h sh -> f (g (extent arr1)) $ h (extent arr2) )

-- cartex = computeUnboxedS $ cartesian (\(a b -> a `div` b) ex64 ex64 :: Array U DIM2 Int

workFn :: Double -> Double -> Double
workFn x y = log(x+1) * log(y+1)

workArray :: Array D DIM1 Double
workArray = fromFunction (Z :. 100000 :: DIM1) (\ (Z :. i) -> fromIntegral i)

work :: Array D DIM2 Double
work = cartesian workFn workArray workArray

main = sumAllP work >>= print
