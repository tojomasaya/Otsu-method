module Lib
    ( plotHistogram, histogram
    , otsu'sMethod, toIntensity, toRGB, word8OfBool
    , threshold
    ) where

import Data.Array.Repa
import qualified Data.Array.Repa as Repa
import Data.Word
import Data.List as L
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST as ST
import Prelude as P

type Image a = Array U DIM2 a
data Histogram = Histogram { getHistogram :: V.Vector Int
                           , maxOfHistogram :: Int
                           , minOfHistogram :: Int
                           , sumOfHistogram :: Int
                           , countOfHistogram :: Int
                           }

plotHistogram :: Monad m => Int -> Histogram -> m (Image Bool)
plotHistogram height hist = do
  computeP $ fromFunction (Z:.height:.256) f
  where f (Z:.i:.j) =
          if i < h
          then True else False
          where x = getHistogram hist V.! j
                h = (x * height) `div` fromIntegral m
        m = maxOfHistogram hist

histogram :: Monad m => Image Word8 -> m Histogram
histogram image =
  return $ Histogram { getHistogram = v
                     , maxOfHistogram = V.maximum v
                     , minOfHistogram = V.minimum v
                     , sumOfHistogram = sum'
                     , countOfHistogram = rs*cs}
  where
    xs = L.map fromIntegral $ Repa.toList image
    (Z:.rs:.cs) = extent image
    len = 256
    sum' = V.ifoldl (\acc i x -> acc + i*x) 0 v
    inc mv x = do
      c <- VM.read mv x
      VM.write mv x (succ c)
    v = runST $ do
      mv <- VM.replicate len 0
      P.mapM_ (inc mv) xs
      V.unsafeFreeze mv

threshold :: Monad m => Word8 -> Image Word8 -> m (Image Bool)
threshold t image =
  Repa.computeP $ Repa.map f image
  where f x | t <= x = True
            | otherwise = False
    
otsu'sMethod :: Monad m => Image Word8 -> m Word8
otsu'sMethod image = do
  h <- histogram image
  let v = getHistogram h
  let (_, _, t, _) =  V.ifoldl' step (init h) v
  return (fromIntegral t)
  where
    step ((c1, s1), (c2, s2), t, max') i c =
      if max' < max''
      then ((c1', s1'), (c2', s2'), i, max'')
      else ((c1', s1'), (c2', s2'), t, max')
      where c1' = c1 + fromIntegral c
            s1' = s1 + fromIntegral c * fromIntegral i
            c2' = c2 - fromIntegral c
            s2' = s2 - fromIntegral c * fromIntegral i
            m1 = s1' / c1' :: Float
            m2 = s2' / c2' :: Float
            max'' = c1'*c2'*(m1 - m2)**2
    init h = ((0, 0), (c, s), 0, 0)
      where s = fromIntegral (sumOfHistogram h)
            c = fromIntegral (countOfHistogram h)
                        
toIntensity :: (Word8, Word8, Word8) -> Word8
toIntensity (r, g, b) =
  fromIntegral ((306 * (fromIntegral r :: Int)
                 + 601 * fromIntegral g
                  + 117 * fromIntegral b) `div` 1024)

toRGB :: Word8 -> (Word8, Word8, Word8)
toRGB y = (y, y, y)

word8OfBool :: Bool -> Word8
word8OfBool True = 255
word8OfBool False = 0
