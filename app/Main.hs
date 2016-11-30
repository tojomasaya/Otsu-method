module Main where
import System.Environment
import Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP as BMP
import Data.Word
import Data.List as L
import Lib
import Data.Time
type Image a = Array U DIM2 a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ src, dst] ->
      go src dst
    _ -> putStrLn "Usage: src dst"

time :: String -> IO a -> IO a
time msg m = do
  start <- getCurrentTime
  x <- m
  end <- getCurrentTime
  putStrLn (msg L.++ ": "
             L.++ show (diffUTCTime end start))
  return x

go :: FilePath -> FilePath -> IO ()
go src dst = do
  result <- BMP.readImageFromBMP src
  case result of
    Left s -> putStrLn (show s)
    Right input -> do
      output <- convert input
      BMP.writeImageToBMP dst output
      return ()

convert :: Image (Word8, Word8, Word8)
        -> IO (Image (Word8, Word8, Word8))
convert = time "convert" . treshold'

treshold' :: Image (Word8, Word8, Word8) ->
             IO (Image (Word8, Word8, Word8))
treshold' x = do
  gray <- Repa.computeP (Repa.map toIntensity x)
  t <- otsu'sMethod gray
  output <- threshold t gray
  putStrLn (show t)
  Repa.computeP (Repa.map (toRGB . word8OfBool) output)
