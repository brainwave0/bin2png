{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.Image as I
import Graphics.Image.ColorSpace
import System.Environment
import Data.ByteString as BS
import Data.List.Split
import Data.Bits
import Data.Maybe
import Prelude as P
import Data.List as L

main :: IO ()
main = do
    [inPath, outPath] <- getArgs
    if ".png" `L.isSuffixOf` inPath
        then do
            img <- readImageY VU inPath
            BS.writeFile outPath $ imgToBin img 
        else do
            inFile <- BS.readFile inPath
            let dim = ceiling $ sqrt $ fromIntegral $ BS.length inFile * 8
                image :: Image VU Y Double
                image = makeImage (dim, dim) $ makePixel dim inFile
            writeImage outPath image
    
-- image to binary

imgToBin :: Image VU Y Double -> ByteString
imgToBin img = pack $ P.map pxsToBin $ chunksOf 8 $ P.filter isBit $ P.concat $ toLists img where
    isBit p = not $ abs (p - 0.5) < 0.1

pxsToBin :: [Pixel Y Double] -> Word8
pxsToBin pxs = L.foldl' f 0 $ P.zip [7,6..0] pxs where
    f w (i, p)
        | p > 0.5 = setBit w i
        | p < 0.5 = clearBit w i

-- binary to image

makePixel :: Int -> ByteString -> (Int, Int) -> Pixel Y Double
makePixel dim bytes (y, x) =
    case maybeBit of
        Just x -> if x then 1 else 0
        Nothing -> 0.5
    where
        maybeBit = if bytesIndex < BS.length bytes then Just bit else Nothing
        bit = testBit byte byteIndex
        byte = BS.index bytes bytesIndex
        bitsIndex = dim * y + x
        bytesIndex = bitsIndex `div` 8
        byteIndex = 7 - (bitsIndex `mod` 8)

