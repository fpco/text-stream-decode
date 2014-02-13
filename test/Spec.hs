{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Text.StreamDecoding as SD
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Exception (evaluate, try, SomeException)
import Control.DeepSeq (deepseq, NFData)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

try' :: NFData a => a -> IO (Either SomeException a)
try' a = try $ evaluate (a `deepseq` a)

main :: IO ()
main = hspec $ modifyMaxSuccess (const 10000) $ do
    let test name lazy stream = describe name $ do
            prop "bytes" $ check lazy stream
            prop "chars" $ \css -> do
                let ts = map T.pack css
                    lt = TL.fromChunks ts
                    lbs = TLE.encodeUtf8 lt
                    bss = L.toChunks lbs
                    wss = map S.unpack bss
                 in check lazy stream wss

        check lazy stream wss = do
            let bss = map S.pack wss
                lbs = L.fromChunks bss
            x <- try' $ feedLazy stream lbs
            y <- try' $ lazy lbs
            case (x, y) of
                (Right x', Right y') -> x' `shouldBe` y'
                (Left _, Left _) -> return ()
                _ -> error $ show (x, y)
    test "UTF8" TLE.decodeUtf8 SD.streamUtf8
    test "UTF8 pure" TLE.decodeUtf8 SD.streamUtf8Pure
    test "UTF16LE" TLE.decodeUtf16LE SD.streamUtf16LE
    test "UTF16BE" TLE.decodeUtf16BE SD.streamUtf16BE
    test "UTF32LE" TLE.decodeUtf32LE SD.streamUtf32LE
    test "UTF32BE" TLE.decodeUtf32BE SD.streamUtf32BE

    it "UTF8 leftovers" $ do
        let bs = "good\128\128bad"
        case SD.streamUtf8 bs of
            SD.DecodeResultSuccess _ _ -> error "Shouldn't have succeeded"
            SD.DecodeResultFailure t bs' -> do
                t `shouldBe` "good"
                bs' `shouldBe` "\128\128bad"

feedLazy :: (S.ByteString -> SD.DecodeResult)
         -> L.ByteString
         -> TL.Text
feedLazy start =
    TL.fromChunks . loop start . L.toChunks
  where
    loop dec [] =
        case dec S.empty of
            SD.DecodeResultSuccess t _ -> [t]
            SD.DecodeResultFailure _ _ -> [error "invalid sequence 1"]
    loop dec (bs:bss) =
        case dec bs of
            SD.DecodeResultSuccess t dec' -> t : loop dec' bss
            SD.DecodeResultFailure _ _ -> [error "invalid sequence 2"]
