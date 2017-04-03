{-# LANGUAGE OverloadedStrings #-}
-- |

module Network.Wai.Middleware.StaticEmbedded (static) where

import           Crypto.Hash
import           Data.ByteArray.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text as T
import           Network.HTTP.Types (status200, status304)
import           Network.Mime (defaultMimeLookup)
import           Network.Wai

static :: [(FilePath, B.ByteString)] -> Middleware
static files =
  let files' = map computeEtag files
  in \app req callback ->
       fromMaybe (app req callback) $ do
         let fileName = T.unpack . T.intercalate "/" $ pathInfo req
         (bs, etag) <- lookup fileName files'
         let mime = defaultMimeLookup (T.pack fileName)
         let hdrs = computeHeaders etag
         return . callback $
           if Just etag == lookup "If-None-Match" (requestHeaders req)
             then responseLBS status304 hdrs BL.empty
             else responseLBS status200 (("Content-Type", mime) : hdrs) bs
  where
    computeHeaders etag =
      [ ("Cache-Control", "no-transform,public,max-age=300,s-maxage=900")
      , ("ETag", etag)
      , ("Vary", "Accept-Encoding")
      ]

computeEtag
  :: (FilePath, B.ByteString)
  -> (FilePath, (BL.ByteString, B.ByteString))
computeEtag (fp, bs) =
  let bs' = BL.fromStrict bs in
  (fp, (bs', convertToBase Base16 (hashlazy bs' :: Digest SHA1)))
