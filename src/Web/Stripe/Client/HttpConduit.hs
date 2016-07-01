{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Client.HttpConduit
       ( stripe
       , stripeMan
       , StripeRequest (..)
       , StripeError (..)
       , StripeConfig (..)
       , callAPI
       ) where

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson.Parser (json)
import           Data.Aeson.Types (FromJSON, fromJSON, Result, Value)
import           Data.Conduit (($$+-))
import           Data.Conduit.Attoparsec (ParseError(..), sinkParserEither)
import           Data.Default (def)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Conduit (http, Manager, newManager, tlsManagerSettings)
import qualified Network.HTTP.Conduit as H
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Web.Stripe.Client (APIVersion(..), attemptDecode, handleStream, parseFail, StripeConfig(..), StripeError(..), StripeKey(..), StripeRequest, StripeReturn, toBytestring, unknownCode)
import qualified Web.Stripe.StripeRequest as S

stripe :: (FromJSON (StripeReturn a))
       => StripeConfig
       -> StripeRequest a
       -> IO (Either StripeError (StripeReturn a))
stripe config request = do
  man <- newManager tlsManagerSettings
  stripeMan man config request

stripeMan :: (FromJSON (StripeReturn a))
          => Manager
          -> StripeConfig
          -> StripeRequest a
          -> IO (Either StripeError (StripeReturn a))
stripeMan man config request = callAPI man fromJSON config request

callAPI :: Manager
        -> (Value -> Result b)
        -> StripeConfig
        -> StripeRequest a
        -> IO (Either StripeError b)
callAPI man fromJSON' config stripeRequest = runResourceT $ do
  let req = H.applyBasicAuth (getStripeKey $ secretKey config) mempty $
            def { H.method = (m2m $ S.method stripeRequest)
                , H.secure = True
                , H.host = "api.stripe.com"
                , H.port = 443
                , H.path = encodeUtf8 $ "/v1/" <> S.endpoint stripeRequest
                , H.requestHeaders = [("Stripe-Version", toBytestring V20141007)]
                , H.checkStatus = \_ _ _ -> Nothing
                }
  let req' = if S.GET == S.method stripeRequest
             then H.setQueryString (map (\(f,s) -> (f, Just s)) $ S.queryParams stripeRequest) req
             else H.urlEncodedBody (S.queryParams stripeRequest) req
  res <- http req' man
  let status = statusCode $ H.responseStatus res
  if not $ attemptDecode status
    then return unknownCode
    else do v <- H.responseBody res $$+- sinkParserEither json
            return $ case v of
              (Left e) -> parseFail . intercalate " | " $ errorMessage e: errorContexts e
              (Right a) -> handleStream fromJSON' status $ return a

m2m :: S.Method -> Method
m2m S.GET = methodGet
m2m S.POST = methodPost
m2m S.DELETE = methodDelete
