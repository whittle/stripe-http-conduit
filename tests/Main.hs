module Main where

import           Web.Stripe.Client              (StripeConfig(..), StripeError(..))
import           Web.Stripe.Client.HttpConduit  (callAPI, withManager)
import           Web.Stripe.Test.AllTests       (allTests)
import           Web.Stripe.Test.Prelude        (Stripe, StripeRequestF(..))
import           Web.Stripe.Test.AllTests       (allTests)

main :: IO ()
main = allTests runStripe

runStripe :: StripeConfig -> Stripe a -> IO (Either StripeError a)
runStripe = undefined
