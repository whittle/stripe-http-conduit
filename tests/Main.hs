module Main where

import Control.Monad.Trans.Free (FreeF(..), FreeT(..))
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import           Web.Stripe.Client              (StripeConfig(..), StripeError(..))
import           Web.Stripe.Client.HttpConduit  (callAPI)
import           Web.Stripe.Test.AllTests       (allTests)
import           Web.Stripe.Test.Prelude        (Stripe, StripeRequestF(..))

main :: IO ()
main = do
  man <- newManager tlsManagerSettings
  allTests (runStripe man)

runStripe :: Manager -> StripeConfig -> Stripe a -> IO (Either StripeError a)
runStripe man config (FreeT m) = do
  f <- m
  case f of
   (Pure a) -> return (Right a)
   (Free (StripeRequestF req decode')) -> do
     r <- callAPI man decode' config req
     case r of
      (Left e) -> return (Left e)
      (Right next) -> runStripe man config next
