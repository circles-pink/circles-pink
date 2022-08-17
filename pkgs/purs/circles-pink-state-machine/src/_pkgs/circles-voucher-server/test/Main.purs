module Test.VoucherServer.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
--import VoucherServer.Routes.TrustsReport as VoucherServer.Routes.TrustsReport

main :: Effect Unit
main = launchAff_ do
  --specs <- discover """VoucherServer\.(?!Types|Specs\.Xbge|Spec).*"""
  --let x = spy "" specs
  --runSpec [ consoleReporter ] 
    --VoucherServer.Routes.TrustsReport.spec
  
  pure unit