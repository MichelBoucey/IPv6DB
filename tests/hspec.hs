{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
import           Network.Wreq
import           Test.Hspec

import           Network.IPv6DB.Types

main :: IO ()
main = hspec $ do

    describe "" $

        it "POST address aaaa::bbbb in a list named test" $ do

            r <- post "http://localhost:4446/ipv6db/v1/list/test/addresses/::192.168.1.1" (object [("ttl",Number 10),("source",String "DATA")])
            r ^. responseStatus . statusCode `shouldBe` 204

