{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Network.Wreq
import           Test.Hspec

import           Network.IPv6DB.Types

main :: IO ()
main = hspec $ do

    describe "POST /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Creates the resource for the given address in a list named hosts with a JSON object in source field" $ do

            r <- post "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234" (object [("ttl",Number 60),("source", object [("data",String "A B C D")])])
            r ^. responseStatus . statusCode `shouldBe` 204

    describe "PUT /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Replaces the current source field in the previous POSTed resource with new data" $ do

            r <- put "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234" (object [("ttl",Number 60),("source", object [("data",String "E F G H")])])
            r ^. responseStatus . statusCode `shouldBe` 204

    describe "GET /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Gets the resource with its optional TTL counter" $ do

            r <- get "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234"
            r ^.. responseBody . key "source" . key "data" `shouldBe` [String "E F G H"]

    describe "DELETE /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Deletes the resource" $ do

            r <- delete "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234"
            r ^. responseStatus . statusCode `shouldBe` 204
{-
    describe "POST /ipv6db/v1/list/hosts/addresses" $

        it "Creates resources for the given list named hosts" $ do

            r <- post "http://localhost:4446/ipv6db/v1/list/hosts/addresses" TODO...
            r ^. responseStatus . statusCode `shouldBe` 204
-}
