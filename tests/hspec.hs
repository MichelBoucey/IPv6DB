{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
-- import           Data.Aeson.Lens
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Data.Vector
import           Test.Hspec

import           Network.IPv6DB.Types

main :: IO ()
main = hspec $ do

    describe "POST /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Creates the resource for the given address in a list named hosts with a JSON object in source field" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234"
          let req = initReq { method = "POST", requestBody = RequestBodyLBS $ encode (object [("ttl", Null),("source", object [("data",String "A B C D")])]) }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "PUT /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Updates the resource previously POSTed" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234"
          let req = initReq { method = "PUT", requestBody = RequestBodyLBS $ encode (object [("ttl", Null),("source", object [("data",String "E F G H")])]) }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "GET /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Gets the resource" $ do

          mngr <- newManager defaultManagerSettings
          req <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234"
          res <- httpLbs req mngr
          responseBody res `shouldBe` "{\"ttl\":null,\"list\":\"hosts\",\"address\":\"abcd::1234\",\"source\":{\"data\":\"E F G H\"}}"

    describe "DELETE /ipv6db/v1/list/hosts/addresses/abcd::1234" $

        it "Deletes the resource" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses/abcd::1234"
          let req = initReq { method = "DELETE" }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "POST /ipv6db/v1/list/hosts/addresses" $

        it "Creates resources for the given list named hosts" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses"
          let req = initReq
                      { method = "POST"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("address","abcd::1234")
                                        , ("ttl", Null)
                                        , ("source", object [ ("data", String "E F G H") ])
                                      ]
                                  , object
                                      [ ("address","abcd::5678")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "I J K L") ])
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "PUT /ipv6db/v1/list/hosts/addresses" $

        it "Updates the resources previously POSTed" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses"
          let req = initReq
                      { method = "PUT"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("address","abcd::1234")
                                        , ("ttl", Null)
                                        , ("source", object [ ("data", String "H G F E") ])
                                      ]
                                  , object
                                      [ ("address","abcd::5678")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "L K J I") ])
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "GET /ipv6db/v1/list/hosts/addresses" $

        it "Gets the resources previously POSTed and PUTed" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses"
          let req = initReq
                      { method = "GET"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ String "abcd::1234"
                                  , String "abcd::5678"
                                  ]
                       }
          res <- httpLbs req mngr
          responseBody res `shouldBe` "[{\"ttl\":null,\"list\":\"hosts\",\"address\":\"abcd::1234\",\"source\":{\"data\":\"H G F E\"}},{\"ttl\":null,\"list\":\"hosts\",\"address\":\"abcd::5678\",\"source\":{\"data\":\"L K J I\"}}]"

    describe "DELETE /ipv6db/v1/list/hosts/addresses" $

        it "Deletes the resources previously POSTed and PUTed" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/hosts/addresses"
          let req = initReq
                      { method = "DELETE"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ String "abcd::1234"
                                  , String "abcd::5678"
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

