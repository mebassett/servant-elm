{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.Elm.Internal.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Text       (pack)
import           Elm             (ElmDatatype, ElmType, toElmType)
import           Servant         ((:>))
import           Servant.Auth.Server (Auth)
import           Servant.Foreign (Arg(..), Foreign, GenerateList, HasForeign(..),
                                  HasForeignType, HeaderArg(..), PathSegment(..),
                                  Req(..), listFromAPI, typeFor)



data LangElm

instance (ElmType a) => HasForeignType LangElm ElmDatatype a where
  typeFor _ _ _ =
    toElmType (Proxy :: Proxy a)

instance (HasForeign LangElm ElmDatatype api) =>
  HasForeign LangElm ElmDatatype (Auth auths types :> api) where
  type Foreign ElmDatatype (Auth auths types :> api) = Foreign ElmDatatype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) newReq
      where newReq = req { _reqHeaders = [authHeader] }
            authHeader = HeaderArg { _headerArg = 
                                       Arg { _argName = PathSegment "Authorization"
                                           , _argType = toElmType $ pack "ElmType" }}


getEndpoints
  :: ( HasForeign LangElm ElmDatatype api
     , GenerateList ElmDatatype (Foreign ElmDatatype api))
  => Proxy api
  -> [Req ElmDatatype]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDatatype)
