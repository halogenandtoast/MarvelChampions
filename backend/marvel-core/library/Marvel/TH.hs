{-# LANGUAGE TemplateHaskell #-}
module Marvel.TH where

import Marvel.Prelude

import Data.Char qualified as C
import Language.Haskell.TH.Syntax hiding (Name)
import Language.Haskell.TH.Syntax qualified as TH

buildEntity :: String -> Q [Dec]
buildEntity nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let conz = mapMaybe extractCon instances
  pure
    [ DataD
        []
        (TH.mkName nm)
        []
        Nothing
        conz
        [ DerivClause (Just StockStrategy) (map ConT [''Show, ''Generic, ''Eq])
        , DerivClause (Just AnyclassStrategy) (map ConT [''ToJSON, ''FromJSON])
        ]
    ]
 where
  extractCon (InstanceD _ _ (AppT _ con@(ConT name)) _) = Just $ NormalC
    (TH.mkName $ nameBase name ++ "'")
    [(Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, con)]
  extractCon _ = Nothing

buildEntityLookupList :: String -> Q Exp
buildEntityLookupList nm = do
  ClassI _ instances <- reify (TH.mkName $ "Is" ++ nm)
  let conz = mapMaybe extractCon instances
  pure $ ListE conz
 where
  extractCon (InstanceD _ _ (AppT _ (ConT name)) _) = Just $ AppE
    (AppE (VarE $ TH.mkName "fmap") (ConE $ TH.mkName $ nameBase name ++ "'"))
    (VarE $ toFunName $ nameBase name)
  extractCon _ = Nothing
  toFunName [] = TH.mkName ""
  toFunName (x : xs) = TH.mkName $ C.toLower x : xs
