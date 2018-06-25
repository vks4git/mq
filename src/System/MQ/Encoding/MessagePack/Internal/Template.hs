{-# LANGUAGE TemplateHaskell #-}

module System.MQ.Encoding.MessagePack.Internal.Template
  (
    makeMsgPackDictionary
  , makeMsgPackDictionaryGen
  , snakeCase
  , snakePostfix
  , Modifier
  ) where

import           Cases                        (snakify)
import           Control.Lens                 (view, _1)
import           Control.Monad                (when, (>=>))
import           Data.Char                    (isLower)
import           Data.Map.Strict              (fromList, member, (!))
import           Data.MessagePack.Types.Class (MessagePack (..))
import           Data.Text                    (pack, unpack)
import           Instances.TH.Lift            ()
import           Language.Haskell.TH

type Modifier = String -> String

snakeCase :: Modifier
snakeCase = unpack . snakify . pack

snakePostfix :: Modifier
snakePostfix = snakeCase . dropWhile isLower

makeMsgPackDictionary :: Name -> Q [Dec]
makeMsgPackDictionary = makeMsgPackDictionaryGen id

-- Declare a type instance of MessagePack.
-- Only 'data' and 'newtype' declarations are supported. They should be in form of field records.
-- Conversion is performed in two steps:
-- 1) Convert data into a 'Map' with 'Text' keys and 'Object' values. Key names are obtained from data field labels with some user-defined modification.
-- 2) Convers the 'Map' into 'Object'
--
-- Backward conversion is performed as follows:
-- 1) Convert 'Object' into 'Map Text Object' and extract all values into a list in the same order as they are described in type declaration.
-- 2) Construct the data: ```pure DataConstructor <$> fromObject value1 <*> ... <*> fromObject valueN
--
makeMsgPackDictionaryGen :: Modifier -> Name -> Q [Dec]
makeMsgPackDictionaryGen modifier typeCon = do

    -- 'reify' function gives 'Info' about 'Name' such as constructor name and its fields.
    -- See: https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#t:Info
    TyConI declaration <- reify typeCon

    -- Get type declaration parameters: type name and fields. Supports data and newtype only.
    -- These will be used in properties Map formation.
    let (tyName, tyConstructors) = getTypeCons declaration

    -- Types with multiple constructors are not supported currently.
    when (length tyConstructors /= 1) $ error ("Type " ++ show tyName ++ " has more than one constructor.")

    let tyCons = head tyConstructors
    let (conName, dataFields) = getConsFields tyCons

    let dictKeys = fmap (modifier . nameBase) dataFields

    varX <- newName "x"

    toClause   <- makeToClause varX dataFields dictKeys
    fromClause <- makeFromClause conName dictKeys

    -- function declarations themselves.
    let bodyDecl = [FunD 'toObject [toClause], FunD 'fromObject [fromClause]]

    -- Instance declaration itself.
    pure [InstanceD Nothing [] (AppT (ConT ''MessagePack) (ConT typeCon)) bodyDecl]


makeToClause :: Name -> [Name] -> [String] -> Q Clause
makeToClause varX dataFields fieldNames = pure $ Clause [VarP varX] (NormalB mapE) []
  where
    -- apply field record function to a data.
    getValue :: Name -> Exp
    getValue name = AppE (VarE name) (VarE varX)

    objectsExp :: [Exp]
    objectsExp = fmap (\fld -> AppE (VarE 'toObject) (getValue fld)) dataFields

    -- List of pairs :: [(key, value)]
    -- `key` is field record name.
    -- `value` is the data that corresponding field holds.
    pairs :: [Exp]
    pairs = zipWith (\fld val -> TupE [strToTextE fld, val]) fieldNames objectsExp

    -- Map representation:
    -- mapE = toObject $ fromList pairs
    -- in terms of Haskell.
    mapE :: Exp
    mapE = AppE (VarE 'toObject) $ AppE (VarE 'fromList) (ListE pairs)


makeFromClause :: Name -> [String] -> Q Clause
makeFromClause conName fieldNames = do

    lamVarN <- newName "obj"
    let lamVarE = pure $ VarE lamVarN

    -- Field record names packed in Exp
    -- \x -> [|x|] :: a -> Q Exp
    -- Therefore, fieldNamesE :: [Exp]
    fieldNamesE <- mapM (\x -> [|x|]) fieldNames

    valuesFun <- [|(\fld -> if pack fld `member` $(lamVarE)
                            then $(lamVarE) ! pack fld
                            else error $"makeFromClause :: could not unpack field " ++ fld)|]

    let values = fmap (AppE valuesFun) fieldNamesE

    let objE = LamE [VarP lamVarN] $ foldl apE (AppE (VarE 'pure) (ConE conName)) values
    let result = AppE (AppE (VarE '(>=>)) (VarE 'fromObject)) objE

    pure $ Clause [] (NormalB result) []

-- | Applies lifted data constructor to unpacked Object:
-- acc <*> fromObject fieldName
apE :: Exp -> Exp -> Exp
apE acc fieldName = AppE (AppE (VarE '(<*>)) acc) (AppE (VarE 'fromObject) fieldName)


-- | Extract information about type: constructor name and field record names.
--
getConsFields :: Con -> (Name, [Name])
getConsFields (RecC cName decs)           = (cName, fmap (view _1) decs)
getConsFields (ForallC _ _ cons)          = getConsFields cons
getConsFields (RecGadtC (cName:_) decs _) = (cName, fmap (view _1) decs)
getConsFields (NormalC cName _)           = (cName, [])
getConsFields _ = error $ moduleName ++ ".getConsFields :: unsupported data declaration."

-- | Parse a type declaration and retrieve its name and its constructors.
--
getTypeCons :: Dec -> (Name, [Con])
getTypeCons (DataD    _ typeName _ _ constructors _) = (typeName, constructors)
getTypeCons (NewtypeD _ typeName _ _ constructor  _) = (typeName, [constructor])
getTypeCons otherDecl = error $ moduleName ++ ".getTypeCons :: unsupported declaration: " ++ show otherDecl ++ "\nShould be either 'data' or 'newtype'."

moduleName :: String
moduleName = "System.MQ.Encoding.MessagePack.Internal.Template"

-- | Transform a String into Text Literal expression
--
strToTextE :: String -> Exp
strToTextE str = AppE (VarE 'pack) (LitE . StringL $ str)
