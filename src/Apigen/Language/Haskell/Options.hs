{-# LANGUAGE OverloadedStrings #-}
-- | @Network.Tox.C.Options@: the C options object as a Haskell record.
module Apigen.Language.Haskell.Options
    ( optionsModule
    ) where

import qualified Apigen.Knowledge              as K
import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Model
import           Apigen.Language.Haskell.Names (typeName, valueName)
import           Apigen.Language.Haskell.Safe  (safeType)
import           Apigen.Semantic
import           Data.List                     (find)
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)

data Option = Option
    { optField  :: Text
    , optCName  :: Text
    , optType   :: SType
    , optGetter :: Text
    , optSetter :: Text
    , optSize   :: Maybe Text
    }

optionsModule :: (Text -> Maybe Text) -> SemanticModel -> Maybe ([Decl], [Export])
optionsModule memberDoc model = do
    res : _ <- Just (K.handlesOfKind model SettingsHandle)
    new <- methodName <$> find ((== Constructor) . methodRole) (methods res)
    free <- methodName <$> find ((== Destructor) . methodRole) (methods res)
    errType <- methodErrorType =<< find ((== Constructor) . methodRole) (methods res)
    let options = mapMaybe (option res) (properties res)
    fields <- mapM field options
    let err = TCon (typeOf errType)
        ptr = TCon (handleName res <> "Ptr")
        either' t = TApp (TCon "IO") [TApp (TCon "Either") [err, t]]
        raw n = Var ("Raw." <> n)
    return
        ( [ RecordDecl (Just "The options for a new Tox instance.") "Options" fields ["Eq", "Read", "Show"]
          , Signature (Just "The options C gives a new instance.") "defaultOptions" (either' (TCon "Options"))
          , Binding "defaultOptions" []
                (App (Var "withNew") [raw new, raw free, Var "peekOptions"])
          , Signature (Just "C options with these values, for the action.") "withOptions"
                (TFun [TCon "Options", TFun [ptr] (TApp (TCon "IO") [TCon "a"])] (either' (TCon "a")))
          , Binding "withOptions" [PVar "options", PVar "action"] $
                With ((App (Var "withNew") [raw new, raw free], [PVar "ptr"]) : mapMaybe withScope options)
                    (Do (map (Run . set) options ++ [Run (App (Var "action") [Var "ptr"])]))
          , Signature (Just "The values of C options.") "peekOptions" (TFun [ptr] (TApp (TCon "IO") [TCon "Options"]))
          , Binding "peekOptions" [PVar "ptr"] (applicative (Var "Options") (map get options))
          ]
        , [ExportAll "Options", ExportVar "defaultOptions", ExportVar "withOptions", ExportVar "peekOptions"]
        )
  where
    typeOf e = maybe (typeName e) (typeName . enumSemanticName) (findEnum model e)

    option res p
        -- The safe API keeps C from taking ownership (see 'withOptions').
        | propName p == "experimental_owned_data" = Nothing
        | otherwise = case propType p of
            SCallback _    -> Nothing
            SHandle "void" -> Nothing
            ty -> do
                getter <- cName' res =<< propRead p
                setter <- cName' res =<< propWrite p
                return Option
                    { optField  = valueName (propName p)
                    , optCName  = propName p
                    , optType   = ty
                    , optGetter = getter
                    , optSetter = setter
                    , optSize   = cName' res =<< propSize p
                    }

    cName' res name = cFunctionName . mappingOf model res <$> find ((== name) . methodName) (methods res)

    field o = do
        ty <- safeType model (optType o)
        return (Field (optField o) ty (doc o))

    -- The struct member's doc: @savedata@ is @savedata_data@ there.
    doc o = case memberDoc (optCName o) of
        Just d  -> Just d
        Nothing -> memberDoc (optCName o <> "_data")

    value o = App (Var (optField o)) [Var "options"]
    ptrName o = valueName (optCName o) <> "Ptr"
    lenName o = valueName (optCName o) <> "Len"

    -- Strings and bytes C reads from pointers that must outlive the setters.
    withScope o = case optType o of
        SString -> Just (App (Var "withCString") [value o], [PVar (ptrName o)])
        SBytes  -> Just (App (Var "withBytesLen") [value o], [PVar (ptrName o), PVar (lenName o)])
        _       -> Nothing

    set o = case optType o of
        SBool   -> call [App (Var "fromBool") [value o]]
        SEnum _ -> call [App (Var "toCEnum") [value o]]
        SString -> App (Var "void") [call [Var (ptrName o)]]
        SBytes  -> App (Var "void") [call [Var (ptrName o), Var (lenName o)]]
        _       -> call [value o]
      where
        call args = App (Var ("Raw." <> optSetter o)) (Var "ptr" : args)

    get o = case optType o of
        SBool   -> Op (Var "toBool") "<$>" getCall
        SEnum _ -> Op (Var "fromCEnum") "<$>" getCall
        SString -> Op getCall ">>=" (Var "peekNullableString")
        SBytes  -> case optSize o of
            Just size -> App (Var "peekSizedBytes") [getCall, App (Var ("Raw." <> size)) [Var "ptr"]]
            Nothing   -> getCall
        _       -> getCall
      where
        getCall = App (Var ("Raw." <> optGetter o)) [Var "ptr"]

applicative :: Expr -> [Expr] -> Expr
applicative f []       = App (Var "pure") [f]
applicative f (a : as) = foldl (\acc x -> Op acc "<*>" x) (Op f "<$>" a) as
