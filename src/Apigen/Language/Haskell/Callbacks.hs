{-# LANGUAGE OverloadedStrings #-}
-- | Registering Haskell handlers for a C object's callbacks, for objects
-- with no event API (ToxAV).
module Apigen.Language.Haskell.Callbacks
    ( callbackDecls
    ) where

import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Model
import           Apigen.Language.Haskell.Names (valueName, varName)
import           Apigen.Language.Haskell.Raw   (registrationName)
import           Apigen.Language.Haskell.Safe  (safeType, sizeExprWith)
import           Apigen.Semantic
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

callbackDecls :: (Text -> [(Text, Text)] -> Maybe Text) -> SemanticModel -> SResource -> Maybe ([Decl], [Text])
callbackDecls doc model res = do
    fns <- mapM registration (events res)
    return (concatMap fst fns, map snd fns)
  where
    registration e = do
        let params = eventParams e
            name = valueName (registrationName res e)
            vars = map (varName . paramName) params
        types <- mapM (safeType' . paramType) params
        conversions <- mapM (convert params) params
        let handlerArgs = [ either id (const (Var (v <> "'"))) c | (c, v) <- zip conversions vars ]
            bound = [ Bind (PVar (v <> "'")) io | (Right io, v) <- zip conversions vars ]
            call = App (Var "handler") handlerArgs
            body = if null bound then call else Do (bound ++ [Run call])
            handlerType = TFun types (TApp (TCon "IO") [TUnit])
        return
            ( [ Signature (callbackDoc e (zip (map paramName params) vars)) name
                  (TFun [TCon (handleName res), handlerType] (TApp (TCon "IO") [TUnit]))
              , BindingWhere name [PCon (handleName res) [PVar "av", PVar "callbacks"], PVar "handler"]
                  (Do
                    [ Bind (PVar "callback") (App (Var ("Raw.wrap" <> callbackTypeOf model (cCallback e))) [Var "callback'"])
                    , Run $ App (Var "registerCallback")
                        [ Var "callbacks", Str (eventName e), Var "callback"
                        , App (Var ("Raw." <> registrationName res e)) [Var "av", Var "callback", Var "nullPtr"] ]
                    ])
                  -- The C callback: the object and user data go unused.
                  [Binding "callback'" (PWild : map PVar vars ++ [PWild]) body]
              ]
            , name )

    -- The registration's doc, then what the C callback's says of its
    -- arguments, without what only C needs.
    callbackDoc e params =
        case filter (not . Text.null) [registered, callback] of
            [] -> Nothing
            ds -> Just (Text.intercalate "\n\n" ds)
      where
        registered = maybe "" (Text.replace " Pass NULL to unset." "")
            (doc (registrationName res e) [])
        callback = maybe "" dropTypeSentence (doc (cCallback e) params)
        -- "The function type for the call callback.", the first sentence.
        dropTypeSentence d = case Text.splitOn "\n\n" d of
            (first : rest) | "The function type for the" `Text.isPrefixOf` first ->
                let first' = Text.stripStart (Text.drop 1 (snd (Text.breakOn ". " first)))
                in Text.intercalate "\n\n" (filter (not . Text.null) (first' : rest))
            _ -> d

    safeType' t = case t of
        SFixedList (SInt 16) _ _ -> Just (TApp (TCon "Vector") [TCon "Int16"])
        SFixedBytes _ _          -> Just (TCon "ByteString")
        _                        -> safeType model t

    convert params p = case paramType p of
        SResourceId i -> (\n -> Left (App (Var n) [v])) <$> idNewtype model i
        SBool         -> Just (Left (App (Var "toBool") [v]))
        SEnum _       -> Just (Left (App (Var "fromCEnum") [v]))
        SSizeT        -> Just (Left (App (Var "fromIntegral") [v]))
        SUInt _       -> Just (Left v)
        SInt _        -> Just (Left v)
        SString       -> Just (Right (App (Var "peekCString") [v]))
        SFixedList (SInt 16) size _ ->
            (\n -> Right (App (Var "peekSizedVector") [v, n])) <$> rawSize size
        SFixedBytes size _ ->
            (\n -> Right (App (Var "peekBytes") [v, n])) <$> rawSize size
        _ -> Nothing
      where
        v = Var (varName (paramName p))
        -- The callback's arguments are all raw C integers.
        rawSize = sizeExprWith (\q -> App (Var "fromIntegral") [Var (varName (paramName q))]) model params
