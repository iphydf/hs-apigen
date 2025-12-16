{-# LANGUAGE OverloadedStrings #-}
-- | @Network.Tox.C.Events@: the events Tox records while iterating, decoded
-- into Haskell records.
module Apigen.Language.Haskell.Events
    ( eventsModule
    ) where

import qualified Apigen.Knowledge              as K
import           Apigen.Language.Haskell.AST
import           Apigen.Language.Haskell.Model
import           Apigen.Language.Haskell.Names (lowerFirst, typeName)
import           Apigen.Language.Haskell.Safe  (safeType)
import           Apigen.Model                  (eventFieldType)
import           Apigen.Namespace              (rootNamespace)
import           Apigen.Semantic
import           Data.List                     (find)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

data EventField = EventField
    { efName  :: Text
    , efCName :: Text  -- ^ The field's C name, as the callback docs call it
    , efType  :: Type
    , efRead  :: Expr
    }

eventsModule :: (Text -> [(Text, Text)] -> Maybe Text) -> SemanticModel -> Maybe ([Decl], [Export])
eventsModule doc model = do
    variant <- K.eventsVariant model
    eventsRes : _ <- Just (K.handlesOfKind model ViewList)
    root <- find isRoot (resources model)
    -- The list's constructor that takes the root: iterating it.
    iterate' <- find (\m -> methodRole m == Constructor && takesRoot (mappingOf model eventsRes m)) (methods eventsRes)
    let iterateC = cFunctionName (mappingOf model eventsRes iterate')
        iterateName = lowerFirst (typeName iterateC)
        rootTy = handleName root
        rootVar = Text.toLower (last (Text.splitOn "_" (resourceName root)))
    errType <- methodErrorType iterate' >>= findEnum model
    typeEnum <- findEnum model (variantTypeEnum variant)
    free <- methodName <$> find ((== Destructor) . methodRole) (methods eventsRes)
    Iterable{iterSize = size, iterAccessor = get} <- find isIterable (traits eventsRes)
    members <- mapM (memberOf eventsRes) (variantMembers variant)
    let eventType = DataDecl (Just "An event, by the callback that would have told of it.") "Event"
            [ Con (typeName (memberName m)) Nothing [TCon (typeName (resourceName r))] | (m, r, _) <- members ]
            ["Eq", "Show"]
        covered = map (memberName . (\(m, _, _) -> m)) members
        uncovered = [ mem | mem <- enumMembers typeEnum, enumMemberName mem `notElem` covered ]
        decode = Binding "peekEvent" [PVar "event"] $ Do
            [ Bind (PVar "eventType") (Op (Var "fromCEnum") "<$>" (App (Var "Raw.tox_event_get_type") [Var "event"]))
            , Run $ Case (Var "eventType") $
                [ ( PCon (enumConstructor typeEnum (memberName m)) []
                  , Op (Var (typeName (memberName m))) "<$>" (App (Var (peekName r)) [Var "event"]) )
                | (m, r, _) <- members ]
                ++ [ (PWild, App (Var "ioError") [App (Var "userError") [Str "an event of a type the bindings don't know"]])
                   | not (null uncovered) ]
            ]
        prefix = cPrefix eventsRes
    return
        ( [eventType]
          ++ concatMap (\(_, r, fields) -> [record r fields]) members
          ++ [ Signature (doc iterateC [])
                 iterateName
                 (TFun [TCon rootTy] (TApp (TCon "IO") [TApp (TCon "Either") [TCon (typeName (enumSemanticName errType)), TApp (TCon "[]") [TCon "Event"]]]))
             , Binding iterateName [PCon rootTy [PVar rootVar]] $
                 Op (App (Var "callErrFun") [App (Var ("Raw." <> iterateC)) [Var rootVar, Var "nullPtr"]])
                    ">>=" (App (Var "traverse") [Var "peekEvents"])
             , Signature (Just "The events in a batch, which this frees.") "peekEvents"
                 (TFun [TCon (handleName eventsRes <> "Ptr")] (TApp (TCon "IO") [TApp (TCon "[]") [TCon "Event"]]))
             , Binding "peekEvents" [PVar "events"] $
                 App (Var "finally")
                    [ Op (App (Var "iterateList") [Var ("Raw." <> prefix <> size), Var ("Raw." <> prefix <> get), Var "events"])
                         ">>=" (App (Var "mapM") [Var "peekEvent"])
                    , App (Var ("Raw." <> cFunctionName (mappingOf model eventsRes (methodByName eventsRes free)))) [Var "events"]
                    ]
             , Signature Nothing "peekEvent" (TFun [TCon "EventPtr"] (TApp (TCon "IO") [TCon "Event"]))
             , decode
             ]
          ++ concatMap (\(m, r, fields) -> peekRecord m r fields) members
        , ExportAll "Event"
          : [ ExportAll (typeName (resourceName r)) | (_, r, _) <- members ]
          ++ [ExportVar iterateName]
        )
  where
    isIterable Iterable{} = True
    takesRoot cm = not (null [ () | PathObject 0 _ <- argMapping cm ])

    methodByName r n = case find ((== n) . methodName) (methods r) of
        Just m  -> m
        Nothing -> error ("no method " <> show n)

    memberOf _ m = do
        r <- find ((== memberType m) . resourceName) (resources model)
        fields <- mapM (fieldOf r) (fieldMethods r)
        return (m, r, fields)

    fieldMethods r =
        [ m | m <- methods r
            , cFunctionName (mappingOf model r m) `notElem` sizeCompanions model ]

    fieldOf r m = do
        let mapping = mappingOf model r m
            cName' = cFunctionName mapping
            short = maybe cName' id (Text.stripPrefix "get_" =<< Text.stripPrefix (cPrefix r) cName')
            ty = eventFieldType model r m
            getCall = App (Var ("Raw." <> cName')) [Var "ptr"]
            sizeCall = (\s -> App (Var ("Raw." <> s)) [Var "ptr"]) <$> cSizeFunctionName mapping
        safe <- safeType model ty
        reader <- case ty of
            SResourceId i -> (\n -> Op (Var n) "<$>" getCall) <$> idNewtype model i
            SEnum _       -> Just (Op (Var "fromCEnum") "<$>" getCall)
            SBool         -> Just (Op (Var "toBool") "<$>" getCall)
            SSizeT        -> Just (Op (Var "fromIntegral") "<$>" getCall)
            SUInt _       -> Just getCall
            SInt _        -> Just getCall
            SFixedBytes _ _ -> Just (Op getCall ">>=" (Var "peekFixedBytes"))
            SBytes        -> (\s -> App (Var "peekSizedBytes") [getCall, s]) <$> sizeCall
            SString       -> (\s -> App (Var "peekSizedString") [getCall, s]) <$> sizeCall
            _             -> Nothing
        return EventField
            { efName  = recordField r short
            , efCName = short
            , efType  = safe
            , efRead  = reader
            }

    -- Without the variant's name: @Friend_Name@ for @Event_Friend_Name@.
    shortName r = maybe (resourceName r) id (Text.stripPrefix eventPrefix (resourceName r))
    eventPrefix = maybe "" ((<> "_") . variantName) (K.eventsVariant model)

    -- The event's name, then the field's, once if it ends with it: @friendName@,
    -- not @friendNameName@.
    recordField r short
        | typeName short `Text.isSuffixOf` event = lowerFirst event
        | otherwise                             = lowerFirst event <> typeName short
      where
        event = typeName (shortName r)

    -- What the callback registration says of the event, and what the
    -- callback's doc says of its fields.
    callbackDoc r fields =
        case filter (not . Text.null) [summary, params] of
            [] -> Nothing
            ds -> Just (Text.intercalate "\n\n" ds)
      where
        event = Text.toLower (shortName r)
        params = maybe "" id (doc (rootNamespace model <> "_" <> event <> "_cb") [ (efCName f, efName f) | f <- fields ])
        summary = case doc (rootNamespace model <> "_callback_" <> event) [] of
            Just d  -> Text.intercalate "\n\n" (filter (not . setter) (Text.splitOn "\n\n" d))
            Nothing -> ""
        -- Set the callback for the @friend_request@ event. Pass NULL to unset.
        setter p = "Set the callback for the" `Text.isPrefixOf` p

    record r fields =
        RecordDecl (callbackDoc r fields) (typeName (resourceName r))
            [ Field (efName f) (efType f) Nothing | f <- fields ]
            ["Eq", "Show"]

    peekName r = "peek" <> typeName (shortName r)

    peekRecord m r fields =
        [ Signature Nothing (peekName r)
            (TFun [TCon "EventPtr"] (TApp (TCon "IO") [TCon (typeName (resourceName r))]))
        , Binding (peekName r) [PVar "event"] $ Do
            [ Bind (PVar "ptr") (App (Var ("Raw." <> memberGetter m)) [Var "event"])
            , Run $ case fields of
                [] -> App (Var "pure") [Var (typeName (resourceName r))]
                (f : rest) ->
                    foldl (\acc x -> Op acc "<*>" (efRead x))
                          (Op (Var (typeName (resourceName r))) "<$>" (efRead f)) rest
            ]
        ]

    enumConstructor e member = typeName (enumSemanticName e) <> typeName member

