{-# LANGUAGE OverloadedStrings #-}
-- | Lookups in the semantic model that every backend needs.
module Apigen.Model
    ( mappingOf
    , allMethods
    , findEnum
    , findResource
    , idModel
    , sizeCompanions
    , defineValue
    , errorMembers
    , isSizeCompanion
    , eventFieldType
    , headerOf
    , enumIn
    , callbackIn
    , isEnumNamer
    , declaredElsewhere
    , returnsBorrowed
    , membersOf
    , rootMembers
    , variantTags
    ) where

import qualified Apigen.Facts     as Facts
import           Apigen.Inference (findRoot, inferCFunctionMapping)
import qualified Apigen.Knowledge as Knowledge
import           Apigen.Semantic
import           Data.List        (find)
import           Data.Maybe       (mapMaybe)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           System.FilePath  (takeFileName)

mappingOf :: SemanticModel -> SResource -> SMethod -> CFunctionMapping
mappingOf model res m = case methodMapping m of
    StandardMapping  -> inferCFunctionMapping (resources model) res m
    CustomMapping cm -> cm

allMethods :: SemanticModel -> [(SResource, SMethod)]
allMethods model = [ (r, m) | r <- resources model, m <- methods r ]

-- | By semantic or C name.
findEnum :: SemanticModel -> Text -> Maybe SEnumModel
findEnum model name =
    find (\e -> enumSemanticName e == name || enumName e == name) (enums model)

findResource :: SemanticModel -> Text -> Maybe SResource
findResource model name = find ((== name) . resourceName) (resources model)

-- | By its name or its resource's.
idModel :: SemanticModel -> Text -> Maybe SIdTypeModel
idModel model name = case find ((== name) . idName) (idTypes model) of
    Just i -> Just i
    Nothing -> case findResource model name of
        Just SResource{resourceType = ResId (SResourceId i)} ->
            find ((== i) . idName) (idTypes model)
        _ -> Nothing

-- | Functions other functions ask for a buffer size.
sizeCompanions :: SemanticModel -> [Text]
sizeCompanions model =
    mapMaybe (\(r, m) -> cSizeFunctionName (mappingOf model r m)) (allMethods model)

isSizeCompanion :: SemanticModel -> Text -> Bool
isSizeCompanion model name = name `elem` sizeCompanions model

defineValue :: SemanticModel -> Text -> Maybe Int
defineValue model name = constantValue <$> find ((== name) . constantName) (constants model)

-- | Without the OK.
errorMembers :: SEnumModel -> [SEnumMember]
errorMembers e = filter ((/= "OK") . enumMemberName) (enumMembers e)

-- | Type of an event's field, from the callback parameter of that name:
-- @tox_event_friend_message_get_friend_number@ is @friend_message@'s
-- @friend_number@.
eventFieldType :: SemanticModel -> SResource -> SMethod -> SType
eventFieldType model r m = case typed ++ known of
    t : _ -> t
    []    -> output m
  where
    field = Text.stripPrefix (cPrefix r <> "get_") (methodName m)
    typed =
        [ paramType p
        | Just f <- [field]
        , root <- filter isRoot (resources model)
        , e <- events root
        , cPrefix r == eventPrefix root (eventName e)
        , p <- eventParams e
        , paramName p == f
        ]
    known =
        [ t
        | Just f <- [field]
        , root <- filter isRoot (resources model)
        , ((e, f'), t) <- Facts.eventFieldTypes
        , cPrefix r == eventPrefix root e, f' == f
        ]
    eventPrefix root e = cPrefix root <> "event_" <> e <> "_"

-- | A declaration's header, without its directory: @toxav.h@.
headerOf :: Text -> Text
headerOf = Text.pack . takeFileName . Text.unpack

enumIn :: Text -> SEnumModel -> Bool
enumIn h e = headerOf (enumFile e) == h

callbackIn :: Text -> SCallbackTypeModel -> Bool
callbackIn h cb = headerOf (cbFile cb) == h

-- | A static function from an enum to a string: @tox_err_set_info_to_string@.
isEnumNamer :: SMethod -> Bool
isEnumNamer m = case map paramType (inputs m) of
    [SEnum _] -> methodRole m == StaticRole && output m == SString
    _         -> False

-- | Declared in another header than its resource: toxav.h's old group
-- calls work on a Tox.
declaredElsewhere :: SResource -> SMethod -> Bool
declaredElsewhere r m = headerOf (methodFile m) /= headerOf (resourceFile r)

-- | Gives another resource's handle without making it: @toxav_get_tox@.
returnsBorrowed :: SResource -> SMethod -> Bool
returnsBorrowed r m = methodRole m /= Constructor && case output m of
    SHandle h -> h /= resourceName r
    _         -> False

-- | A handle and the resources it numbers: a Tox's friends, their files,
-- its groups and their peers.
membersOf :: [SResource] -> SResource -> [SResource]
membersOf allRes h = h :
    [ r | r@SResource{resourceType = ResId _} <- allRes, findRoot allRes r == resourceName h ]

-- | The C enums saying which member of a variant a value is, like the
-- events' type. They aren't values a client passes.
variantTags :: SemanticModel -> [Text]
variantTags = Knowledge.variantTags

-- | The root handles' members: what a client calls on its Tox.
rootMembers :: SemanticModel -> [SResource]
rootMembers model = concatMap (membersOf (resources model)) (filter isRoot (resources model))
