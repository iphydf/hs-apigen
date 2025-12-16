{-# LANGUAGE OverloadedStrings #-}

-- | Naming and type mapping shared by the Kotlin interfaces and the
-- @Tox*Impl.kt@ classes.
module Apigen.Language.Jvm.Kotlin.Common
    ( -- * Naming
      pascalCase
    , camelCase
    , kotlinMethodName
    , kotlinFunctionName
    , kotlinNameOf
    , kotlinPropertyName
    , kotlinMemberName
    , isKotlinProperty
    , givesValue

      -- * Type rendering
    , renderType
    , jvmBits
    , cRange

      -- * Files and imports
    , kotlinSource
    , typeImports

      -- * Path-id args
    , pathIdArgs
    , pathIdArgsFor

      -- * Method filtering
    , isSkippableMethod
    , isSkippableMethodFor
    ) where

import qualified Apigen.Knowledge                as K
import           Apigen.Language.Jvm.Conventions (hasNoJniShim,
                                                  interfaceAllowsStatics,
                                                  isHiddenFromPublicApi,
                                                  isLifecycleConstructor)
import           Apigen.Language.Jvm.Jni         (pathIds)
import           Apigen.Language.Jvm.Model       (Subsystem (..), classPrefix,
                                                  fileSubsystem, jvmName,
                                                  jvmSource, selfPrefix,
                                                  typeClassName)
import           Apigen.Language.Jvm.Names       (camelCase, pascalCase)
import           Apigen.Model                    (allMethods, arrayTypeOf,
                                                  findEnum, ignoresReturn,
                                                  isEnumNamer, unitBase)
import           Apigen.Semantic                 (SEnumModel (..), SMethod (..),
                                                  SMethodRole (..),
                                                  SParameter (..),
                                                  SResource (..),
                                                  SResourceKind (..),
                                                  SResourceType (..),
                                                  SScalarTypeModel (..),
                                                  SType (..),
                                                  SemanticModel (..))
import qualified Data.List                       as List
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (fromMaybe, listToMaybe,
                                                  mapMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Numeric                         (showHex)

--------------------------------------------------------------------------------
-- Naming
--------------------------------------------------------------------------------

kotlinMethodName :: SemanticModel -> SMethod -> Text
kotlinMethodName model = kotlinFunctionName model . methodName

-- | A C function's Kotlin name: @tox_friend_add@ is @friendAdd@.
kotlinFunctionName :: SemanticModel -> Text -> Text
kotlinFunctionName model = camelCase . functionStem model

-- | A C function's name without its library's prefix: @tox_friend_add@ is
-- @friend_add@.
functionStem :: SemanticModel -> Text -> Text
functionStem model n = fromMaybe (stripLongestPrefix n) ownPrefix
  where
    -- A standalone handle's own functions act on it: @tox_pass_key_encrypt@
    -- is @encrypt@.
    ownPrefix = listToMaybe
        [ rest
        | r <- K.handlesOfKind model StandaloneHandle, m <- methods r
        , methodName m == n, methodRole m == ActionRole
        , Just rest <- [Text.stripPrefix (cPrefix r) n] ]
    stripLongestPrefix t = fromMaybe t (listToMaybe (mapMaybe (`Text.stripPrefix` t) prefixes))
    -- The root's own, then the libraries', longest first. A standalone
    -- handle's isn't stripped: @passKeyDerive@.
    prefixes = selfPrefix model : List.sortOn (negate . Text.length) [ w <> "_" | w <- K.libraryWords model ]

-- | Name of a method emitted as a @val@, without the verb: @tox.savedata@,
-- not @tox.getSavedata@, and @tox.groupList@ for @tox_group_get_group_list@.
kotlinPropertyName :: SemanticModel -> SMethod -> Text
kotlinPropertyName model m =
    camelCase (Text.intercalate "_" (map NonEmpty.head (NonEmpty.group (filter (/= "get") parts))))
  where
    parts = Text.splitOn "_" (functionStem model (methodName m))

-- | Whether the Kotlin API has a method as a @val@: it takes nothing, not
-- even the handle ([takesHandle]), and gives something back.
-- @tox_conference_new@ takes nothing but makes something, so it's a @fun@.
isKotlinProperty :: SemanticModel -> SResource -> SMethod -> Bool -> Bool
isKotlinProperty model r m takesHandle =
    not takesHandle
        && null (pathIdArgsFor model r (methodRole m /= Constructor))
        && null (inputs m)
        && givesValue m
        && methodRole m /= Constructor

-- | Whether the Kotlin API returns anything: not for @void@, nor for a
-- @bool@ that only mirrors the error, which throws instead.
givesValue :: SMethod -> Bool
givesValue m = case (output m, methodErrorType m) of
    (SVoid, _)                        -> False
    (SBool, Just _) | ignoresReturn m -> False
    _                                 -> True

-- | The Kotlin member a C function is, for links: its @val@'s name or its
-- @fun@'s. A standalone handle's methods take the handle, so they're funs.
kotlinMemberName :: SemanticModel -> Text -> Text
kotlinMemberName model n = case [ (r, m) | (r, m) <- allMethods model, methodName m == n ] of
    (r, m) : _ | isKotlinProperty model r m (takesHandle r m) -> kotlinPropertyName model m
    _                                                         -> kotlinFunctionName model n
  where
    takesHandle r m =
        resourceName r `elem` map resourceName (K.handlesOfKind model StandaloneHandle)
            && methodRole m `notElem` [Constructor, StaticRole]

-- | A type's Kotlin name: the root's own @Tox@ and the library's short word,
-- then the rest: @ToxConnection@, @ToxAvCallControl@.
kotlinNameOf :: SemanticModel -> Text -> Text
kotlinNameOf = typeClassName

--------------------------------------------------------------------------------
-- Type rendering
--------------------------------------------------------------------------------

renderType :: SemanticModel -> SType -> Text
renderType model ty = case ty of
    SScalar n _ -> kotlinNameOf model n
    SVoid -> "Unit"
    SBool -> "Boolean"
    SInt n -> intWidth n
    SUInt n -> intWidth n
    SSizeT -> "Long"
    SString -> "String"
    SBytes -> "ByteArray"
    SFixedBytes sizeConst _ -> case arrayTypeOf model sizeConst of
        Just typedefName -> classPrefix model <> pascalCase typedefName
        Nothing          -> "ByteArray"
    SFixedList (SInt 16) _ _ -> "ShortArray"
    SFixedList (SUInt 16) _ _ -> "ShortArray"
    SFixedList (SInt 8) _ _ -> "ByteArray"
    SFixedList (SUInt 8) _ _ -> "ByteArray"
    SFixedList (SInt 32) _ _ -> "IntArray"
    SFixedList (SUInt 32) _ _ -> "IntArray"
    SFixedList element _ _ -> "List<" <> renderType model element <> ">"
    SEnum name -> kotlinNameOf model name
    SHandle name -> pascalCase name
    SCallback name -> kotlinNameOf model name <> "Callback"
    SResourceId name -> kotlinNameOf model name
    SList (SUInt 8) -> "ByteArray"
    SList (SInt 8) -> "ByteArray"
    SList (SInt 16) -> "ShortArray"
    SList (SUInt 16) -> "ShortArray"
    SList (SInt _) -> "IntArray"
    SList (SUInt _) -> "IntArray"
    SList element -> "List<" <> renderType model element <> ">"
  where
    intWidth n = case jvmBits n of
        8  -> "Byte"
        64 -> "Long"
        _  -> "Int"

-- | The bits of the JVM number holding a C one of these: 16-bit ones are
-- @Int@s, as in 'Apigen.Language.Jvm.Java.jniType'.
jvmBits :: Int -> Int
jvmBits n = case n of
    8  -> 8
    64 -> 64
    _  -> 32

-- | A C number's type and range, where its JVM number holds numbers C would
-- truncate: @uint16_t@, @0@ and @0xFFFF@ for an @Int@.
cRange :: SType -> Maybe (Text, Text, Text)
cRange t = case unitBase t of
    SUInt n | jvmBits n > n -> Just ("uint" <> tshow n <> "_t", number 0, number (2 ^ n - 1))
    SInt n | jvmBits n > n  -> Just ("int" <> tshow n <> "_t", number (negate (2 ^ (n - 1))), number (2 ^ (n - 1) - 1))
    _                       -> Nothing
  where
    tshow = Text.pack . show
    number :: Integer -> Text
    number n
        | n < 0     = "-" <> number (negate n)
        | n == 0    = "0"
        | otherwise = "0x" <> Text.toUpper (Text.pack (showHex n ""))

--------------------------------------------------------------------------------
-- Files and imports
--------------------------------------------------------------------------------

-- | A class's file in a package under @im.tox.tox4j@: @core.enums@.
kotlinSource :: SemanticModel -> Text -> Text -> FilePath
kotlinSource model pkg cls = jvmSource model "kotlin" pkg (cls <> ".kt")

-- | The classes a type needs imported.
typeImports :: SemanticModel -> SType -> [Text]
typeImports model ty = case ty of
    SResourceId n -> [jvmName model ("core.data." <> kotlinNameOf model n)]
    SScalar n _ -> [ jvmName model (subWord (fileSubsystem model (scalarFile u)) <> ".data." <> kotlinNameOf model n)
                   | u <- scalarTypes model, scalarName u == n ]
    SEnum n -> [jvmName model (library n <> ".enums." <> kotlinNameOf model n)]
    SFixedBytes size _ -> case arrayTypeOf model size of
        Just typedef -> [jvmName model ("core.data." <> classPrefix model <> pascalCase typedef)]
        Nothing      -> []
    _ -> []
  where
    library n = subWord (fileSubsystem model (maybe "" enumFile (findEnum model n)))

--------------------------------------------------------------------------------
-- Path-id args
--------------------------------------------------------------------------------

pathIdArgs :: SemanticModel -> SResource -> [(Text, Text)]
pathIdArgs model r = pathIdArgsFor model r True

-- | Optionally without the resource's own id (for constructors).
pathIdArgsFor :: SemanticModel -> SResource -> Bool -> [(Text, Text)]
pathIdArgsFor model r includeOwn =
    [ (camelCase short, classPrefix model <> pascalCase idType) | (short, idType) <- pathIds model r includeOwn ]

--------------------------------------------------------------------------------
-- Method filtering
--------------------------------------------------------------------------------

isSkippableMethod :: SemanticModel -> SMethod -> Bool
isSkippableMethod model = isSkippableMethodFor model ""

-- | For interface @iface@ (@ToxCrypto@ keeps statics).
isSkippableMethodFor :: SemanticModel -> Text -> SMethod -> Bool
isSkippableMethodFor model iface m
    | isHiddenFromPublicApi model name = True
    | otherwise = case methodRole m of
        -- Sub-resource constructors like @tox_friend_add@ are public.
        Constructor   -> isLifecycleConstructor model name
        Destructor    -> True
        RegistrarRole -> True
        StaticRole
            | interfaceAllowsStatics model iface -> isEnumNamer m || isCStaticHelper name
            | otherwise                    -> True
        _             -> isIterationDriver name || hasNoJniShim model m
  where
    name = methodName m
    -- Covered by @Enum.name@ and the @*Constants@ objects.
    isCStaticHelper n = "_length" `Text.isSuffixOf` n
    -- Emitted as one generic @iterate(handler, state)@.
    isIterationDriver n = any (`Text.isSuffixOf` n) ["_iterate", "_iteration_interval"]
