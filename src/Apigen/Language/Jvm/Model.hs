{-# LANGUAGE OverloadedStrings #-}
-- | What the JVM bindings take from the knowledge: which resources each JNI
-- class binds, and names built from the libraries' words.
module Apigen.Language.Jvm.Model
    ( Subsystem (..)
    , subsystems
    , rootSubsystem
    , subsystemOf
    , fileSubsystem
    , typeClassName
    , jvmName
    , jniPrefix
    , nativeHelper
    , stateType
    , optionsClass
    , rootVar
    , native
    , intervalNative
    , jvmSource
    , rootLibraryName
    , coreResourceNames
    , avResourceNames
    , cryptoResourceNames
    , exceptionClassFor
    , libraryClassPrefix
    , libraryMethodName
    , jniMethodName
    , classPrefix
    , standalonePrefix
    , selfPrefix
    , isSettingsName
    , rootIterate
    ) where

import qualified Apigen.Knowledge          as K
import           Apigen.Language.Jvm.Names (camelCase, pascalCase)
import           Apigen.Model              (rootMembers)
import           Apigen.Namespace          (errorStem, shortNamespace,
                                            splitNamespace)
import           Apigen.Semantic
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           System.FilePath           (takeDirectory, takeFileName)

-- | The root and what its ids name. Not the settings: they cross JNI as a
-- message the root's constructor takes.
coreResourceNames :: SemanticModel -> [Text]
coreResourceNames = map resourceName . rootMembers

-- | The other libraries' handles.
avResourceNames :: SemanticModel -> [Text]
avResourceNames = map resourceName . K.libraryHandles

-- | The standalone handles.
cryptoResourceNames :: SemanticModel -> [Text]
cryptoResourceNames model = map resourceName (K.handlesOfKind model StandaloneHandle)

-- | An error enum's exception: @Tox_Err_Friend_Add@ is
-- @ToxFriendAddException@, and @Toxav_Err_Call@ @ToxAvCallException@.
exceptionClassFor :: SemanticModel -> (Text -> Text) -> Text -> Text
exceptionClassFor model pascal err = case (splitNamespace nss err, errorStem nss err) of
    (Just (ns, _), Just stem) -> libraryClassPrefix model ns <> pascal stem <> "Exception"
    _                         -> err
  where
    nss = K.libraryWords model

-- | What a library's classes start with: @Tox@ for the root's, @ToxAv@ for
-- toxav's.
libraryClassPrefix :: SemanticModel -> Text -> Text
libraryClassPrefix model ns
    | ns == K.rootLibrary model = classPrefix model
    | otherwise                 = classPrefix model <> capitalise (shortNamespace (K.libraryWords model) ns)
  where
    capitalise w = Text.toUpper (Text.take 1 w) <> Text.drop 1 w

-- | A function's JNI name: its library's word, then the rest in Pascal
-- case, as @toxFriendAdd@ and @toxavCall@.
libraryMethodName :: SemanticModel -> (Text -> Text) -> (Text -> Text) -> Text -> Text
libraryMethodName model pascalRest otherwise' n = case splitNamespace (K.libraryWords model) n of
    Just (ns, rest) -> ns <> pascalRest rest
    Nothing         -> otherwise' n

-- | A native's name: @toxFriendAdd@.
jniMethodName :: SemanticModel -> Text -> Text
jniMethodName model = libraryMethodName model pascalCase camelCase

-- | Classes start with the root's name: @ToxFriendNumber@.
classPrefix :: SemanticModel -> Text
classPrefix model = maybe "" resourceName (find isRoot (resources model))

-- | What the standalone handles' functions and constants start with, before
-- their own word: @tox_pass_@ for @tox_pass_key_@, @TOX_PASS_@ in upper case.
standalonePrefix :: SemanticModel -> Text
standalonePrefix model = case K.handlesOfKind model StandaloneHandle of
    r : _ -> Text.intercalate "_" (init' (Text.splitOn "_" (Text.dropWhileEnd (== '_') (cPrefix r)))) <> "_"
    []    -> ""
  where
    init' xs = take (length xs - 1) xs

-- | The root's own functions start with this: @tox_self_@.
selfPrefix :: SemanticModel -> Text
selfPrefix model = K.rootLibrary model <> "_self_"

-- | Whether a handle name is the settings'.
isSettingsName :: SemanticModel -> Text -> Bool
isSettingsName model n = n `elem` map resourceName (K.handlesOfKind model SettingsHandle)

-- | The root's iterate function: @tox_iterate@.
rootIterate :: SemanticModel -> Text
rootIterate model = case [ methodName m | r <- resources model, isRoot r, m <- methods r, methodRole m == IterateRole ] of
    f : _ -> f
    []    -> ""

-- | A part of the API with its own classes: the root's (@core@), another
-- library's (@av@) and a standalone handle's (@crypto@).
data Subsystem = Subsystem
    { subWord      :: Text           -- ^ @core@
    , subClass     :: Text           -- ^ @ToxCore@
    , subHandle    :: SResource
    , subKind      :: SResourceKind
    , subResources :: [Text]         -- ^ Those whose functions it binds.
    }

-- | The root's, the other libraries' and the standalone handles', in that
-- order.
subsystems :: SemanticModel -> [Subsystem]
subsystems model =
    [ sub RootHandle r (map resourceName (rootMembers model)) | r <- filter isRoot (resources model) ]
    ++ [ sub LibraryHandle r [resourceName r] | r <- K.libraryHandles model ]
    ++ [ sub StandaloneHandle r [resourceName r] | r <- K.handlesOfKind model StandaloneHandle ]
  where
    sub kind r rs = Subsystem
        { subWord      = word r
        , subClass     = classPrefix model <> capitalise (word r)
        , subHandle    = r
        , subKind      = kind
        , subResources = rs
        }
    -- Its header's directory without the root's word: @toxcore@ is @core@.
    word r =
        let dir = Text.pack (takeFileName (takeDirectory (Text.unpack (resourceFile r))))
        in fromMaybe (fromMaybe dir (Text.stripPrefix (K.rootLibrary model) dir)) (lookup dir (jvmSubsystems (targets model)))
    capitalise w = Text.toUpper (Text.take 1 w) <> Text.drop 1 w

rootSubsystem :: SemanticModel -> Subsystem
rootSubsystem model = case subsystems model of
    s : _ -> s
    []    -> error "Jvm: no root"

-- | The subsystem binding a resource's functions.
subsystemOf :: SemanticModel -> Text -> Maybe Subsystem
subsystemOf model name = find ((name `elem`) . subResources) (subsystems model)

-- | The subsystem of a header's declarations: @av@ for @toxav.h@'s, the
-- root's for a header without a handle.
fileSubsystem :: SemanticModel -> Text -> Subsystem
fileSubsystem model file = fromMaybe (rootSubsystem model) $
    find ((== file) . resourceFile . subHandle) (subsystems model)

-- | A type's class: @ToxAvCallControl@ for @Av_Call_Control@, @ToxPort@ for
-- @Port@.
typeClassName :: SemanticModel -> Text -> Text
typeClassName model n = case splitNamespace (K.libraryWords model) n of
    Just (ns, rest) -> libraryClassPrefix model ns <> pascalCase rest
    Nothing         -> classPrefix model <> pascalCase n

-- | A name in the package: @im.tox.tox4j.core.data@.
jvmName :: SemanticModel -> Text -> Text
jvmName model rest = Text.intercalate "." (filter (not . Text.null) [jvmPackage (targets model), rest])

-- | The package as JNI writes it: @im_tox_tox4j_@.
jniPrefix :: SemanticModel -> Text
jniPrefix model = Text.replace "." "_" (jvmPackage (targets model)) <> "_"

-- | A helper of the hand-written C++, named for the package: @tox4j_fatal@.
nativeHelper :: SemanticModel -> Text -> Text
nativeHelper model name = last (Text.splitOn "." (jvmPackage (targets model))) <> "_" <> name

-- | The root's library, by its header's directory: @toxcore@.
rootLibraryName :: SemanticModel -> Text
rootLibraryName model =
    Text.pack (takeFileName (takeDirectory (Text.unpack (resourceFile (subHandle (rootSubsystem model))))))

-- | The listeners' type parameter: @ToxCoreState@.
stateType :: SemanticModel -> Text
stateType model = subClass (rootSubsystem model) <> "State"

-- | The hand-written options class: @ToxOptions@.
optionsClass :: SemanticModel -> Text
optionsClass model = classPrefix model <> "Options"

-- | What the root's instance is called: @tox@.
rootVar :: SemanticModel -> Text
rootVar model = Text.toLower (Text.take 1 c) <> Text.drop 1 c
  where
    c = classPrefix model

-- | The native of a subsystem's handle's function with a role: @toxNew@.
native :: SemanticModel -> Subsystem -> SMethodRole -> Text
native model s role =
    maybe "" (jniMethodName model . methodName) (find ((== role) . methodRole) (methods (subHandle s)))

-- | The native of how long to wait before iterating: @toxIterationInterval@.
intervalNative :: SemanticModel -> Subsystem -> Text
intervalNative model s = maybe "" (jniMethodName model . methodName) $
    find (\m -> K.isLoopFunction model (methodName m) && output m `elem` [SUInt 32, SInt 32]) (methods (subHandle s))

-- | A source file in a package under the root, in a source set:
-- @lib/src/main/java/im/tox/tox4j/impl/jni/ToxCoreImpl.kt@.
jvmSource :: SemanticModel -> Text -> Text -> Text -> FilePath
jvmSource model set pkg file =
    Text.unpack ("lib/src/main/" <> set <> "/" <> Text.replace "." "/" (jvmName model pkg) <> "/" <> file)
