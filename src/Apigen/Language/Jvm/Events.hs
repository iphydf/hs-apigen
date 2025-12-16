{-# LANGUAGE OverloadedStrings #-}
-- | Events, from C to Kotlin: a C++ callback per event appends it to a byte
-- buffer, and Kotlin reads that back into a sealed class per subsystem. One
-- description of each type's wire form makes both sides.
module Apigen.Language.Jvm.Events (generate) where

import           Apigen.Language.Jvm.Kdoc          (renderMemberKdoc,
                                                    transformKdoc)
import           Apigen.Language.Jvm.Kotlin.Common (camelCase, jvmBits,
                                                    kotlinNameOf, kotlinSource,
                                                    pascalCase, renderType,
                                                    typeImports)
import           Apigen.Language.Jvm.Model         (Subsystem (..), classPrefix,
                                                    isShortList, jvmName,
                                                    jvmSource, subsystems)
import           Apigen.Model                      (arrayTypeOf, findEnum,
                                                    findResource, idModel)
import           Apigen.Parser.Docs                (Docs (..))
import           Apigen.Semantic                   (SCallbackTypeModel (..),
                                                    SEnumModel (..),
                                                    SEvent (..),
                                                    SIdTypeModel (..),
                                                    SParameter (..),
                                                    SResource (..),
                                                    SResourceKind (..),
                                                    SType (..),
                                                    SemanticModel (..))
import           Apigen.SizeExpr                   (SizeExpr (..),
                                                    renderSizeExpr)
import           Data.List                         (find, nub, sort, sortOn)
import qualified Data.List                         as List
import qualified Data.Map.Strict                   as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

generate :: Docs -> SemanticModel -> [(FilePath, Text)]
generate docs model = concat
    [ [ eventsFile docs model s evs, readerFile model s evs, writerFile model s evs ]
    | s <- subsystems model, subKind s `elem` [RootHandle, LibraryHandle]
    , let evs = eventsOf model s
    ]

data Event = Event
    { evEvent    :: SEvent
    , evCallback :: SCallbackTypeModel
    }

-- | In name order, which gives each its tag.
eventsOf :: SemanticModel -> Subsystem -> [Event]
eventsOf model s = sortOn (eventName . evEvent)
    [ Event e cb
    | e <- events (subHandle s)
    , Just cb <- [find ((== cCallback e) . cbCName) (callbacks model)]
    ]

params :: Event -> [SParameter]
params = eventParams . evEvent

className :: Event -> Text
className = pascalCase . eventName . evEvent

eventInterface :: Subsystem -> Text
eventInterface s = subClass s <> "Event"

readerObject :: Subsystem -> Text
readerObject s = subClass s <> "EventReader"

--------------------------------------------------------------------------------
-- The wire
--------------------------------------------------------------------------------

-- | How a value travels, in the machine's byte order: the buffer never
-- leaves the process.
data Wire
    = WBool   -- ^ One byte.
    | WInt    -- ^ 4 bytes: numbers up to 32 bits, IDs, enums (C's value).
    | WLong   -- ^ 8 bytes.
    | WBytes  -- ^ A 4-byte length, then the bytes.
    | WShorts -- ^ A 4-byte count, then the shorts.

wire :: SType -> Wire
wire ty = case ty of
    SScalar _ b     -> wire b
    SBool           -> WBool
    SInt n          -> number n
    SUInt n         -> number n
    SSizeT          -> WLong
    SEnum _         -> WInt
    SResourceId _   -> WInt
    SBytes          -> WBytes
    SFixedBytes _ _ -> WBytes
    _ | isShortList ty -> WShorts
    _ -> error ("Apigen.Language.Jvm.Events: no wire form for " <> show ty)
  where
    number n = if jvmBits n == 64 then WLong else WInt

--------------------------------------------------------------------------------
-- Kotlin: the events
--------------------------------------------------------------------------------

eventsFile :: Docs -> SemanticModel -> Subsystem -> [Event] -> (FilePath, Text)
eventsFile docs model s evs = (kotlinSource model (subWord s) name, body)
  where
    name = eventInterface s
    body = Text.unlines $
        [ "package " <> jvmName model (subWord s)
        , ""
        ]
        ++ imports model (concatMap params evs)
        ++ [ "/** What [" <> subClass s <> ".iterate] reports. */"
           , "sealed interface " <> name <> " {"
           ]
        ++ List.intercalate [""] (map eventClass evs)
        ++ ["}"]

    eventClass ev = kdoc ev ++ case params ev of
        [] -> ["    data object " <> className ev <> " : " <> name]
        ps ->
            ["    class " <> className ev <> "("]
            ++ [ "        val " <> camelCase (paramName p) <> ": " <> renderType model (paramType p) <> "," | p <- ps ]
            ++ ["    ) : " <> name]

    -- The callback's doxygen, whose parameters are now properties.
    kdoc ev = maybe [] (renderMemberKdoc 4 . Text.replace "@param " "@property " . transformKdoc model)
        (Map.lookup (cbCName (evCallback ev)) (funcDocs docs))

imports :: SemanticModel -> [SParameter] -> [Text]
imports model ps = case sort (nub (concatMap (typeImports model . paramType) ps)) of
    []   -> []
    imps -> map ("import " <>) imps ++ [""]

--------------------------------------------------------------------------------
-- Kotlin: reading them
--------------------------------------------------------------------------------

readerFile :: SemanticModel -> Subsystem -> [Event] -> (FilePath, Text)
readerFile model s evs = (jvmSource model "java" "impl.jni" (readerObject s <> ".kt"), body)
  where
    iface = eventInterface s
    types = map paramType (concatMap params evs)
    body = Text.unlines $
        [ "package " <> jvmName model "impl.jni"
        , ""
        ]
        ++ map ("import " <>) (sort (nub (jvmName model (subWord s <> "." <> iface)
            : concatMap (typeImports model) types
            ++ ["java.nio.ByteBuffer", "java.nio.ByteOrder"])))
        ++ [ ""
           , "/** Reads what the C++ callbacks wrote: each event's tag, then its fields. */"
           , "internal object " <> readerObject s <> " {"
           , "    fun read(bytes: ByteArray?): List<" <> iface <> "> {"
           , "        if (bytes == null) return emptyList()"
           , "        val buf = ByteBuffer.wrap(bytes).order(ByteOrder.nativeOrder())"
           , "        return buildList {"
           , "            while (buf.hasRemaining()) add(event(buf))"
           , "        }"
           , "    }"
           , ""
           , "    private fun event(buf: ByteBuffer): " <> iface <> " ="
           , "        when (val tag = buf.get().toInt()) {"
           ]
        ++ List.intercalate [""] (zipWith branch [0 :: Int ..] evs
            ++ [[ "            else -> {", "                error(\"Unknown event $tag\")", "            }" ]])
        ++ [ "        }"
           , "}"
           ]
        ++ helpers

    branch tag ev =
        [ "            " <> tshow tag <> " -> {" ]
        ++ call 16 (iface <> "." <> className ev) (map (readValue model . paramType) (params ev))
        ++ [ "            }" ]

    helpers = concat
        [ [ "", "private fun ByteBuffer.bool(): Boolean = get() != 0.toByte()" ] | any (isWire WBool) types ]
        ++ concat [ [ "", "private fun ByteBuffer.bytes(): ByteArray = ByteArray(int).also(this::get)" ] | any (isWire WBytes) types ]
        ++ concat [ [ "", "private fun ByteBuffer.shorts(): ShortArray = ShortArray(int) { short }" ] | any (isWire WShorts) types ]
        ++ concat [ [ ""
                    , "// A newer C library can send values we don't know."
                    , "private fun <T> List<T>.atOrFirst(index: Int): T = getOrNull(index) ?: first()"
                    ] | any isEnum types ]

    isWire w t = case (w, wire t) of
        (WBool, WBool)     -> True
        (WBytes, WBytes)   -> True
        (WShorts, WShorts) -> True
        _                  -> False
    isEnum t = case t of
        SEnum _ -> True
        _       -> False

-- | A call on one line if it fits in ktlint's 140 columns, else an argument
-- a line.
call :: Int -> Text -> [Text] -> [Text]
call col f args
    | Text.length one <= 140 = [one]
    | otherwise = [indent <> f <> "("] ++ [ indent <> "    " <> a <> "," | a <- args ] ++ [indent <> ")"]
  where
    indent = Text.replicate col " "
    one = indent <> f <> "(" <> Text.intercalate ", " args <> ")"

readValue :: SemanticModel -> SType -> Text
readValue model ty = case ty of
    SScalar n b     -> kotlinNameOf model n <> "(" <> readValue model b <> ")"
    SResourceId n   -> kotlinNameOf model n <> "(buf.int)"
    SEnum n         -> kotlinNameOf model n <> ".entries.atOrFirst(buf.int)"
    SFixedBytes s _ | Just t <- arrayTypeOf model s -> classPrefix model <> pascalCase t <> "(buf.bytes())"
    SInt 8          -> "buf.int.toByte()"
    SUInt 8         -> "buf.int.toByte()"
    _ -> case wire ty of
        WBool   -> "buf.bool()"
        WInt    -> "buf.int"
        WLong   -> "buf.long"
        WBytes  -> "buf.bytes()"
        WShorts -> "buf.shorts()"

--------------------------------------------------------------------------------
-- C++: writing them
--------------------------------------------------------------------------------

writerFile :: SemanticModel -> Subsystem -> [Event] -> (FilePath, Text)
writerFile model s evs = ("lib/src/main/cpp/" <> Text.unpack (subClass s) <> "/generated/events.h", body)
  where
    handle = cName (subHandle s) <> " *"
    withUserData = any (eventHasUserData . evEvent) evs
    body = Text.unlines $
        concat (zipWith (trampoline model) [0 :: Int ..] evs)
        ++ [ "static void"
           , "register_callbacks (" <> handle <> "self" <> (if withUserData then ", EventWriter *events" else "") <> ")"
           , "{"
           ]
        ++ [ "  " <> eventRegistrar (evEvent ev) <> " (self, " <> callbackName ev
                <> (if eventHasUserData (evEvent ev) then ", events" else "") <> ");"
           | ev <- evs ]
        ++ [ "}" ]

callbackName :: Event -> Text
callbackName ev = "on_" <> eventName (evEvent ev)

-- | A callback of C's exact type, so it registers as it is.
trampoline :: SemanticModel -> Int -> Event -> [Text]
trampoline model tag ev =
    [ "static void"
    , callbackName ev <> " (" <> Text.intercalate ", " (concat (zipWith cParam [0 :: Int ..] (cbParams (evCallback ev)))) <> ")"
    , "{"
    , "  auto event = static_cast<EventWriter *> (user_data)->event (" <> tshow tag <> ");"
    ]
    ++ [ "  event." <> write p <> ";" | p <- params ev ]
    ++ [ "}"
       , ""
       ]
  where
    cParam i p = case paramType p of
        SHandle "void" -> ["void *user_data"]
        -- The library's own handle: the writer doesn't need it.
        SHandle h | i == 0 -> [maybe h cName (findResource model h) <> " *"]
        SBytes -> ["uint8_t const *" <> paramName p, "size_t " <> paramName p <> "_length"]
        t -> let c = cType model t in [c <> (if "*" `Text.isSuffixOf` c then "" else " ") <> paramName p]

    write p = let n = paramName p in case wire (paramType p) of
        WBool   -> "b (" <> n <> ")"
        WInt    -> "i32 (" <> n <> ")"
        WLong   -> "i64 (" <> n <> ")"
        WBytes  -> "bytes (" <> n <> ", " <> size p <> ")"
        WShorts -> "shorts (" <> n <> ", " <> size p <> ")"

    size p = case paramType p of
        SFixedBytes e _  -> cSize e
        SFixedList _ e _ -> cSize e
        _                -> paramName p <> "_length"

-- | The size, with the writer's @max@ and @abs@, which take any number.
cSize :: SizeExpr -> Text
cSize = renderSizeExpr . go
  where
    go e = case e of
        SizeCall f as -> SizeCall ("size_expr::" <> f) (map go as)
        SizeOp o a b  -> SizeOp o (go a) (go b)
        _             -> e

cType :: SemanticModel -> SType -> Text
cType model t = case t of
    SScalar _ b      -> cType model b
    SBool            -> "bool"
    SInt n           -> "int" <> tshow n <> "_t"
    SUInt n          -> "uint" <> tshow n <> "_t"
    SSizeT           -> "size_t"
    SEnum e          -> maybe e enumName (findEnum model e)
    SResourceId i    -> maybe i idCName (idModel model i)
    SFixedBytes _ _  -> "uint8_t const *"
    SFixedList e _ _ -> cType model e <> " const *"
    _                -> error ("Apigen.Language.Jvm.Events: no C type for " <> show t)

tshow :: Show a => a -> Text
tshow = Text.pack . show
