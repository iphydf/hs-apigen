{-# LANGUAGE OverloadedStrings #-}
-- | A library that isn't Tox, in Tox's standards: what holds the
-- backends to knowing only the standards.
module Apigen.OtherLibrary
    ( headers
    , fooTargets
    , model
    ) where

import           Apigen.Parser          (parseModel)
import qualified Apigen.Parser.Semantic as Semantic
import           Apigen.Semantic
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Language.Cimple.IO     (parseText)

header :: Text
header = Text.unlines
    [ "#ifndef C_FOO_FOO_H"
    , "#define C_FOO_FOO_H"
    , ""
    , "#include <stdbool.h>"
    , "#include <stddef.h>"
    , "#include <stdint.h>"
    , ""
    , "typedef struct Foo Foo;"
    , ""
    , "typedef uint32_t Foo_Bar_Number;"
    , ""
    , "typedef uint16_t Foo_Weight;"
    , ""
    , "#define FOO_KEY_SIZE 32"
    , ""
    , "typedef enum Foo_Colour {"
    , "    FOO_COLOUR_RED,"
    , "    FOO_COLOUR_BLUE,"
    , "} Foo_Colour;"
    , ""
    , "typedef enum Foo_Err_New {"
    , "    FOO_ERR_NEW_OK,"
    , "    FOO_ERR_NEW_MALLOC,"
    , "} Foo_Err_New;"
    , ""
    , "typedef enum Foo_Err_Bar_Add {"
    , "    FOO_ERR_BAR_ADD_OK,"
    , "    FOO_ERR_BAR_ADD_NULL,"
    , "    FOO_ERR_BAR_ADD_TOO_LONG,"
    , "} Foo_Err_Bar_Add;"
    , ""
    , "#define FOO_VERSION_MAJOR 1"
    , "#define FOO_VERSION_MINOR 2"
    , "#define FOO_VERSION_PATCH 3"
    , ""
    , "uint32_t foo_version_major(void);"
    , "uint32_t foo_version_minor(void);"
    , "uint32_t foo_version_patch(void);"
    , "bool foo_version_is_compatible(uint32_t major, uint32_t minor, uint32_t patch);"
    , ""
    , "#define FOO_MAX_NAME_LENGTH 64"
    , ""
    , "uint32_t foo_max_name_length(void);"
    , ""
    , "typedef struct Foo_Options Foo_Options;"
    , ""
    , "bool foo_options_get_quiet(const Foo_Options *options);"
    , "void foo_options_set_quiet(Foo_Options *options, bool quiet);"
    , "Foo_Colour foo_options_get_colour(const Foo_Options *options);"
    , "void foo_options_set_colour(Foo_Options *options, Foo_Colour colour);"
    , "bool foo_options_get_experimental_thread_safety(const Foo_Options *options);"
    , "void foo_options_set_experimental_thread_safety(Foo_Options *options, bool experimental_thread_safety);"
    , "void foo_options_default(Foo_Options *options);"
    , ""
    , "typedef enum Foo_Err_Options_New {"
    , "    FOO_ERR_OPTIONS_NEW_OK,"
    , "    FOO_ERR_OPTIONS_NEW_MALLOC,"
    , "} Foo_Err_Options_New;"
    , ""
    , "Foo_Options *foo_options_new(Foo_Err_Options_New *error);"
    , "void foo_options_free(Foo_Options *options);"
    , ""
    , "Foo *foo_new(const Foo_Options *options, Foo_Err_New *error);"
    , "void foo_kill(Foo *foo);"
    , "void foo_iterate(Foo *foo, void *user_data);"
    , "uint32_t foo_iteration_interval(const Foo *foo);"
    , ""
    , "Foo_Colour foo_self_get_colour(const Foo *foo);"
    , "void foo_self_set_colour(Foo *foo, Foo_Colour colour);"
    , ""
    , "Foo_Bar_Number foo_bar_add(Foo *foo, const uint8_t name[], size_t length, Foo_Err_Bar_Add *error);"
    , "bool foo_bar_exists(const Foo *foo, Foo_Bar_Number bar_number);"
    , "void foo_bar_set_weight(Foo *foo, Foo_Bar_Number bar_number, Foo_Weight weight);"
    , "typedef void foo_bar_weight_cb(Foo *foo, Foo_Bar_Number bar_number, Foo_Weight weight, void *user_data);"
    , "void foo_callback_bar_weight(Foo *foo, foo_bar_weight_cb *callback);"
    , ""
    , "uint32_t foo_bar_get_bar_count(const Foo *foo);"
    , ""
    , "size_t foo_self_get_bar_list_size(const Foo *foo);"
    , "void foo_self_get_bar_list(const Foo *foo, Foo_Bar_Number bar_list[]);"
    , ""
    , "typedef enum Foo_Err_Bar_Query {"
    , "    FOO_ERR_BAR_QUERY_OK,"
    , "    FOO_ERR_BAR_QUERY_NOT_FOUND,"
    , "} Foo_Err_Bar_Query;"
    , ""
    , "size_t foo_bar_get_name_size(const Foo *foo, Foo_Bar_Number bar_number, Foo_Err_Bar_Query *error);"
    , "bool foo_bar_get_name(const Foo *foo, Foo_Bar_Number bar_number, uint8_t name[], Foo_Err_Bar_Query *error);"
    , ""
    , "typedef void foo_bar_name_cb(Foo *foo, Foo_Bar_Number bar_number, const uint8_t name[], size_t length, void *user_data);"
    , "void foo_callback_bar_name(Foo *foo, foo_bar_name_cb *callback);"
    , ""
    , "bool foo_bar_send_sound(Foo *foo, Foo_Bar_Number bar_number, const int16_t pcm[/*! sample_count * channels */], size_t sample_count, uint8_t channels);"
    , "bool foo_bar_send_frame(Foo *foo, Foo_Bar_Number bar_number, const uint8_t data[/*! width * height */], uint16_t width, uint16_t height);"
    , "typedef void foo_bar_receive_frame_cb(Foo *foo, Foo_Bar_Number bar_number, const uint8_t data[/*! width * height */], uint16_t width, uint16_t height, void *user_data);"
    , "void foo_callback_bar_receive_frame(Foo *foo, foo_bar_receive_frame_cb *callback);"
    , ""
    , "#endif /* C_FOO_FOO_H */"
    ]

eventsHeader :: Text
eventsHeader = Text.unlines
    [ "#ifndef C_FOO_FOO_EVENTS_H"
    , "#define C_FOO_FOO_EVENTS_H"
    , ""
    , "#include <stdint.h>"
    , ""
    , "#include \"foo.h\""
    , ""
    , "typedef struct Foo_Event_Bar_Name Foo_Event_Bar_Name;"
    , "Foo_Bar_Number foo_event_bar_name_get_bar_number(const Foo_Event_Bar_Name *bar_name);"
    , "const uint8_t *foo_event_bar_name_get_name(const Foo_Event_Bar_Name *bar_name);"
    , "uint32_t foo_event_bar_name_get_name_length(const Foo_Event_Bar_Name *bar_name);"
    , ""
    , "typedef enum Foo_Event_Type {"
    , "    FOO_EVENT_BAR_NAME = 0,"
    , "    FOO_EVENT_INVALID = 255,"
    , "} Foo_Event_Type;"
    , ""
    , "const char *foo_event_type_to_string(Foo_Event_Type type);"
    , ""
    , "typedef struct Foo_Event Foo_Event;"
    , "Foo_Event_Type foo_event_get_type(const Foo_Event *event);"
    , "const Foo_Event_Bar_Name *foo_event_get_bar_name(const Foo_Event *event);"
    , ""
    , "typedef struct Foo_Events Foo_Events;"
    , "uint32_t foo_events_get_size(const Foo_Events *events);"
    , "const Foo_Event *foo_events_get(const Foo_Events *events, uint32_t index);"
    , ""
    , "typedef enum Foo_Err_Events_Iterate {"
    , "    FOO_ERR_EVENTS_ITERATE_OK,"
    , "    FOO_ERR_EVENTS_ITERATE_MALLOC,"
    , "} Foo_Err_Events_Iterate;"
    , ""
    , "Foo_Events *foo_events_iterate(Foo *foo, Foo_Err_Events_Iterate *error);"
    , "void foo_events_free(Foo_Events *events);"
    , ""
    , "#endif /* C_FOO_FOO_EVENTS_H */"
    ]

-- | The headers, by where they're installed.
headers :: [(FilePath, Text)]
headers = [("foo/foo.h", header), ("foo/foo_events.h", eventsHeader)]

fooTargets :: STargets
fooTargets = emptyTargets
    { haskellModules    = "Network.Foo.C"
    , haskellRawModules = "FFI.Foo"
    , includeDir        = "foo"
    , pythonPackage     = "pyfoo"
    , cppNamespace      = "foo"
    , dartPackage       = "foo"
    , dartLibrary       = ("ffi/foo_library.dart", "FooLibrary")
    , dartFfi           = ("ffi/generated/foo.ffi.dart", "FooFfi")
    , jvmPackage        = "org.example.foo"
    , jvmNativeLibrary  = "foo-jni"
    }

model :: IO SemanticModel
model = do
    tus <- mapM (\(f, t) -> either fail (return . (,) f) (parseText t)) headers
    let sem = Semantic.toSemantic False (parseModel tus)
    return sem { targets = fooTargets }
