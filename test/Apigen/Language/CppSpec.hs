{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.CppSpec where

import           Apigen.Language.Cpp    (generate)
import           Apigen.Parser          (parseModel)
import qualified Apigen.Parser.Semantic as Semantic
import           Apigen.Semantic
import           Apigen.Types           (Constness (..))
import           Data.List              (isInfixOf)
import qualified Data.Text              as Text
import           Language.Cimple.IO     (parseText)
import           Test.Hspec             (Spec, describe, it, shouldBe,
                                         shouldContain)

-- Helper to compile C source to C++
compileToCpp :: Text.Text -> IO String
compileToCpp source = do
    let tuResult = parseText source
    case tuResult of
        Left err -> fail err
        Right nodes -> do
            let tu = ("test.h", nodes)
            let model = parseModel [tu]
            let sem = Semantic.toSemantic False model
            return $ Text.unpack $ generate sem

shouldGenerate :: [Text.Text] -> [String] -> IO ()
shouldGenerate cLines expectedCppLines = do
    cpp <- compileToCpp (Text.unlines cLines)
    let cppLines = lines cpp
    if expectedCppLines `isInfixOf` cppLines
        then return ()
        else fail $ "Expected C++ lines to be consecutive in output:\n" ++ show expectedCppLines ++ "\n\nActual output:\n" ++ cpp

spec :: Spec
spec = do
    describe "generate" $ do
        it "generates C++ from C source correctly" $ do
            let source =
                    [ "typedef struct Tox Tox;"
                    , "typedef struct Tox_Options Tox_Options;"
                    , "typedef enum Tox_Err_New {"
                    , "  TOX_ERR_NEW_OK,"
                    , "  TOX_ERR_NEW_NULL,"
                    , "} Tox_Err_New;"
                    , "Tox *tox_new(const struct Tox_Options *options, Tox_Err_New *error);"
                    , "void tox_kill(Tox *tox);"
                    ]
            let expected =
                    [ "    ::Tox_Err_New error = static_cast<::Tox_Err_New>(TOX_ERR_NEW_OK);"
                    , "    auto result = ::tox_new(options.instance_, &error);"
                    , "    if (error != static_cast<::Tox_Err_New>(TOX_ERR_NEW_OK)) {"
                    ]
            source `shouldGenerate` expected

        it "generates group self get name correctly" $ do
            let source =
                    [ "typedef struct Tox Tox;"
                    , "typedef uint32_t Tox_Group_Number;"
                    , "typedef enum Tox_Err_Group_Self_Query {"
                    , "  TOX_ERR_GROUP_SELF_QUERY_OK,"
                    , "  TOX_ERR_GROUP_SELF_QUERY_GROUP_NOT_FOUND,"
                    , "} Tox_Err_Group_Self_Query;"
                    , "size_t tox_group_self_get_name_size(const Tox *tox, Tox_Group_Number group_number, Tox_Err_Group_Self_Query *error);"
                    , "bool tox_group_self_get_name(const Tox *tox, Tox_Group_Number group_number, uint8_t *name, Tox_Err_Group_Self_Query *error);"
                    ]
            let expected =
                    [ "    ::Tox_Err_Group_Self_Query error = static_cast<::Tox_Err_Group_Self_Query>(TOX_ERR_GROUP_SELF_QUERY_OK);"
                    , "    size_t size = ::tox_group_self_get_name_size(core_.instance_, id_.value, &error);"
                    , "    std::vector<uint8_t> result(size);"
                    , "    bool ok = ::tox_group_self_get_name(core_.instance_, id_.value, result.data(), &error);"
                    , "    if (!ok) {"
                    , "      if (error != static_cast<::Tox_Err_Group_Self_Query>(TOX_ERR_GROUP_SELF_QUERY_OK)) {"
                    , "        return static_cast<Err_Group_Self_Query>(error);"
                    , "      }"
                    , "    }"
                    , "    return result;"
                    ]
            source `shouldGenerate` expected

        it "generates const method for const C pointer" $ do
            let source =
                    [ "typedef struct Tox Tox;"
                    , "uint32_t tox_get_something(const Tox *tox);"
                    ]
            let expected =
                    [ "  uint32_t Tox::get_something() const {"
                    , "    return ::tox_get_something(instance_);"
                    ]
            source `shouldGenerate` expected

        it "generates non-const method for non-const C pointer" $ do
            let source =
                    [ "typedef struct Tox Tox;"
                    , "void tox_set_something(Tox *tox, uint32_t val);"
                    ]
            let expected =
                    [ "  void Tox::set_something(uint32_t something) {"
                    , "    ::tox_set_something(instance_, something);"
                    ]
            source `shouldGenerate` expected

        it "generates correct header structure and includes" $ do
            let model = SemanticModel [] [] [] [] [] [] "Test" []
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "#ifndef TEST_CPP_API_H"
            generatedCode `shouldContain` "#define TEST_CPP_API_H"
            generatedCode `shouldContain` "#include \"tox_result.h\""
            generatedCode `shouldContain` "namespace test {"
            generatedCode `shouldContain` "} // namespace test"

        it "generates enums correctly" $ do
            let model = SemanticModel
                    { enums = [ SEnumModel "Tox_Status" "Status" [("TOX_STATUS_NONE", "None")] ]
                    , constants = [], idTypes = [], callbacks = [], resources = [], variants = []
                    , commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "enum class Status {"
            generatedCode `shouldContain` "  NONE"
            generatedCode `shouldContain` "};"

        it "generates ID types correctly" $ do
            let model = SemanticModel
                    { enums = [], constants = []
                    , idTypes = [ SIdTypeModel "FriendId" "uint32_t" (SUInt 32) ]
                    , callbacks = [], resources = [], variants = []
                    , commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "class FriendId {"
            generatedCode `shouldContain` "::uint32_t value;"
            generatedCode `shouldContain` "operator ::uint32_t() const"

        it "generates handle-based resource correctly" $ do
            let model = SemanticModel
                    { enums = [], constants = [], idTypes = [], callbacks = []
                    , resources =
                        [ SResource "Tox" "Tox" ResHandle "tox_" True Nothing Nothing []
                            [ SMethod "delete" Destructor [] SVoid MutableThis Nothing IgnoreReturn
                                (CustomMapping $ CFunctionMapping "tox_kill" [ThisObject MutableThis] Nothing Nothing Nothing [] SVoid)
                                False []
                            ]
                            [] []
                        ]
                    , variants = [], commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "class Tox {"
            generatedCode `shouldContain` "~Tox();"
            generatedCode `shouldContain` "::tox_kill(instance_)"
            generatedCode `shouldContain` "Tox(const Tox& )  = delete;"
            generatedCode `shouldContain` "Tox(Tox&& other)"

        it "generates ID-based resource correctly with hierarchy" $ do
            let model = SemanticModel
                    { enums = [], constants = []
                    , idTypes = [ SIdTypeModel "FriendId" "uint32_t" (SUInt 32) ]
                    , callbacks = []
                    , resources =
                        [ SResource "Tox" "Tox" ResHandle "tox_" True Nothing Nothing [] [] [] []
                        , SResource "Friend" "Tox_Friend" (ResId (SResourceId "FriendId")) "tox_friend_" False (Just "Tox") (Just "Tox") [] [] [] []
                        ]
                    , variants = [], commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "template <typename ToxT>"
            generatedCode `shouldContain` "class FriendHandle {"
            generatedCode `shouldContain` "using Friend = FriendHandle<Tox>;"
            generatedCode `shouldContain` "FriendHandle(ToxT& core, ToxT& parent, FriendId id)"
            generatedCode `shouldContain` "ToxT& core_"
            generatedCode `shouldContain` "FriendId id_"

        it "generates methods with result type" $ do
            let model = SemanticModel
                    { enums = [ SEnumModel "Tox_Err_Test" "ErrTest" [("TOX_ERR_TEST_OK", "Ok")] ]
                    , constants = [], idTypes = [], callbacks = []
                    , resources =
                        [ SResource "Tox" "Tox" ResHandle "tox_" True Nothing Nothing []
                            [ SMethod "do_something" ActionRole [] SVoid MutableThis (Just "Tox_Err_Test") ReturnIsValue
                                (CustomMapping $ CFunctionMapping "tox_do_something" [ThisObject MutableThis, ErrorPtr] Nothing Nothing (Just "Tox_Err_Test") [] SVoid)
                                False []
                            ]
                            [] []
                        ]
                    , variants = [], commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "Result<void, ErrTest> do_something()"
            generatedCode `shouldContain` "return static_cast<ErrTest>(error);"

        it "generates getters with return values" $ do
            let model = SemanticModel
                    { enums = [], constants = [], idTypes = [], callbacks = []
                    , resources =
                        [ SResource "Tox" "Tox" ResHandle "tox_" True Nothing Nothing []
                            [ SMethod "get_val" GetterRole [] (SUInt 32) ConstThis Nothing ReturnIsValue
                                (CustomMapping $ CFunctionMapping "tox_get_val" [ThisObject ConstThis] Nothing Nothing Nothing [] (SUInt 32))
                                False []
                            ]
                            [] []
                        ]
                    , variants = [], commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "uint32_t get_val() const"
            generatedCode `shouldContain` "auto result = ::tox_get_val(instance_);"
            generatedCode `shouldContain` "return result;"

        it "generates callbacks correctly" $ do
            let model = SemanticModel
                    { enums = [], constants = [], idTypes = [], callbacks = []
                    , resources =
                        [ SResource "Tox" "Tox" ResHandle "tox_" True Nothing Nothing [] [] []
                            [ SEvent "msg" [SParameter "m" (SUInt 32) ConstThis []] "tox_msg_cb" False ]
                        ]
                    , variants = [], commonPrefix = "Tox", diagnostics = []
                    }
            let generatedCode = Text.unpack $ generate model
            generatedCode `shouldContain` "class Handler {"
            generatedCode `shouldContain` "virtual void on_msg(uint32_t m)"
            generatedCode `shouldContain` "void set_handler(Handler* handler)"
            generatedCode `shouldContain` "static void msg_cb_static"

        it "reproduces group self get name bug" $ do
            let source =
                    [ "typedef struct Tox Tox;"
                    , "typedef uint32_t Tox_Group_Number;"
                    , "typedef enum Tox_Err_Group_Self_Query {"
                    , "  TOX_ERR_GROUP_SELF_QUERY_OK,"
                    , "  TOX_ERR_GROUP_SELF_QUERY_GROUP_NOT_FOUND,"
                    , "} Tox_Err_Group_Self_Query;"
                    , "size_t tox_group_self_get_name_size(const Tox *tox, Tox_Group_Number group_number, Tox_Err_Group_Self_Query *error);"
                    , "bool tox_group_self_get_name(const Tox *tox, Tox_Group_Number group_number, uint8_t *name, Tox_Err_Group_Self_Query *error);"
                    ]
            let expected =
                    [ "    ::Tox_Err_Group_Self_Query error = static_cast<::Tox_Err_Group_Self_Query>(TOX_ERR_GROUP_SELF_QUERY_OK);"
                    , "    size_t size = ::tox_group_self_get_name_size(core_.instance_, id_.value, &error);"
                    , "    std::vector<uint8_t> result(size);"
                    , "    bool ok = ::tox_group_self_get_name(core_.instance_, id_.value, result.data(), &error);"
                    , "    if (!ok) {"
                    , "      if (error != static_cast<::Tox_Err_Group_Self_Query>(TOX_ERR_GROUP_SELF_QUERY_OK)) {"
                    , "        return static_cast<Err_Group_Self_Query>(error);"
                    , "      }"
                    , "    }"
                    , "    return result;"
                    ]
            source `shouldGenerate` expected
            -- Also verify the method name is correct
            compileToCpp (Text.unlines source) >>= \cpp ->
                cpp `shouldContain` "Result<std::vector<uint8_t>, Err_Group_Self_Query> GroupHandle<ToxT>::get_self_name() const"

        it "generates function taking byte array as span" $ do
            let source =
                    [ "typedef struct Tox Tox;"
                    , "typedef enum Tox_Err_Send {"
                    , "  TOX_ERR_SEND_OK,"
                    , "} Tox_Err_Send;"
                    , "void tox_send(Tox *tox, const uint8_t *data, size_t length, Tox_Err_Send *error);"
                    ]
            let expected =
                    [ "  Result<void, Err_Send> Tox::send(std::span<const uint8_t> data) {"
                    , "    ::Tox_Err_Send error = static_cast<::Tox_Err_Send>(TOX_ERR_SEND_OK);"
                    , "    ::tox_send(instance_, const_cast<uint8_t*>(data.data()), data.size(), &error);"
                    ]
            source `shouldGenerate` expected
