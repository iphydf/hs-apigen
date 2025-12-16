{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Apigen.RoundtripSpec (spec) where

import           Control.Monad                   (filterM)
import           Test.Hspec                      (Spec, describe, it, xit, shouldBe, pendingWith)

import qualified Apigen.Language.C               as C
import           Apigen.Parser                   (parseModel)
import qualified Apigen.Parser.Semantic          as Semantic
import           Apigen.Semantic                 as S
import           Apigen.Types                    as T
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.Cimple                 (Lexeme (..), Node)
import           Language.Cimple.IO              (parseFile, parseText)
import qualified Language.Cimple.Program         as Program
import           Language.Cimple.TranslationUnit (TranslationUnit)
import           System.Directory                (doesFileExist)


mustParse :: MonadFail m => FilePath -> [Text] -> m (TranslationUnit Text)
mustParse file code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return (file, ok)


mustParseFile :: FilePath -> IO (TranslationUnit Text)
mustParseFile file =
    parseFile file >>= \case
        Left err -> fail err
        Right ok -> return ok


roundtrip :: [Text] -> IO S.SemanticModel
roundtrip code = do
    tu <- mustParse "test.h" code
    let model1 = parseModel [tu]
        sem1 = Semantic.toSemantic False model1
        loweredC = C.generate sem1

    -- putStrLn $ Text.unpack loweredC
    tu2 <- mustParse "roundtrip.h" [loweredC]
    let model2 = parseModel [tu2]
    return $ Semantic.toSemantic False model2

spec :: Spec
spec = do
  describe "Semantic Roundtrip" $ do
    it "roundtrips actual toxcore headers" $ do
        -- These paths assume the test is run with data = ["//c-toxcore:public_headers"]
        -- Bazel places data dependencies in the runfiles tree.
        -- Depending on how hspec_test runs, we might need to adjust paths.
        -- But usually relative to workspace root works if we are in the root.
        let headers = [ "c-toxcore/tox/tox_log_level.h"
                      , "c-toxcore/tox/tox_options.h"
                      , "c-toxcore/tox/tox.h"
                      , "c-toxcore/tox/toxav.h"
                      , "c-toxcore/tox/toxencryptsave.h"
                      ]
        missing <- filterM (fmap not . doesFileExist) headers
        if not (null missing)
        then pendingWith $ "Missing toxcore headers: " ++ show missing
        else do
            tus <- mapM mustParseFile headers
            let sem1 = Semantic.toSemantic False (parseModel tus)

            let loweredC = C.generate sem1
            -- putStrLn $ Text.unpack loweredC
            tu2 <- mustParse "roundtrip.h" [loweredC]
            let sem2 = Semantic.toSemantic False (parseModel [tu2])
            sem2 `shouldBe` sem1

    it "roundtrips enums correctly" $ do
        let code = [ "typedef enum Tox_User_Status { TOX_USER_STATUS_NONE, TOX_USER_STATUS_AWAY } Tox_User_Status;" ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips simple classes and methods" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "Tox *tox_new(void);"
                   , "void tox_kill(Tox *tox);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips entities and instance methods" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Friend_Number;"
                   , "uint32_t tox_friend_get_connection_status(const Tox *tox, Tox_Friend_Number friend_number);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips properties with getters and setters" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef enum Tox_Err_Set_Info { TOX_ERR_SET_INFO_OK } Tox_Err_Set_Info;"
                   , "void tox_self_set_name(Tox *tox, const uint8_t *name, size_t length, Tox_Err_Set_Info *error);"
                   , "size_t tox_self_get_name_size(const Tox *tox);"
                   , "void tox_self_get_name(const Tox *tox, uint8_t *name);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips string properties" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "size_t tox_self_get_status_message_size(const Tox *tox);"
                   , "const char *tox_self_get_status_message(const Tox *tox);"
                   , "void tox_self_set_status_message(Tox *tox, const char *message);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips complex methods with various types" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef enum Tox_Err_Bootstrap { TOX_ERR_BOOTSTRAP_OK } Tox_Err_Bootstrap;"
                   , "bool tox_bootstrap(Tox *tox, const char *host, uint16_t port, const uint8_t *public_key, Tox_Err_Bootstrap *error);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips hierarchical resources" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef struct Tox_Options Tox_Options;"
                   , "Tox_Options *tox_options_new(void);"
                   , "void tox_options_set_ipv6_enabled(Tox_Options *opts, bool enabled);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    {-
    it "roundtrips events and callbacks" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef void tox_friend_message_cb(Tox *tox, uint32_t friend_number, const uint8_t *message, size_t length, void *user_data);"
                   , "void tox_callback_friend_message(Tox *tox, tox_friend_message_cb *callback);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1
    -}

    it "roundtrips byte array parameters" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "#define TOX_PUBLIC_KEY_SIZE 32"
                   , "#define TOX_ADDRESS_SIZE 38"
                   , "void tox_self_get_public_key(const Tox *tox, uint8_t public_key[TOX_PUBLIC_KEY_SIZE]);"
                   , "void tox_self_get_address(const Tox *tox, uint8_t address[TOX_ADDRESS_SIZE]);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips SList of non-bytes" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "size_t tox_self_get_friend_list_size(const Tox *tox);"
                   , "void tox_self_get_friend_list(const Tox *tox, uint32_t *friend_list);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "consistently lifts identity types to SResourceId" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Friend_Number;"
                   , "Tox_Friend_Number tox_friend_add(Tox *tox);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
            [friend] = filter (\r -> S.resourceName r == "Friend") (S.resources sem1)
            [add] = S.methods friend
        S.output add `shouldBe` S.SResourceId "Friend_Number"

        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips ID-based sub-resources consistently" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Friend_Number;"
                   , "size_t tox_friend_get_name_size(const Tox *tox, Tox_Friend_Number friend_number);"
                   , "bool tox_friend_get_name(const Tox *tox, Tox_Friend_Number friend_number, uint8_t *data);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips across multiple modules correctly" $ do
        tu1 <- mustParse "tox.h" [ "typedef struct Tox Tox;", "Tox *tox_new(void);" ]
        tu2 <- mustParse "toxav.h" [ "typedef struct ToxAV ToxAV;", "ToxAV *toxav_new(Tox *tox, void *err);" ]
        let sem1 = Semantic.toSemantic False (parseModel [tu1, tu2])

        let loweredC = C.generate sem1
        tu3 <- mustParse "roundtrip.h" [loweredC]
        let sem2 = Semantic.toSemantic False (parseModel [tu3])
        sem2 `shouldBe` sem1

    it "roundtrips constructors with byte arrays consistently" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Friend_Number;"
                   , "Tox_Friend_Number tox_friend_add(Tox *tox, const uint8_t *address, const uint8_t *message, size_t length);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips SList of identity types consistently" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Friend_Number;"
                   , "size_t tox_self_get_friend_list_size(const Tox *tox);"
                   , "void tox_self_get_friend_list(const Tox *tox, Tox_Friend_Number *friend_list);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips 2-level deep hierarchies consistently" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Group_Number;"
                   , "typedef uint32_t Tox_Group_Peer_Number;"
                   , "size_t tox_group_peer_get_name_size(const Tox *tox, Tox_Group_Number group_number, Tox_Group_Peer_Number peer_id);"
                   , "bool tox_group_peer_get_name(const Tox *tox, Tox_Group_Number group_number, Tox_Group_Peer_Number peer_id, uint8_t *name);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips 3-level deep hierarchies consistently" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Conference_Number;"
                   , "typedef uint32_t Tox_Conference_Peer_Number;"
                   , "typedef uint32_t Tox_Conference_Offline_Peer_Number;"
                   , "uint32_t tox_conference_offline_peer_count(const Tox *tox, Tox_Conference_Number conference_number, Tox_Conference_Offline_Peer_Number offline_peer_number);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips ToxAV as a child of Tox correctly" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef struct ToxAV ToxAV;"
                   , "ToxAV *toxav_new(Tox *tox, void *error);"
                   , "void toxav_kill(ToxAV *av);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips child resource properties consistently" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef struct Tox_Options Tox_Options;"
                   , "bool tox_options_get_ipv6_enabled(const Tox_Options *opts);"
                   , "void tox_options_set_ipv6_enabled(Tox_Options *opts, bool enabled);"
                   ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    it "roundtrips static global functions consistently" $ do
        let code = [ "uint32_t tox_version_major(void);" ]
        tu <- mustParse "test.h" code
        let sem1 = Semantic.toSemantic False (parseModel [tu])
        sem2 <- roundtrip code
        sem2 `shouldBe` sem1

    xit "assigns collection-level count functions to the parent resource" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Conference_Number;"
                   , "typedef uint32_t Tox_Conference_Offline_Peer_Number;"
                   , "uint32_t tox_conference_offline_peer_count(const Tox *tox, Tox_Conference_Number conference_number);"
                   ]
        tu <- mustParse "test.h" code
        let sem = Semantic.toSemantic False (parseModel [tu])
            resources_ = S.resources sem
            [conf] = filter (\r -> S.resourceName r == "Conference") resources_
            [offlinePeer] = filter (\r -> S.resourceName r == "Conference_Offline_Peer") resources_

        -- Should be in Conference, not Conference_Offline_Peer
        S.methods offlinePeer `shouldBe` []
        length (S.methods conf) `shouldBe` 1
        case S.methods conf of
          (m:_) -> S.methodName m `shouldBe` "tox_conference_offline_peer_count"
          []    -> fail "No methods in conf"

    it "does not lift setters with extra arguments to properties" $ do
        let code = [ "typedef struct Tox Tox;"
                   , "typedef uint32_t Tox_Group_Number;"
                   , "typedef uint32_t Tox_Group_Peer_Number;"
                   , "typedef enum Tox_Group_Role { TOX_GROUP_ROLE_USER } Tox_Group_Role;"
                   , "typedef enum Tox_Err_Group_Set_Role { TOX_ERR_GROUP_SET_ROLE_OK } Tox_Err_Group_Set_Role;"
                   , "bool tox_group_set_role(Tox *tox, Tox_Group_Number group_number, Tox_Group_Peer_Number peer_id, Tox_Group_Role role, Tox_Err_Group_Set_Role *error);"
                   ]
        tu <- mustParse "test.h" code
        let sem = Semantic.toSemantic False (parseModel [tu])
            [group] = filter (\r -> S.resourceName r == "Group") (S.resources sem)

        -- Should NOT be a property 'role'
        filter (\p -> S.propName p == "role") (S.properties group) `shouldBe` []
        -- Should be a method 'tox_group_set_role'
        length (S.methods group) `shouldBe` 1
        let [meth] = S.methods group
        S.methodName meth `shouldBe` "tox_group_set_role"
        length (S.inputs meth) `shouldBe` 2 -- peer_id and role
