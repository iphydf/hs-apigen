{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-name-shadowing #-}
module Apigen.Parser.SemanticSpec (spec) where

import           Test.Hspec             (Spec, describe, it, shouldBe,
                                         shouldSatisfy)

import           Apigen.Parser          (parseModel)
import qualified Apigen.Parser.Semantic as Semantic
import           Apigen.Semantic        as S
import           Apigen.Types           as T
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Language.Cimple        (Lexeme (..), Node)
import           Language.Cimple.IO     (parseText)


mustParse :: MonadFail m => FilePath -> [Text] -> m (FilePath, [Node (Lexeme Text)])
mustParse file code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return (file, ok)


lookupResource :: Text -> S.SemanticModel -> Maybe S.SResource
lookupResource name = foldr pick Nothing . S.resources
  where pick r rest = if S.resourceName r == name then Just r else rest


spec :: Spec
spec = do
    describe "Apigen.Parser.Semantic" $ do
        it "gathers enums correctly" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef enum Tox_User_Status { TOX_USER_STATUS_NONE } Tox_User_Status;" ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                isToxUserStatus e = S.enumName e == "Tox_User_Status" && "TOX_USER_STATUS_NONE" `elem` map S.enumMemberCName (S.enumMembers e)
            S.enums sem `shouldSatisfy` any isToxUserStatus

        it "leaves out with a warning what it can't bind yet" $ do
            tu <- mustParse "tox_private.h"
                [ "typedef struct Tox Tox;"
                , "typedef struct Memory Memory;"
                , "typedef enum Tox_Err_New { TOX_ERR_NEW_OK } Tox_Err_New;"
                , "typedef uint64_t tox_mono_time_cb(void *user_data);"
                , "typedef struct Tox_System { const Memory *mem; } Tox_System;"
                , "Tox_System tox_default_system(void);"
                , "const Tox_System *tox_get_system(const Tox *tox);"
                , "Tox *tox_new_testing(Tox_Err_New *error, Tox_Err_New *testing_error);"
                ]
            let sem = Semantic.toSemantic False (parseModel [tu])
                warned = [ m | S.SDiagnostic S.Warning _ m <- S.diagnostics sem ]
            map S.resourceName (filter S.isRoot (S.resources sem)) `shouldBe` ["Tox"]
            map S.resourceName (S.resources sem) `shouldSatisfy` notElem "Memory"
            map (Text.takeWhile (/= ':')) warned `shouldBe` ["tox_default_system", "tox_new_testing", "tox_mono_time_cb"]
            [ () | S.SDiagnostic S.Error _ _ <- S.diagnostics sem ] `shouldBe` []

        it "names another library's things by its short word, and its handle by it" $ do
            tu <- mustParse "toxav.h"
                [ "typedef struct Tox Tox;"
                , "typedef struct ToxAV ToxAV;"
                , "typedef enum Toxav_Call_Control { TOXAV_CALL_CONTROL_RESUME } Toxav_Call_Control;"
                , "typedef void toxav_call_cb(ToxAV *av, uint32_t friend_number, void *user_data);"
                , "Tox *tox_new(void);"
                , "ToxAV *toxav_new(Tox *tox);"
                , "Tox *toxav_get_tox(const ToxAV *av);"
                , "void toxav_callback_call(ToxAV *av, toxav_call_cb *callback, void *user_data);"
                ]
            let sem = Semantic.toSemantic False (parseModel [tu])
                Just av = lookupResource "Tox_Av" sem
            map S.enumSemanticName (S.enums sem) `shouldBe` ["Av_Call_Control"]
            map S.cbName (S.callbacks sem) `shouldBe` ["av_call_cb"]
            -- Its own handle isn't a parameter of its functions.
            [ map S.paramName (S.inputs m) | m <- S.methods av, S.methodName m == "toxav_get_tox" ] `shouldBe` [[]]

        it "identifies Resources from ClassDecl" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "Tox *tox_new(void);"
                ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                isTox r = S.resourceName r == "Tox"
            S.resources sem `shouldSatisfy` any isTox

        it "identifies Resources from EntityDecl" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef uint32_t Tox_Friend_Number;"
                , "void tox_friend_get_name(const Tox *tox, Tox_Friend_Number friend_number, uint8_t *name);"
                ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                isFriend r = S.resourceName r == "Friend"
            S.resources sem `shouldSatisfy` any isFriend

        it "identifies methods case-insensitively" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "bool tox_bootstrap(Tox *tox, const char *host);"
                ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                [tox] = filter (\r -> S.resourceName r == "Tox") (S.resources sem)
            S.methods tox `shouldSatisfy` any (\m -> S.methodName m == "tox_bootstrap")

        it "identifies resource ID lists correctly" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef uint32_t Tox_Friend_Number;"
                , "size_t tox_self_get_friend_list_size(const Tox *tox);"
                , "void tox_self_get_friend_list(const Tox *tox, Tox_Friend_Number friend_list[]);"
                ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                [tox] = filter (\r -> S.resourceName r == "Tox") (S.resources sem)
                prop = case filter (\p -> S.propName p == "self_friend_list") (S.properties tox) of
                         (p:_) -> p
                         []    -> error "prop self_friend_list not found"
            S.propType prop `shouldBe` S.SList (S.SResourceId "Friend_Number")

        it "identifies self_get properties with size correctly" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef uint32_t Tox_Group_Number;"
                , "size_t tox_group_self_get_name_size(const Tox *tox, Tox_Group_Number group_number);"
                , "bool tox_group_self_get_name(const Tox *tox, Tox_Group_Number group_number, uint8_t *name);"
                ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                [group] = filter (\r -> S.resourceName r == "Group") (S.resources sem)
                props = S.properties group
                selfName = case filter (\p -> S.propName p == "self_name") props of
                             (p:_) -> p
                             []    -> error "prop self_name not found"
            S.propRead selfName `shouldBe` Just "tox_group_self_get_name"
            S.propSize selfName `shouldBe` Just "tox_group_self_get_name_size"
