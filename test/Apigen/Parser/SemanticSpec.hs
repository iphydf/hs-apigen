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


spec :: Spec
spec = do
    describe "Apigen.Parser.Semantic" $ do
        it "gathers enums correctly" $ do
            tu <- mustParse "tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef enum Tox_User_Status { TOX_USER_STATUS_NONE } Tox_User_Status;" ]
            let model = parseModel [tu]
                sem = Semantic.toSemantic False model
                isToxUserStatus e = S.enumName e == "Tox_User_Status" && "TOX_USER_STATUS_NONE" `elem` map fst (S.enumMembers e)
            S.enums sem `shouldSatisfy` any isToxUserStatus

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
