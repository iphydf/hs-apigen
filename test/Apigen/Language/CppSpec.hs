{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.CppSpec where

import           Test.Hspec                (Spec, describe, it, shouldBe,
                                            shouldSatisfy)

import           Apigen.Language.Cpp       (generate)
import           Apigen.Language.Cpp.Names (commandWords, enumTypeName)
import           Apigen.Language.Cpp.Text  (sizeExpr)
import           Apigen.Parser             (parseModel)
import           Apigen.Parser.Docs        (extractDocs)
import qualified Apigen.Parser.Semantic    as Semantic
import           Apigen.SizeExpr           (parseSizeExpr)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Language.Cimple.IO        (parseText)

-- | The libraries' words, as tox.h and toxav.h have them.
nss :: [Text]
nss = ["tox", "toxav"]

-- | The files generated for a header, by path.
generated :: [Text] -> IO [(FilePath, Text)]
generated code = case parseText (Text.unlines code) of
    Left err -> fail err
    Right nodes ->
        let tu = ("tox/tox.h", nodes)
        in return (generate (extractDocs [tu]) (Semantic.toSemantic False (parseModel [tu])))

shouldContain :: [(FilePath, Text)] -> (FilePath, [Text]) -> IO ()
shouldContain files (path, block) =
    lookup path files `shouldSatisfy` maybe False (Text.isInfixOf (Text.intercalate "\n" block))

header :: [Text]
header =
    [ "typedef struct Tox Tox;"
    , "typedef uint32_t Tox_Friend_Number;"
    , "typedef enum Tox_Err_Friend_Query {"
    , "    TOX_ERR_FRIEND_QUERY_OK,"
    , "    TOX_ERR_FRIEND_QUERY_NULL,"
    , "    TOX_ERR_FRIEND_QUERY_FRIEND_NOT_FOUND,"
    , "} Tox_Err_Friend_Query;"
    , "/**"
    , " * Return the length of the friend's name."
    , " */"
    , "size_t tox_friend_get_name_size(const Tox *tox, Tox_Friend_Number friend_number, Tox_Err_Friend_Query *error);"
    , "/**"
    , " * Write the name of the friend designated by the given friend number to a byte array."
    , " */"
    , "bool tox_friend_get_name(const Tox *tox, Tox_Friend_Number friend_number, uint8_t name[], Tox_Err_Friend_Query *error);"
    ]

spec :: Spec
spec = do
    describe "generate" $ do
        it "gives error enums their C values, without OK" $ do
            files <- generated header
            files `shouldContain` ("include/tox/generated/errors.hpp",
                [ "enum class FriendQueryError"
                , "{"
                , "    Null = 1,"
                , "    FriendNotFound = 2,"
                , "};"
                ])

        it "asks C for a buffer's size, then fills it" $ do
            files <- generated header
            files `shouldContain` ("src/generated/commands.cpp",
                [ "    void on(CommandId id, command::GetFriendName& payload) override"
                , "    {"
                , "        Tox_Err_Friend_Query err = TOX_ERR_FRIEND_QUERY_OK;"
                , "        std::vector<uint8_t> name(tox_friend_get_name_size(tox_, payload.friend_number.value, &err));"
                , "        if (err == TOX_ERR_FRIEND_QUERY_OK) {"
                , "            tox_friend_get_name(tox_, payload.friend_number.value, name.data(), &err);"
                , "        }"
                , "        push<command::GetFriendName>(id, result<command::GetFriendName>(err, TOX_ERR_FRIEND_QUERY_OK, std::move(name)));"
                , "    }"
                ])

        it "names a proxy's method without its object" $ do
            files <- generated header
            files `shouldContain` ("include/tox/generated/friend.hpp",
                [ "    /**"
                , "     * @brief Write the name of the friend designated by the given friend number to a byte array."
                , "     */"
                , "    CommandId get_name() const noexcept;"
                ])

    describe "commandWords" $
        it "puts the verb first, and keeps a function's order without one" $ do
            commandWords nss "tox_friend_get_name" `shouldBe` ["get", "friend", "name"]
            commandWords nss "tox_file_get_file_id" `shouldBe` ["get", "file", "id"]
            commandWords nss "tox_group_invite_accept" `shouldBe` ["accept", "group", "invite"]
            commandWords nss "tox_group_invite_friend" `shouldBe` ["invite", "group", "friend"]
            commandWords nss "tox_friend_by_public_key" `shouldBe` ["friend", "by", "public", "key"]

    describe "enumTypeName" $
        it "gives a toxav enum Av only where a toxcore one has its name" $ do
            let names = ["Tox_Err_New", "Toxav_Err_New", "Toxav_Err_Call"]
            enumTypeName nss names "Toxav_Err_New" `shouldBe` "AvNewError"
            enumTypeName nss names "Toxav_Err_Call" `shouldBe` "CallError"

    describe "sizeExpr" $
        it "keeps a size from wrapping below zero" $
            fmap (sizeExpr (\v -> if v == "ciphertext_len" then "ciphertext.size()" else v))
                (parseSizeExpr "ciphertext_len - TOX_PASS_ENCRYPTION_EXTRA_LENGTH")
                `shouldBe` Just "ciphertext.size() < TOX_PASS_ENCRYPTION_EXTRA_LENGTH ? 0 : ciphertext.size() - TOX_PASS_ENCRYPTION_EXTRA_LENGTH"
