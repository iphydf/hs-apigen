{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.HaskellSpec where

import           Test.Hspec                      (Spec, describe, it, shouldBe,
                                                  shouldSatisfy)

import           Apigen.Language.Haskell         (generate)
import           Apigen.Language.Haskell.AST     (Import (..), Module (..))
import           Apigen.Language.Haskell.Haddock (DocContext (..), haddock)
import           Apigen.Language.Haskell.Pretty  (renderModule)
import           Apigen.Parser                   (parseModel)
import           Apigen.Parser.Docs              (extractDocs)
import qualified Apigen.Parser.Semantic          as Semantic
import           Apigen.SizeExpr                 (SizeExpr (..), parseSizeExpr)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.Cimple.IO              (parseText)

-- | The modules generated for a header, by path.
generated :: [Text] -> IO [(FilePath, Text)]
generated code = case parseText (Text.unlines code) of
    Left err -> fail err
    Right nodes ->
        let tu = ("tox/tox.h", nodes)
        in return (generate (extractDocs [tu]) (Semantic.toSemantic False (parseModel [tu])))

shouldContain :: [(FilePath, Text)] -> (FilePath, [Text]) -> IO ()
shouldContain files (path, block) =
    lookup path files `shouldSatisfy` maybe False (Text.isInfixOf (Text.unlines block))

header :: [Text]
header =
    [ "typedef struct Tox Tox;"
    , "#define TOX_PUBLIC_KEY_SIZE 32"
    , "typedef uint8_t Tox_Public_Key[TOX_PUBLIC_KEY_SIZE];"
    , "typedef enum Tox_User_Status {"
    , "    TOX_USER_STATUS_NONE,"
    , "    TOX_USER_STATUS_AWAY,"
    , "} Tox_User_Status;"
    , "typedef enum Tox_Err_Set_Info {"
    , "    TOX_ERR_SET_INFO_OK,"
    , "    TOX_ERR_SET_INFO_NULL,"
    , "    TOX_ERR_SET_INFO_TOO_LONG,"
    , "} Tox_Err_Set_Info;"
    , "/**"
    , " * Set the nickname for the Tox client."
    , " *"
    , " * @param name A byte array containing the new nickname."
    , " * @param length The size of the name byte array."
    , " *"
    , " * @return true on success."
    , " */"
    , "bool tox_self_set_name(Tox *tox, const uint8_t name[], size_t length, Tox_Err_Set_Info *error);"
    , "size_t tox_self_get_name_size(const Tox *tox);"
    , "void tox_self_get_name(const Tox *tox, uint8_t name[]);"
    , "void tox_self_get_public_key(const Tox *tox, Tox_Public_Key public_key);"
    , "void tox_self_set_status(Tox *tox, Tox_User_Status status);"
    ]

spec :: Spec
spec = do
    describe "generate" $ do
        it "marshals enums by their C values, error enums without OK" $ do
            files <- generated header
            files `shouldContain` ("Network/Tox/C/Types.hs",
                [ "instance CEnum ErrSetInfo where"
                , "    toCEnum ErrSetInfoNull    = 1"
                , "    toCEnum ErrSetInfoTooLong = 2"
                , ""
                , "    fromCEnum 1 = ErrSetInfoNull"
                , "    fromCEnum 2 = ErrSetInfoTooLong"
                , "    fromCEnum n = unknownCEnum \"Tox_Err_Set_Info\" n"
                ])

        it "passes bytes with their length, and returns C's error" $ do
            files <- generated header
            files `shouldContain` ("Network/Tox/C/Tox.hs",
                [ "-- | Set the nickname for the Tox client."
                , "--"
                , "-- * @name@: A byte array containing the new nickname."
                , "toxSelfSetName :: Tox -> ByteString -> IO (Either ErrSetInfo ())"
                , "toxSelfSetName (Tox tox) name ="
                , "    withBytesLen name $ \\namePtr nameLen ->"
                , "        callErrFun_ $ Raw.tox_self_set_name tox namePtr nameLen"
                ])

        it "asks C for a size, then for the bytes" $ do
            files <- generated header
            files `shouldContain` ("Network/Tox/C/Tox.hs",
                [ "toxSelfGetName :: Tox -> IO ByteString"
                , "toxSelfGetName (Tox tox) ="
                , "    getSized (Raw.tox_self_get_name_size tox) (Raw.tox_self_get_name tox)"
                ])

        it "reads a fixed-size array as its own type" $ do
            files <- generated header
            files `shouldContain` ("Network/Tox/C/Tox.hs",
                [ "toxSelfGetPublicKey :: Tox -> IO PublicKey"
                , "toxSelfGetPublicKey (Tox tox) = getFixedBytes $ Raw.tox_self_get_public_key tox"
                ])
            files `shouldContain` ("Network/Tox/C/Types.hs",
                [ "instance FixedBytes PublicKey where"
                , "    fixedSize _ = toxPublicKeySize"
                ])

        it "passes enums by their C values" $ do
            files <- generated header
            files `shouldContain` ("Network/Tox/C/Tox.hs",
                [ "toxSelfSetStatus :: Tox -> UserStatus -> IO ()"
                , "toxSelfSetStatus (Tox tox) status = Raw.tox_self_set_status tox (toCEnum status)"
                ])

        it "imports C functions with C types" $ do
            files <- generated header
            files `shouldContain` ("FFI/Tox/Tox.hs",
                [ "foreign import ccall tox_self_set_name"
                , "    :: ToxPtr -> Ptr Word8 -> CSize -> CErr -> IO CBool"
                ])

    describe "haddock" $ do
        let ctx = DocContext [("friend_number", "friendNumber")] (\n -> lookup n [("tox_friend_add", "toxFriendAdd")])

        it "lists the parameters the Haskell function takes, and drops return values" $
            haddock ctx "Delete a friend.\n\n@param friend_number The friend.\n@param error An error.\n\n@return true on success."
                `shouldBe` "Delete a friend.\n\n* @friendNumber@: The friend."

        it "links functions, and escapes Haddock markup" $
            haddock ctx "See tox_friend_add() or `tox_iterate` for IPv4/IPv6."
                `shouldBe` "See 'toxFriendAdd' or @tox_iterate@ for IPv4\\/IPv6."

    describe "renderModule" $
        it "aligns imports as stylish-haskell does" $
            renderModule (Module "M" Nothing [] Nothing
                [ Import "Data.Word" Nothing (Just ["Word8", "Word16"])
                , Import "Data.ByteString" (Just "BS") Nothing
                ] [])
                `shouldBe` Text.unlines
                    [ "module M where"
                    , ""
                    , "import qualified Data.ByteString as BS"
                    , "import           Data.Word       (Word16, Word8)"
                    ]

    describe "parseSizeExpr" $
        it "parses C size expressions" $
            parseSizeExpr "max(width, abs(ystride)) * height"
                `shouldBe` Just (SizeOp '*'
                    (SizeCall "max" [SizeVar "width", SizeCall "abs" [SizeVar "ystride"]])
                    (SizeVar "height"))
