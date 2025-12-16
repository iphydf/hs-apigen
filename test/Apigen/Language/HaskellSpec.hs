{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.HaskellSpec where

import           Test.Hspec              (Spec, describe, it, shouldBe,
                                          shouldSatisfy)

import           Apigen.Language.Haskell (Options (..), generate)
import           Apigen.Parser           (parseModel)
import qualified Apigen.Parser.Semantic  as Semantic
import qualified Apigen.Semantic         as S
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Language.Cimple         (Lexeme (..), Node)
import           Language.Cimple.IO      (parseText)


mustParse :: MonadFail m => FilePath -> [Text] -> m (FilePath, [Node (Lexeme Text)])
mustParse file code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return (file, ok)

toSem :: (FilePath, [Node (Lexeme Text)]) -> S.SemanticModel
toSem tu = Semantic.toSemantic False (parseModel [tu])

spec :: Spec
spec = do
    describe "generate" $ do
        it "generates enums correctly" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef enum Tox_User_Status {"
                , "    TOX_USER_STATUS_NONE,"
                , "    TOX_USER_STATUS_AWAY,"
                , "    TOX_USER_STATUS_BUSY"
                , "} Tox_User_Status;"
                ]
            let opts = Options Nothing True
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output `shouldSatisfy` Text.isInfixOf "UserStatus(..)"
            output `shouldSatisfy` Text.isInfixOf "{# enum Tox_User_Status as UserStatus"

        it "handles error enums (skipping OK)" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef enum Tox_Err_Friend_Add {"
                , "    TOX_ERR_FRIEND_ADD_OK,"
                , "    TOX_ERR_FRIEND_ADD_NULL,"
                , "    TOX_ERR_FRIEND_ADD_TOO_LONG"
                , "} Tox_Err_Friend_Add;"
                , "uint32_t tox_friend_add(Tox *tox, const uint8_t *address, const uint8_t *message, size_t length, Tox_Err_Friend_Add *error);"
                ]
            let opts = Options Nothing True
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output `shouldSatisfy` Text.isInfixOf "ErrFriendAdd(..)"
            output `shouldSatisfy` Text.isInfixOf "{# enum Tox_Err_Friend_Add as ErrFriendAdd"
            output `shouldSatisfy` (not . Text.isInfixOf "TOX_ERR_FRIEND_ADD_OK")

        it "generates structs and pointers" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "struct Tox_Options {"
                , "    bool ipv6_enabled;"
                , "};"
                ]
            let opts = Options Nothing True
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output `shouldSatisfy` Text.isInfixOf "ToxStruct"
            output `shouldSatisfy` Text.isInfixOf "type ToxPtr = Ptr ToxStruct"
            output `shouldSatisfy` Text.isInfixOf "OptionsStruct"
            output `shouldSatisfy` Text.isInfixOf "type OptionsPtr = Ptr OptionsStruct"

        it "generates typedefs of standard types" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef uint32_t Tox_Friend_Number;" ]
            let opts = Options Nothing True
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy`
                Text.isInfixOf "newtype FriendNumber = FriendNumber Word32"

        it "generates function declarations" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "uint32_t tox_self_get_friend_number(const Tox *tox);"
                ]
            let opts = Options (Just "FFI.Tox.Types") False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "foreign import ccall \"tox_self_get_friend_number\" tox_self_get_friend_number :: ToxPtr -> IO Word32"

        it "handles function pointers (callbacks)" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef void tox_friend_name_cb(Tox *tox, uint32_t friend_number, const uint8_t *name, size_t length, void *user_data);"
                ]
            let opts = Options Nothing True
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "type FriendNameCb = ToxPtr -> Word32 -> CString -> Ptr () -> IO ()"
            output `shouldSatisfy` Text.isInfixOf "foreign import ccall \"wrapper\" wrapFriendNameCb :: FriendNameCb -> IO (FunPtr FriendNameCb)"

        it "replaces enum placeholders in function signatures" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "typedef enum Tox_User_Status { TOX_USER_STATUS_NONE } Tox_User_Status;"
                , "void tox_self_set_status(Tox *tox, Tox_User_Status status);"
                ]
            let opts = Options Nothing False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "tox_self_set_status :: ToxPtr -> (CEnum UserStatus) -> IO ()"

        it "handles pointers to enums (error codes)" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef enum Tox_Err_New { TOX_ERR_NEW_OK } Tox_Err_New;"
                , "typedef struct Tox Tox;"
                , "Tox *tox_new(const void *options, Tox_Err_New *error);"
                ]
            let opts = Options Nothing False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "tox_new :: Ptr () -> CErr ErrNew -> IO ToxPtr"

        it "handles const pointers to standard types as CString for uint8_t/char" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "void tox_self_set_name(const uint8_t *name);"
                , "void tox_self_set_status_message(const char *message);"
                ]
            let opts = Options Nothing False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "tox_self_set_name :: CString -> IO ()"
            output `shouldSatisfy` Text.isInfixOf "tox_self_set_status_message :: CString -> IO ()"

        it "handles non-const pointers to standard types" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "void tox_self_get_name(uint8_t *name);"
                , "void tox_friend_get_public_key(uint32_t friend_number, uint8_t *public_key);"
                ]
            let opts = Options Nothing False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "tox_self_get_name :: CString -> IO ()"
            output `shouldSatisfy` Text.isInfixOf "tox_friend_get_public_key :: Word32 -> CString -> IO ()"

        it "handles pointers to other standard types" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "void tox_some_func(uint32_t *val);" ]
            let opts = Options Nothing False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output
              `shouldSatisfy` Text.isInfixOf "tox_some_func :: Ptr Word32 -> IO ()"

        it "handles int16_t (common in toxav)" $ do
            tu <- mustParse "toxav/toxav.h"
                [ "typedef struct ToxAV ToxAV;"
                , "void toxav_audio_send_frame(const int16_t pcm[]);" ]
            let opts = Options Nothing False
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output `shouldSatisfy` Text.isInfixOf "toxav_audio_send_frame :: Ptr Int16 -> IO ()"

        it "handles #define constants" $ do
            tu <- mustParse "tox/tox.h"
                [ "typedef struct Tox Tox;"
                , "#define TOX_PUBLIC_KEY_SIZE 32" ]
            let opts = Options Nothing True
                output = Text.concat $ map snd $ generate opts (toSem tu)
            output `shouldSatisfy` Text.isInfixOf "toxPublicKeySize :: Word32"
            output `shouldSatisfy` Text.isInfixOf "toxPublicKeySize  = 32"
