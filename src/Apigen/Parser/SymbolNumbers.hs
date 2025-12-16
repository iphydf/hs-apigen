{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wwarn #-}
module Apigen.Parser.SymbolNumbers (module Apigen.Parser.SymbolNumbers, module Apigen.Patterns) where

import           Apigen.Parser.SymbolTable  (NameToSId, SIdToName)
import           Apigen.Patterns
import           Control.Arrow              (Arrow (second))
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Fix                   (Fix (..))
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Tuple                 (swap)
import           GHC.Stack                  (HasCallStack)
import           Language.Cimple            (Lexeme, Node, NodeF (..))
import           Language.Cimple.MapAst     (AstActions (..), astActions,
                                             mapAst, mapFileAst)

type TranslationUnit text = (FilePath, [Node (Lexeme text)])

builtins :: NameToSId
builtins = HashMap.fromList
    [ (([], ["APIGEN","IGNORE"]), SYM_APIGEN_IGNORE)
    , (([], ["TOX","HIDE","DEPRECATED"]), SYM_TOX_HIDE_DEPRECATED)
    , (([], ["void"      ]), TY_void    )
    , (([], ["char"      ]), TY_char    )
    , (([], ["int"       ]), TY_int     )
    , (([], ["unsigned"  ]), TY_unsigned)
    , (([], ["bool"      ]), TY_bool    )
    , (([], ["int8","t"  ]), TY_int8_t  )
    , (([], ["uint8","t" ]), TY_uint8_t )
    , (([], ["int16","t" ]), TY_int16_t )
    , (([], ["uint16","t"]), TY_uint16_t)
    , (([], ["int32","t" ]), TY_int32_t )
    , (([], ["uint32","t"]), TY_uint32_t)
    , (([], ["int64","t" ]), TY_int64_t )
    , (([], ["uint64","t"]), TY_uint64_t)
    , (([], ["size","t"  ]), TY_size_t  )
    , (([], ["abs"       ]), SYM_abs    )
    , (([], ["max"       ]), SYM_max    )
    , (([], ["sizeof"    ]), SYM_sizeof )
    ]

symtabActions :: HasCallStack => AstActions (State NameToSId) Text Int
symtabActions = (astActions lookupSym)
    { doNode = \file node act -> case unFix node of
        LicenseDecl{}        -> return $ Fix Ellipsis
        Comment{}            -> return $ Fix Ellipsis
        Commented _ e        -> mapFileAst symtabActions file e
        CommentSection _ e _ -> Fix . Group <$> mapFileAst symtabActions file e

        _                    -> act
    }

lookupSym :: HasCallStack => Text -> State NameToSId Int
lookupSym nameText = do
    syms <- State.get
    let name = ([], Text.splitOn "_" nameText)
    case HashMap.lookup name syms of
        Just sym -> return sym
        Nothing -> do
            let num = HashMap.size syms
            State.modify $ HashMap.insert name num
            return num

collect :: HasCallStack => [TranslationUnit Text] -> ([TranslationUnit Int], (SIdToName, ()))
collect = second ((,()) . invert) . flip State.runState builtins . mapAst symtabActions

invert :: NameToSId -> SIdToName
invert = HashMap.fromList . map swap . HashMap.toList
