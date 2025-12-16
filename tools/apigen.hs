{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Apigen.Language.C          as C
import qualified Apigen.Language.Cpp        as Cpp
import qualified Apigen.Language.Haskell    as Haskell
import qualified Apigen.Language.Python     as Python
import qualified Apigen.Language.Rust       as Rust
import qualified Apigen.Parser              as Parser
import           Apigen.Parser.AST          (Model)
import qualified Apigen.Parser.AST          as T
import qualified Apigen.Parser.Semantic     as Semantic
import           Apigen.Parser.SymbolTable  (Name)
import           Apigen.Semantic            (SemanticModel)
import qualified Apigen.Semantic            as S
import           Control.Monad              (when)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  ((\\))
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Language.Cimple            (Lexeme (..))
import           Language.Cimple.IO         (parseProgram, parseText)
import qualified Language.Cimple.Program    as Program
import           Options.Applicative
import           System.Directory           (createDirectoryIfMissing)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeDirectory, (</>))
import           System.IO                  (hPutStrLn, stderr)


data Options = Options
    { optNoRoundtrip :: Bool
    , optStrict      :: Bool
    , optVerbose     :: Bool
    , optOutputs     :: [OutputFormat]
    , optInputs      :: [FilePath]
    }

data OutputFormat
    = FormatJSON FilePath
    | FormatC FilePath
    | FormatCpp FilePath
    | FormatHaskell Haskell.Options (Maybe FilePath)
    | FormatHaskellSafe Haskell.Options (Maybe FilePath)
    | FormatHaskellRaw Haskell.Options (Maybe FilePath)
    | FormatRust Rust.Options (Maybe FilePath)
    | FormatPython (Maybe FilePath)

options :: Parser Options
options = Options
    <$> switch (long "no-roundtrip" <> help "Disable round-trip check")
    <*> switch (long "strict" <> help "Enable strict mode with non-standard mapping diagnostics")
    <*> switch (long "verbose" <> help "Enable verbose output")
    <*> many (
            (FormatJSON <$> strOption (long "json" <> metavar "FILE" <> help "Output JSON to FILE"))
        <|> (FormatC    <$> strOption (long "c"    <> metavar "FILE" <> help "Output C to FILE"))
        <|> (FormatCpp  <$> strOption (long "cpp"  <> metavar "FILE" <> help "Output C++ to FILE"))
        <|> (FormatHaskell
                <$> haskellOptions
                <*> (   Just <$> strOption (long "hs-out" <> metavar "DIR" <> help "Output Haskell to DIR")
                    <|> flag' Nothing (long "hs" <> help "Output Haskell to stdout (or check syntax)")
                    )
            )
        <|> (FormatHaskellSafe
                <$> haskellOptions
                <*> (   Just <$> strOption (long "hs-safe-out" <> metavar "DIR" <> help "Output Haskell Safe Wrappers to DIR")
                    <|> flag' Nothing (long "hs-safe" <> help "Output Haskell Safe Wrappers to stdout")
                    )
            )
        <|> (FormatHaskellRaw
                <$> haskellOptions
                <*> (   Just <$> strOption (long "chs-out" <> metavar "DIR" <> help "Output Haskell Raw FFI (c2hs) to DIR")
                    <|> flag' Nothing (long "chs" <> help "Output Haskell Raw FFI (c2hs) to stdout")
                    )
            )
        <|> (FormatRust
                <$> rustOptions
                <*> (   Just <$> strOption (long "rs-out" <> metavar "DIR" <> help "Output Rust to DIR")
                    <|> flag' Nothing (long "rs" <> help "Output Rust to stdout")
                    )
            )
        <|> (FormatPython
                <$> (   Just <$> strOption (long "py-out" <> metavar "DIR" <> help "Output Python (Cython) to DIR")
                    <|> flag' Nothing (long "py" <> help "Output Python (Cython) to stdout")
                    )
            )
        )
    <*> some (strArgument (metavar "FILE..." <> help "Input files"))

haskellOptions :: Parser Haskell.Options
haskellOptions = Haskell.Options
    <$> optional (Text.pack <$> strOption (long "import-types" <> metavar "MODULE" <> help "Import types from MODULE"))
    <*> switch (long "types-only" <> help "Generate only types")

rustOptions :: Parser Rust.Options
rustOptions = Rust.Options
    <$> switch (long "types-only" <> help "Generate only types") -- Placeholder for now


generate :: Options -> Model (Lexeme Name) -> OutputFormat -> IO ()
generate opts model (FormatJSON output) = do
    let sem1 = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem1
    if optNoRoundtrip opts
        then LBS.writeFile output (Aeson.encodePretty sem1)
        else do
            let loweredC = C.generate sem1
            case parseText loweredC of
                Left err -> do
                    Text.putStrLn $ "Round-trip failed to parse generated C: " <> Text.pack err
                    let expected = output <> ".expected"
                    LBS.writeFile expected (Aeson.encodePretty sem1)
                    Text.putStrLn $ "Expected model saved to: " <> Text.pack expected
                    exitFailure
                Right nodes2 -> do
                    let model2 = Parser.parseModel [("roundtrip.h", nodes2)]
                        sem2 = Semantic.toSemantic (optStrict opts) model2
                    if sem1 == sem2
                        then LBS.writeFile output (Aeson.encodePretty sem1)
                        else do
                            Text.putStrLn "Semantic round-trip check failed!"
                            diffModels sem1 sem2
                            let expected = output <> ".expected"
                                actual = output <> ".actual"
                            LBS.writeFile expected (Aeson.encodePretty sem1)
                            LBS.writeFile actual (Aeson.encodePretty sem2)
                            Text.putStrLn $ "Expected model saved to: " <> Text.pack expected
                            Text.putStrLn $ "Actual model saved to:   " <> Text.pack actual
                            exitFailure
generate opts model (FormatC output) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    Text.writeFile output $ C.generate sem
generate opts model (FormatCpp output) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    Text.writeFile output $ Cpp.generate sem
generate opts model (FormatHaskell hsOpts outputDir) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    let generated = Haskell.generate hsOpts sem
    case outputDir of
        Just dir -> do
            mapM_ (\(file, text) -> do
                let path = dir </> file
                createDirectoryIfMissing True (takeDirectory path)
                Text.writeFile path text
                when (optVerbose opts) $ putStrLn $ "Generated " <> path) generated
        Nothing ->
            mapM_ (\(file, text) -> do
                Text.putStrLn $ "-- " <> Text.pack file
                Text.putStrLn text) generated
generate opts model (FormatHaskellSafe hsOpts outputDir) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    let generated = Haskell.generateSafe hsOpts sem
    case outputDir of
        Just dir -> do
            mapM_ (\(file, text) -> do
                let path = dir </> file
                createDirectoryIfMissing True (takeDirectory path)
                Text.writeFile path text
                when (optVerbose opts) $ putStrLn $ "Generated " <> path) generated
        Nothing ->
            mapM_ (\(file, text) -> do
                Text.putStrLn $ "-- " <> Text.pack file
                Text.putStrLn text) generated
generate opts model (FormatHaskellRaw hsOpts outputDir) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    let generated = Haskell.generateRaw hsOpts sem
    case outputDir of
        Just dir -> do
            mapM_ (\(file, text) -> do
                let path = dir </> file
                createDirectoryIfMissing True (takeDirectory path)
                Text.writeFile path text
                when (optVerbose opts) $ putStrLn $ "Generated " <> path) generated
        Nothing ->
            mapM_ (\(file, text) -> do
                Text.putStrLn $ "-- " <> Text.pack file
                Text.putStrLn text) generated
generate opts model (FormatRust rsOpts outputDir) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    let generated = Rust.generate rsOpts sem
    case outputDir of
        Just dir -> do
            mapM_ (\(file, text) -> do
                let path = dir </> file
                createDirectoryIfMissing True (takeDirectory path)
                Text.writeFile path text
                when (optVerbose opts) $ putStrLn $ "Generated " <> path) generated
        Nothing ->
            mapM_ (\(file, text) -> do
                Text.putStrLn $ "-- " <> Text.pack file
                Text.putStrLn text) generated
generate opts model (FormatPython outputDir) = do
    let sem = Semantic.toSemantic (optStrict opts) model
    printDiagnostics sem
    let generated = Python.generate sem
    case outputDir of
        Just dir -> do
            mapM_ (\(file, text) -> do
                let path = dir </> file
                createDirectoryIfMissing True (takeDirectory path)
                Text.writeFile path text
                when (optVerbose opts) $ putStrLn $ "Generated " <> path) generated
        Nothing ->
            mapM_ (\(file, text) -> do
                Text.putStrLn $ "# " <> Text.pack file
                Text.putStrLn text) generated


diffModels :: S.SemanticModel -> S.SemanticModel -> IO ()
diffModels m1 m2 = do
    diffList "Enums" S.enums S.enumName m1 m2
    diffList "Constants" S.constants S.constantName m1 m2
    diffList "Resources" S.resources S.resourceName m1 m2
    if S.diagnostics m1 /= S.diagnostics m2
        then Text.putStrLn "Diagnostics differ"
        else return ()

diffList :: Eq a => Text.Text -> (S.SemanticModel -> [a]) -> (a -> Text.Text) -> S.SemanticModel -> S.SemanticModel -> IO ()
diffList label getList getName m1 m2 = do
    let l1 = getList m1
        l2 = getList m2
    if l1 /= l2
        then do
            let n1 = map getName l1
                n2 = map getName l2
                missing = n1 \\ n2
                extra = n2 \\ n1
                common = filter (`elem` n2) n1
                changed = filter (
                    \n ->
                        let v1 = case filter ((== n) . getName) l1 of (x:_) -> x; [] -> error "impossible v1"
                            v2 = case filter ((== n) . getName) l2 of (x:_) -> x; [] -> error "impossible v2"
                        in v1 /= v2) common
            if not (null missing && null extra && null changed)
                then do
                    Text.putStrLn $ label <> " differ:"
                    let report items pref = do
                            let (shown, others) = splitAt 10 items
                            mapM_ (Text.putStrLn . (("  " <> pref <> ": ") <>) ) shown
                            if not (null others)
                                then Text.putStrLn $ "  ... and " <> Text.pack (show (length others)) <> " more"
                                else return ()
                    report missing "Missing"
                    report extra "Extra"
                    report changed "Changed"
                else return ()
        else return ()


printDiagnostics :: S.SemanticModel -> IO ()
printDiagnostics model = do
    mapM_ (hPutStrLn stderr . Text.unpack . formatDiagnostic) (S.diagnostics model)
    when (any ((== S.Error) . S.severity) (S.diagnostics model)) exitFailure
  where
    formatDiagnostic d =
        let loc = maybe "" (\l -> S.locFile l <> ":" <> Text.pack (show (S.locLine l)) <> ":" <> Text.pack (show (S.locColumn l)) <> ": ") (S.location d)
            sev = Text.pack (show (S.severity d)) <> ": "
        in loc <> sev <> S.message d


main :: IO ()
main = do
    opts <- execParser (info (options <**> helper) fullDesc)
    asts <- Program.toList <$> (parseProgram (optInputs opts) >>= getRight)
    when (optVerbose opts) $ hPutStrLn stderr $ "Parsed " ++ show (length asts) ++ " ASTs"
    let model = Parser.parseModel asts
    when (optVerbose opts) $ hPutStrLn stderr $ "Model has " ++ show (length (T.modelMods model)) ++ " modules"
    let outputs = optOutputs opts
        hasRustFile = any (\o -> case o of FormatRust _ (Just _) -> True; _ -> False) outputs
        outputs' = filter (\o -> case o of FormatRust _ Nothing -> not hasRustFile; _ -> True) outputs
    mapM_ (generate opts model) outputs'
    where
        getRight (Left err) = fail err
        getRight (Right ok) = return ok
