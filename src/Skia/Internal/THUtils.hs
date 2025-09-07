-- | Internal utilities used for generating boilerplate using template-haskell.
module Skia.Internal.THUtils (
  qGenerateSkEnum,
  qGenerateSkObject,
  qGatherInlineSkObjectEntries,
  formInlineCppTypePairs,
)
where

import Control.Monad
import Data.Data
import Data.Foldable
import Data.Functor
import Data.IntMap qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Traversable
import Language.C.Inline.Cpp qualified as C
import Language.C.Types (cIdentifierFromString)
import Language.C.Types.Parse (CIdentifier)
import Language.Haskell.Exts.Extension qualified as Extension
import Language.Haskell.Exts.Parser
import Language.Haskell.Meta.Parse
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as TH
import NeatInterpolation
import Skia.Core

appsT :: TH.Type -> [TH.Type] -> TH.Type
appsT = foldl' AppT

apps :: Exp -> [Exp] -> Exp
apps = foldl' AppE

mkPromotedListT :: [Type] -> Type
mkPromotedListT [] = PromotedNilT
mkPromotedListT (t : ts) = appsT PromotedConsT [t, mkPromotedListT ts]

defParseMode :: ParseMode
defParseMode =
  defaultParseMode
    { extensions = map Extension.EnableExtension exts
    }
 where
  exts =
    [ Extension.TypeFamilies
    , Extension.MultiParamTypeClasses
    ]

parseSourceOrDie :: (String -> Either String a) -> T.Text -> Q a
parseSourceOrDie parserFunc source = do
  let result = parserFunc (T.unpack source)
  case result of
    Left errMsg -> do
      error errMsg
    Right result -> do
      pure result

-- | Helper structure used by 'qGenerateSkEnum'
data EnumValueEntry = EnumValueEntry
  { hsValueName :: Name
  , cSymbol :: String
  , cValueExp :: Exp
  , docstring :: T.Text
  }

-- NOTE: This function is intentionally kept.
--
-- renderName :: Name -> T.Text
-- renderName n = T.pack $ pprint n

{- | Generates 1) the data declaration, and 2) instance SkEnum of a C enum type.

Example usage:

@
\$( qGenerateSkEnum
    \"SKPointMode\"
    ''Sk_point_mode
    "Docstring about Sk_point_mode"
    [ (\"Points\", 'POINTS_SK_POINT_MODE, \"Docstring for SKPointMode'Points\")
    , (\"Lines\", 'LINES_SK_POINT_MODE, \"\")
    , (\"Polygon\", 'POLYGON_SK_POINT_MODE, \"\")
    ]
 )
@

... generates the following:

@
-- | Docstring about Sk_point_mode
data SKPointMode
    = SKPointMode'Points -- ^ Docstring for SKPointMode'Points
    | SKPointMode'Lines
    | SKPointMode'Polygon
    deriving (Show, Eq, Ord, Enum, Bounded)

instance SkEnum where
    marshalSkEnum SKPointMode'Points = POINTS_SK_POINT_MODE
    marshalSkEnum SKPointMode'Lines = LINES_SK_POINT_MODE
    marshalSkEnum SKPointMode'Polygon = POLYGON_SK_POINT_MODE

    unmarshalSkEnum POINTS_SK_POINT_MODE = Just SKPointMode'Points
    unmarshalSkEnum LINES_SK_POINT_MODE = Just SKPointMode'Lines
    unmarshalSkEnum POLYGON_SK_POINT_MODE = Just SKPointMode'Polygon
    unmarshalSkEnum _ = Nothing
@
-}
qGenerateSkEnum ::
  -- | Haskell enum name string
  String ->
  -- | Optional docstring of the enum datatype (use "" to indicate no
  -- docstring))
  T.Text ->
  -- | Pairs of (Haskell enum value name string, C symbol, Optional docstring of
  -- the enum value (use "" to indicate no docstring))
  [(String, String, T.Text)] ->
  DecsQ
qGenerateSkEnum hsEnumNameStr datatypeDocstring inPairs = do
  let
    hsEnumName = mkName hsEnumNameStr

  entries <- for inPairs \(hsValueNameStr, cSymbol, docstring) -> do
    cValueExp <- quoteExp C.pure ("int { (int) " <> cSymbol <> " }")
    pure EnumValueEntry
      { hsValueName = mkName (hsEnumNameStr <> "'" <> hsValueNameStr)
      , cSymbol = cSymbol
      , cValueExp = cValueExp
      , docstring = T.unlines
          [ docstring
          , ""
          , "C enum value: @" <> T.pack cSymbol <> "@"
          ]
      }

  let
    enumDerivingStock =
      DerivClause
        (Just StockStrategy)
        [ConT (mkName n) | n <- ["Show", "Eq", "Ord", "Enum", "Bounded"]]
    enumCons = do
      EnumValueEntry{hsValueName} <- entries
      pure $ NormalC hsValueName []
    enumAdtDec = DataD [] hsEnumName [] Nothing enumCons [enumDerivingStock]

  let
    skEnumType = appsT (ConT ''SkEnum) [ConT hsEnumName, ConT (mkName "CInt")]

    skEnumMarshal = FunD 'marshalSkEnum do
      EnumValueEntry{hsValueName, cValueExp} <- entries
      pure $ Clause [ConP hsValueName [] []] (NormalB cValueExp) []

    skEnumUnmarshal = FunD 'unmarshalSkEnum do
      let
        -- Creates an IntMap as a lookup table
        intmapPairs = ListE do
          EnumValueEntry{hsValueName, cValueExp} <- entries
          pure $ TupE [Just (AppE (VarE 'fromIntegral) cValueExp), Just $ ConE hsValueName]
        intmap = AppE (VarE 'IntMap.fromList) intmapPairs
        intmapName = mkName "lookuptable"

        whereClause = FunD intmapName [Clause [] (NormalB intmap) []]

        inputName = mkName "input"

        unmarshalClause = Clause
          [VarP inputName]
          (NormalB (apps (VarE 'IntMap.lookup) [AppE (VarE 'fromIntegral) (VarE inputName), VarE intmapName]))
          [whereClause]

      [unmarshalClause]

    skEnumInstanceDec = InstanceD Nothing [] skEnumType [skEnumMarshal, skEnumUnmarshal]

  -- ### Add docstrings
  --
  -- The generated definitions must be in-scope first, so addModFinalizer is
  -- necessary.
  TH.addModFinalizer do
    -- Docstrings for the enum type
    unless (T.null datatypeDocstring) do
      TH.putDoc (TH.DeclDoc hsEnumName) (T.unpack datatypeDocstring)

    -- Docstrings for enum values
    for_ entries \EnumValueEntry{hsValueName, docstring} -> do
      unless (T.null docstring) do
        TH.putDoc (TH.DeclDoc hsValueName) (T.unpack docstring)

  pure [enumAdtDec, skEnumInstanceDec]

{- | Example usage:

@
createClassExtends 'SKStreamRewindable ['SKStream])
@

... generates the following:

@
type instance ParentTypes SKStreamRewindable = '[SKStream]
type IsSKStreamRewindable = IsSubclassOf SKStreamRewindable
@
-}
createClassExtends ::
  -- | Class
  Name ->
  -- | Parents. Can be empty.
  [Name] ->
  DecsQ
createClassExtends cls parents = do
  -- The following code does not work and I don't know how to fix it.
  --
  -- The parser fails with "Improper character constant or misplaced '"
  --
  -- ```
  -- let cls = renderName clsName
  -- let parents = "'[" <> T.intercalate ", " (map renderName parentNames) <> "]"
  -- parseSourceOrDie
  --     (parseDecsWithMode defParseMode)
  --     [text|
  --         type instance ParentTypes ${cls} = ${parents}
  --         type Is${cls} = IsSubclassOf ${cls}
  --     |]
  -- ```

  let
    parentTypesDec =
      TySynInstD (TySynEqn Nothing lhs rhs)
     where
      lhs = appsT (ConT ''ParentTypes) [ConT cls]
      rhs = mkPromotedListT [ConT parent | parent <- parents]

  let
    isTypeName = mkName ("Is" <> nameBase cls)
    isTypeAliasDec =
      TySynD
        isTypeName
        [PlainTV (mkName "a") ()]
        ( appsT
            (ConT ''IsSubclassOf)
            [ ConT cls
            , VarT (mkName "a")
            ]
        )

  pure
    [ parentTypesDec
    , isTypeAliasDec
    ]

data SkObjectEntry = SkObjectEntry
  { cSymbol :: String
  -- ^ C symbol name, e.g. @skia::textlayout::Paragraph@
  , hsName :: Name
  -- ^ Haskell newtype name
  , cName :: Name
  -- ^ C opaque type name
  --
  -- Consider that a SkObject newtype is defined as the following:
  --
  -- @
  -- data <cName> -- Opaque ADT
  --
  -- newtype <hsName> = <hsName> { un<hsName> :: Ptr <cName> }
  -- @
  }

-- | Internal state of THUtils. Used to create other utilities.
data THUtilsState = THUtilsState
  { objectEntries :: Seq.Seq SkObjectEntry
  }
  deriving (Typeable)

getTHUtilsState :: Q THUtilsState
getTHUtilsState = do
  st <- getQ @THUtilsState
  case st of
    Nothing -> do
      -- Returns the default state in the beginning
      pure THUtilsState
        { objectEntries = Seq.empty
        }
    Just st -> do
      pure st

putTHUtilsState :: THUtilsState -> Q ()
putTHUtilsState = putQ @THUtilsState

modifyTHUtilsState :: (THUtilsState -> THUtilsState) -> Q ()
modifyTHUtilsState f = do
  st <- getTHUtilsState
  putTHUtilsState (f st)

registerSkObjectEntry :: SkObjectEntry -> Q ()
registerSkObjectEntry entry = do
  modifyTHUtilsState \st ->
    st{objectEntries = objectEntries st Seq.|> entry}

qGenerateSkObject ::
  -- | C symbol
  String ->
  -- | Haskell object name string
  String ->
  -- | Superclasses
  [Name] ->
  -- | Optional docstring of the enum datatype (use "" to indicate no
  -- docstring))
  T.Text ->
  DecsQ
qGenerateSkObject cSymbol (T.pack -> hsNameStr) superClasses docstring = do
  -- NOTE: We are using T.Text only because NeatInterpolation only allows text
  -- parameters.
  let hsName :: Name = mkName (T.unpack hsNameStr)

  let hsNameProjectorStr :: T.Text = "un" <> hsNameStr

  let cNameStr :: T.Text = "C'" <> hsNameStr
  let cName :: Name = mkName (T.unpack cNameStr)

  registerSkObjectEntry SkObjectEntry
    { cSymbol = cSymbol
    , hsName = hsName
    , cName = cName
    }

  -- ### Add docstrings
  --
  -- The generated definitions must be in-scope first, so addModFinalizer is
  -- necessary.
  TH.addModFinalizer do
    -- Docstrings for the newtype declaration
    unless (T.null docstring) do
      TH.putDoc (TH.DeclDoc hsName) (T.unpack docstring)


  decs1 <- parseSourceOrDie
    (parseDecsWithMode defParseMode)
    [text|
      data ${cNameStr}

      newtype ${hsNameStr} = ${hsNameStr}
        { ${hsNameProjectorStr} :: Ptr ${cNameStr}
        }
        deriving (Show)

      instance PtrNewType ${hsNameStr} ${cNameStr} where
        fromPtr = ${hsNameStr}
        ptr = ${hsNameProjectorStr}

      instance SkObject ${hsNameStr} where
        fromAnyPtr = ${hsNameStr} . castPtr
        toAnyPtr = castPtr . ${hsNameProjectorStr}
    |]

  decs2 <- createClassExtends hsName superClasses

  pure (decs1 <> decs2)

-- | Creates a C.cppTypePairs
qGatherInlineSkObjectEntries :: Q Exp
qGatherInlineSkObjectEntries = do
  st <- getTHUtilsState
  let
    entries = st.objectEntries
    list =
      ListE $
        toList entries <&> \entry ->
          TupE
            [ Just $ LitE (StringL entry.cSymbol)
            , Just $ LitE (StringL (showName entry.cName))
            ]
  pure $ list

formInlineCppTypePairs :: [(String, String)] -> C.Context
formInlineCppTypePairs pairs =
  C.cppTypePairs $
    pairs <&> \(cSymbol, cNameStr) ->
      (mkCppIdentifier cSymbol, pure $ ConT $ mkName cNameStr)
  where
  mkCppIdentifier :: String -> CIdentifier
  mkCppIdentifier s = 
    case cIdentifierFromString True s of
      Left err -> error $ "CIdentifier fromString: invalid string " ++ show s ++ "\n" ++ err
      Right x -> x
