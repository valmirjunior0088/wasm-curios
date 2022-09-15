module Syntax.Module
  ( magic
  , version
  , CustomSec (..)
  , customSecId
  , TypeSec (..)
  , typeSecId
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)
  , importSecId
  , FuncSec (..)
  , funcSecId
  , Table (..)
  , TableSec (..)
  , tableSecId
  , Mem (..)
  , MemSec (..)
  , memSecId
  , Global (..)
  , GlobalSec (..)
  , globalSecId
  , ExportDesc (..)
  , Export (..)
  , ExportSec (..)
  , exportSecId
  , Start (..)
  , StartSec (..)
  , startSecId
  , Elem (..)
  , ElemSec (..)
  , elemSecId
  , Locals (..)
  , Func (..)
  , Code (..)
  , CodeSec (..)
  , codeSecId
  , Data (..)
  , DataSec (..)
  , dataSecId
  , DataCountSec (..)
  , dataCountSecId
  , Module (..)
  , emptyModule
  )
  where

import Syntax.Conventions (TypeIdx, FuncIdx, TableIdx, MemIdx, GlobalIdx, Vec, Name)
import Syntax.Types (ValType, FuncType, TableType, MemType, GlobalType)
import Syntax.Instructions (Expr)
import Syntax.LLVM (RelocEntry, SymInfo)
import Data.Word (Word8, Word32)

magic :: String
magic = "\0asm"

version :: Word32
version = 1

data CustomSec =
  CustomSec Name [Word8]
  deriving (Show)

customSecId :: Word8
customSecId = 0

data TypeSec =
  TypeSec (Vec FuncType)
  deriving (Show)

typeSecId :: Word8
typeSecId = 1

data ImportDesc =
  ImportFunc TypeIdx |
  ImportTable TableType |
  ImportMem MemType |
  ImportGlobal GlobalType
  deriving (Show)

data Import =
  Import Name Name ImportDesc
  deriving (Show)

data ImportSec =
  ImportSec (Vec Import)
  deriving (Show)

importSecId :: Word8
importSecId = 2

data FuncSec =
  FuncSec (Vec TypeIdx)
  deriving (Show)

funcSecId :: Word8
funcSecId = 3

data Table =
  Table TableType
  deriving (Show)

data TableSec =
  TableSec (Vec Table)
  deriving (Show)

tableSecId :: Word8
tableSecId = 4

data Mem =
  Mem MemType
  deriving (Show)

data MemSec =
  MemSec (Vec Mem)
  deriving (Show)

memSecId :: Word8
memSecId = 5

data Global =
  Global GlobalType Expr
  deriving (Show)

data GlobalSec =
  GlobalSec (Vec Global)
  deriving (Show)

globalSecId :: Word8
globalSecId = 6

data ExportDesc =
  ExportFunc FuncIdx |
  ExportTable TableIdx |
  ExportMem MemIdx |
  ExportGlobal GlobalIdx
  deriving (Show)

data Export =
  Export Name ExportDesc
  deriving (Show)

data ExportSec =
  ExportSec (Vec Export)
  deriving (Show)

exportSecId :: Word8
exportSecId = 7

data Start =
  Start FuncIdx
  deriving (Show)

data StartSec =
  StartSec Start
  deriving (Show)

startSecId :: Word8
startSecId = 8

data Elem =
  Elem Expr (Vec FuncIdx)
  deriving (Show)

data ElemSec =
  ElemSec (Vec Elem)
  deriving (Show)

elemSecId :: Word8
elemSecId = 9

data Locals =
  Locals Word32 ValType
  deriving (Show)

data Func =
  Func (Vec Locals) Expr
  deriving (Show)

data Code =
  Code Func
  deriving (Show)

data CodeSec =
  CodeSec (Vec Code)
  deriving (Show)

codeSecId :: Word8
codeSecId = 10

data Data =
  Data Expr (Vec Word8) [RelocEntry]
  deriving (Show)

data DataSec =
  DataSec (Vec Data)
  deriving (Show)

dataSecId :: Word8
dataSecId = 11

data DataCountSec =
  DataCountSec Word32
  deriving (Show)

dataCountSecId :: Word8
dataCountSecId = 12

data Module = Module
  { typeSec :: [FuncType]
  , importSec :: [Import]
  , funcSec :: [TypeIdx]
  , tableSec :: [Table]
  , memSec :: [Mem]
  , globalSec :: [Global]
  , exportSec :: [Export]
  , startSec :: Maybe FuncIdx
  , elemSec :: [Elem]
  , codeSec :: [Code]
  , dataSec :: [Data]
  , linkingSec :: [SymInfo]
  }

emptyModule :: Module
emptyModule = Module
  { typeSec = []
  , importSec = []
  , funcSec = []
  , tableSec = []
  , memSec = []
  , globalSec = []
  , exportSec = []
  , startSec = Nothing
  , elemSec = []
  , codeSec = []
  , dataSec = []
  , linkingSec = []
  }
