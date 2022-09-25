module Construct
  ( Construct
  , runConstruct
  , importFunc
  , importTable
  , importMem
  , importGlobal
  , declareFunc
  , declareTable
  , declareMem
  , declareGlobal
  , exportFunc
  , exportTable
  , exportMem
  , exportGlobal
  , setStart
  , commitFuncRefs
  , addParameter
  , addLocal
  , pushFrame
  , pushUnreachable
  , pushNop
  , pushBlock
  , pushLoop
  , pushBr
  , pushBrIf
  , pushBrTable
  , pushReturn
  , pushCall
  , pushCallIndirect
  , pushDrop
  , pushLocalGet
  , pushLocalSet
  , pushGlobalGet
  , pushGlobalSet
  , pushI32Load
  , pushI64Load
  , pushF32Load
  , pushF64Load
  , pushI32Store
  , pushI64Store
  , pushF32Store
  , pushF64Store
  , pushI32Const
  , pushI64Const
  , pushF32Const
  , pushF64Const
  , pushI32FuncRef
  , commitCode
  )
  where

import Syntax.Conventions
  ( TypeIdx
  , FuncIdx
  , TableIdx
  , MemIdx
  , GlobalIdx
  , LocalIdx
  , LabelIdx
  , SymIdx
  , Vec (..)
  , Name (..)
  )

import Syntax.Types
  ( ValType (..)
  , ResultType (..)
  , FuncType (..)
  , RefType (..)
  , TableType (..)
  , GlobalType (..)
  , Limits (..)
  , MemType (..)
  , Mut (..)
  )

import Syntax.Module
  ( ImportDesc (..)
  , Import (..)
  , ExportDesc (..)
  , Export (..)
  , Global (..)
  , Table (..)
  , Mem (..)
  , Elem (..)
  , Locals (..)
  , Func (..)
  , Code (..)
  , Module (..)
  , emptyModule
  )

import Syntax.Instructions (BlockType (..), MemArg (..), Instr (..), Expr (..))
import Syntax.LLVM (SymType (..), SymFlags (..), SymInfo (..))
import Control.Monad (unless)
import Control.Monad.State (State, evalState)
import Data.Int (Int32, Int64)
import Data.List (group)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (.=), (%=), (<>=), mapped, _2, _head)

data ModlState = ModlState
  { nextTypeIdx :: TypeIdx
  , nextFuncIdx :: FuncIdx
  , nextTableIdx :: TableIdx
  , nextMemIdx :: MemIdx
  , nextGlobalIdx :: GlobalIdx
  , nextSymIdx :: SymIdx

  , typeIdxs :: [(FuncType, TypeIdx)]
  , funcIdxs :: [(String, (FuncIdx, SymIdx))]
  , tableIdxs :: [(String, TableIdx)]
  , memIdxs :: [(String, MemIdx)]
  , globalIdxs :: [(String, (GlobalIdx, SymIdx))]
  , funcRefs :: [(String, (Int32, SymIdx))] 
  }
  deriving (Show, Generic)

emptyModlState :: ModlState
emptyModlState = ModlState
  { nextTypeIdx = 0
  , nextFuncIdx = 0
  , nextTableIdx = 0
  , nextMemIdx = 0
  , nextGlobalIdx = 0
  , nextSymIdx = 0

  , typeIdxs = []
  , funcIdxs = []
  , tableIdxs = []
  , memIdxs = []
  , globalIdxs = []
  , funcRefs = []
  }

data CodeState = CodeState
  { nextLocalIdx :: LocalIdx

  , locals :: [ValType]
  , variables :: [(String, LocalIdx)]

  , frames :: [[Instr]]
  , labels :: [(String, LabelIdx)]
  }
  deriving (Show, Generic)

emptyCodeState :: CodeState
emptyCodeState = CodeState
  { nextLocalIdx = 0

  , locals = []
  , variables = []
  
  , frames = [[]]
  , labels = [("return", 0)]
  }

data ConstructState = ConstructState
  { modl :: Module
  , modlState :: ModlState
  , codeState :: CodeState
  }
  deriving (Show, Generic)

emptyState :: ConstructState
emptyState = ConstructState 
  { modl = emptyModule
  , modlState = emptyModlState
  , codeState = emptyCodeState
  }

type Construct = State ConstructState

runConstruct :: Construct a -> Module
runConstruct action = evalState (action >> use (the @"modl")) emptyState

getType :: FuncType -> Construct TypeIdx
getType funcType = do
  typeIdxs <- use (the @"modlState" . the @"typeIdxs")

  case lookup funcType typeIdxs of
    Nothing -> do
      typeIdx <- use (the @"modlState" . the @"nextTypeIdx")
      (the @"modlState" . the @"nextTypeIdx") .= succ typeIdx

      (the @"modl" . the @"typeSec") <>= [funcType]
      (the @"modlState" . the @"typeIdxs") <>= [(funcType, typeIdx)]

      return typeIdx
    
    Just typeIdx ->
      return typeIdx

importFunc :: String -> String -> [ValType] -> [ValType] -> Construct ()
importFunc namespace name inputType outputType = do
  funcSec <- use (the @"modl" . the @"funcSec")

  unless (null funcSec)
    (error "cannot import func after having declared a func")

  typeIdx <- getType
    (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

  funcIdx <- use (the @"modlState" . the @"nextFuncIdx")
  (the @"modlState" . the @"nextFuncIdx") .= succ funcIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    func = Import (Name namespace) (Name name) (ImportFunc typeIdx)

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = True
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_FUNCTION funcIdx (Just (Name name))) flags
  
  (the @"modl" . the @"importSec") <>= [func]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"funcIdxs") <>= [(name, (funcIdx, symIdx))]

importTable :: String -> String -> RefType -> Limits -> Construct ()
importTable namespace name refType limits = do
  tableSec <- use (the @"modl" . the @"tableSec")

  unless (null tableSec)
    (error "cannot import a table after having declared a table")

  tableIdx <- use (the @"modlState" . the @"nextTableIdx")
  (the @"modlState" . the @"nextTableIdx") .= succ tableIdx

  let table = Import (Name namespace) (Name name) (ImportTable (TableType refType limits))
  
  (the @"modl" . the @"importSec") <>= [table]
  (the @"modlState" . the @"tableIdxs") <>= [(name, tableIdx)]

importMem :: String -> String -> Limits -> Construct ()
importMem namespace name limits = do
  memSec <- use (the @"modl" . the @"memSec")

  unless (null memSec)
    (error "cannot import a mem after having declared a mem")

  memIdx <- use (the @"modlState" . the @"nextMemIdx")
  (the @"modlState" . the @"nextMemIdx") .= succ memIdx

  let mem = Import (Name namespace) (Name name) (ImportMem (MemType limits))
  
  (the @"modl" . the @"importSec") <>= [mem]
  (the @"modlState" . the @"memIdxs") <>= [(name, memIdx)]

importGlobal :: String -> String -> ValType -> Mut -> Construct ()
importGlobal namespace name valType mut = do
  globalSec <- use (the @"modl" . the @"globalSec")

  unless (null globalSec)
    (error "cannot import a global after having declared a global")

  globalIdx <- use (the @"modlState" . the @"nextGlobalIdx")
  (the @"modlState" . the @"nextGlobalIdx") .= succ globalIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    global = Import (Name namespace) (Name name) (ImportGlobal (GlobalType valType mut))

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = True
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx (Just (Name name))) flags
  
  (the @"modl" . the @"importSec") <>= [global]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"globalIdxs") <>= [(name, (globalIdx, symIdx))]

declareFunc :: String -> [ValType] -> [ValType] -> Construct ()
declareFunc name inputType outputType = do
  typeIdx <- getType
    (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

  funcIdx <- use (the @"modlState" . the @"nextFuncIdx")
  (the @"modlState" . the @"nextFuncIdx") .= succ funcIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = False
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }

    info = SymInfo (SYMTAB_FUNCTION funcIdx (Just (Name name))) flags
  
  (the @"modl" . the @"funcSec") <>= [typeIdx]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"funcIdxs") <>= [(name, (funcIdx, symIdx))]

declareTable :: String -> RefType -> Limits -> Construct ()
declareTable name refType limits = do
  tableIdx <- use (the @"modlState" . the @"nextTableIdx")
  (the @"modlState" . the @"nextTableIdx") .= succ tableIdx

  let table = Table (TableType refType limits)
  
  (the @"modl" . the @"tableSec") <>= [table]
  (the @"modlState" . the @"tableIdxs") <>= [(name, tableIdx)]

declareMem :: String -> Limits -> Construct ()
declareMem name limits = do
  memIdx <- use (the @"modlState" . the @"nextMemIdx")
  (the @"modlState" . the @"nextMemIdx") .= succ memIdx

  let mem = Mem (MemType limits)

  (the @"modl" . the @"memSec") <>= [mem]
  (the @"modlState" . the @"memIdxs") <>= [(name, memIdx)]

declareGlobal :: String -> ValType -> Mut -> Expr -> Construct ()
declareGlobal name valType mut expr = do
  globalIdx <- use (the @"modlState" . the @"nextGlobalIdx")
  (the @"modlState" . the @"nextGlobalIdx") .= succ globalIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    global = Global (GlobalType valType mut) expr

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = False
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx (Just (Name name))) flags
  
  (the @"modl" . the @"globalSec") <>= [global]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"globalIdxs") <>= [(name, (globalIdx, symIdx))]

exportFunc :: String -> Construct ()
exportFunc name = do
  funcIdxs <- use (the @"modlState" . the @"funcIdxs")

  case lookup name funcIdxs of
    Nothing ->
      error ("tried to export unknown function \"" ++ name ++ "\"")

    Just (funcIdx, _) ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportFunc funcIdx)]

exportTable :: String -> Construct ()
exportTable name = do
  tableIdxs <- use (the @"modlState" . the @"tableIdxs")

  case lookup name tableIdxs of
    Nothing ->
      error ("tried to export unknown table \"" ++ name ++ "\"")

    Just tableIdx ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportTable tableIdx)]

exportMem :: String -> Construct ()
exportMem name = do
  memIdxs <- use (the @"modlState" . the @"memIdxs")

  case lookup name memIdxs of
    Nothing ->
      error ("tried to export unknown memory \"" ++ name ++ "\"")

    Just memIdx ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportMem memIdx)]

exportGlobal :: String -> Construct ()
exportGlobal name = do
  globalIdxs <- use (the @"modlState" . the @"globalIdxs")

  case lookup name globalIdxs of
    Nothing ->
      error ("tried to export unknown global \"" ++ name ++ "\"")

    Just (globalIdx, _) ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportGlobal globalIdx)]

setStart :: String -> Construct ()
setStart name = do
  funcIdxs <- use (the @"modlState" . the @"funcIdxs")

  case lookup name funcIdxs of
    Nothing ->
      error ("tried to set unknown function \"" ++ name ++ "\" as start")

    Just (funcIdx, _) ->
      (the @"modl" . the @"startSec") .= Just funcIdx

commitFuncRefs :: Construct ()
commitFuncRefs = do
  elemSec <- use (the @"modl" . the @"elemSec")
  funcRefs <- use (the @"modlState" . the @"funcRefs")

  unless (null elemSec && null funcRefs)
    (error "funcrefs are already committed and it is not possible to update them")

  funcIdxs <- use (the @"modlState" . the @"funcIdxs")

  (the @"modl" . the @"elemSec") .=
    [Elem (Expr [I32Const 1]) (Vec [funcIdx | (_, (funcIdx, _)) <- funcIdxs])]

  (the @"modlState" . the @"funcRefs") .=
    [(name, (funcRef, symIdx)) | (name, (_, symIdx)) <- funcIdxs | funcRef <- [1..]]

addParameter :: String -> Construct ()
addParameter name = do
  locals <- use (the @"codeState" . the @"locals")

  unless (null locals)
    (error "cannot add a parameter after having added a local")
  
  localIdx <- use (the @"codeState" . the @"nextLocalIdx")
  (the @"codeState" . the @"nextLocalIdx") .= succ localIdx

  (the @"codeState" . the @"variables") <>= [(name, localIdx)]

addLocal :: String -> ValType -> Construct ()
addLocal name valType = do
  localIdx <- use (the @"codeState" . the @"nextLocalIdx")
  (the @"codeState" . the @"nextLocalIdx") .= succ localIdx

  (the @"codeState" . the @"locals") <>= [valType]
  (the @"codeState" . the @"variables") <>= [(name, localIdx)]

pushFrame :: String -> Construct ()
pushFrame name = do
  (the @"codeState" . the @"frames") %= ([] :)

  (the @"codeState" . the @"labels" . mapped . _2) %= succ
  (the @"codeState" . the @"labels") %= ((name, 0) :)

popFrame :: Construct [Instr]
popFrame = do
  frame <- use (the @"codeState" . the @"frames" . _head)
  (the @"codeState" . the @"frames") %= tail

  (the @"codeState" . the @"labels") %= tail
  (the @"codeState" . the @"labels" . mapped . _2) %= pred

  return frame

pushInstr :: Instr -> Construct ()
pushInstr instr =
  (the @"codeState" . the @"frames" . _head) <>= [instr]

pushUnreachable :: Construct ()
pushUnreachable = pushInstr Unreachable

pushNop :: Construct ()
pushNop = pushInstr Nop

getBlockType :: ([ValType], [ValType]) -> Construct BlockType
getBlockType = \case
  ([], []) ->
    return BlockEmpty
  
  ([], [valType]) ->
    return (BlockValType valType)
  
  (inputType, outputType) -> do
    typeIdx <- getType
      (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

    return (BlockTypeIdx typeIdx)

pushBlock :: [ValType] -> [ValType] -> Construct ()
pushBlock inputType outputType =
  pushInstr =<< Block <$> getBlockType (inputType, outputType) <*> popFrame

pushLoop :: [ValType] -> [ValType] -> Construct ()
pushLoop inputType outputType =
  pushInstr =<< Loop <$> getBlockType (inputType, outputType) <*> popFrame

getLabel :: String -> Construct LabelIdx
getLabel name = do
  labels <- use (the @"codeState" . the @"labels")

  case lookup name labels of
    Nothing -> error ("tried to get unknown label \"" ++ name ++ "\"")
    Just labelIdx -> return labelIdx

pushBr :: String -> Construct ()
pushBr name = pushInstr . Br =<< getLabel name

pushBrIf :: String -> Construct ()
pushBrIf name = pushInstr . BrIf =<< getLabel name

pushBrTable :: [String] -> String -> Construct ()
pushBrTable names name =
  pushInstr =<< BrTable <$> (Vec <$> mapM getLabel names) <*> getLabel name

pushReturn :: Construct ()
pushReturn = pushInstr Return

pushCall :: String -> Construct ()
pushCall name = do
  funcs <- use (the @"modlState" . the @"funcIdxs")

  case lookup name funcs of
    Nothing -> error ("tried to call unknown function \"" ++ name ++ "\"")
    Just (funcIdx, symIdx) -> pushInstr (Call funcIdx symIdx)

pushCallIndirect :: [ValType] -> [ValType] -> Construct ()
pushCallIndirect inputType outputType = do
  typeIdx <- getType
    (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))
    
  pushInstr (CallIndirect typeIdx 0)
      
pushDrop :: Construct ()
pushDrop = pushInstr Drop

getLocal :: String -> Construct LocalIdx
getLocal name = do
  variables <- use (the @"codeState" . the @"variables")

  case lookup name variables of
    Nothing -> error ("tried to get unknown variable \"" ++ name ++ "\"")
    Just localIdx -> return localIdx

pushLocalGet :: String -> Construct ()
pushLocalGet name = pushInstr . LocalGet =<< getLocal name

pushLocalSet :: String -> Construct ()
pushLocalSet name = pushInstr . LocalSet =<< getLocal name

pushLocalTee :: String -> Construct ()
pushLocalTee name = pushInstr . LocalTee =<< getLocal name

getGlobal :: String -> Construct (GlobalIdx, SymIdx)
getGlobal name = do
  globals <- use (the @"modlState" . the @"globalIdxs")

  case lookup name globals of
    Nothing -> error ("tried to get unknown global \"" ++ name ++ "\"")
    Just (globalIdx, symIdx) -> return (globalIdx, symIdx)

pushGlobalGet :: String -> Construct ()
pushGlobalGet name =
  pushInstr . uncurry GlobalGet =<< getGlobal name

pushGlobalSet :: String -> Construct ()
pushGlobalSet name =
  pushInstr . uncurry GlobalSet =<< getGlobal name

pushI32Load :: MemArg -> Construct ()
pushI32Load memArg = pushInstr (I32Load memArg)

pushI64Load :: MemArg -> Construct ()
pushI64Load memArg = pushInstr (I64Load memArg)

pushF32Load :: MemArg -> Construct ()
pushF32Load memArg = pushInstr (F32Load memArg)

pushF64Load :: MemArg -> Construct ()
pushF64Load memArg = pushInstr (F64Load memArg)

pushI32Store :: MemArg -> Construct ()
pushI32Store memArg = pushInstr (I32Store memArg)

pushI64Store :: MemArg -> Construct ()
pushI64Store memArg = pushInstr (I64Store memArg)

pushF32Store :: MemArg -> Construct ()
pushF32Store memArg = pushInstr (F32Store memArg)

pushF64Store :: MemArg -> Construct ()
pushF64Store memArg = pushInstr (F64Store memArg)

pushI32Const :: Int32 -> Construct ()
pushI32Const value = pushInstr (I32Const value)

pushI64Const :: Int64 -> Construct ()
pushI64Const value = pushInstr (I64Const value)

pushF32Const :: Float -> Construct ()
pushF32Const value = pushInstr (F32Const value)

pushF64Const :: Double -> Construct ()
pushF64Const value = pushInstr (F64Const value)

pushI32FuncRef :: String -> Construct ()
pushI32FuncRef name = do
  funcRefs <- use (the @"modlState" . the @"funcRefs")

  case lookup name funcRefs of
    Nothing -> error ("tried to create unknown func ref \"" ++ name ++ "\"")
    Just (funcRef, symIdx) -> pushInstr (I32FuncRef funcRef symIdx)

commitCode :: Construct ()
commitCode = do
  locals <- use (the @"codeState" . the @"locals")
  frames <- use (the @"codeState" . the @"frames")

  unless (length frames == 1)
    (error "wrong number of frames found when commiting code")
  
  (the @"modl" . the @"codeSec") <>=
    [Code (Func (Vec [Locals (fromIntegral $ length valTypes) (head valTypes) | valTypes <- group locals]) (Expr (head frames)))]
  
  (the @"codeState") .= emptyCodeState
