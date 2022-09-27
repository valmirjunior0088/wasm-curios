module Construct
  ( Construct
  , construct
  , Emit
  , emit
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
  , declareExportFunc
  , declareExportTable
  , declareExportMem
  , declareExportGlobal
  , setStart
  , commitFuncRefs
  , addLocal
  , pushUnreachable
  , pushNop
  , pushBlock
  , popBlock
  , pushLoop
  , popLoop
  , pushBr
  , pushBrIf
  , pushBrTable
  , pushReturn
  , pushCall
  , pushCallIndirect
  , pushDrop
  , pushLocalGet
  , pushLocalSet
  , pushLocalTee
  , pushAddLocalSet
  , pushAddLocalTee
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
  , pushI32Add
  , pushI32Sub
  , pushI64Add
  , pushI64Sub
  , pushF32Add
  , pushF32Sub
  , pushF64Add
  , pushF64Sub
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
import Control.Monad (void, unless)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT, execStateT, State, execState)
import Data.Int (Int32, Int64)
import Data.List (group)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (.=), (%=), (<>=), (<~), mapped, _2, _head)

data ModlState = ModlState
  { modl :: Module
  
  , nextTypeIdx :: TypeIdx
  , nextFuncIdx :: FuncIdx
  , nextTableIdx :: TableIdx
  , nextMemIdx :: MemIdx
  , nextGlobalIdx :: GlobalIdx
  , nextSymIdx :: SymIdx

  , types :: [(FuncType, TypeIdx)]
  , funcs :: [(String, (FuncIdx, SymIdx))]
  , emitters :: [([String], Emit ())]
  , tables :: [(String, TableIdx)]
  , mems :: [(String, MemIdx)]
  , globals :: [(String, (GlobalIdx, SymIdx))]
  , funcRefs :: [(String, (Int32, SymIdx))] 
  }
  deriving (Generic)

emptyState :: ModlState
emptyState = ModlState 
  { modl = emptyModule

  , nextTypeIdx = 0
  , nextFuncIdx = 0
  , nextTableIdx = 0
  , nextMemIdx = 0
  , nextGlobalIdx = 0
  , nextSymIdx = 0

  , types = []
  , funcs = []
  , emitters = []
  , tables = []
  , mems = []
  , globals = []
  , funcRefs = []
  }

type Construct = State ModlState

construct :: Construct a -> Module
construct action = modl where
  ModlState { modl } = execState (action >> commitCodes) emptyState

data Frame =
  RootFrame |
  BlockFrame BlockType |
  LoopFrame BlockType

data CodeState = CodeState
  { nextLocalIdx :: LocalIdx

  , locals :: [ValType]
  , variables :: [(String, LocalIdx)]

  , frames :: [(Frame, [Instr])]
  , labels :: [(String, LabelIdx)]
  }
  deriving (Generic)

emptyCodeState :: [String] -> CodeState
emptyCodeState names = do
  let
    go (parameters, nextLocalIdx) name =
      (parameters ++ [(name, nextLocalIdx)], succ nextLocalIdx)

    (variables, localIdx) = foldl go ([], 0) names 

  CodeState
    { nextLocalIdx = localIdx

    , locals = []
    , variables = variables

    , frames = [(RootFrame, [])]
    , labels = [("root", 0)]
    }

type Emit = StateT CodeState Construct

emit :: [String] -> Emit a -> Construct Code
emit names emitter = do
  CodeState { locals, frames } <- execStateT emitter (emptyCodeState names)

  case frames of
    [(RootFrame, instrs)] ->
      return (Code (Func (Vec built) (Expr instrs))) where
        build valTypes = Locals (fromIntegral $ length valTypes) (head valTypes)
        built = [build valTypes | valTypes <- group locals]

    _ ->
      error "found undelimited frame while trying to emit code"

getType :: FuncType -> Construct TypeIdx
getType funcType = do
  types <- use (the @"types")

  case lookup funcType types of
    Nothing -> do
      typeIdx <- use (the @"nextTypeIdx")
      (the @"nextTypeIdx") .= succ typeIdx

      (the @"modl" . the @"typeSec") <>= [funcType]
      (the @"types") <>= [(funcType, typeIdx)]

      return typeIdx
    
    Just typeIdx ->
      return typeIdx

importFunc :: String -> String -> [ValType] -> [ValType] -> Construct ()
importFunc namespace name inputs outputs = do
  funcSec <- use (the @"modl" . the @"funcSec")

  unless (null funcSec)
    (error "cannot import func after having declared a func")

  typeIdx <- getType
    (FuncType (ResultType (Vec inputs)) (ResultType (Vec outputs)))

  funcIdx <- use (the @"nextFuncIdx")
  (the @"nextFuncIdx") .= succ funcIdx

  symIdx <- use (the @"nextSymIdx")
  (the @"nextSymIdx") .= succ symIdx

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
  (the @"funcs") <>= [(name, (funcIdx, symIdx))]

importTable :: String -> String -> TableType -> Construct ()
importTable namespace name tableType = do
  tableSec <- use (the @"modl" . the @"tableSec")

  unless (null tableSec)
    (error "cannot import a table after having declared a table")

  tableIdx <- use (the @"nextTableIdx")
  (the @"nextTableIdx") .= succ tableIdx

  let table = Import (Name namespace) (Name name) (ImportTable tableType)
  
  (the @"modl" . the @"importSec") <>= [table]
  (the @"tables") <>= [(name, tableIdx)]

importMem :: String -> String -> MemType -> Construct ()
importMem namespace name memType = do
  memSec <- use (the @"modl" . the @"memSec")

  unless (null memSec)
    (error "cannot import a mem after having declared a mem")

  memIdx <- use (the @"nextMemIdx")
  (the @"nextMemIdx") .= succ memIdx

  let mem = Import (Name namespace) (Name name) (ImportMem memType)
  
  (the @"modl" . the @"importSec") <>= [mem]
  (the @"mems") <>= [(name, memIdx)]

importGlobal :: String -> String -> GlobalType -> Construct ()
importGlobal namespace name globalType = do
  globalSec <- use (the @"modl" . the @"globalSec")

  unless (null globalSec)
    (error "cannot import a global after having declared a global")

  globalIdx <- use (the @"nextGlobalIdx")
  (the @"nextGlobalIdx") .= succ globalIdx

  symIdx <- use (the @"nextSymIdx")
  (the @"nextSymIdx") .= succ symIdx

  let
    global = Import (Name namespace) (Name name) (ImportGlobal globalType)

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
  (the @"globals") <>= [(name, (globalIdx, symIdx))]

declareFunc :: String -> [(String, ValType)] -> [ValType] -> Emit a -> Construct ()
declareFunc name inputs outputs emitter = do
  typeIdx <- getType
    (FuncType (ResultType (Vec [valType | (_, valType) <- inputs])) (ResultType (Vec outputs)))

  funcIdx <- use (the @"nextFuncIdx")
  (the @"nextFuncIdx") .= succ funcIdx

  symIdx <- use (the @"nextSymIdx")
  (the @"nextSymIdx") .= succ symIdx

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
  (the @"funcs") <>= [(name, (funcIdx, symIdx))]
  (the @"emitters") <>= [([parameter | (parameter, _) <- inputs], void emitter)]

declareTable :: String -> TableType -> Construct ()
declareTable name tableType = do
  tableIdx <- use (the @"nextTableIdx")
  (the @"nextTableIdx") .= succ tableIdx
  
  (the @"modl" . the @"tableSec") <>= [Table tableType]
  (the @"tables") <>= [(name, tableIdx)]

declareMem :: String -> MemType -> Construct ()
declareMem name memType = do
  memIdx <- use (the @"nextMemIdx")
  (the @"nextMemIdx") .= succ memIdx

  (the @"modl" . the @"memSec") <>= [Mem memType]
  (the @"mems") <>= [(name, memIdx)]

declareGlobal :: String -> GlobalType -> Expr -> Construct ()
declareGlobal name globalType expr = do
  globalIdx <- use (the @"nextGlobalIdx")
  (the @"nextGlobalIdx") .= succ globalIdx

  symIdx <- use (the @"nextSymIdx")
  (the @"nextSymIdx") .= succ symIdx

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
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx (Just (Name name))) flags
  
  (the @"modl" . the @"globalSec") <>= [Global globalType expr]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"globals") <>= [(name, (globalIdx, symIdx))]

exportFunc :: String -> Construct ()
exportFunc name = do
  funcs <- use (the @"funcs")

  case lookup name funcs of
    Nothing ->
      error ("tried to export unknown function \"" ++ name ++ "\"")

    Just (funcIdx, _) ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportFunc funcIdx)]

exportTable :: String -> Construct ()
exportTable name = do
  tables <- use (the @"tables")

  case lookup name tables of
    Nothing ->
      error ("tried to export unknown table \"" ++ name ++ "\"")

    Just tableIdx ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportTable tableIdx)]

exportMem :: String -> Construct ()
exportMem name = do
  mems <- use (the @"mems")

  case lookup name mems of
    Nothing ->
      error ("tried to export unknown memory \"" ++ name ++ "\"")

    Just memIdx ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportMem memIdx)]

exportGlobal :: String -> Construct ()
exportGlobal name = do
  globals <- use (the @"globals")

  case lookup name globals of
    Nothing ->
      error ("tried to export unknown global \"" ++ name ++ "\"")

    Just (globalIdx, _) ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportGlobal globalIdx)]

declareExportFunc :: String -> [(String, ValType)] -> [ValType] -> Emit a -> Construct ()
declareExportFunc name inputs outputs emitter =
  declareFunc name inputs outputs emitter >> exportFunc name

declareExportTable :: String -> TableType -> Construct ()
declareExportTable name tableType =
  declareTable name tableType >> exportTable name

declareExportMem :: String -> MemType -> Construct ()
declareExportMem name memType =
  declareMem name memType >> exportMem name

declareExportGlobal :: String -> GlobalType -> Expr -> Construct ()
declareExportGlobal name globalType expr =
  declareGlobal name globalType expr >> exportGlobal name

setStart :: String -> Construct ()
setStart name = do
  funcs <- use (the @"funcs")

  case lookup name funcs of
    Nothing ->
      error ("tried to set unknown function \"" ++ name ++ "\" as start")

    Just (funcIdx, _) ->
      (the @"modl" . the @"startSec") .= Just funcIdx

commitFuncRefs :: Construct TableType
commitFuncRefs = do
  elemSec <- use (the @"modl" . the @"elemSec")
  funcRefs <- use (the @"funcRefs")

  unless (null elemSec && null funcRefs)
    (error "func refs have already been committed")

  funcs <- use (the @"funcs")

  (the @"modl" . the @"elemSec") .=
    [Elem (Expr [I32Const 1]) (Vec [funcIdx | (_, (funcIdx, _)) <- funcs])]

  (the @"funcRefs") .=
    [(name, (funcRef, symIdx)) | (name, (_, symIdx)) <- funcs | funcRef <- [1..]]
  
  let
    size = 1 + length funcs
    limits = Bounded (fromIntegral size) (fromIntegral size)
    tableType = TableType FuncRef limits

  return tableType

commitCodes :: Construct ()
commitCodes = do
  emitters <- use (the @"emitters")

  (the @"modl" . the @"codeSec") <~
    sequence [emit parameters emitter | (parameters, emitter) <- emitters]

addLocal :: String -> ValType -> Emit ()
addLocal name valType = do
  localIdx <- use (the @"nextLocalIdx")
  (the @"nextLocalIdx") .= succ localIdx

  (the @"locals") <>= [valType]
  (the @"variables") <>= [(name, localIdx)]

pushInstr :: Instr -> Emit ()
pushInstr instr =
  (the @"frames" . _head . _2) <>= [instr]

pushUnreachable :: Emit ()
pushUnreachable = pushInstr Unreachable

pushNop :: Emit ()
pushNop = pushInstr Nop

pushFrame :: Frame -> String -> Emit ()
pushFrame frame name = do
  (the @"frames") %= ((frame, []) :)

  (the @"labels" . mapped . _2) %= succ
  (the @"labels") %= ((name, 0) :)

popFrame :: Emit (Frame, [Instr])
popFrame = do
  frames <- use (the @"frames")
  (the @"frames") .= tail frames

  (the @"labels") %= tail
  (the @"labels" . mapped . _2) %= pred

  return (head frames)

getBlockType :: ([ValType], [ValType]) -> Emit BlockType
getBlockType = \case
  ([], []) ->
    return BlockEmpty
  
  ([], [valType]) ->
    return (BlockValType valType)
  
  (inputs, outputs) -> do
    typeIdx <- lift $ getType
      (FuncType (ResultType (Vec inputs)) (ResultType (Vec outputs)))

    return (BlockTypeIdx typeIdx)

pushBlock :: String -> [ValType] -> [ValType] -> Emit ()
pushBlock name inputs outputs = do
  blockType <- getBlockType (inputs, outputs)
  pushFrame (BlockFrame blockType) name

popBlock :: Emit ()
popBlock = popFrame >>= \case
  (BlockFrame blockType, instrs) -> pushInstr (Block blockType instrs)
  _ -> error "tried to pop something that was not a block"

pushLoop :: String -> [ValType] -> [ValType] -> Emit ()
pushLoop name inputs outputs = do
  blockType <- getBlockType (inputs, outputs)
  pushFrame (LoopFrame blockType) name

popLoop :: Emit ()
popLoop = popFrame >>= \case
  (LoopFrame blockType, instrs) -> pushInstr (Loop blockType instrs)
  _ -> error "tried to pop something that was not a loop"

getLabel :: String -> Emit LabelIdx
getLabel name = do
  labels <- use (the @"labels")

  case lookup name labels of
    Nothing -> error ("tried to get unknown label \"" ++ name ++ "\"")
    Just labelIdx -> return labelIdx

pushBr :: String -> Emit ()
pushBr name = pushInstr . Br =<< getLabel name

pushBrIf :: String -> Emit ()
pushBrIf name = pushInstr . BrIf =<< getLabel name

pushBrTable :: [String] -> String -> Emit ()
pushBrTable names name =
  pushInstr =<< BrTable <$> (Vec <$> mapM getLabel names) <*> getLabel name

pushReturn :: Emit ()
pushReturn = pushInstr Return

pushCall :: String -> Emit ()
pushCall name = do
  funcs <- lift $ use (the @"funcs")

  case lookup name funcs of
    Nothing -> error ("tried to call unknown function \"" ++ name ++ "\"")
    Just (funcIdx, symIdx) -> pushInstr (Call funcIdx symIdx)

pushCallIndirect :: [ValType] -> [ValType] -> Emit ()
pushCallIndirect inputs outputs = do
  typeIdx <- lift $ getType
    (FuncType (ResultType (Vec inputs)) (ResultType (Vec outputs)))
    
  pushInstr (CallIndirect typeIdx 0)

pushDrop :: Emit ()
pushDrop = pushInstr Drop

getVariable :: String -> Emit LocalIdx
getVariable name = do
  variables <- use (the @"variables")

  case lookup name variables of
    Nothing -> error ("tried to get unknown variable \"" ++ name ++ "\"")
    Just localIdx -> return localIdx

pushLocalGet :: String -> Emit ()
pushLocalGet name = pushInstr . LocalGet =<< getVariable name

pushLocalSet :: String -> Emit ()
pushLocalSet name = pushInstr . LocalSet =<< getVariable name

pushLocalTee :: String -> Emit ()
pushLocalTee name = pushInstr . LocalTee =<< getVariable name

pushAddLocalSet :: String -> ValType -> Emit ()
pushAddLocalSet name valType =
  addLocal name valType >> pushLocalSet name

pushAddLocalTee :: String -> ValType -> Emit ()
pushAddLocalTee name valType =
  addLocal name valType >> pushLocalTee name

getGlobal :: String -> Emit (GlobalIdx, SymIdx)
getGlobal name = do
  globals <- lift $ use (the @"globals")

  case lookup name globals of
    Nothing -> error ("tried to get unknown global \"" ++ name ++ "\"")
    Just (globalIdx, symIdx) -> return (globalIdx, symIdx)

pushGlobalGet :: String -> Emit ()
pushGlobalGet name =
  pushInstr . uncurry GlobalGet =<< getGlobal name

pushGlobalSet :: String -> Emit ()
pushGlobalSet name =
  pushInstr . uncurry GlobalSet =<< getGlobal name

pushI32Load :: MemArg -> Emit ()
pushI32Load memArg = pushInstr (I32Load memArg)

pushI64Load :: MemArg -> Emit ()
pushI64Load memArg = pushInstr (I64Load memArg)

pushF32Load :: MemArg -> Emit ()
pushF32Load memArg = pushInstr (F32Load memArg)

pushF64Load :: MemArg -> Emit ()
pushF64Load memArg = pushInstr (F64Load memArg)

pushI32Store :: MemArg -> Emit ()
pushI32Store memArg = pushInstr (I32Store memArg)

pushI64Store :: MemArg -> Emit ()
pushI64Store memArg = pushInstr (I64Store memArg)

pushF32Store :: MemArg -> Emit ()
pushF32Store memArg = pushInstr (F32Store memArg)

pushF64Store :: MemArg -> Emit ()
pushF64Store memArg = pushInstr (F64Store memArg)

pushI32Const :: Int32 -> Emit ()
pushI32Const value = pushInstr (I32Const value)

pushI64Const :: Int64 -> Emit ()
pushI64Const value = pushInstr (I64Const value)

pushF32Const :: Float -> Emit ()
pushF32Const value = pushInstr (F32Const value)

pushF64Const :: Double -> Emit ()
pushF64Const value = pushInstr (F64Const value)

pushI32FuncRef :: String -> Emit ()
pushI32FuncRef name = do
  funcRefs <- lift $ use (the @"funcRefs")

  case lookup name funcRefs of
    Nothing -> error ("tried to create func ref from unknown function \"" ++ name ++ "\"")
    Just (funcRef, symIdx) -> pushInstr (I32FuncRef funcRef symIdx)

pushI32Add :: Emit ()
pushI32Add = pushInstr I32Add

pushI32Sub :: Emit ()
pushI32Sub = pushInstr I32Sub

pushI64Add :: Emit ()
pushI64Add = pushInstr I64Add

pushI64Sub :: Emit ()
pushI64Sub = pushInstr I64Sub

pushF32Add :: Emit ()
pushF32Add = pushInstr F32Add

pushF32Sub :: Emit ()
pushF32Sub = pushInstr F32Sub

pushF64Add :: Emit ()
pushF64Add = pushInstr F64Add

pushF64Sub :: Emit ()
pushF64Sub = pushInstr F64Sub
