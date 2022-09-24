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
  )
  where

import Syntax.Conventions
  ( TypeIdx
  , FuncIdx
  , TableIdx
  , MemIdx
  , GlobalIdx
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
  , Module (..)
  , emptyModule
  )

import Syntax.Instructions (Instr (..), Expr (..))
import Syntax.LLVM (SymType (..), SymFlags (..), SymInfo (..))
import Control.Monad (unless)
import Control.Monad.State (State, evalState)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (.=), (<>=))

data ModlState = ModlState
  { nextTypeIdx :: TypeIdx
  , nextFuncIdx :: FuncIdx
  , nextTableIdx :: TableIdx
  , nextMemIdx :: MemIdx
  , nextGlobalIdx :: GlobalIdx
  , nextSymIdx :: SymIdx

  , typeIdxs :: [(FuncType, TypeIdx)]
  , funcIdxs :: [(String, (FuncIdx, SymIdx))]
  , tableIdxs :: [(String, (TableIdx, SymIdx))]
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

data ConstructState = ConstructState
  { modl :: Module
  , modlState :: ModlState
  }
  deriving (Show, Generic)

emptyState :: ConstructState
emptyState = ConstructState 
  { modl = emptyModule
  , modlState = emptyModlState
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

  typeIdx <- getType (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

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
      , wasm_sym_explicit_name = False
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_FUNCTION funcIdx Nothing) flags
  
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

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    table = Import (Name namespace) (Name name) (ImportTable (TableType refType limits))

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = True
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = False
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_TABLE tableIdx Nothing) flags
  
  (the @"modl" . the @"importSec") <>= [table]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"tableIdxs") <>= [(name, (tableIdx, symIdx))]

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
      , wasm_sym_explicit_name = False
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx Nothing) flags
  
  (the @"modl" . the @"importSec") <>= [global]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"globalIdxs") <>= [(name, (globalIdx, symIdx))]

declareFunc :: String -> [ValType] -> [ValType] -> Construct ()
declareFunc name inputType outputType = do
  typeIdx <- getType (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

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

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    table = Table (TableType refType limits)

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = False
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_TABLE tableIdx (Just (Name name))) flags
  
  (the @"modl" . the @"tableSec") <>= [table]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"tableIdxs") <>= [(name, (tableIdx, symIdx))]

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

    Just (tableIdx, _) ->
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
