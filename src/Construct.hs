{-# LANGUAGE ParallelListComp #-}

module Construct
  ( MonadConstruct
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
  , ConstructT
  , runConstructT
  , Construct
  , runConstruct
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
import Util (push)
import Control.Monad (unless)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (.=), (%=))

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

class MonadState ConstructState m => MonadConstruct m

getType :: MonadConstruct m => FuncType -> m TypeIdx
getType funcType = do
  typeIdxs <- use (the @"modlState" . the @"typeIdxs")

  case lookup funcType typeIdxs of
    Nothing -> do
      typeIdx <- use (the @"modlState" . the @"nextTypeIdx")
      (the @"modlState" . the @"nextTypeIdx") .= succ typeIdx

      (the @"modl" . the @"typeSec") %= push funcType
      (the @"modlState" . the @"typeIdxs") %= push (funcType, typeIdx)

      return typeIdx
    
    Just typeIdx ->
      return typeIdx

importFunc :: MonadConstruct m => String -> String -> [ValType] -> [ValType] -> m ()
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
  
  (the @"modl" . the @"importSec") %= push func
  (the @"modl" . the @"linkingSec") %= push info
  (the @"modlState" . the @"funcIdxs") %= push (name, (funcIdx, symIdx))

importTable :: MonadConstruct m => String -> String -> RefType -> Limits -> m ()
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
  
  (the @"modl" . the @"importSec") %= push table
  (the @"modl" . the @"linkingSec") %= push info
  (the @"modlState" . the @"tableIdxs") %= push (name, (tableIdx, symIdx))

importMem :: MonadConstruct m => String -> String -> Limits -> m ()
importMem namespace name limits = do
  memSec <- use (the @"modl" . the @"memSec")

  unless (null memSec)
    (error "cannot import a mem after having declared a mem")

  memIdx <- use (the @"modlState" . the @"nextMemIdx")
  (the @"modlState" . the @"nextMemIdx") .= succ memIdx

  let mem = Import (Name namespace) (Name name) (ImportMem (MemType limits))
  
  (the @"modl" . the @"importSec") %= push mem
  (the @"modlState" . the @"memIdxs") %= push (name, memIdx)

importGlobal :: MonadConstruct m => String -> String -> ValType -> Mut -> m ()
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
  
  (the @"modl" . the @"importSec") %= push global
  (the @"modl" . the @"linkingSec") %= push info
  (the @"modlState" . the @"globalIdxs") %= push (name, (globalIdx, symIdx))

declareFunc :: MonadConstruct m => String -> [ValType] -> [ValType] -> m ()
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
      , wasm_sym_explicit_name = False
      , wasm_sym_no_strip = False
      }

    info = SymInfo (SYMTAB_FUNCTION funcIdx (Just (Name name))) flags
  
  (the @"modl" . the @"funcSec") %= push typeIdx
  (the @"modl" . the @"linkingSec") %= push info
  (the @"modlState" . the @"funcIdxs") %= push (name, (funcIdx, symIdx))

declareTable :: MonadConstruct m => String -> RefType -> Limits -> m ()
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
      , wasm_sym_explicit_name = False
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_TABLE tableIdx (Just (Name name))) flags
  
  (the @"modl" . the @"tableSec") %= push table
  (the @"modl" . the @"linkingSec") %= push info
  (the @"modlState" . the @"tableIdxs") %= push (name, (tableIdx, symIdx))

declareMem :: MonadConstruct m => String -> Limits -> m ()
declareMem name limits = do
  memIdx <- use (the @"modlState" . the @"nextMemIdx")
  (the @"modlState" . the @"nextMemIdx") .= succ memIdx

  let mem = Mem (MemType limits)

  (the @"modl" . the @"memSec") %= push mem
  (the @"modlState" . the @"memIdxs") %= push (name, memIdx)

declareGlobal :: MonadConstruct m => String -> ValType -> Mut -> Expr -> m ()
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
      , wasm_sym_explicit_name = False
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx (Just (Name name))) flags
  
  (the @"modl" . the @"globalSec") %= push global
  (the @"modl" . the @"linkingSec") %= push info
  (the @"modlState" . the @"globalIdxs") %= push (name, (globalIdx, symIdx))

exportFunc :: MonadConstruct m => String -> m ()
exportFunc name = do
  funcIdxs <- use (the @"modlState" . the @"funcIdxs")

  case lookup name funcIdxs of
    Nothing ->
      error ("tried to export unknown function \"" ++ name ++ "\"")

    Just (funcIdx, _) ->
      (the @"modl" . the @"exportSec") %= push (Export (Name name) (ExportFunc funcIdx))

exportTable :: MonadConstruct m => String -> m ()
exportTable name = do
  tableIdxs <- use (the @"modlState" . the @"tableIdxs")

  case lookup name tableIdxs of
    Nothing ->
      error ("tried to export unknown table \"" ++ name ++ "\"")

    Just (tableIdx, _) ->
      (the @"modl" . the @"exportSec") %= push (Export (Name name) (ExportTable tableIdx))

exportMem :: MonadConstruct m => String -> m ()
exportMem name = do
  memIdxs <- use (the @"modlState" . the @"memIdxs")

  case lookup name memIdxs of
    Nothing ->
      error ("tried to export unknown memory \"" ++ name ++ "\"")

    Just memIdx ->
      (the @"modl" . the @"exportSec") %= push (Export (Name name) (ExportMem memIdx))

exportGlobal :: MonadConstruct m => String -> m ()
exportGlobal name = do
  globalIdxs <- use (the @"modlState" . the @"globalIdxs")

  case lookup name globalIdxs of
    Nothing ->
      error ("tried to export unknown global \"" ++ name ++ "\"")

    Just (globalIdx, _) ->
      (the @"modl" . the @"exportSec") %= push (Export (Name name) (ExportGlobal globalIdx))

setStart :: MonadConstruct m => String -> m ()
setStart name = do
  funcIdxs <- use (the @"modlState" . the @"funcIdxs")

  case lookup name funcIdxs of
    Nothing ->
      error ("tried to set unknown function \"" ++ name ++ "\" as start")

    Just (funcIdx, _) ->
      (the @"modl" . the @"startSec") .= Just funcIdx

commitFuncRefs :: MonadConstruct m => m ()
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

newtype ConstructT m a =
  ConstructT (StateT ConstructState m a)
  deriving (Functor, Applicative, Monad, MonadState ConstructState)

instance Monad m => MonadConstruct (ConstructT m)

runConstructT :: Monad m => ConstructT m a -> m Module
runConstructT (ConstructT action) =
  evalStateT (action >> use (the @"modl")) emptyState

type Construct = ConstructT Identity

runConstruct :: Construct a -> Module
runConstruct action = runIdentity (runConstructT action)
