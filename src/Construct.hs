{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

module Construct
  ( MonadConstruct (..)
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
import Control.Monad (when)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.State (StateT, evalStateT, execStateT, get, put)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Int (Int32)

data Indexes = Indexes
  { nextTypeIdx :: TypeIdx
  , nextFuncIdx :: FuncIdx
  , nextTableIdx :: TableIdx
  , nextMemIdx :: MemIdx
  , nextGlobalIdx :: GlobalIdx
  , nextSymIdx :: SymIdx
  }
  deriving (Show)

emptyIndexes :: Indexes
emptyIndexes = Indexes
  { nextTypeIdx = 0
  , nextFuncIdx = 0
  , nextTableIdx = 0
  , nextMemIdx = 0
  , nextGlobalIdx = 0
  , nextSymIdx = 0
  }

data Mappings = Mappings
  { typeIdxs :: [(FuncType, TypeIdx)]
  , funcIdxs :: [(String, (FuncIdx, SymIdx))]
  , tableIdxs :: [(String, (TableIdx, SymIdx))]
  , memIdxs :: [(String, MemIdx)]
  , globalIdxs :: [(String, (GlobalIdx, SymIdx))]
  , funcRefs :: [(String, (Int32, SymIdx))]
  }
  deriving (Show)

emptyMappings :: Mappings
emptyMappings = Mappings
  { typeIdxs = []
  , funcIdxs = []
  , tableIdxs = []
  , memIdxs = []
  , globalIdxs = []
  , funcRefs = []
  }

class Monad m => MonadConstruct m where
  getModl :: m Module
  putModl :: Module -> m ()
  getIdxs :: m Indexes
  putIdxs :: Indexes -> m ()
  getMaps :: m Mappings
  putMaps :: Mappings -> m ()

getType :: MonadConstruct m => FuncType -> m TypeIdx
getType funcType = do
  modl@Module { typeSec } <- getModl
  idxs@Indexes { nextTypeIdx } <- getIdxs
  maps@Mappings { typeIdxs } <- getMaps

  case lookup funcType typeIdxs of
    Nothing -> do
      putModl modl { typeSec = typeSec ++ [funcType] }
      putIdxs idxs { nextTypeIdx = succ nextTypeIdx }
      putMaps maps { typeIdxs = typeIdxs ++ [(funcType, nextTypeIdx)] }
      return nextTypeIdx
    
    Just typeIdx ->
      return typeIdx

importFunc :: MonadConstruct m => String -> String -> [ValType] -> [ValType] -> m ()
importFunc namespace name inputType outputType = do
  typeIdx <- getType (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

  modl@Module { importSec, funcSec, linkingSec } <- getModl
  idxs@Indexes { nextFuncIdx, nextSymIdx } <- getIdxs
  maps@Mappings { funcIdxs } <- getMaps

  when (length funcSec > 0)
    (error "Can't import functions after having defined a function")
  
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
    
    info = SymInfo (SYMTAB_FUNCTION nextFuncIdx Nothing) flags
  
  putModl modl { importSec = importSec ++ [func], linkingSec = linkingSec ++ [info] }
  putIdxs idxs { nextFuncIdx = succ nextFuncIdx, nextSymIdx = succ nextSymIdx }
  putMaps maps { funcIdxs = funcIdxs ++ [(name, (nextFuncIdx, nextSymIdx))] }

importTable :: MonadConstruct m => String -> String -> RefType -> Limits -> m ()
importTable namespace name refType limits = do
  modl@Module { importSec, tableSec, linkingSec } <- getModl
  idxs@Indexes { nextTableIdx, nextSymIdx } <- getIdxs
  maps@Mappings { tableIdxs } <- getMaps

  when (length tableSec > 0)
    (error "can't import tables after having declared a table")
  
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
    
    info = SymInfo (SYMTAB_TABLE nextTableIdx Nothing) flags

  putModl modl { importSec = importSec ++ [table], linkingSec = linkingSec ++ [info] }
  putIdxs idxs { nextTableIdx = succ nextTableIdx, nextSymIdx = succ nextSymIdx }
  putMaps maps { tableIdxs = tableIdxs ++ [(name, (nextTableIdx, nextSymIdx))] }

importMem :: MonadConstruct m => String -> String -> Limits -> m ()
importMem namespace name limits = do
  modl@Module { importSec, memSec } <- getModl
  idxs@Indexes { nextMemIdx } <- getIdxs
  maps@Mappings { memIdxs } <- getMaps

  when (length memSec > 0)
    (error "can't import memories after having declared a memory")
  
  let mem = Import (Name namespace) (Name name) (ImportMem (MemType limits))

  putModl modl { importSec = importSec ++ [mem] }
  putIdxs idxs { nextMemIdx = succ nextMemIdx }
  putMaps maps { memIdxs = memIdxs ++ [(name, nextMemIdx)] }

importGlobal :: MonadConstruct m => String -> String -> ValType -> Mut -> m ()
importGlobal namespace name valType mut = do
  modl@Module { importSec, globalSec, linkingSec } <- getModl
  idxs@Indexes { nextGlobalIdx, nextSymIdx } <- getIdxs
  maps@Mappings { globalIdxs } <- getMaps

  when (length globalSec > 0)
    (error "can't import globals after having declared a global")
  
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
    
    info = SymInfo (SYMTAB_GLOBAL nextGlobalIdx Nothing) flags
  
  putModl modl { importSec = importSec ++ [global], linkingSec = linkingSec ++ [info] }
  putIdxs idxs { nextGlobalIdx = succ nextGlobalIdx, nextSymIdx = succ nextSymIdx }
  putMaps maps { globalIdxs = globalIdxs ++ [(name, (nextGlobalIdx, nextSymIdx))] }

declareFunc :: MonadConstruct m => String -> [ValType] -> [ValType] -> m ()
declareFunc name inputType outputType = do
  typeIdx <- getType (FuncType (ResultType (Vec inputType)) (ResultType (Vec outputType)))

  modl@Module { funcSec, linkingSec } <- getModl
  idxs@Indexes { nextFuncIdx, nextSymIdx } <- getIdxs
  maps@Mappings { funcIdxs } <- getMaps

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

    info = SymInfo (SYMTAB_FUNCTION nextFuncIdx (Just (Name name))) flags
  
  putModl modl { funcSec = funcSec ++ [typeIdx], linkingSec = linkingSec ++ [info] }
  putIdxs idxs { nextFuncIdx = succ nextFuncIdx, nextSymIdx = succ nextSymIdx }
  putMaps maps { funcIdxs = funcIdxs ++ [(name, (nextFuncIdx, nextSymIdx))] }

declareTable :: MonadConstruct m => String -> RefType -> Limits -> m ()
declareTable name refType limits = do
  modl@Module { tableSec, linkingSec } <- getModl
  idxs@Indexes { nextTableIdx, nextSymIdx } <- getIdxs
  maps@Mappings { tableIdxs } <- getMaps

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
    
    info = SymInfo (SYMTAB_TABLE nextTableIdx (Just (Name name))) flags
  
  putModl modl { tableSec = tableSec ++ [table], linkingSec = linkingSec ++ [info] }
  putIdxs idxs { nextTableIdx = succ nextTableIdx, nextSymIdx = succ nextSymIdx }
  putMaps maps { tableIdxs = tableIdxs ++ [(name, (nextTableIdx, nextSymIdx))] }

declareMem :: MonadConstruct m => String -> Limits -> m ()
declareMem name limits = do
  modl@Module { memSec } <- getModl
  idxs@Indexes { nextMemIdx } <- getIdxs
  maps@Mappings { memIdxs } <- getMaps

  let mem = Mem (MemType limits)

  putModl modl { memSec = memSec ++ [mem] }
  putIdxs idxs { nextMemIdx = succ nextMemIdx }
  putMaps maps { memIdxs = memIdxs ++ [(name, nextMemIdx)] }

declareGlobal :: MonadConstruct m => String -> ValType -> Mut -> Expr -> m ()
declareGlobal name valType mut expr = do
  modl@Module { globalSec, linkingSec } <- getModl
  idxs@Indexes { nextGlobalIdx, nextSymIdx } <- getIdxs
  maps@Mappings { globalIdxs } <- getMaps

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
    
    info = SymInfo (SYMTAB_GLOBAL nextGlobalIdx (Just (Name name))) flags
  
  putModl modl { globalSec = globalSec ++ [global], linkingSec = linkingSec ++ [info] }
  putIdxs idxs { nextGlobalIdx = succ nextGlobalIdx, nextSymIdx = succ nextSymIdx }
  putMaps maps { globalIdxs = globalIdxs ++ [(name, (nextGlobalIdx, nextSymIdx))] }

exportFunc :: MonadConstruct m => String -> m ()
exportFunc name = do
  modl@Module { exportSec } <- getModl
  Mappings { funcIdxs } <- getMaps

  case lookup name funcIdxs of
    Nothing -> error ("tried to export unknown function \"" ++ name ++ "\"")
    Just (funcIdx, _) -> putModl modl { exportSec = exportSec ++ [Export (Name name) (ExportFunc funcIdx)] }

exportTable :: MonadConstruct m => String -> m ()
exportTable name = do
  modl@Module { exportSec } <- getModl
  Mappings { tableIdxs } <- getMaps

  case lookup name tableIdxs of
    Nothing -> error ("tried to export unknown table \"" ++ name ++ "\"")
    Just (tableIdx, _) -> putModl modl { exportSec = exportSec ++ [Export (Name name) (ExportTable tableIdx)] }

exportMem :: MonadConstruct m => String -> m ()
exportMem name = do
  modl@Module { exportSec } <- getModl
  Mappings { memIdxs } <- getMaps

  case lookup name memIdxs of
    Nothing -> error ("tried to export unknown memory \"" ++ name ++ "\"")
    Just memIdx -> putModl modl { exportSec = exportSec ++ [Export (Name name) (ExportMem memIdx)] }

exportGlobal :: MonadConstruct m => String -> m ()
exportGlobal name = do
  modl@Module { exportSec } <- getModl
  Mappings { globalIdxs } <- getMaps

  case lookup name globalIdxs of
    Nothing -> error ("tried to export unknown global \"" ++ name ++ "\"")
    Just (globalIdx, _) -> putModl modl { exportSec = exportSec ++ [Export (Name name) (ExportGlobal globalIdx)] }

setStart :: MonadConstruct m => String -> m ()
setStart name = do
  modl <- getModl
  Mappings { funcIdxs } <- getMaps

  case lookup name funcIdxs of
    Nothing -> error ("tried to set unknown function \"" ++ name ++ "\" as start")
    Just (funcIdx, _) -> putModl modl { startSec = Just funcIdx }

commitFuncRefs :: MonadConstruct m => m ()
commitFuncRefs = do
  modl@Module { elemSec } <- getModl
  maps@Mappings { funcIdxs, funcRefs } <- getMaps

  when (length elemSec > 0) (error "elem already committed")
  when (length funcRefs > 0) (error "funcrefs already committed")

  let
    funcRefElem =
      Elem (Expr [I32Const 1]) (Vec [funcIdx | (_, (funcIdx, _)) <- funcIdxs])

    funcRefMapping =
      [(name, (funcRef, symIdx)) | (name, (_, symIdx)) <- funcIdxs | funcRef <- [1..]]

  putModl modl { elemSec = [funcRefElem] }
  putMaps maps { funcRefs = funcRefMapping }

newtype ConstructT m a =
  ConstructT (StateT Mappings (StateT Indexes (StateT Module m)) a)
  deriving (Functor, Applicative, Monad)

runConstructT :: Monad m => ConstructT m a -> m Module
runConstructT (ConstructT action) =
  execStateT (evalStateT (evalStateT action emptyMappings) emptyIndexes) emptyModule

instance MonadTrans ConstructT where
  lift action = ConstructT $ lift $ lift $ lift action

instance Monad m => MonadConstruct (ConstructT m) where
  getModl = ConstructT $ lift $ lift $ get
  putModl modl = ConstructT $ lift $ lift $ put modl
  getIdxs = ConstructT $ lift $ get
  putIdxs idxs = ConstructT $ lift $ put idxs
  getMaps = ConstructT $ get
  putMaps maps = ConstructT $ put maps

type Construct = ConstructT Identity

runConstruct :: Construct a -> Module
runConstruct action = runIdentity (runConstructT action)
