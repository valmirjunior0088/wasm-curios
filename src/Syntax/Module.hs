{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Module
  ( Module (..)
  , empty
  , MonadConstruct (..)
  , putType
  , putFuncImport
  , putTableImport
  , putMemImport
  , putGlobalImport
  , putFunc
  , putTable
  , putMem
  , putGlobal
  , putFuncExport
  , putTableExport
  , putMemExport
  , putGlobalExport
  , setStart
  , putElem
  , putCode
  , putData
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
  , DataIdx
  , SymIdx
  , Name
  , Vec
  )

import Syntax.Types (FuncType, TableType, MemType, GlobalType)
import Syntax.Instructions (Expr)

import Syntax.Sections
  ( ImportDesc (..)
  , Import (..)
  , Table (..)
  , Mem (..)
  , Global (..)
  , ExportDesc (..)
  , Export (..)
  , Start (..)
  , Elem (..)
  , Code (..)
  , Data (..)
  )

import Syntax.LLVM
  ( SymType (..)
  , SymFlags (..)
  , SymInfo (..)
  , RelocEntry (..)
  )

import Data.Word (Word8)
import Control.Monad (when)
import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import Control.Monad.Identity (Identity, runIdentity)

data Module = Module
  { typeSec :: [FuncType]
  , importSec :: [Import]
  , funcSec :: [TypeIdx]
  , tableSec :: [Table]
  , memSec :: [Mem]
  , globalSec :: [Global]
  , exportSec :: [Export]
  , startSec :: Maybe Start
  , elemSec :: [Elem]
  , codeSec :: [Code]
  , dataSec :: [Data]
  , linkingSec :: [SymInfo]
  , nextTypeIdx :: TypeIdx
  , nextFuncIdx :: FuncIdx
  , nextTableIdx :: TableIdx
  , nextMemIdx :: MemIdx
  , nextGlobalIdx :: GlobalIdx
  , nextDataIdx :: DataIdx
  , nextSymIdx :: SymIdx
  }
  deriving (Show)

empty :: Module
empty = Module
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
  , nextTypeIdx = 0
  , nextFuncIdx = 0
  , nextTableIdx = 0
  , nextMemIdx = 0
  , nextGlobalIdx = 0
  , nextDataIdx = 0
  , nextSymIdx = 0
  }

class Monad m => MonadConstruct m where
  getModule :: m Module
  putModule :: Module -> m ()

putType :: MonadConstruct m => FuncType -> m TypeIdx
putType funcType = do
  state@Module
    { typeSec
    , nextTypeIdx
    }
    <- getModule

  putModule state
    { typeSec = typeSec ++ [funcType]
    , nextTypeIdx = succ nextTypeIdx
    }

  return nextTypeIdx

putFuncImport :: MonadConstruct m => Name -> Name -> TypeIdx -> m (FuncIdx, SymIdx)
putFuncImport namespace name typeIdx = do
  state@Module
    { importSec
    , funcSec
    , linkingSec
    , nextFuncIdx
    , nextSymIdx
    }
    <- getModule
  
  when (length funcSec > 0)
    (error "can't import functions after having declared a function")
  
  let
    item = Import namespace name (ImportFunc typeIdx)

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

  putModule state
    { importSec = importSec ++ [item]
    , linkingSec = linkingSec ++ [info]
    , nextFuncIdx = succ nextFuncIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextFuncIdx, nextSymIdx)

putTableImport :: MonadConstruct m => Name -> Name -> TableType -> m (TableIdx, SymIdx)
putTableImport namespace name tableType = do
  state@Module
    { importSec
    , tableSec
    , linkingSec
    , nextTableIdx
    , nextSymIdx
    }
    <- getModule
  
  when (length tableSec > 0)
    (error "can't import tables after having declared a table")
  
  let
    item = Import namespace name (ImportTable tableType)

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
  
  putModule state
    { importSec = importSec ++ [item]
    , linkingSec = linkingSec ++ [info]
    , nextTableIdx = succ nextTableIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextTableIdx, nextSymIdx)

putMemImport :: MonadConstruct m => Name -> Name -> MemType -> m MemIdx
putMemImport namespace name memType = do
  state@Module
    { importSec
    , memSec
    , nextMemIdx
    }
    <- getModule

  when (length memSec > 0)
    (error "can't import memories after having declared a memory")
  
  let
    item = Import namespace name (ImportMem memType)
  
  putModule state
    { importSec = importSec ++ [item]
    , nextMemIdx = succ nextMemIdx
    }
  
  return nextMemIdx

putGlobalImport :: MonadConstruct m => Name -> Name -> GlobalType -> m (GlobalIdx, SymIdx)
putGlobalImport namespace name globalType = do
  state@Module
    { importSec
    , globalSec
    , linkingSec
    , nextGlobalIdx
    , nextSymIdx
    }
    <- getModule
  
  when (length globalSec > 0)
    (error "can't import globals after having declared a global")
  
  let
    item = Import namespace name (ImportGlobal globalType)

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
  
  putModule state
    { importSec = importSec ++ [item]
    , linkingSec = linkingSec ++ [info]
    , nextGlobalIdx = succ nextGlobalIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextGlobalIdx, nextSymIdx)

putFunc :: MonadConstruct m => Name -> TypeIdx -> m (FuncIdx, SymIdx)
putFunc name typeIdx = do
  state@Module
    { funcSec
    , linkingSec
    , nextFuncIdx
    , nextSymIdx
    }
    <- getModule

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

    info = SymInfo (SYMTAB_FUNCTION nextFuncIdx (Just name)) flags
  
  putModule state
    { funcSec = funcSec ++ [typeIdx]
    , linkingSec = linkingSec ++ [info]
    , nextFuncIdx = succ nextFuncIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextFuncIdx, nextSymIdx)

putTable :: MonadConstruct m => Name -> TableType -> m (TableIdx, SymIdx)
putTable name tableType = do
  state@Module
    { tableSec
    , linkingSec
    , nextTableIdx
    , nextSymIdx
    }
    <- getModule
  
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
    
    info = SymInfo (SYMTAB_TABLE nextTableIdx (Just name)) flags
  
  putModule state
    { tableSec = tableSec ++ [Table tableType]
    , linkingSec = linkingSec ++ [info]
    , nextTableIdx = succ nextTableIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextTableIdx, nextSymIdx)

putMem :: MonadConstruct m => MemType -> m MemIdx
putMem memType = do
  state@Module
    { memSec
    , nextMemIdx
    }
    <- getModule
  
  putModule state
    { memSec = memSec ++ [Mem memType]
    , nextMemIdx = succ nextMemIdx
    }
  
  return nextMemIdx

putGlobal :: MonadConstruct m => Name -> GlobalType -> Expr -> m (GlobalIdx, SymIdx)
putGlobal name globalType expr = do
  state@Module
    { globalSec
    , linkingSec
    , nextGlobalIdx
    , nextSymIdx
    }
    <- getModule
  
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
    
    info = SymInfo (SYMTAB_GLOBAL nextGlobalIdx (Just name)) flags
  
  putModule state
    { globalSec = globalSec ++ [Global globalType expr]
    , linkingSec = linkingSec ++ [info]
    , nextGlobalIdx = succ nextGlobalIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextGlobalIdx, nextSymIdx)

putFuncExport :: MonadConstruct m => Name -> FuncIdx -> m ()
putFuncExport name funcIdx = do
  state@Module { exportSec } <- getModule
  putModule state { exportSec = exportSec ++ [Export name (ExportFunc funcIdx)] }

putTableExport :: MonadConstruct m => Name -> TableIdx -> m ()
putTableExport name tableIdx = do
  state@Module { exportSec } <- getModule
  putModule state { exportSec = exportSec ++ [Export name (ExportTable tableIdx)] }

putMemExport :: MonadConstruct m => Name -> MemIdx -> m ()
putMemExport name memIdx = do
  state@Module { exportSec } <- getModule
  putModule state { exportSec = exportSec ++ [Export name (ExportMem memIdx)] }

putGlobalExport :: MonadConstruct m => Name -> GlobalIdx -> m ()
putGlobalExport name globalIdx = do
  state@Module { exportSec } <- getModule
  putModule state { exportSec = exportSec ++ [Export name (ExportGlobal globalIdx)] }

setStart :: MonadConstruct m => FuncIdx -> m ()
setStart funcIdx = do
  state <- getModule
  putModule state { startSec = Just (Start funcIdx) }

putElem :: MonadConstruct m => Expr -> Vec FuncIdx -> m ()
putElem expr funcIdxs = do
  state@Module { elemSec } <- getModule
  putModule state { elemSec = elemSec ++ [Elem expr funcIdxs] }

putCode :: MonadConstruct m => Code -> m ()
putCode code = do
  state@Module { codeSec } <- getModule
  putModule state { codeSec = codeSec ++ [code] }

putData :: MonadConstruct m => Name -> Expr -> Vec Word8 -> [RelocEntry] -> m (DataIdx, SymIdx)
putData name expr bytes entries = do
  state@Module
    { dataSec
    , linkingSec
    , nextDataIdx
    , nextSymIdx
    }
    <- getModule
  
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
    
    size = fromIntegral (length bytes)
    info = SymInfo (SYMTAB_DATA name nextDataIdx 0 size) flags
  
  putModule state
    { dataSec = dataSec ++ [Data expr bytes entries]
    , linkingSec = linkingSec ++ [info]
    , nextDataIdx = succ nextDataIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextDataIdx, nextSymIdx)

newtype ConstructT m a =
  ConstructT (StateT Module m a)
  deriving (Functor, Applicative, Monad, MonadState Module)

instance Monad m => MonadConstruct (ConstructT m) where
  getModule = get
  putModule = put

runConstructT :: Monad m => ConstructT m a -> m Module
runConstructT (ConstructT action) = execStateT action empty

type Construct = ConstructT Identity

runConstruct :: Construct a -> Module
runConstruct action = runIdentity (runConstructT action)
