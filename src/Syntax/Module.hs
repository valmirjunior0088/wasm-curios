{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Module
  ( Module (..)
  , Construct (..)
  , construct
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
import Control.Monad.State (MonadState, State, execState, get, put)

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

newtype Construct a =
  Construct (State Module a)
  deriving (Functor, Applicative, Monad, MonadState Module)

construct :: Construct a -> Module
construct (Construct action) = execState action empty

putType :: FuncType -> Construct TypeIdx
putType funcType = do
  state@Module
    { typeSec
    , nextTypeIdx
    }
    <- get

  put state
    { typeSec = typeSec ++ [funcType]
    , nextTypeIdx = succ nextTypeIdx
    }

  return nextTypeIdx

putFuncImport :: Name -> Name -> TypeIdx -> Construct (FuncIdx, SymIdx)
putFuncImport namespace name typeIdx = do
  state@Module
    { importSec
    , funcSec
    , linkingSec
    , nextFuncIdx
    , nextSymIdx
    }
    <- get
  
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

  put state
    { importSec = importSec ++ [item]
    , linkingSec = linkingSec ++ [info]
    , nextFuncIdx = succ nextFuncIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextFuncIdx, nextSymIdx)

putTableImport :: Name -> Name -> TableType -> Construct (TableIdx, SymIdx)
putTableImport namespace name tableType = do
  state@Module
    { importSec
    , tableSec
    , linkingSec
    , nextTableIdx
    , nextSymIdx
    }
    <- get
  
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
  
  put state
    { importSec = importSec ++ [item]
    , linkingSec = linkingSec ++ [info]
    , nextTableIdx = succ nextTableIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextTableIdx, nextSymIdx)

putMemImport :: Name -> Name -> MemType -> Construct MemIdx
putMemImport namespace name memType = do
  state@Module
    { importSec
    , memSec
    , nextMemIdx
    }
    <- get
  
  when (length memSec > 0)
    (error "can't import memories after having declared a memory")
  
  let
    item = Import namespace name (ImportMem memType)
  
  put state
    { importSec = importSec ++ [item]
    , nextMemIdx = succ nextMemIdx
    }
  
  return nextMemIdx

putGlobalImport :: Name -> Name -> GlobalType -> Construct (GlobalIdx, SymIdx)
putGlobalImport namespace name globalType = do
  state@Module
    { importSec
    , globalSec
    , linkingSec
    , nextGlobalIdx
    , nextSymIdx
    }
    <- get
  
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
  
  put state
    { importSec = importSec ++ [item]
    , linkingSec = linkingSec ++ [info]
    , nextGlobalIdx = succ nextGlobalIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextGlobalIdx, nextSymIdx)

putFunc :: Name -> TypeIdx -> Construct (FuncIdx, SymIdx)
putFunc name typeIdx = do
  state@Module
    { funcSec
    , linkingSec
    , nextFuncIdx
    , nextSymIdx
    }
    <- get

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
  
  put state
    { funcSec = funcSec ++ [typeIdx]
    , linkingSec = linkingSec ++ [info]
    , nextFuncIdx = succ nextFuncIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextFuncIdx, nextSymIdx)

putTable :: Name -> TableType -> Construct (TableIdx, SymIdx)
putTable name tableType = do
  state@Module
    { tableSec
    , linkingSec
    , nextTableIdx
    , nextSymIdx
    }
    <- get
  
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
  
  put state
    { tableSec = tableSec ++ [Table tableType]
    , linkingSec = linkingSec ++ [info]
    , nextTableIdx = succ nextTableIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextTableIdx, nextSymIdx)

putMem :: MemType -> Construct MemIdx
putMem memType = do
  state@Module
    { memSec
    , nextMemIdx
    }
    <- get
  
  put state
    { memSec = memSec ++ [Mem memType]
    , nextMemIdx = succ nextMemIdx
    }
  
  return nextMemIdx

putGlobal :: Name -> GlobalType -> Expr -> Construct (GlobalIdx, SymIdx)
putGlobal name globalType expr = do
  state@Module
    { globalSec
    , linkingSec
    , nextGlobalIdx
    , nextSymIdx
    }
    <- get
  
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
  
  put state
    { globalSec = globalSec ++ [Global globalType expr]
    , linkingSec = linkingSec ++ [info]
    , nextGlobalIdx = succ nextGlobalIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextGlobalIdx, nextSymIdx)

putFuncExport :: Name -> FuncIdx -> Construct ()
putFuncExport name funcIdx = do
  state@Module { exportSec } <- get
  put state { exportSec = exportSec ++ [Export name (ExportFunc funcIdx)] }

putTableExport :: Name -> TableIdx -> Construct ()
putTableExport name tableIdx = do
  state@Module { exportSec } <- get
  put state { exportSec = exportSec ++ [Export name (ExportTable tableIdx)] }

putMemExport :: Name -> MemIdx -> Construct ()
putMemExport name memIdx = do
  state@Module { exportSec } <- get
  put state { exportSec = exportSec ++ [Export name (ExportMem memIdx)] }

putGlobalExport :: Name -> GlobalIdx -> Construct ()
putGlobalExport name globalIdx = do
  state@Module { exportSec } <- get
  put state { exportSec = exportSec ++ [Export name (ExportGlobal globalIdx)] }

setStart :: FuncIdx -> Construct ()
setStart funcIdx = do
  state <- get
  put state { startSec = Just (Start funcIdx) }

putElem :: Expr -> Vec FuncIdx -> Construct ()
putElem expr funcIdxs = do
  state@Module { elemSec } <- get
  put state { elemSec = elemSec ++ [Elem expr funcIdxs] }

putCode :: Code -> Construct ()
putCode code = do
  state@Module { codeSec } <- get
  put state { codeSec = codeSec ++ [code] }

putData :: Name -> Expr -> Vec Word8 -> [RelocEntry] -> Construct (DataIdx, SymIdx)
putData name expr bytes entries = do
  state@Module
    { dataSec
    , linkingSec
    , nextDataIdx
    , nextSymIdx
    }
    <- get
  
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
  
  put state
    { dataSec = dataSec ++ [Data expr bytes entries]
    , linkingSec = linkingSec ++ [info]
    , nextDataIdx = succ nextDataIdx
    , nextSymIdx = succ nextSymIdx
    }
  
  return (nextDataIdx, nextSymIdx)
