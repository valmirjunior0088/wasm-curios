module Syntax.Types
  ( NumType (..)
  , VecType (..)
  , RefType (..)
  , ValType (..)
  , ResultType (..)
  , FuncType (..)
  , Limits (..)
  , MemType (..)
  , TableType (..)
  , Mut (..)
  , GlobalType (..)
  )
  where

import Syntax.Conventions (Vec)
import Data.Word (Word32)

data NumType =
  I32 |
  I64 |
  F32 |
  F64
  deriving (Show)

data VecType =
  V128
  deriving (Show)

data RefType =
  FuncRef |
  ExternRef
  deriving (Show)

data ValType =
  ValNumType NumType |
  ValVecType VecType |
  ValRefType RefType
  deriving (Show)

data ResultType =
  ResultType (Vec ValType)
  deriving (Show)

data FuncType =
  FuncType ResultType ResultType
  deriving (Show)

data Limits =
  Unbounded Word32 |
  Bounded Word32 Word32
  deriving (Show)

data MemType =
  MemType Limits
  deriving (Show)

data TableType =
  TableType RefType Limits
  deriving (Show)

data Mut =
  Const |
  Var
  deriving (Show)

data GlobalType =
  GlobalType ValType Mut
  deriving (Show)
