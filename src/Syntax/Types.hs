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
  deriving (Show, Eq)

data VecType =
  V128
  deriving (Show, Eq)

data RefType =
  FuncRef |
  ExternRef
  deriving (Show, Eq)

data ValType =
  ValNumType NumType |
  ValVecType VecType |
  ValRefType RefType
  deriving (Show, Eq)

data ResultType =
  ResultType (Vec ValType)
  deriving (Show, Eq)

data FuncType =
  FuncType ResultType ResultType
  deriving (Show, Eq)

data Limits =
  Unbounded Word32 |
  Bounded Word32 Word32
  deriving (Show, Eq)

data MemType =
  MemType Limits
  deriving (Show, Eq)

data TableType =
  TableType RefType Limits
  deriving (Show, Eq)

data Mut =
  Const |
  Var
  deriving (Show, Eq)

data GlobalType =
  GlobalType ValType Mut
  deriving (Show, Eq)
