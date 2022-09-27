module Syntax.Instructions
  ( BlockType (..)
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  )
  where

import Syntax.Conventions (TypeIdx, FuncIdx, TableIdx, GlobalIdx, LocalIdx, LabelIdx, SymIdx, Vec)
import Syntax.Types (ValType)
import Data.Word (Word32)
import Data.Int (Int32, Int64)

data BlockType =
  BlockEmpty |
  BlockValType ValType |
  BlockTypeIdx TypeIdx
  deriving (Show)

data MemArg =
  MemArg { alignment :: Word32, offset :: Word32 }
  deriving (Show)

data Instr =
  Unreachable |
  Nop |
  Block BlockType [Instr] |
  Loop BlockType [Instr] |
  Br LabelIdx |
  BrIf LabelIdx |
  BrTable (Vec LabelIdx) LabelIdx |
  Return |
  Call FuncIdx SymIdx |
  CallIndirect TypeIdx TableIdx |
  Drop |
  LocalGet LocalIdx |
  LocalSet LocalIdx |
  LocalTee LocalIdx |
  GlobalGet GlobalIdx SymIdx |
  GlobalSet GlobalIdx SymIdx |
  I32Load MemArg |
  I64Load MemArg |
  F32Load MemArg |
  F64Load MemArg |
  I32Store MemArg |
  I64Store MemArg |
  F32Store MemArg |
  F64Store MemArg |
  I32Const Int32 |
  I64Const Int64 |
  F32Const Float |
  F64Const Double |
  I32FuncRef Int32 SymIdx |
  I32DataRef Int32 SymIdx Int32 |
  I32Add |
  I32Sub |
  I64Add |
  I64Sub |
  F32Add |
  F32Sub |
  F64Add |
  F64Sub
  deriving (Show)

newtype Expr =
  Expr [Instr]
  deriving (Show)
