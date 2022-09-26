import Syntax.Types
import Serialize
import Construct

main :: IO ()
main = writeModule "./test.wasm" $ runConstruct $ do
  importTable "env" "__indirect_function_table" FuncRef (Unbounded 0)
  importMem "env" "__linear_memory" (Unbounded 1)
  importFunc "env" "alloc" [ValNumType I32] [ValNumType I32]
  importFunc "env" "dealloc" [ValNumType I32] []
  importGlobal "env" "__stack_pointer" (ValNumType I32) Var

  declareFunc "jump_table_test" [ValNumType I32] [ValNumType I32]
  declareFunc "call_indirect_test" [] [ValNumType I32]
  
  commitFuncRefs

  addParameter "value"
  pushLocalGet "value"
  pushFrame "exit"
  pushFrame "default"
  pushFrame "case-2"
  pushFrame "case-1"
  pushFrame "case-0"
  pushBrTable ["case-0", "case-1", "case-2"] "default"
  pushBlock [ValNumType I32] [] -- case-0
  pushI32Const 5
  pushBr "exit"
  pushBlock [ValNumType I32] [] -- case-1
  pushI32Const 6
  pushBr "exit"
  pushBlock [ValNumType I32] [] -- case-2
  pushI32Const 7
  pushBr "exit"
  pushBlock [ValNumType I32] [] -- default
  pushUnreachable
  pushBlock [ValNumType I32] [ValNumType I32] -- exit
  commitCode

  pushI32FuncRef "jump_table_test"
  pushI32Const 1
  pushCallIndirect [ValNumType I32] [ValNumType I32]
  commitCode