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

  declareFunc "one_test" [] [ValNumType I32]
  declareFunc "another_test" [] [ValNumType I32]
  declareFunc "one_more_test" [] [ValNumType I32]
  
  commitFuncRefs

  pushI32FuncRef "another_test"
  pushCallIndirect [] [ValNumType I32]
  commitCode

  pushFrame "outer"
  pushFrame "inner"
  pushI32Const 6
  pushBr "outer"
  pushBlock [] [ValNumType I32]
  pushI32Const 7
  pushBr "outer"
  pushBlock [] [ValNumType I32]
  commitCode

  pushGlobalGet "__stack_pointer"
  commitCode
