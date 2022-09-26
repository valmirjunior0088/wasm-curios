import Syntax.Types
import Serialize
import Construct

main :: IO ()
main = writeModule "./test.wasm" $ construct $ do
  declareTable "table" FuncRef (Bounded 4 4)
  declareMem "mem" (Unbounded 1)

  declareFunc "call_indirect_test" [] [ValNumType I32] $ do
    pushI32FuncRef "it_works"
    pushCallIndirect [] [ValNumType I32]
  
  exportFunc "call_indirect_test"

  declareFunc "it_works" [] [ValNumType I32] $ do
    pushI32Const 5
  
  exportFunc "it_works"

  declareFunc "jump_table_test" [("value", ValNumType I32)] [ValNumType I32] $ do
    pushLocalGet "value"
    pushBlock "result" [ValNumType I32] [ValNumType I32]
    pushBlock "default" [ValNumType I32] []
    pushBlock "case-2" [ValNumType I32] []
    pushBlock "case-1" [ValNumType I32] []
    pushBlock "case-0" [ValNumType I32] []
    pushBrTable ["case-0", "case-1", "case-2"] "default"
    popBlock -- case-0
    pushI32Const 5
    pushBr "result"
    popBlock -- case-1
    pushI32Const 6
    pushBr "result"
    popBlock -- case-2
    pushI32Const 7
    pushBr "result"
    popBlock -- default
    pushUnreachable
    popBlock -- result
    pushAddLocalSet "result" (ValNumType I32)
    pushLocalGet "result"

  exportFunc "jump_table_test"
