import Syntax.Types
import Syntax.Instructions
import Serialize
import Construct

main :: IO ()
main = writeModule "./test.wasm" $ construct $ do
  declareMem "mem" (MemType $ Unbounded 1)

  declareExportFunc "call_indirect_test" [] [ValNumType I32] $ do
    pushI32FuncRef "jump_table_test"
    pushI32Const 1
    pushCallIndirect [ValNumType I32] [ValNumType I32]

  declareExportFunc "jump_table_test" [("value", ValNumType I32)] [ValNumType I32] $ do
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

  declareExportFunc "local_test" [] [ValNumType I32] $ do
    pushI32Const 5
    pushAddLocalSet "one" (ValNumType I32)

    pushI32Const 6
    pushLocalSet "one"

    pushI32Const 7
    pushAddLocalSet "other" (ValNumType I32)

    pushI32Const 8
    pushLocalSet "other"

    pushLocalGet "one"

  declareExportGlobal "global" (GlobalType (ValNumType I32) Var) (Expr [I32Const 5])

  declareExportFunc "global_test" [] [ValNumType I32] $ do
    pushGlobalGet "global"
    pushDrop

    pushI32Const 9
    pushGlobalSet "global"

    pushGlobalGet "global"

  declareTable "table" =<< commitFuncRefs
