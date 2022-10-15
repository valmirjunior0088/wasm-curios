import Syntax.Types
import Syntax.Instructions
import Serialize
import Construct

main :: IO ()
main = writeModule "./test.wasm" $ runConstruct $ do
  declareExportFunc "_start" [] [i32]
  declareExportFunc "jump_table_test" [("value", i32)] [i32]
  declareExportFunc "local_test" [] [i32]
  declareExportFunc "global_test" [] [i32]

  declareExportGlobal "global" (GlobalType i32 Var) (Expr [I32Const 5])
  declareExportMem "mem" (MemType $ Unbounded 1)
  commitFuncTable Nothing

  startCode
  pushI32Const 0
  pushI32FuncRef "jump_table_test"
  pushCallIndirect [i32] [i32]
  endCode

  startCode
  pushBlock "result" [] [i32]
  pushBlock "default" [] []
  pushBlock "case-2" [] []
  pushBlock "case-1" [] []
  pushBlock "case-0" [] []
  pushLocalGet "value"
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
  endCode

  startCode
  pushLocal "one" i32
  pushLocal "other" i32
  pushI32Const 5
  pushLocalSet "one"
  pushI32Const 6
  pushLocalSet "one"
  pushI32Const 7
  pushLocalSet "other"
  pushI32Const 8
  pushLocalSet "other"
  pushLocalGet "one"
  endCode

  startCode
  pushGlobalGet "global"
  pushI32Const 5
  pushI32Add
  pushGlobalSet "global"
  pushGlobalGet "global"
  endCode
