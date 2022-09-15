import Syntax.Types
import Serialize
import Construct

main :: IO ()
main = writeModule "./test.wasm" $ runConstruct $ do
  importTable "env" "__indirect_function_table" FuncRef (Unbounded 0)
  importMem "env" "__linear_memory" (Unbounded 1)
  importFunc "env" "alloc" [ValNumType I32] [ValNumType I32]
  importFunc "env" "dealloc" [ValNumType I32] []
  
  commitFuncRefs

  exportFunc "alloc"
  exportFunc "dealloc"
