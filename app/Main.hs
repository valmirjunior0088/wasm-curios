import Serialize (write)
import Syntax.Conventions (Vec (..), Name (..))
import Syntax.Types (FuncType (..), ResultType (..), ValType (..), NumType (..), Mut (..), GlobalType (..))
import Syntax.Instructions (Instr (..), Expr (..))
import Syntax.Sections (Code (..), Func (..), Locals (..))
import Syntax.Module (construct, putType, putFunc, putCode, putFuncExport, setStart, putGlobalImport)

main :: IO ()
main = write "./result.wasm" $ construct $ do
  type_v <- putType (FuncType (ResultType (Vec [])) (ResultType (Vec [])))
  type_i_i <- putType (FuncType (ResultType (Vec [ValNumType I32])) (ResultType (Vec [ValNumType I32])))
  type_v_i <- putType (FuncType (ResultType (Vec [])) (ResultType (Vec [ValNumType I32])))

  (funcIdx, _) <- putFunc (Name "test") type_v
  putCode (Code (Func (Vec [Locals 1 (ValNumType I32)]) (Expr [Nop])))

  (_, _) <- putFunc (Name "another_test") type_i_i
  putCode (Code (Func (Vec []) (Expr [I32Const 5])))

  (globalIdx, globalSymIdx) <- putGlobalImport (Name "env") (Name "__heap_base") (GlobalType (ValNumType I32) Const)

  (_, _) <- putFunc (Name "_start") type_v_i
  putCode (Code (Func (Vec []) (Expr [GlobalGet globalIdx globalSymIdx])))

  putFuncExport (Name "test_export") funcIdx
  setStart funcIdx
  
  return ()
