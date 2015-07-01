import LLVM.General.AST hiding (type')
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import LLVM.General.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.General.Context (withContext)
import Data.Char (ord)
import Control.Monad.Except (runExceptT)

constStr :: String -> C.Constant
constStr = C.Array T.i8 . map (C.Int 8 . fromIntegral . ord)

global :: T.Type -> String -> C.Constant
global tp nm = C.GlobalReference tp $ Name nm

prog :: AST.Module
prog = defaultModule { moduleName = "main", moduleDefinitions = [str, puts, main'] }
  where str = GlobalDefinition $ globalVariableDefaults {
                name = Name "str"
              , linkage = L.Internal
              , isConstant = True
              , type' = ArrayType 13 T.i8
              , initializer = Just $ constStr "Hello world!\0"
              }
        puts = GlobalDefinition $ functionDefaults {
                 name = Name "puts"
               , parameters = ([Parameter (T.ptr T.i8) (UnName 0) []] , False)
               , returnType = T.i32
               }
        main' = GlobalDefinition $ functionDefaults {
                  name = Name "main"
                , returnType = T.i32
                , basicBlocks =
                    [BasicBlock
                      (UnName 0)
                      [Do $ Call False CC.C []
                              (Right . ConstantOperand $ global T.i32 "puts")
                              [( ConstantOperand
                                  (C.GetElementPtr True
                                   (global (T.ptr (ArrayType 13 T.i8)) "str")
                                   [C.Int 32 0, C.Int 32 0])
                               , [])]
                       [] []]
                      (Do $ Ret (Just .ConstantOperand $ C.Int 32 0) [])]
                }

main :: IO ()
main = withContext $ \ctx -> do
         runExceptT $ withModuleFromAST ctx prog $ \m -> do
                         llstr <- moduleLLVMAssembly m
                         putStrLn llstr
         return ()
