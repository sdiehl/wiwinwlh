module Standalone where

-- Pretty Printer
import LLVM.General.Pretty (ppllvm)

-- AST
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Linkage as Linkage
import qualified LLVM.General.AST.Visibility as Visibility
import qualified LLVM.General.AST.CallingConvention as Convention

import Data.Text.Lazy.IO as TIO

astModule :: AST.Module
astModule = AST.Module
    { AST.moduleName         = "example-llvm-module"
    , AST.moduleDataLayout   = Nothing
    , AST.moduleTargetTriple = Nothing
    , AST.moduleDefinitions  =
        [ AST.GlobalDefinition
            (AST.Function
                Linkage.External
                Visibility.Default
                Nothing
                Convention.C
                []
                (AST.IntegerType 8)
                (AST.Name "f")
                ([AST.Parameter (AST.IntegerType 8) (AST.Name "x") []], False)
                []
                Nothing
                Nothing
                0
                Nothing
                Nothing
                [ AST.BasicBlock
                    (AST.Name "entry")
                    []
                    (AST.Do
                        (AST.Ret
                            (Just
                                (AST.LocalReference
                                    (AST.IntegerType 8)
                                    (AST.Name "x")
                                )
                            )
                            []
                        )
                    )
                ]
            )
        ]
    }

main :: IO ()
main = TIO.putStrLn (ppllvm astModule)
