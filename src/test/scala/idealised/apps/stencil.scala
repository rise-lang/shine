package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class stencil extends idealised.util.Tests {

  private val simpleStencil = nFun(n => fun(ArrayType(n, float))(xs =>
    xs :>> slide(3, 1) :>> mapSeq(fun(nbh =>
      nbh :>> reduceSeq(fun(x => fun(a => x + a)), 0.0f)
    ))
  ))

  test("Simple stencil compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleStencil, Map())))
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleStencil, Map())))
    println(p.code)
    SyntaxChecker(p.code)
  }

  // currently fails do to a missing address space at a new
  ignore ("Simple scan compiles to syntactically correct OpenCL") {
    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleStencil, Map())))
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

}
