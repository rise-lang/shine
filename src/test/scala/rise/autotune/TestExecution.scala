package rise.autotune

import rise.autotune
import rise.core.Expr
import rise.core.types.{Nat, TuningParameter}

class TestExecution extends test_util.Tests {

  def executeConfig(e: Expr, params: Map[Nat, Nat], hostCode: HostCode): ExecutionResult = {

    val eReplaced = rise.core.substitute.natsInExpr(params, e)

    val result = autotune.execution.execute(
      expression = eReplaced,
      hostCode = hostCode,
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )

    result
  }

  // FIXME
  ignore("execute convolution") {
    val goodParameters: Map[Nat, Nat] = Map(
      TuningParameter("vec") -> (4: Nat),
      TuningParameter("tile") -> (16: Nat)
    )

    val e: Expr = util.expressions.convolution.convolutionOcl
    val e2 = rise.core.substitute.natsInExpr(goodParameters, e)

    val result = autotune.execution.execute(
      expression = e2,
      hostCode = util.hostcode.convolution(32),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median)

    //     check if result has valid runtime
    assert(result.runtime.isRight)
    println("result: " + result)

    // try to execute with zero execution iterations
    assertThrows[java.lang.AssertionError] {
      autotune.execution.execute(
        expression = e2,
        hostCode = util.hostcode.convolution(32),
        timeouts = Timeouts(5000, 5000, 5000),
        executionIterations = 0,
        speedupFactor = 100,
        execution = Median)
    }
  }

  test("execute scal") {
    val e: Expr = util.expressions.scal.scalOcl(1024)

    val result = autotune.execution.execute(
      expression = e,
      hostCode = util.hostcode.scal(1024),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Minimum)

    // check if result has valid runtime
    assert(result.runtime.isRight)
    println("result: \n" + result)
  }

  test("execute mm"){
    val mm: Expr = util.expressions.mm.mmOclGsLsWrap

    val params0:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (4: Nat),
      TuningParameter("v6") -> (64: Nat),
      TuningParameter("v7") -> (256: Nat),
      TuningParameter("v8") -> (32: Nat)
    )

    val params1:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (8: Nat),
      TuningParameter("ls1") -> (4: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (32: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (8: Nat),
      TuningParameter("v5") -> (32: Nat),
      TuningParameter("v6") -> (64: Nat),
      TuningParameter("v7") -> (32: Nat),
      TuningParameter("v8") -> (128: Nat)
    )

    val paramSet = Set(params0, params1)

    paramSet.foreach(params => {

      val result = executeConfig(mm, params, util.hostcode.mm(1024, 1024, 1024))

      println("result: " + result.runtime)

      // check for execution error
      result.runtime match {
        case Left(value) => value.errorLevel.equals(EXECUTION_ERROR)
        case Right(_) => // do nothing result is fine
      }
    })
  }

  test("generate huge amount of code") {
    // expression
    val e: Expr = util.expressions.convolution.convolutionOclGsLs(1024)

    // define parameters to break the code-gen
    val parameters: Map[Nat, Nat] = Map(
      TuningParameter("vec") -> (16: Nat),
      TuningParameter("tile") -> (32: Nat),
      TuningParameter("gs0") -> (1: Nat),
      TuningParameter("gs1") -> (512: Nat),
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (64: Nat)
    )

    // substitute parameters
    val eWithParams = rise.core.substitute.natsInExpr(parameters, e)

    println("run codegen with timeout ")
    // WARNING: timeout does not stop the thread, it only returns to the host thread
    val result = autotune.execution.execute(
      eWithParams,
      util.hostcode.convolution(1024),
      Timeouts(5000, 5000, 5000),
      10,
      100,
      execution = Median
    )

    print("result: " + result)

    result.runtime match {
      case Right(_) => assert(false)
      case Left(error) => assert(error.errorLevel.equals(autotune.CODE_GENERATION_ERROR))
    }
  }

  test("test xml parsing") {

    // scalastyle:off
    val xmlString =
      """
<trace date="2021-03-30 18:04:26" profiler_version="0.1.0" ocl_version="1.2">
  <device name="GeForce RTX 2070" id="1"/>
  <queue properties="CL_QUEUE_PROFILING_ENABLE" device_id="1" id="1"/>
  <program build_options="-cl-fast-relaxed-math -Werror -cl-std=CL1.2" id="1"/>
  <kernel name="k0" program_id="1" id="1"/>
  <kernel_instance kernel_id="1" id="1" unique_id="1" command_queue_id="1">
    <event forced="true" queued="1617120266695090400" submit="1617120266695092832" start="1617120266695097344" end="1617120266695107456"/>
    <offset_range/>
    <global_range dim="3" x="1" y="1" z="1"/>
    <local_range dim="3" x="1" y="1" z="1"/>
  </kernel_instance>
  <mem_object type="Buffer" flag="CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="2"/>
  <mem_object type="Buffer" flag="CL_MEM_READ_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="1"/>
</trace>
    """
    // scalastyle:on
    assert(autotune.execution.getRuntimeFromClap(xmlString)(0).value.toFloat == 0.010112f)
  }

  test("text xml parsing with corrupted xml string") {
    // scalastyle:off
    val corruptedXmlString =
      """<trace date="2021-04-01 18:42:51" profiler_version="0.1.0" ocl_version="1.2"/>
<trace date="2021-04-01 18:42:51" profiler_version="0.1.0" ocl_version="1.2">
  <device name="pthread-Intel(R) Core(TM) i5-8265U CPU @ 1.60GHz" id="1"/>
  <queue properties="CL_QUEUE_PROFILING_ENABLE" device_id="1" id="1"/>
  <program build_options="-cl-fast-relaxed-math -Werror -cl-std=CL1.2" id="1"/>
  <kernel name="k0" program_id="1" id="1"/>
  <kernel_instance kernel_id="1" id="1" unique_id="1" command_queue_id="1">
    <event forced="true" queued="42573479270519" submit="42573479270669" start="42573583785589" end="42573583944408"/>
    <offset_range/>
    <global_range dim="3" x="32" y="1" z="1"/>
    <local_range dim="3" x="1" y="1" z="1"/>
  </kernel_instance>
  <mem_object type="Buffer" flag="CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="2"/>
  <mem_object type="Buffer" flag="CL_MEM_READ_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="1"/>
</trace>
    """
    // scalastyle:on
    assert(autotune.execution.getRuntimeFromClap(corruptedXmlString)(0).value.toFloat == 0.158819f)
  }

  test("test xml parsing for multiple runtimes"){
    // scalastyle:off
    val xmlString =
      """<trace date="2021-07-12 19:22:15" profiler_version="0.1.0" ocl_version="1.2">
        <device name="NVIDIA GeForce RTX 2070" id="1"/>
        <queue properties="CL_QUEUE_PROFILING_ENABLE" device_id="1" id="1"/>
        <program build_options="-cl-fast-relaxed-math -Werror -cl-std=CL1.2" id="1"/>
        <kernel name="k0" program_id="1" id="1"/>
        <kernel_instance kernel_id="1" id="1" unique_id="1" command_queue_id="1">
          <event forced="true" queued="1626110536128169248" submit="1626110536128172352" start="1626110536128271232" end="1626110536132397760"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="2" unique_id="2" command_queue_id="1">
          <event forced="true" queued="1626110536132419072" submit="1626110536132421024" start="1626110536132424704" end="1626110536136711168"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="3" unique_id="3" command_queue_id="1">
          <event forced="true" queued="1626110536136718592" submit="1626110536136720448" start="1626110536136944000" end="1626110536140495232"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="4" unique_id="4" command_queue_id="1">
          <event forced="true" queued="1626110536140501856" submit="1626110536140503712" start="1626110536140507840" end="1626110536144057344"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="5" unique_id="5" command_queue_id="1">
          <event forced="true" queued="1626110536144064256" submit="1626110536144066080" start="1626110536144069632" end="1626110536147620736"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="6" unique_id="6" command_queue_id="1">
          <event forced="true" queued="1626110536147629792" submit="1626110536147632224" start="1626110536148222976" end="1626110536152535744"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="7" unique_id="7" command_queue_id="1">
          <event forced="true" queued="1626110536152542432" submit="1626110536152544736" start="1626110536152768672" end="1626110536156320768"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="8" unique_id="8" command_queue_id="1">
          <event forced="true" queued="1626110536156329088" submit="1626110536156331584" start="1626110536156336288" end="1626110536160467648"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="9" unique_id="9" command_queue_id="1">
          <event forced="true" queued="1626110536160474304" submit="1626110536160476608" start="1626110536161701984" end="1626110536165956064"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <kernel_instance kernel_id="1" id="10" unique_id="10" command_queue_id="1">
          <event forced="true" queued="1626110536165962976" submit="1626110536165965248" start="1626110536165970848" end="1626110536170273792"/>
          <offset_range/>
          <global_range dim="3" x="8192" y="1024" z="1"/>
          <local_range dim="3" x="16" y="8" z="1"/>
        </kernel_instance>
        <mem_object type="Buffer" flag="CL_MEM_READ_ONLY|CL_MEM_ALLOC_HOST_PTR" size="268435456" id="1"/>
        <mem_object type="Buffer" flag="CL_MEM_READ_ONLY|CL_MEM_ALLOC_HOST_PTR" size="68" id="2"/>
        <mem_object type="Buffer" flag="CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR" size="268435456" id="3"/>
      </trace>
      """

    // scalastyle:on

    val gold = Seq(
      4.126528,
      4.286464,
      3.551232,
      3.549504,
      3.551104,
      4.312768,
      3.552096,
      4.13136,
      4.25408,
      4.302944
    )

    val runtimes = autotune.execution.getRuntimeFromClap(xmlString)

    assert(runtimes.map(elem => elem.value) == gold)
  }
}
