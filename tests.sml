(*  Tests for COMP 323 Project 2.
*
*   N. Danner
*)

structure Main =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure Ast = Ast
  structure Toks = Tokens

  structure T = TextIO

  fun runTests() : int =
    TR.runTestSuites (TestsParsing.test_suites(), true)

  fun runTests'(testFn, doBT) =
    if doBT
    then BackTrace.monitor testFn
    else testFn()

  exception Usage

  val usage : string = String.concatWith "\n" [
    "tests [options]",
    "\tRun unit tests.",
    "",
    "Options:",
    "\t--bt:     enable backtraces.",
    "\t--timed:  stop tests if any test takes > 30 seconds.",
    "\t--help:   print this message.",
    ""
  ]

  fun main(arg0 : string, argv : string list) : OS.Process.status =
  let
    val doBT = ref false
    val doTimed = ref false

    (*  handleOpt : handle a single option by setting stream or parser
    *   appropriately.
    *
    *   Pre-condition:  oa = "--" ^ oa'.
    *)
    fun handleOpt (oa : string) : unit =
    let
    in
      case String.substring(oa, 2, String.size oa - 2) of
           "timed" => doTimed := true
         | "bt"  => doBT := true
         | "help" => raise Usage
         | _ => raise Usage
    end

    (*  handleOpts : handle all options by calling handleOpt o for each option o
    *   on the command line.
    *)
    fun handleOpts (optsargs : string list) : unit =
    let
    in
      case optsargs of
           [] => ()
         | oa :: oas =>
             if String.isPrefix "--" oa then (handleOpt oa ; handleOpts oas)
             else raise Usage
    end

    val () = handleOpts argv

    val testFn = 
      if !doTimed then
        fn () => TR.runTimedTestSuites (TestsInterp.test_suites(), 30, true)
      else
        fn () => TR.runTestSuites (TestsInterp.test_suites(), true)

  in

    (runTests'(testFn, !doBT) ; OS.Process.success)

  end
  handle
    (* Usage errors *)
    Usage => (print usage ; OS.Process.failure)

end
