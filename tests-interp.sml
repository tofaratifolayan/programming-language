(*  Interpretation tests for WML.
*
*   N. Danner
*)

structure TestsInterp =
struct

  structure U = UnitTest
  structure TR = TestRunner
  structure TU = TestsUtility

  structure Ast = Ast
  structure Toks = Tokens

  structure T = TextIO

  structure I = Interp

  infix 0 $
  fun f $ x = f x

  fun testExec (pgmfile : string) : U.test =
  let
    val name = pgmfile

    val pgmOutFile = pgmfile ^ ".output"
    val pgmExpFile = pgmfile ^ ".expected"

    (*  trimLines s = concatWith "\n" [s_0,...,s_{n-1}], where
    *     s = s_0' ^ "\n" ^ ... ^ "\n" ^ s_{n-1}'
    *     s_i = s_i' with leading and trailing whitespace removed.
    *)
    fun trimLines (s : string) : string =
    let
      val lines = Substring.fields (fn c => c = #"\n") (Substring.full s)
      val isSpace = fn c => c = #" "
    in
      Substring.concatWith "\n"
        (map ((Substring.dropr isSpace) o (Substring.dropl isSpace)) lines)
    end

    (* Read the contents of the expected output file.
    *)
    val expectedIn = TextIO.openIn pgmExpFile
    val expectedOutput = trimLines (TextIO.inputAll expectedIn)
    val () = TextIO.closeIn expectedIn

    fun test() : string =
    let
      val pgmIn = TextIO.openIn pgmfile
      val strm = Lexer.streamifyInstream pgmIn
      val p = Parse.parsePgm strm
      val () = TextIO.closeIn pgmIn

      (*  Get the underlying stream for terminal output.
      *)
      val stdOutSaveStream = TextIO.getOutstream TextIO.stdOut
      val stdErrSaveStream = TextIO.getOutstream TextIO.stdErr

      (*  Open an output file for execution.
      *)
      val pgmOut = TextIO.openOut pgmOutFile

      (*  Get underlying stream for the output file.
      *)
      val pgmOutStream = TextIO.getOutstream pgmOut

      (*  Set the underlying stream for stdOut to the file.
      *)
      val () = TextIO.setOutstream (TextIO.stdOut, pgmOutStream)
      val () = TextIO.setOutstream (TextIO.stdErr, pgmOutStream)

      (*  Execute the program; output will go to the file instead of the
      *  terminal.
      *)
      val () = I.execPgm p
               handle e => TextIO.output (
                             TextIO.stdErr, 
                             (String.concatWith "\n" 
                                (SMLofNJ.exnHistory e)^"\n")
                           )

      (*  Restore the underlying stream for stdOut to terminal output.
      *)
      val () = TextIO.flushOut TextIO.stdOut
      val () = TextIO.flushOut TextIO.stdErr
      val () = TextIO.setOutstream (TextIO.stdOut, stdOutSaveStream)
      val () = TextIO.setOutstream (TextIO.stdErr, stdErrSaveStream)

      (*  Close the output file.
      *)
      val () = TextIO.closeOut pgmOut

      (* Read the contents of the output file.
      *)
      val outputIn = TextIO.openIn pgmOutFile
      val pgmOutput = trimLines (TextIO.inputAll outputIn)
      val () = TextIO.closeIn outputIn
    in
      pgmOutput
    end

  in
    U.assertEq(name, test, expectedOutput, String.toString)
  end

  val testDir = TU.testfilesDir ^ "/interp"

  fun makeTestSuite dir =
    (dir, map testExec (TU.wmlFilesExpected dir))
                              
  fun test_suites() =
    map
      (makeTestSuite o (fn d => (testDir ^ "/" ^ d)))
      ["synth", "ullman", "paulson", "multifn"]

end
