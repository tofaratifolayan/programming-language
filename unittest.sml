(*
*   Unit-testing framework for SML.
*
*   Most clients will use only the assert* functions in UnitTest.  Those
*   functions typically take at least two arguments:  a name of type string
*   and a test function of type unit -> 'a.  They return a value t of type
*   test, which we informally call an 'a-type test.  Tests are typically run
*   through a test runner (see the TestRunner module, below), and when a test is
*   run, the outcome is either success, failure, or an exception.  This
*   information is passed to the test runner, which in turn communicates the
*   information to the client.  Typically when a test is not successful,
*   additional information is passed to the test runner, such as a message to
*   display to indicate how the test did not succeed; we call this the failure
*   message.
*
*   Most clients will use one of the run* functions in TestRunner to run tests.
*   Those functions take some variant of a list of tests to be run or or a test
*   suite, which is a value of type (string * (test list)) list.  Each element
*   of a suite consists of a name and a list of tests to be run.
*
*   N. Danner.
*
*)
structure UnitTest =
struct

  (*  Possible results of running a single test.
  *)
  datatype test_status = SUCCESS | FAIL | EXN

  (*  A test_result is a record with fields for the status of the test,
  *   a failure message, and traceback.
  *)
  type test_result = {status : test_status, info : string option,
                        tb : string option}

  (*  The type of a test.  A test consists of a name and a delayed
  *   function of test_result type.
  *)
  type test = string * (unit -> test_result)


  (*  statusToString st = a string representation of the status st.
  *)
  fun statusToString SUCCESS = "SUCCESS"
    | statusToString FAIL = "FAILURE"
    | statusToString EXN = "EXCEPTION"

  (*  failMsg name exp act = s where s is a short message describing
  *   the failure of test name that had expected value exp and actual
  *   value act.  s shows the expected and actual value, coloring those two
  *   substrings at the point at which they differ.
  *)
  fun failMsg(name, exp, act) =
  let
    fun findAndMark(cs0 : char list, cs1 : char list) :
        (char list)*(char list)*(char list) =
      case (cs0, cs1) of
           ([], []) => ([], [], [])
         | (_, []) => ([], cs0, [])
         | ([], _) => ([], [], cs1)
         | (c0 :: cs00, c1 :: cs11) =>
             if c0 = c1 then
             let
               val (common, cs0', cs1') = findAndMark (cs00, cs11)
             in
               (c0 :: common, cs0', cs1')
             end
             else
               ([], cs0, cs1)

    val (common, expRest, actRest) = findAndMark(explode exp, explode act)

    (*
    val exp = String.concat [implode common, "\^[[34m",
                             implode expRest, "\^[[0m"]
    val act = String.concat [implode common, "\^[[34m",
                             implode actRest, "\^[[0m"]
    *)
    val exp = String.concat [
      implode common, ANSITerm.toString [ANSITerm.REV],
      implode expRest, ANSITerm.toString []]
    val act = String.concat [
      implode common, ANSITerm.toString [ANSITerm.REV],
      implode actRest, ANSITerm.toString []]
  in
    String.concatWith " "
        ["FAILURE (", name, "): expected", exp, "; got", act, "."]
  end

  (*  exnMsg name = s where s is a short message describing
  *   that test name raised an exception.
  *)
  fun exnMsg name =
    String.concatWith " "
        ["Failure (", name, "): raised exception; see backtrace."]

  (*  makeTestEq(name, test, exp, eq, toStr) = t, where t is a test.  The
  *   name of t is name.  If test : unit -> 'a, then we say that
  *   t is an 'a-type test.  When t is run (usually via runTest t),
  *   there are three possibilities, where we write t() when we
  *   really mean (#2 t)():
  *
  *   eq (exp, test()) = true; we say t succeeds.  t() evaluates
  *   to {SUCCESS, NONE, NONE}.
  *
  *   eq (exp, test()) = false; we say t fails.  t() evaluates
  *   to {FAIL, SOME info, NONE}, where info is an error message that
  *   identifies both the expected and actual value (exp and test(),
  *   respectively), converting both to strings with toStr.
  *
  *   test() raises an exception; we say t has an exception.  t()
  *   evaluates to {EXN, SOME info, SOME tb}, where info is an
  *   error message indicating that test() raises an exception and
  *   tb is a traceback for the exception.  Note that t() itself
  *   does not raise an exception in this case!
  *
  *   It is rare for clients to use makeTestEq directly; most clients will
  *   instead use one of the assert* functions, below.
  *)
  fun makeTestEq (test_name : string, 
                 test : unit -> 'a, 
                 exp : 'a,
                 eq : 'a*'a -> bool,
                 to_str : 'a -> string) : test =
  let
    fun test_fn () =
    let 
      val tb = ref("") : string ref 
      val oldout = !(Control.Print.out)
      val _ = Control.Print.out := 
        {
            say=fn s => tb := concat [!tb, s], 
            flush=fn() => ()
        }
      val ret = 
        BackTrace.monitor(
        fn () =>
        let
          val act = test()
        in
          if eq(exp, act) then {status = SUCCESS, info = NONE, tb = NONE}
          else {status = FAIL, 
                info = SOME (failMsg(test_name, to_str(exp), to_str(act))), 
                tb = NONE}
        end
        ) 
        handle _ => 
          { status = EXN,
            info = SOME(exnMsg(test_name)),
            tb = SOME(!tb)
          }
      val _ = Control.Print.out := oldout
    in
      ret
    end
  in
    (test_name, test_fn)
  end

  (*  assertPred (test_name, test_fn, p, toString) = t, where t is a test 
  *   named test_name that succeeds if p(test_fn()) = true.  The failure message
  *   includes toString(test_fn()).
  *)
  fun assertPred (test_name : string, 
                 test : unit -> 'a, 
                 p : 'a -> bool,
                 to_str : 'a -> string) : test =
  let

    (*  failMsg name exp act = s where s is a short message describing
    *   the failure of test name that had expected value exp and actual
    *   value act.
    *)
    fun failMsg(name, act) =
      String.concatWith " "
          ["FAILURE (", name, "): got", act, "."]

    fun test_fn () =
    let 
      val tb = ref("") : string ref 
      val oldout = !(Control.Print.out)
      val _ = Control.Print.out := 
        {
            say=fn s => tb := concat [!tb, s], 
            flush=fn() => ()
        }
      val ret = 
        BackTrace.monitor(
        fn () =>
        let
          val act = test()
        in
          if p(act) then {status = SUCCESS, info = NONE, tb = NONE}
          else {status = FAIL, 
                info = SOME (failMsg(test_name, to_str(act))), 
                tb = NONE}
        end
        ) 
        handle _ => 
          { status = EXN,
            info = SOME(exnMsg(test_name)),
            tb = SOME(!tb)
          }
      val _ = Control.Print.out := oldout
    in
      ret
    end
  in
    (test_name, test_fn)
  end

  (*  assertEq(name, test, exp, toStr) = t, where t is a test with name name
  *   that succeeds just in case test() = exp (i.e., this test constructor
  *   is for tests on equality types).  The failure message includes both
  *   toStr(test()) and toStr(exp).
  *)
  fun assertEq (test_name : string, 
                    test : unit -> ''a, 
                    exp : ''a,
                    to_str : ''a -> string) : test =
    makeTestEq(test_name, test, exp, op=, to_str)

  val assertEq' = makeTestEq

  (*  assertTrue(name, test) = t, where t is a bool-type test with name name 
  *   that succeeds just in case test() = true.
  *)
  fun assertTrue(name : string,
                 test : unit -> bool) : test =
    assertEq(name, test, true, Bool.toString)

  (*  assertTrue(name, test) = t, where t is a bool-type test with name name 
  *   that succeeds just in case test() = false.
  *)
  fun assertFalse(name : string,
                  test : unit -> bool) : test =
    assertEq(name, test, false, Bool.toString)

  (*  assertTrueMsg(name, test) = t, where t is a bool-type test with name 
  *   name that succeeds just in case #1(test()) = true.  The failure message
  *   is #2(test()).  This function can be used to construct a bool-type test
  *   that provides more information in the failure message than just that the
  *   test returned false instead of true.
  *)
  fun assertTrueMsg(name : string,
                    test : unit -> bool*string) : test =
    makeTestEq(name,
        test,
        (true, "true"),
        fn (z1, z2) => (#1(z1) = #1(z2)),
        fn (_, s) => s)

  (*  assertTrueMsg(name, test) = t, where t is a bool-type test with name 
  *   name that succeeds just in case #1(test()) = false.  The failure message
  *   is #2(test()).  This function can be used to construct a bool-type test
  *   that provides more information in the failure message than just that the
  *   test returned false instead of true.
  *)
  fun assertFalseMsg(name : string,
                    test : unit -> bool*string) : test =
    makeTestEq(name,
        test,
        (false, "false"),
        fn (z1, z2) => (#1(z1) = #1(z2)),
        fn (_, s) => s)

  (*  assertEqInt(name, test, exp) = t, where t is an int-type test
  *   with name name that succeeds just in case test() = exp.  The failure
  *   message includes both test() and exp.
  *)
  fun assertEqInt (test_name : string,
                     test : unit -> int,
                     exp : int) : test =
    assertEq(test_name, test, exp, Int.toString)

  (*  assertEqStr(name, test, exp) = t, where t is an string-type test
  *   with name name that succeeds just in case test() = exp.  The failure
  *   message includes both test() and exp.
  *)
  fun assertEqStr (name : string, test : unit -> string, exp : string) : test =
    assertEq(name, test, exp, String.toString)

  (*  assertEqBool(name, test, exp) = t, where t is an string-type test
  *   with name name that succeeds just in case test() = exp.  The failure
  *   message includes both test() and exp.
  *)
  fun assertEqBool (name : string, test : unit -> bool, exp : bool) : test =
    assertEq(name, test, exp, Bool.toString)

  (*  assertEqReal(name, test, exp) = t, where t is an real-type test
  *   with name name that succeeds just in case test() = exp.  Equality
  *   of expected and actual value is tested with Real.==.  The failure
  *   message includes both test() and exp.
  *)
  fun assertEqReal (test_name : string,
                      test : unit -> real,
                      exp : real) : test =
    makeTestEq(test_name, test, exp, Real.==, Real.toString)

  (*  assertAlmostEqReal (name, test, exp, err) = t, where t is a
  *   real-type test that succeeds when |test() - exp| < err.  The failure
  *   message includes both test() and exp.
  *)
  fun assertAlmostEqReal (name, test, exp, err) =
    makeTestEq(name, test, exp, fn (x, y) => Real.<(Real.abs(x-y), err),
               Real.toString)


  (*  assertEqList(name, test, exp, toStr) = t, where t is an ''a-list type 
  *   test with name name that succeeds just in case test() = exp.  The failure
  *   message includes both test() and exp, where toStr is used to convert 
  *   list items to strings.
  *)
  local
    (*  listToString itemToString [x_0,...,x_n] =
    *     "[s_0,...,s_n]" where s_i = itemToString x_i.  Empty and
    *     one-element lists are handled properly.
    *)
    fun listToString itemToString xs =
      concat ["[", String.concatWith "," (map itemToString xs), "]"]
  in
    fun assertEqList (test_name, test, exp, itemToString) =
      assertEq(test_name, test, exp, listToString itemToString)
  end

  (*  assertNil(name, test, toStr) = t, where t is an 'a-list type test
  *   with name name that succeeds just in case test() = nil.  The failure 
  *   message includes test(), where toStr is used to convert list items to 
  *   strings.
  *)
  fun assertNil (test_name, test, itemToString) =
    assertPred(test_name, test, null, itemToString)

  (*  assertNotNil(name, test) = t, where t is an 'a-list type test
  *   with name name that succeeds just in case test() <> nil.
  *)
  fun assertNotNil (test_name, test) =
    assertPred(test_name, test, not o null, fn _ => "[]")

  (*  assertEqOption(name, test, exp, eq, toString) = t, where t is an
  *   'a option test that succeeds when eq(test(), exp) = true.  The failure
  *   message includes a string representation of test() and exp, where
  *   toString is used to convert values of type 'a to string.
  *
  *   assertSomeOption(name, test, toString) = t, where t is an 'a option test
  *   that succeeds when test() = SOME(x) for some x.
  *
  *   assertNoneOption(name, test) = t, where t is an 'a option test that
  *   succeds when test() = NONE.
  *)
  local
    fun eqOpt (eq : 'a*'a -> bool) (x : 'a option, y : 'a option) : bool =
      case (x, y) of
           (NONE, NONE) => true
         | (SOME x', SOME y') => eq(x', y')
         | _ => false

    fun optToString (toString : 'a -> string) (x : 'a option) : string =
      case x of
           NONE => "NONE"
         | SOME x' => "SOME(" ^ (toString x') ^ ")"
  in
    fun assertEqOption (name : string, 
        test : unit -> 'a option, 
        exp : 'a option, 
        eq : 'a*'a -> bool, 
        toString : 'a -> string) : test =
      makeTestEq(name, test, exp, eqOpt eq, optToString toString)

    fun assertSomeOption (name : string, 
        test : unit -> 'a option,
        toString : 'a -> string) : test =
      assertPred(name, test, Option.isSome, optToString toString)

    fun assertNoneOption (name : string, test : unit -> 'a option) : test =
      assertPred(name, test, not o Option.isSome, fn _ => "NONE")

  end

  (*  assertExn(name, test, exp) = t, where t is an 'a test for some 'a that
  *   succeds if test() raises an exception e such that exnName e = exnName exp.
  *   The failure message includes the names of the expected and actual (if
  *   any) exceptions raised.
  *)
  fun assertExn(name, test, exp) =
  let
    fun test'() : bool*string =
    let
      val _ = test()
    in
      (false, "Expected " ^ (exnName exp) ^ ", but no exn raised.")
    end
    handle e => 
      if exnName e = exnName exp then (true, "")
      else (false, "Expected " ^ (exnName exp) ^ ", got " ^ (exnName e) ^ ".")
  in
    assertTrueMsg(name, test')
  end

  fun doIt(test_name : string, test : unit -> unit) =
    assertPred (test_name, test, fn _ => true, fn _ => "")

  (*  seqTest(name, [t_0,...,t_{n-1}]) = t, where t is a test that succeeds if
  *   each of t_0, t_1,..., t_{n-1} succeeds.  The tests are evaluated in the
  *   order given.  If t_i does not succeed, then t_{i+1},...t_{n-1} are not
  *   evaluated.
  *)
  fun seqTest(name : string, tests : test list) : test =
  let
    fun runTests(tests : test list) : test_result =
      case tests of
           [] => {status=SUCCESS, info=NONE, tb=NONE}
         | (testName, test) :: ts =>
           let
             val res as {status,info,tb} = test()
           in
             case status of
                  SUCCESS => runTests ts
                (* | _ => res *)
                | _ => {status=status, 
                        info=SOME(name ^ ": " ^ (valOf info)),
                        tb=tb}
           end
  in
    (name, fn () => runTests(tests))
  end

  (*  seqTest(name, [t_0,...,t_{n-1}]) = t, where t is a test that succeeds if
  *   one of t_0, t_1,..., t_{n-1} succeeds.  The tests are evaluated in the
  *   order given.  If t_i succeeds, then t_{i+1},...t_{n-1} are not evaluated.
  *)
  fun orTest(name : string, tests : test list) : test =
  let
    fun runTests(tests : test list) (infos : string option list): test_result =
      case tests of
           [] => 
             {
               status=FAIL, 
               info=SOME(String.concatWith " " [
                 "FAILURE (",
                 name,
                 "): failed all sub-tests.",
                 "Subtest messages: ",
                 String.concatWith "; " (rev (map valOf infos))
               ]), 
               tb=NONE
             }
         | (name, test) :: ts =>
           let
             val res as {status,info,tb} = test()
           in
             case status of
                  SUCCESS => {status=SUCCESS, info=NONE, tb=NONE}
                | _ => runTests ts (info :: infos)
           end
  in
    (name, fn () => runTests tests [])
  end

end

structure TestRunner =
struct

  structure U = UnitTest
  open U

  val maxMsgs = ~1

  (*  runTest test = (name, result), where name is the name of test
  *   and result is the test_result that describes the result of running
  *   the test.
  *)
  fun runTest (test : test) : string*test_result =
  let
    val (test_name, test_fn) = test
    val () = print (concat [test_name, "..."])
    val result = test_fn()
    val () = print (concat [statusToString (#status(result)), "\n"])
  in
    (test_name, result)
  end

  fun printV (verbose : bool) (s : string) : unit =
    if verbose then print s else ()

  fun runTest (verbose : bool) (test : test) : string*test_result =
  let
    val (test_name, test_fn) = test
    val () = printV verbose (concat [test_name, "..."])
    val result = test_fn()
    val () = printV verbose (concat [statusToString (#status(result)), "\n"])
  in
    (test_name, result)
  end

  (*  printList ss prints each string s that occurs as SOME(s) in ss.
  *)
  (*
  fun printList ([] : string option list) : unit = ()
    | printList ((SOME s)::ss) = (print (s ^ "\n") ; printList ss)
    | printList (NONE::ss) = printList ss
  *)
  fun printList ss =
  let
    val realMsgs = List.filter (fn x => x <> NONE) ss
    val msgs = 
      if maxMsgs = ~1 then 
        realMsgs 
      else 
        List.take(realMsgs, Int.min(length realMsgs, maxMsgs))
    val msgsS = ListFormat.fmt 
      {init="", sep="\n", final="", fmt=(fn s => (valOf s))} 
      msgs
    val msgsS =
      if length msgs < length realMsgs then
        msgsS ^ "\n***ERROR MESSAGES TRUNCATED***\n"
      else
        msgsS
  in
    print msgsS
  end

  (*  print tbs [(name_1, tb_1), (name_2, tb_2),...] prints each name_i
  *   and then tb_i for each non-NONE tb_i.  The intent is that
  *   if tb_i is not NONE, then its value is a traceback for the
  *   test name_i.
  *)
  fun printTbs (tbs) =
  let
    val tbs = List.filter (fn (_, x) => x <> NONE) tbs
    val realTbs =
      if maxMsgs = ~1 then
        tbs
      else
        List.take(tbs, Int.min(length tbs, maxMsgs))
    val tbMsgs = ListFormat.fmt
      {init="", 
        sep="\n", final="", 
        fmt=(fn (name, tb) => concat [name, "\n", valOf tb])}
      realTbs
    val tbMsgs =
      if length realTbs < length tbs then
        tbMsgs ^ "\n***TRACEBACKS TRUNCATED***\n"
      else
        tbMsgs
  in
    print tbMsgs
  end

  (*  runTests([t_1,t_2,...,t_n], v) = n, where n is the number of tests among
  *   t_1,..., t_n that are not successful.
  *
  *   As a side-effect, the tests t_1, t_2,... are run in order by
  *   evaluating (runTest v t_i), and messages are printed to the terminal 
  *   indicate success, failure, or exception.  One summary line is printed 
  *   per test, and then failure messages and tracebacks are printed after
  *   the summary lines.
  *)
  fun runTests (tests : test list, verbose : bool) : int =
  let
    val rt = Timer.startRealTimer ()
    val names_results = map (runTest verbose) tests
    val elapsed = Timer.checkRealTimer rt
    val n_succ = length (List.filter 
          (fn nr => case nr of (_, {status=SUCCESS,...}) => true 
                             | _ => false) 
          names_results)
    val n_fail = length (List.filter 
          (fn nr => case nr of (_, {status=FAIL,...}) => true 
                             | _ => false) 
          names_results)
    val n_exn = length (List.filter 
          (fn nr => case nr of (_, {status=EXN,...}) => true 
                             | _ => false) 
          names_results)
    val failMsgs = map (#info o #2) names_results
    val tbs = map (fn nr => (#1(nr), (#tb o #2)(nr))) names_results
    val return = n_fail + n_exn
  in
    (
      print "\n\n" ; 
      print (concat ["Ran ", Int.toString(length(tests)), " tests in about ",
        IntInf.toString (Time.toMilliseconds elapsed), " ms.\n"]) ;
      print (concat ["Passed: ", Int.toString(n_succ), "; ",
                     "Failed: ", Int.toString(n_fail), "; ",
                     "Exceptions: ", Int.toString(n_exn), "."]) ;
      print "\n\n" ;
      printList failMsgs ;
      print "\n\n" ; 
      printTbs tbs ;
      return
    )
  end

  (*  runTestSuites([s_0,...,s_{n-1}], v) =
  *     runTests(s_0, v) + runTests(s_1, v) + ... + runTests(s_{n-1}, v).
  *)
  fun runTestSuites(tests: test list list, verbose : bool) =
    case tests of
         [s] => runTests(s, verbose)
       | s :: ss => runTests(s, verbose) + runTestSuites(ss, verbose)

  fun runTestSuites(tests: (string *(test list)) list, verbose : bool) =
  let
    fun runTestSuites (tests : (string*(test list)) list) =
      case tests of
           [] => 0
         | (name, s) :: ss => 
             let
               val () = print ("*** Suite: " ^ name ^ "\n\n")
               val thisRet = runTests(s, verbose) 
               val restRet = runTestSuites(ss)
             in
               thisRet + restRet
             end

    val ret = runTestSuites tests
  in
    (
      print "Tests completed.\n" ;
      (*
      print (if ret > 0 then "\^[[34m" else "") ;
      *)
      print (if ret > 0 then ANSITerm.toString [ANSITerm.REV] else "") ;
      print (String.concat [Int.toString ret, " failures and exceptions.\n"]) ;
      print "\^[[0m" ;
      ret
    )
  end


  exception Timeout

  (*  runTimedTests(ts, tm, v) = runTests(ts, v) with the additional constraint
  *   that the set of tests must terminate within tm seconds.  If evaluating
  *   the tests in ts takes longer than tm seconds, then the testing process is
  *   terminated.
  *)
  fun runTimedTests(tests : test list, waitTime : int, verbose : bool) =
  let
    val dieMsg = String.concatWith " "
      ["\n\nRunning tests taking >", Int.toString waitTime, "seconds;",
       "assuming failure and terminating.\n\n"]
  in
    TimeLimit.timeLimit
      ((Time.fromSeconds o LargeInt.fromInt) waitTime)
      runTests
      (tests, verbose)
    handle TimeLimit.TimeOut =>
      (print dieMsg ; length tests)
  end

  (*  runTimedTestSuites(ts, tm, v) is just like runTestSuites(ts, v) with the 
  *   additional constraint that the set of tests must terminate within tm 
  *   seconds.  If evaluating the tests in ts takes longer than tm seconds, 
  *   then the testing process is terminated.
  *)
  fun runTimedTestSuites(tests: (string *(test list)) list, 
                         waitTime : int, 
                         verbose : bool) =
  let
    fun runTestSuites (tests : (string*(test list)) list) =
      case tests of
           [] => 0
         | (name, s) :: ss => 
             let
               val () = print ("*** Suite: " ^ name ^ "\n\n")
               val thisRet = runTimedTests(s, waitTime, verbose) 
               val restRet = runTestSuites(ss)
             in
               thisRet + restRet
             end

    val ret = runTestSuites tests
  in
    (
      print "Tests completed.\n" ;
      (*
      print (if ret > 0 then "\^[[34m" else "") ;
      *)
      print (if ret > 0 then ANSITerm.toString [ANSITerm.REV] else "") ;
      print (String.concat [Int.toString ret, " failures and exceptions.\n"]) ;
      print "\^[[0m" ;
      ret
    )
  end


end

