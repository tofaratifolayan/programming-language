(*  COMP 323 sample code:  standard driver.
*   
*   After building this driver, execute
*
*     $ ./driver
*
*   in the shell to get usage instructions.
*
*   N. Danner
*)

structure Main =
struct

  structure Toks = Tokens

  structure T = TextIO

  infix 0 $
  fun f $ x = f x

  (*  Lexing types and functions.
  *)
  type stream = MakeLex.stream
  val lex = MakeLex.makelex()

  (*  printnl s = ().
  *
  *   As a side-effect, s will be printed to the terminal followed by a newline.
  *)
  fun printnl(s : string) : unit =
    print (String.concat [s, "\n"])

  val printnls = printnl o String.concat

  (* doLex strm = ()
  *
  * Side-effect:  prints tokens from strm to terminal until EOF reached.
  *)
  fun doLex (strm : stream) : unit =
  let
    fun printTokens (strm : stream) : unit =
      case lex strm of
           (Toks.EOF, _) => printnl (Toks.toString Toks.EOF)
         | (t, strm) => print ((Toks.toString t) ^ " ") 
                           before printTokens strm
  in
    printTokens strm
  end

  (*  tokStreamToString n <t_0,...> 
  *     = String.concatWith " " [t_0,...,t_{n-1}], t_i <> EOF for i < n,
  *       String.concatWith " " [t_0,...,t_{j-1}], t_j = EOF for j < n.
  *)
  fun tokStreamToString (n : int) (strm : stream) : string =
    if n = 0 then "..."
    else 
      case lex strm of
           (Toks.EOF, _) => Toks.toString Toks.EOF
         | (t, strm) => (Toks.toString t) ^ " " ^ (tokStreamToString n strm)

  val doParseExp : stream -> unit =
    printnl o Ast.expToString o Parse.parseExp

  val doParsePgm : stream -> unit =
    printnl o Ast.pgmToString o Parse.parsePgm

  val doEvalExp : stream -> unit =
    printnl o Interp.valueToString o Interp.evalExp o Parse.parseExp

  val doExecPgm : stream -> unit =
    Interp.execPgm o Parse.parsePgm

  structure M = SplayMapFn(
    struct type ord_key = string val compare = String.compare end : ORD_KEY)

  exception Usage

  val baseUsage = String.concatWith "\n" [
    "driver cmd [options] s",
    "\tApply cmd to the file named s.",
    "",
    "Options:",
    "\t--arg:  apply cmd to s itself.",
    "\t--bt :  enable backtraces.",
    "",
    "Commands:"
  ]

  val handlers : (string*string*(string*stream -> unit)) list = [
      ("lex", 
        "perform lexical analysis and print the token sequence.",
        doLex o #2)
    , ("parseExp", 
        "parse as expression and print the abstract syntax tree." ,
        doParseExp o #2)
    , ("parsePgm", 
        "parse as program and print the abstract syntax tree.",
        doParsePgm o #2)
    , ("eval",
        "evaluate s as an expression and print its value.",
        doEvalExp o #2)
    , ("exec",
        "execute s as a program.",
        doExecPgm o #2)
  ]

  val usage = 
  let
    val maxCmdLen = foldl Int.max 0 (map (String.size o #1) handlers)
    val commands =
      map
        (fn (cmd, desc, _) => 
           Format.format "\t%s:  %s" [Format.LEFT(maxCmdLen, Format.STR cmd),
           Format.STR desc])
        handlers
  in
    String.concatWith "\n" (baseUsage :: commands @ ["\n"])
  end

  fun main(arg0 : string, argv : string list) : OS.Process.status =
  let

    val handlerMap = 
      foldr 
        (fn ((cmd, _, hndlr), m) => M.insert(m, cmd, hndlr)) 
        M.empty
        handlers

    val streamFromFile = (Lexer.streamifyInstream o TextIO.openIn)
    val streamFromString = (Lexer.streamifyInstream o TextIO.openString)

    val stream = ref (streamFromFile)
    val doBT = ref false

    (*  handleOpt : handle a single option by setting stream or parser
    *   appropriately.
    *
    *   Pre-condition:  oa = "--" ^ oa'.
    *)
    fun handleOpt (oa : string) : unit =
    let
    in
      case String.substring(oa, 2, String.size oa - 2) of
           "arg" => stream := streamFromString
         | "bt"  => doBT := true
         | _ => raise Usage
    end

    (*  handleOpts : handle all options by calling handleOpt o for each option o
    *   on the command line.
    *)
    fun handleOpts (optsargs : string list) : string list =
    let
    in
      case optsargs of
           [] => raise Usage
         | [arg] => [arg]
         | oa :: oas =>
             if String.isPrefix "--" oa then (handleOpt oa ; handleOpts oas)
             else optsargs
    end

    val cmd :: optsArgs = 
      case argv of
           [] => raise Usage
         | _ => argv

    val [arg] = handleOpts optsArgs

    val hndlr = 
      valOf(M.find(handlerMap, cmd))
      handle Option => raise Usage

  in
    if !doBT then
      BackTrace.monitor (
        fn () => ((hndlr (arg, !stream arg)) ; OS.Process.success)
      )
    else
      ((hndlr (arg, !stream arg)) ; OS.Process.success)
  end
  handle 
    (* Usage errors *)
      Usage => (print usage ; OS.Process.failure)

    (* I/O errors *)
    | e as IO.Io {name=name, function=_, cause=cause} => 
        (printnl (String.concatWith " " [
          "I/O error reading",
          name,
          "(",
          exnMessage cause,
          ")"
        ]) ; OS.Process.failure)

    (* User-code errors *)
    | Parse.NoParse s =>
        (printnl $ "Raised NoParse: " ^ s ; OS.Process.failure)
    | Parse.ExtraTokens s =>
        (printnls ["Got extra tokens after parse: ",
                   tokStreamToString 10 s] ; OS.Process.failure)
    | Interp.InterpError s =>
        (printnl s ; OS.Process.failure)

    (* Other errors *)
    | e =>
      (
        printnl $ String.concat [
          "Got exception: ",
          exnName e,
          " (",
          exnMessage e,
          ")."
        ] ;
        OS.Process.failure
      )

end
