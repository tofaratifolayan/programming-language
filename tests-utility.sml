(*  Utility structure for tests.
*
*   N. Danner
*)

structure TestsUtility =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure Ast = Ast
  structure Toks = Tokens

  structure T = TextIO

  val testfilesDir = "testfiles"

  (* filesByExt ext dir = the list of files in dir with extension ext.  Each
  *  file is a string of the form dir/f.ext.
  *)
  fun filesByExt (ext : string) (dir : string)  : string list =
  let
    fun getFiles (ds : OS.FileSys.dirstream) : string list =
      case OS.FileSys.readDir ds of
           NONE => []
         | SOME f =>
             case OS.Path.ext f of
                  SOME s => if s = ext then f :: getFiles ds
                            else getFiles ds
                | _ => getFiles ds

    val ds = OS.FileSys.openDir dir
  in
    map 
      (fn f => dir ^ "/" ^ f) 
      (ListMergeSort.sort op>= (getFiles ds))
    before OS.FileSys.closeDir ds
  end

  val smlFiles : string -> string list = filesByExt "sml"
  val wmlFiles : string -> string list = filesByExt "wml"

  (*  filesWithDerived ext derived dir = the list of files in dir named
  *  f.ext for which there is a file named f.ext.derived.
  *)
  fun filesWithDerived 
      (ext : string) (derived : string) (dir : string) : string list =
    List.filter 
      (fn f => OS.FileSys.access (f ^ "." ^ derived, [OS.FileSys.A_READ])) 
      (filesByExt ext dir)

  val wmlFilesWithDerived : string -> string -> string list =
    filesWithDerived "wml"

  val wmlFilesExpected = wmlFilesWithDerived "expected"

end
