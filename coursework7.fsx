(*

  ITT8060 -- Advanced Programming 2024
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 7: Property-based testing

  ------------------------------------
  Name:  Syed Muhammad Hamza Bacha
  Student ID:  246076IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.taltech.ee repository itt8060-2024 under your name,
  into a file coursework7/coursework7.fsx .
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as described in the question.

  The submission deadline is indicated in Moodle.
*)

#if INTERACTIVE // A directive that makes it possible to to compile the file
                // and still run the interactive console with library dependencies.
//#r    "FsCheck.dll" // in .Net Framework and .Net core versions prior to 5.0
#r    "nuget: FsCheck, Version=3.0.0-rc3" // You can also use nuget directly with F# 5, 6, 7 and 8.
#load "FileSystem.fs"
#endif

open FsCheck
open FsCheck.FSharp

open FileSystem

let arbitrary<'T> = ArbMap.defaults |> ArbMap.arbitrary<'T>
let generate<'T> = arbitrary<'T>.Generator
let shrink<'T> = arbitrary<'T>.Shrinker




(*
   Question 1

   Define the predicates

   fsTreeWf : FsTree -> bool

   pathWf   : Path -> bool

   that evaluate to true on precisely those values of FsTree and Path
   that are well-formed.

   The directory names in a well-formed Path cannot be empty.

   A value of type FsTree is well-formed when:
   - the path to any directory in the filesystem is well-formed
   
   - the filesystem is deterministic (for any given path, there is at
     most one directory in the filesystem with that path)

   Define these predicates so that they traverse their input only
   once.
*)

let pathWf (path: FileSystem.Path): bool =  not (path |> List.isEmpty) && not (path |> List.contains(""))

let rec fsTreeWf (fs: FileSystem.FsTree): bool =
   if fs.name = ""
   then false
   else match fs.children with
         | [] -> true
         | children -> match (children |> List.map (fun child -> child.name) |> List.distinct).Length <> children.Length with
                       | true ->  false
                       | _ ->  
                           let filteredItems = children |> List.filter(fun el -> fsTreeWf el)
                           filteredItems.Length = children.Length



(*
   Question 2

   Define an FsCheck property

   createIsWf : Path -> FsTree -> Property

   which checks that creating a well-formed path p in a well-formed
   filesystem fs results in a well-formed filesystem.

   Define this using a conditional property (==>).

   Is this a good way to check such a property? Why?

   What percentage of the generated test inputs trivially satisfy this
   property?
*)

let createIsWf (p: Path) (fs: FsTree): Property = (pathWf p && fsTreeWf fs) ==> lazy fsTreeWf (create p fs)


(*
   Question 3

   Define a generator

   wfTrees : Gen<FsTree>

   that only generates well-formed filesystems.


   Define a generator

   wfPaths : Gen<Path>

   that only generates well-formed paths.


   Define these generators in such a way that none of the generated
   data is wasted (i.e., discarded). In other words, all the data that
   you (randomly) generate must be used in the the output of the
   generator.

   You may wish to use the predicates defined above to check that the
   generators indeed only generate well-formed data. Or that the
   predicates are defined correctly.
*)


let wfPaths: Gen<Path> =
    Arb.generate<string>
    |> Gen.filter (fun el -> el <> null && el <> "")
    |> Gen.map (fun el -> [ el ])
// Arb.generate<string> |> Gen.map (fun el -> if el <> null && el <> "" then [el] else ["ahoj"])


let wfTrees: Gen<FsTree> =
    let rec wfTrees' s' : Gen<FsTree> =
        gen {
            let! name_val = Arb.generate<string>

            match s' with
            | 0 -> return { name = name_val; children = [] }
            | n when n > 0 ->
                let! x = wfTrees' (n / 2) |> Gen.listOf
                return { name = name_val; children = x }
        }

    Gen.sized wfTrees'


(*
   Question 4

   Define an FsCheck property

   deleteIsWellFormed : Path -> FsTree -> bool

   which checks that given
   
   p  : Path
   fs : FsTree

   we have that the result of deleting p from fs is well-formed.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)

let deleteIsWellFormed (p: Path) (fs: FsTree): bool = fsTreeWf (delete p fs)


(*
   Question 5

   Define an FsCheck property

   createCreates : Path -> FsTree -> bool

   which checks that given

   p  : Path
   fs : FsTree

   we have that the path p is included (exactly once) in the
   result of show after we have created the directory p in fs.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)

let createCreates (p: Path) (fs: FsTree): bool = 
   show (create p fs) 
   |> List.filter(fun path -> path = p) |> List.length = 1


(*
   Question 6

   Define an FsCheck property

   deleteDeletes : Path -> FsTree -> bool

   which checks that given

   p  : Path
   fs : FsTree

   we have that the path p is not in the result of show after we have
   deleted the directory p from fs.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)

let deleteDeletes (p: Path) (fs: FsTree): bool = show (delete p fs) |> List.contains p = false


(*
   Question 7

   Define an FsCheck property

   showShowsEverything : FsTree -> bool

   which checks that given an
   
   fs : FsTree

   we have that by deleting one by one all of the items in the result
   of show fs we end up with an empty filesystem.

   You may assume that this property is only used with "well-formed"
   generators (meaning that fs is well-formed).
*)

let showShowsEverything (fs: FsTree): bool = isEmpty (show fs |> List.fold(fun state path -> delete path state) fs)


(*
   Question 8

   Define an FsCheck property

   createAndDelete : FsTree -> Path -> Path -> Property

   which checks that given
   
   fs : FsTree
   p1 : Path
   p2 : Path

   we have that, if p1 is not a prefix of p2, then

   1) creating directory p1 in fs
   2) creating directory p2 in the result
   3) deleting p1 from the result

   produces a filesystem that still contains p2.

   You may assume that this property is only used with "well-formed"
   generators (meaning that fs, p1 and p2 are well-formed).
*)

let rec isNotPrefix (p1:Path) (p2:Path) : bool =
   match p1, p2 with
   | [], [] -> false
   | x::xs, y::ys -> if x = y then isNotPrefix xs ys else true
   | _::_, [] -> true
   | [], _::_ -> false
   

let createAndDelete (fs: FsTree) (p1: Path) (p2: Path): Property = isNotPrefix p1 p2 ==> lazy List.contains p2 (show (delete p1 (create p2 (create p1 fs))))