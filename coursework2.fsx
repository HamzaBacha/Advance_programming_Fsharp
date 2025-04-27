(*

  ITT8060 -- Advanced Programming 2022
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion,
                combination of functions

  ------------------------------------
  Name:
  Tallinn University of Technology Student ID
  or Uni-ID:
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.taltech.ee repository itt8060-2023 under your UniID,
  into a file coursework2/coursework2.fsx.

  NB! Note that the solution has to be an F# script file that can in one go
  be loaded to the interpreter without any syntax errors!

  If the location, extension or name of the submission file or directory is incorrect
  it will not be graded.

  Deadline for submitting the solution is October 1, 2023.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. all first names are after comma separated by spaces)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number
//   of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// Note that this representation of BibliographyItem is used explicitly to familiarize you to tuples
// In the real world it'd be preferrable to use a Record type

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from
// https://dblp.uni-trier.de/
// Please note that you need not read the papers, just pick 7 papers that look
// interesting to you from the database.


// 2. Make a function compareLists : string list -> string list -> int that
// takes two string lists and returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!
//
// Hint:
// You can change the culture for testing with the following lines
// open System.Globalization
// CultureInfo.CurrentCulture <- CultureInfo("en-US")
//
// Fun fact:
// System.String.Compare and System.Text package members are affected by the
// CultureInfo settings, but System.Text.RegularExpressions
// uses ordinal ordering regardless of the CultureInfo settings.

// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use your solution from task 2.


// 4. Make a function
// compareAuthorsNumPages : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors
// and if the authors are the same then according to the number of pages in the publication.

// 5. Make a function
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.

// 6. Make a function
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in
// the publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.


// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.
//
// If the input list contains a bibliography item b which has a as an author, then
// the result list must contain a pair (a, xs) such that b is an element of xs.
//
// If the result list contains a pair (a, xs), then, for every bibliography item b in xs,
// it must be the case that a is an author of b and b was in the input list.
//
// There cannot be two items (a1, xs1) and (a2, xs2) in the result list such that a1 = a2.
//
// The number of pairs in the output list must be equal to the number of distinct authors in
// all bibliography items.
//
// The number of elements in xs in each item (a, xs) is equal to the number of BibliographyItems
// where a is among the authors.

