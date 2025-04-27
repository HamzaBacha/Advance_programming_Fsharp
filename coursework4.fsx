(*

  ITT8060 -- Advanced Programming 2024
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Working with HTML trees

  ------------------------------------
  Name: Syed Muhammad Hamza Bacha
  Tallinn University of Technology Student ID
  or Uni-ID: Student (regular) - 246076IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2024 under your name,
  into a file coursework4/coursework4.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.

  The deadline is given in Moodle.
*)


(*

Introduction
============

Hypertext Markup Language (HTML) is the standard markup language for
documents designed to be displayed in a web browser. Thus it is used
pervasively.

The task in this coursework is not to write a web application (i.e.,
generate HTML code) but instead work with data structures representing
HTML (like a web browser might have to).

The task is to have in mind the specification of HTML and build a
model for a simplified subset of functionalities supported by HTML, as
described below.

The current specification of HTML is given here:
https://html.spec.whatwg.org/multipage/dom.html#dom

If you are not familiar with HTML, then a basic tutorial is here:
https://www.w3schools.com/html

You have two different tasks in this coursework:
1) define a datatype HTree for representing HTML
2) define some functions operating on this datatype

We will consider only a simplified fragment of the HTML
specification. For example, the datatype HTree must be able to
represent the following HTML fragment:

<div id="root" class="sans-serif">
  <h1 class="bold red">
    This is a heading
  </h1>
  Some content between tags
  <p class="underline grey">
    Here is some text that is a paragraph
  </p>
  Some other content between tags
  <div id="a-child-div">
    <div class="this is a div in a div">
      Some content for this div
    </div>
  </div>
</div>

*)




// You are given the following types:

// The HTML tags that we consider in this coursework
type Kind = Div  // think <div>
          | Par  // think <p>
          | Hdg  // think <h1>


// The type of things that can go into the attribute field 'class' of
// a tag
type Class = string

// The type of the thing that can go in the attribute field 'id' of a
// tag
type Identifier = string

// The type of paths in an HTree
type Path = Kind list



// 0. Define data structure(s) for representing HTML

// Note that HTML can be represented very naturally as a tree
// structure.

// Therefore, your 0th task is to define a type HTree for
// representing HTML.

// Before defining HTree, familiarize yourself with the tasks that
// follow. The type HTree you define must support the operations you are
// asked to define later. Furthermore, if you pick a bad representation,
// then it can be much more cumbersome to solve the later tasks.

// Here are some hints.
// A value of type HTree must "know":
// - what kind of HTML tag it is representing
// - what are the class names attached to it
// - if it has an identifier attached to it and what is then the identifier
// - what is the content of this tag (the stuff between the opening and closing tags)
// - note that the content can also include other HTML tags, i.e.,
//   HTree should be recursive (either directly or indirectly)

type HTree =
   | Node of Kind* Identifier option  * Class list * HTree list  
   | Text of string  





// 1. Define a function
//
// mkNode : Kind -> Identifier option -> HTree
//
// that can be used to create values of type HTree given a kind and an
// optional identifier.
//
// For example, if we would like to represent the node
//
//  <p id="example"></p>
//
// then we would do
//
//  mkNode Par (Some "example")
//
// and for
//
//  <div></div>
//
// we would do
//
//  mkNode Div None

let mkNode (kind: Kind) (id: Identifier option) : HTree =
    Node (kind, id, [], [])




// 2. Define the following functions for adding things to a node:
//
//   addClass : Class -> HTree -> HTree
//
//   addContent : string -> HTree -> HTree
//
//   addChild : HTree -> HTree -> HTree
//
// Let   div : HTree   represent the following HTML
//
//   <div class="abc">content text</div>
//
// Then
//
//   addClass "xyz" div
//
// must result in an HTree representing
//
//   <div class="abc xyz">content text</div>
//
// and
//
//   addContent "; another content" div
//
// must result in an HTree representing
//
//   <div class="abc">content text; another content</div>
//
// and
//
//   addChild (mkNode Hdg None) div
//
// must result in an HTree representing
//
//   <div class="abc">content text<h1></h1></div>
//
// Furthermore, doing
//
//   let hdg = addContent "This is the h1 content" (mkNode Hdg None)
//
//   div
//   |> addChild hdg
//   |> addContent "Here is the content added after adding the child node h1"
//
// must result in an HTree representing
//
//   <div class="abc">content text<h1>This is the h1 content</h1>Here is the content added after adding the child node h1</div>

let addClass (cls: Class) (tree: HTree) : HTree =
    match tree with
    | Node (kind,  id, classes,children) -> 
        Node (kind,id, cls :: classes,children)
    | Text _ -> tree  // No change if input is content node

let addContent (content: string) (tree: HTree) : HTree =
    match tree with
    | Node (kind, id, classes, children) -> 
        Node (kind, id, classes, children @ [Text content])
    | Text _ -> tree

let addChild (child: HTree) (tree: HTree) : HTree =
    match tree with
    | Node (kind, id, classes, children) -> 
        Node (kind, id, classes, children @ [child])
    | Text _ -> tree

// 3. Define the function
//
// countNodes : HTree -> int
//
// that counts the number of nodes in the given HTree. Note that an HTree
// without any child nodes consists of exactly one node: itself.

let rec countNodes (tree: HTree) : int =
    match tree with
    | Node (_, _, _, children) ->
        // Count the current node (1) + recursively count nodes in children
        1 + List.sumBy countNodes children
    | Text _ -> 0  // Text nodes are not counted



// 4. Define the function
//
// listNodes : HTree -> Path list
//
// that for the given HTree returns a list of paths that are the full
// paths to the nodes in the HTree.
// Note that:
// - the path to the given HTree (the root node) is the empty list
//
// - given an HTree representing
//
//    <div>
//      <h1>The heading</h1>
//      <div class="content">
//        <p>This is a paragraph <div id="goal">?</div> right here</p>
//      </div>
//    </div>
//
// the path to the node with id "goal" is [Div; Div; Par].
//
// The order of paths in the result list must respect the order of
// nodes in the HTree. More precisely, in the result list:
// - a path to a node must appear before the paths to its children
// - a path to a node must not appear before the paths to its older
//   siblings and their descendants
//
// Node A is a child node of B if A was added (using addChild) to B.
//
// If A and B are both child nodes of the same node, then we say that:
// - they are sibling nodes
// - A is older than B if A was added (using addChild) to their
//   parent node before B was added

let rec listNodes (tree: HTree) : Path list =
    let rec helper (tree: HTree) (currentPath: Path) : Path list =
        match tree with
        | Node (kind, _, _, children) ->
            let pathToCurrentNode = [currentPath @ [kind]]
            printfn "Current node: %A, Path: %A" kind (currentPath @ [kind])  // Debugging output
            let childPaths = 
                children
                |> List.collect (fun child -> helper child (currentPath @ [kind]))
            pathToCurrentNode @ childPaths
        | Text content ->
            printfn "Content: %s, Current path: %A" content currentPath  // Debugging output for content
            []
    helper tree []





// 5. Define the function
//
// showContent : HTree -> string
//
// that produces the concatenation of the string content in the given HTree.
//
// The content must be shown following the ordering of things in the HTree.
// In particular, if we do
//
//   let h = mkNode Div None
//           |> addContent "A"
//           |> addChild (addContent ";P1;" (mkNode Par None))
//           |> addContent "B"
//           |> addChild (addContent ";P2;" (mkNode Par None))
//           |> addContent "C"
//
// then evaluating
//
//   showContent h
//
// should result in
//
//   "A;P1;B;P2;C"

let rec showContent (tree: HTree) : string =
    match tree with
    | Node (_, _, _, children) ->
        // Concatenate from all children
        List.map showContent children |> String.concat ""
    | Text text ->
        // Return the text
        text





// 6. Define the function
//
// deleteNode : Identifier -> HTree -> HTree option
//
// that removes the node with the given identifier from the given
// tree.
//
// You may assume that there is at most one node with the given
// identifier in the tree.
//
// Removing a node means removing the entire subtree rooted at that node.

let rec deleteNode (id: Identifier) (tree: HTree) : HTree option =
    match tree with
    | Node (kind, Some nodeId, classes, children) when nodeId = id ->
        // Node found, return None to indicate deletion
        None
    | Node (kind, nodeId, classes, children) ->
        // Recurse into children to find the node to delete
        let updatedChildren = 
            children
            |> List.choose (fun child -> deleteNode id child)  // Filter out the deleted nodes
        // Return the updated tree with remaining children
        Some (Node (kind, nodeId, classes, updatedChildren))
    | Text _ -> 
        // Content nodes don't have identifiers, return as is
        Some tree





// 7. Define the function
//
// nodesWithClass : Class -> HTree -> HTree list
//
// that returns a list of those nodes in the tree that have the given
// class attached to them.
//
// The order of nodes in the result list must respect the order of
// nodes in the HTree. More precisely, in the result list:
// - a node must appear before its descendants
// - a node must not appear before its older siblings and their descendants

let rec nodesWithClass (cls: Class) (tree: HTree) : HTree list =
    match tree with
    | Node (kind, idOpt, classes, children) ->
        // Check if the current node has class
        let currentNode =
            if List.contains cls classes then 
                [tree]
            else 
                []

        // Recursively check children and concatenate
        let childrenWithClass = 
            children 
            |> List.collect (fun child -> nodesWithClass cls child)

        // Combine the results
        currentNode @ childrenWithClass
    | Text _ -> 
        []



