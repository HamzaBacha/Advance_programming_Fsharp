(*

  ITT8060 -- Advanced Programming 2024
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Querying HTML trees

  ------------------------------------
  Name: Syed Muhammad Hamza Bacha
  Tallinn University of Technology Student ID
  or Uni-ID: 246076IVSM
  ------------------------------------


  Answer the questions below. You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to your
  https://gitlab.cs.taltech.ee repository itt8060-2024 into a file
  coursework5/coursework5.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.
  
  Your solution must not use mutable state and imperative features
  like for loops.
*)


(*

For introduction to HTML please check coursework4.fsx for references.

In this coursework we continue with the topic of trees and HTML. This
time the task is, given a description of a set of nodes in an HTree,
to retrieve, modify and delete those nodes. This is a bit similar
to questions 6 and 7 in coursework 4.

The following material of basic definitions is taken from CW4. You are
free to use your solution to CW4 or try something different.

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




(*

0. Define data structure(s) for representing HTML

  type HTree = 
      | Node of Kind * Identifier option * Class list * string option * HTree list


Define a function

  mkNode : Kind -> Identifier option -> HTree

that can be used to create values of type HTree given a kind and an
optional identifier.




Define the following functions for adding things to a node:

  addClass : Class -> HTree -> HTree

  addContent : string -> HTree -> HTree

  addChild : HTree -> HTree -> HTree

*)

type HTree = 
      Text of string
    | Tag of Kind * Identifier option * Class list * HTree list

let mkNode (kind : Kind) (id : Identifier option) : HTree =
    Tag(kind, id, [], [])

let addClass (c: Class) (t: HTree): HTree = 
    match t with 
    | Tag(kind, id, classes, children) ->
        Tag(kind, id, c :: classes, children)
    | _ -> t

let addContent (s: string) (t: HTree): HTree = 
    match t with
    | Tag(kind, id, classes, children) ->
        Tag(kind, id, classes, children @ [Text(s)])
    | Text(content) -> Text(content + " " + s) 

let addChild (newElem: HTree) (t: HTree): HTree = 
    match t with
    | Tag(kind, id, classes, children) ->
        Tag(kind, id, classes, children @ [newElem])
    | _ -> t




////////////////////////////////////////////////////////////////////////
// The content of this coursework begins here //////////////////////////


// You are given a type of expressions.

type BExpr = True
           | Not      of BExpr
           | And      of BExpr * BExpr
           | Or       of BExpr * BExpr
           | OfKind   of Kind
           | HasId    of Identifier
           | HasClass of Class

(*

The type BExpr is just a discriminated union. The name 'BExpr' stands
for 'Boolean expression'. The intended interpretation of values of
type BExpr is as predicates on values of type HTree. (Recall that a
predicate is a Boolean-valued function.)

 - True: evaluates to true on any HTree

 - Not b: evaluates to true on precisely those HTree for which b
          evaluates to false

 - And (b1, b2): evaluates to true on precisely those HTree for which
                 both b1 and b2 evaluate to true

 - Or (b1, b2): evaluates to true on precisely those HTree for which
                b1 or b2 evaluates to true

 - OfKind k: evaluates to true on precisely those HTree that are of
             kind k (i.e., the root node of the tree is a k)

 - HasId i: evaluates to true on precisely those HTree that have an
            identifier and this identifier is equal to i (i.e., the
            root node has i)

 - HasClass c: evaluates to true on precisely those HTree that have
               the class c attached to them (i.e., the root node has
               c)

*)



// Here is a type of selector expressions.

type Selector = Match     of BExpr
              | Sequence  of Selector * Selector
              | OneOrMore of Selector


(*

The type Selector is just a discriminated union. The intended
interpretation of values of type Selector is as sets of nodes in an
HTree. We also refer to the set of nodes described by s : Selector as
the set of nodes selected by s.

We can think of Selector as describing a simple query language. As
Selector has three constructors, there are three ways to query an
HTree.

 - Match b: selects the singleton set consisting of the root node if
            the expression b evaluates to true (on the root
            node). Otherwise selects the empty set.

 - Sequence (s, s'): selects those nodes in the tree that are selected
            by the selector s' from any child node of a node that is
            selected by s (starting from the root of the tree). Hence
            any node in the result must be selected by s', but not
            starting from the root node, but from any child of a node
            that is selected by s.

            How to compute the result?

            1. Starting from the root, compute the set of nodes
               selected by s (these are not included in the result).

            2. For every child c of a node from the previous step,
               compute the set of nodes selected by s' (starting from
               c). The result is the union of all such sets.

 - OneOrMore s: selects those nodes in the tree that are selected by s
                (starting from the root) and also those that are
                selected by (OneOrMore s) starting from any child node
                of a node selected by s.

                Thus you can think of the nodes selected by
                (OneOrMore s) as the union of the following sets:
                - nodes selected by s
                - nodes selected by Sequence (s, OneOrMore s)

*)






(*

1. Translate the following informal descriptions into values of type
BExpr and Selector.

Define the values b1, b2 and b3 of type BExpr so that:

  - b1 evaluates to true on those HTree that are Div nodes with
    classes "blue" and "left" attached but do not have the class "red"
    attached to them.

  - b2 evaluates to true on those HTree that are Hdg nodes with
    identifier "main" or that are Par nodes with class "left" or
    "right" attached to them.

  - b3 evaluates to true on those HTree whose identifier is not in the
    set {"i1", "i2", "i3"} but the intersection of this set with the
    set of classes of the HTree is nonempty.

Define the values s1, s2 and s3 of type Selector so that:

  - s1 selects all Par nodes that are at depth 3

  - s2 selects all Hdg nodes h such that h is a child node of some
    node and all of the ancestors of h have the class "xyz"

  - s3 selects all Par nodes p such that:
    * p is a child node of a node q
    * q is not a Par node
    * q is the root node

We consider the root node to be at depth 1.

*)


// Boolean expressions
let b1 = And (OfKind Div, And (HasClass "blue", And (HasClass "left", Not (HasClass "red"))))
let b2 = Or (And (OfKind Hdg, HasId "main"), And (OfKind Par, Or (HasClass "left", HasClass "right")))
let b3 = And (Not (Or (HasId "i1", Or (HasId "i2", HasId "i3"))), Or (HasClass "i1", Or (HasClass "i2", HasClass "i3")))

// Selectors
let s1 = Sequence (Sequence (Match True, Match True), Match (OfKind Par))
let s2 = Sequence (OneOrMore (Match (HasClass "xyz")), Match (OfKind Hdg))
let s3 = Sequence (Match (Not (OfKind Par)), Match (OfKind Par))


(*

2. Define the function

  eval : BExpr -> HTree -> bool

which evaluates the given expression on the given tree.

Note that `eval b : HTree -> bool` is a predicate on HTrees.

Evaluating a BExpr only considers properties of the root node of the
tree. Its child nodes and content is ignored.

In other words, for any b : BExpr and t : HTree, it should be that

  eval b t = eval b t'

where t' is t with all of its child nodes and text content removed.

*)
let rec eval (b: BExpr) (t: HTree) : bool = 
    match b with
    | True -> true
    | Not expr -> not(eval expr t)
    | And(expr1 , expr2) -> (eval expr1 t) && (eval expr2 t)
    | Or(expr1, expr2) -> (eval expr1 t) || (eval expr2 t)
    | OfKind k -> 
        match t with
        | Text _ -> false
        | Tag(kind, _, _, _) -> k = kind
    | HasId ident ->
        match t with
        | Text _ -> false
        | Tag(_, id, _, _) -> (Option.isSome id) && id.Value = ident 
    | HasClass c -> 
        match t with
        | Text _ -> false
        | Tag(_, _, classes, _) -> List.contains c classes







(*

3. Define the function

  select : Selector -> HTree -> (Path * HTree) list

that computes the set of nodes in the given HTree described by the
given Selector. The result is a list of pairs where the second
component is a selected node and the first component is the full path
to that node. (The path to the root node is the empty list.)

The order of nodes in the result list must respect the order of nodes
in the given HTree. More precisely, in the result list:
  - a node must appear before its children
  - a node must not appear before its older siblings and their
    descendants

A node should not be selected more than once.

This is similar to evaluating a BExpr on an HTree. The difference is
that instead of a BExpr we have a Selector and instead of a bool we
compute a (Path * HTree) list. In this case we also consider child
nodes.


Here is one possible strategy to consider.

Observe that every (finite) `s : Selector` contains at least one
`Match b` (s can be seen as a tree and Match is the constructor for
nodes with no children). Furthermore, there is a Match that is the
leftmost (or first) in the Selector.

Why are we interested in the leftmost Match? (More precisely, the
BExpr of the Match.)

It may be useful to extract from the given Selector the leftmost Match
and also the rest of the Selector (corresponding to the part without
the leftmost Match). You can start with an operation like this:

  extract : Selector -> BExpr * Selector option

Why the result type has `Selector option` and not just Selector?

*)
let rec canonicalise (selector: Selector): Selector = 
  match selector with
  | Match expr -> Match expr // match can not be "simplified"
  | Sequence (s1,s2) -> // for sequences, we want to move match to the front
    match s1, s2 with
    | Match _, _ -> Sequence (s1, canonicalise s2) // if seq(match, other) then we canonicalise other
    | Sequence _, Match _ -> Sequence (s2, canonicalise s1) // if seq(seq, match) then swap order and canonicalise inner seq
    | Sequence _, _ -> Sequence(canonicalise s1, canonicalise s2)
    | OneOrMore _, Match _ -> Sequence(s2, canonicalise s1) // put match in the front and canonicalise OneOrMore
    | _, _ -> Sequence(canonicalise s1, canonicalise s2)
  | OneOrMore(s) -> 
    match s with
    | Match _ -> OneOrMore s // OneOrMore(Match _) can be kept as is
    // nested OneOrMores can be removed 
    // OneOrMore(OneOrMore(Match true)) selects all nodes, 
    // OneOrMore(Match true) does the same thing but without duplicates
    | OneOrMore _ -> canonicalise s 
    | Sequence _ -> OneOrMore (canonicalise s) // in case of OneOrMore(Sequence(_,_)) we canonicalise the sequence

let rec select (selector : Selector) (tree : HTree) : (Path * HTree) list =
  let selector = canonicalise selector
  match selector with
  | Match b -> if eval b tree then [([], tree)] else []
  | Sequence (s1, s2) ->
    let selectFromChildren path node =
      match node with
      | Text _ -> []
      | Tag (k,_,_,ch) ->
          let childPaths = List.map (fun t -> k :: path, t) ch
          childPaths
          |> List.collect (fun (childPath, child) ->
              select s2 child 
              |> List.map (fun (p: Path, n: HTree) -> (childPath @ p, n))
          )
    select s1 tree
    |> List.collect (fun (path: Path, node: HTree) -> selectFromChildren path node)

  | OneOrMore s ->
      let rec selectRecursive (parentPath: Path) (node: HTree) =
          let sSelected= select s node // [], root
          let sSelected = List.map (fun (p, t) -> (parentPath @ p, t)) sSelected
          let childrenOfSelected = 
            List.collect (fun (p,t) -> 
                            match t with
                            | Text _ -> []
                            | Tag(k,_,_,ch) ->
                              List.map (fun child -> (p @ [k], child)) ch) sSelected
          let sFromChildren = List.collect (fun (p,t) -> selectRecursive p t) childrenOfSelected
          sSelected @ sFromChildren
      selectRecursive [] tree



let c3 = Tag(Div, Some("left left"), [], [])
let c4 = Tag(Div, Some("left right"), [], [])
let c1 = Tag(Div, Some("left"), [], [c3;c4])
let c2 = Tag(Div, Some("right"), [], [])
let t = Tag (Div, Some("root"), [], [c1;c2])

select (OneOrMore (Match True)) t




// Here is a type collecting some of the information that can be
// attached to a node in an HTree.

type NodeInfo = Kind
              * Identifier option
              * Class list        // the classes attached to the node, as a list


(*

4. Define the function

  update : Selector -> (NodeInfo -> NodeInfo) -> HTree -> HTree

such that

  update s f t

evaluates to an HTree that is otherwise the same as t except that, for
the nodes of t selected by s, the information attached to the nodes
has been updated according to the function f.

*)
let rec helper (selected: HTree list) (f: NodeInfo -> NodeInfo) (t: HTree): HTree = 
    match List.contains t selected with
    | true ->   
      match t with
      | Text _ -> t
      | Tag(k,i,c, ch) ->
          let (nK, nI, nC) = f (k, i, c)
          Tag(nK, nI, nC, List.map (helper selected f) ch)
    | false ->
       match t with
       | Text _ -> t
       | Tag(k, i, c, ch) ->
        Tag(k, i, c, List.map (helper selected f) ch) 

let update (s: Selector) (f: NodeInfo -> NodeInfo) (t: HTree) : HTree = 
    let selected = List.map (fun (_, t) -> t) (select s t)
    helper selected f t




(*

5. Define the function

delete : Selector -> HTree -> HTree option

which removes from the given HTree all nodes that are selected by the
given Selector. Removing a node means removing the entire subtree
rooted at that node.

*)

let rec delete (s: Selector) (t: HTree) : HTree option = 
    match t with
    | Text _ -> Some(t)
    | Tag(k,i,cl,ch) ->
      let selected = (select s t) 
      let selected = List.map (fun (_, t) -> t) selected
      if List.contains t selected
      then None
      else 
        let children = List.map (delete s) ch
        let children = List.filter (fun (e: HTree option) -> Option.isSome e) children
        let children = List.map (fun (e: HTree option) -> e.Value) children
        Some(Tag(k, i, cl, children))



(*

6. Using the function update, define the functions

  addClassTo : Selector -> Class -> HTree -> HTree

and

  removeClassFrom : Selector -> Class -> HTree -> HTree

so that

  addClassTo s c t

evaluates to a tree that is otherwise the same as t except that the
nodes selected by s have the class c attached to them and

  removeClassFrom s c t

evaluates to a tree that is otherwise the same as t except that the
nodes selected by s do not have the class c attached to them.

These functions should not be defined recursively; define them in
terms of update.

*)

let addClassTo (s: Selector) (c: Class) (t: HTree) : HTree = 
    update s (fun (k,i,cl) -> (k, i, c :: cl)) t

let removeClassFrom (s: Selector) (c: Class) (t: HTree) : HTree = 
    update s (fun (k,i,cl) -> (k, i, List.filter (fun e -> not(e = c)) cl)) t



(*

7. Using the function update, define the function

  mapHTree : (NodeInfo -> NodeInfo) -> HTree -> HTree

such that

  mapHTree f t

evaluates to a tree obtained by updating every node in the tree
according to f.

This function should not be defined recursively; define it in terms of
update.

*)
let mapHTree (f: NodeInfo -> NodeInfo) (t: HTree): HTree = 
    update (OneOrMore(Match True)) f t
