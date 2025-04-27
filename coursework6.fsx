(*

  ITT8060 -- Advanced Programming 2024
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name:  Syed Muhammad Hamza Bacha
  Student ID:  246076IVSM
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is stated in Moodle.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function
  notAnyInList : (int -> float -> bool) -> (int * float) list ->  bool
  that returns true if the predicate passed as the first argument does not hold for any
  pair in the list passed as the second argument to the function.
  Make sure your implementation uses explicit tail recursion.

  e.g.

  notAnyInList (fun _ _ -> true) [(1,2.0);(3,4.0)] should return false.

  Tip: You can test in a F# project and add the
  [<TailCall>] attribute above the function to show expected tail call warnings.
  To create a F# project see:
  https://fsharp.pages.taltech.ee/tooling/debugging/
  For more reading on [<TailCall>], see:
  https://devblogs.microsoft.com/dotnet/safer-recursion-in-fsharp/

*)

let rec notAnyInList (predicate : int -> float -> bool) (lst : (int * float) list) : bool =
    match lst with
    | [] -> true
    | (a, b) :: rest -> 
        if predicate a b then 
            false
        else 
            notAnyInList predicate rest

(*
  Task 2:

  Write a function
  addHeadAsLastElementIf : 'a -> ('a -> bool) -> 'a list list -> 'a list list
  that takes a list of lists of 'a-s, takes the head if present of each list of 'a-s in the list of lists
  and adds it as the last element to the current list if the predicate function passed as the second argument
  evaluates to true. If the current list is empty, the previous element added  as the last element of the list
  should be added to the empty list. If the first list in the list of lists is empty , the default value passed as
  the first argument to the function should be added as the element of the list, if it satisfies the predicate function.
  Make sure your implementation uses explicit tail recursion.


  e.g. evaluating
  addHeadAsLastElementIf 0 (fun _ -> true) [[1;2;3];[]] should produce
  [[1;2;3;1];[1]]

  addHeadAsLastElementIf 0 (fun _ -> true) [[];[1;2;3]] should produce
  [[0];[1;2;3;1]]

*)

let addHeadAsLastElementIf (defaultValue: 'a) (predicate: 'a -> bool) (lists: 'a list list) : 'a list list =
    let rec loop (accum: 'a list list) (lastElem: 'a option) (remaining: 'a list list) : 'a list list =
        match remaining with
        | [] -> List.rev accum 
        | headList :: tail ->
            match headList with
            | [] ->
                let elementToAdd =
                    match lastElem with
                    | Some elem when predicate elem -> [elem] 
                    | None when predicate defaultValue -> [defaultValue] 
                    | _ -> [] 

                loop (elementToAdd :: accum) lastElem tail
            | h :: _ when predicate h ->
                let updatedList = headList @ [h]
                loop (updatedList :: accum) (Some h) tail
            | _ ->
                loop (headList :: accum) lastElem tail
    loop [] None lists

(*
  Task 3:

  Write a function
  addHeadAsLastElementIfFold : 'a -> ('a -> bool) -> 'a list list -> 'a list list
  that behaves similarly to the function defined in Task 2.
  Make sure your implementation uses List.fold or List.foldBack or its multiple
  argument conterparts appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)

let addHeadAsLastElementIfFold (defaultValue: 'a) (predicate: 'a -> bool) (lists: 'a list list) : 'a list list =
    let folder (accum: 'a list list * 'a option) (currentList: 'a list) =
        let (result, lastElem) = accum
        match currentList with
        | [] ->
            let elementToAdd =
                match lastElem with
                | Some elem when predicate elem -> [elem] 
                | None when predicate defaultValue -> [defaultValue]
                | _ -> [] 
            (elementToAdd :: result, lastElem) 
        | h :: _ when predicate h ->
            let updatedList = currentList @ [h]
            (updatedList :: result, Some h) 
        | _ ->
            (currentList :: result, lastElem) 
    let finalResult, _ = List.fold folder ([], None) lists
    List.rev finalResult

(*
  Task 4:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function maxAndMax2InTree : int Tree -> int * int that returns the maximum and and the second largest elements
  of the tree.
  Use continuation-passing style in your implementation. Flattening the tree to a list and processing the list is not considered
  a correct solution.

  If the input is a tree with a single element, then the second element in the returned pair should be the minimum value of int.

  If there are 2 instances of the same maximum value in the tree, then the two values should be returned as pair.
*)

type 'a Tree =
  | El of 'a
  | Br of 'a Tree * 'a Tree
  
let maxAndMax2InTree tree =
    let minValue = System.Int32.MinValue

    // Helper function to update the max and second max values
    let updateMax (currentMax, secondMax) value =
        if value > currentMax then (value, currentMax)
        elif value > secondMax then (currentMax, value)
        else (currentMax, secondMax)

    // Continuation-passing style function to traverse the tree
    let rec traverse tree continuation =
        match tree with
        | El x -> continuation (x, minValue)
        | Br (left, right) ->
            traverse left (fun (leftMax, leftSecondMax) ->
                traverse right (fun (rightMax, rightSecondMax) ->
                    // Combine results from left and right branches
                    let (max1, max2) = updateMax (updateMax (leftMax, leftSecondMax) rightMax) rightSecondMax
                    continuation (max1, max2)
                )
            )

    // Start the traversal
    traverse tree (fun result -> result)

