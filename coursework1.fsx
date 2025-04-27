

List.filter (fun n -> n % 3 = 0) [3;9;1;8;4]

List.fold (>>) id [(-) 3; (*) 2] 1

(lazy (1,2))


Option.bind Some (Some(Some (None : bool option)))

Option.bind Some  (None : bool option)

let rec s = seq {yield 3; yield! Seq.map (fun n -> n) s} 

let rec g x y =
    match y with
    | [] -> 1
    | h :: t -> x h + g x t


List.collect (fun x -> [x + x]) [2; 3; 5] 

List.reduce (*) [1]

let rec f x =
    seq{
    yield x
    yield! f (x)
    }


let rec c b = 
    match b with
    |[]     -> []
    |d :: e -> (c [(List.head e)])


let rec c b = 
   match b with
   | [] -> []
   | d :: e -> e :: (c (List.map id e))

["a",1;"b",3] |> List.map (id >> fst) 

let failingWorkflow =
    async { do failwith "fail" }
Async.RunSynchronously failingWorkflow


List.collect (fun x -> x ) [2; 3; 5]

Seq.unfold (fun x -> None: (int * int) option) 1

(printf "X"; (fun n -> printf "Y"; n + n)) (printf "Z"; 2)

List.collect (fun x -> [x + x]) [2; 3; 5]

List.reduce (-) [1;2;3;4] 

let foo x = printf "f"; 2 * x
let bar x = printf "b"; [x; x]

[1..5] |> List.map foo |> List.take 3 |> List.collect bar


let rec generate n a b f =
    if n <= 0 then
        []
    elif n = 1 then
        [a]
    elif n = 2 then
        [a; b]
    else
        a :: generate (n - 1) b (f a b) f

let fibonacci = generate 8 0 1 (+)
printfn "%A" fibonacci



let applyEvens f lst =
    let _, result =
        List.fold 
            (fun (index, acc) x ->
                if index % 2 = 0 then
                    (index + 1, acc @ [f x]) // Apply `f` to even index elements
                else
                    (index + 1, acc @ [x])   // Keep odd index elements unchanged
            )
            (0, []) // Initial accumulator: position = 0, empty list
            lst
    result

let result = applyEvens (fun x -> x * 2) [1; 2; 3; 4; 5]
printfn "%A" result // Output: [2; 2; 6; 4; 10]


let rec f x y =
    match y with
       | [] -> None
       | (x’, z’, s’) :: w -> if x’ = x then
                               Some (z’,s’)
                             else
                                f x w






