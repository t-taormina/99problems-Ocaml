let rec last list = 
  match list with 
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl;;

let rec last_two list = 
  match list with 
  | [] | [_] -> None
  | [x; y] -> Some(x, y)
  | _ :: tl -> last_two tl;;

let rec atindex list idx = 
  match list with 
  | [] -> None
  | h :: t -> if idx = 0 then Some h else atindex t (idx - 1) 

let length list = 
  let rec len list n = 
    match list with
    | [] -> n
    | _ :: tl -> len tl (n + 1)
  in 
  len list 0;;

let rev list = 
  let rl = [] in 
    let rec r l rl = 
      match l with 
      [] -> rl 
      | hd :: tl -> r tl (hd :: rl) 
    in 
    r list rl;;

let () = 
  let a = last ["a" ; "b" ; "c" ; "d"] in 
  assert (a = Some "d");
  let b = last_two ["a"; "b"; "c"; "d"] in 
  assert (b = Some ("c", "d"));
  let c = atindex ["a"; "b"; "c"; "d"; "e"] 3 in
  assert (c = Some "d");
  let d = length ["a"; "b"; "c"; "d"; "e"] in 
  assert (d = 5);
  let e = rev ["a"; "b"; "c"] in
  assert (e = ["c"; "b"; "a"]);
  ()
