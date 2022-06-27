open Olockfree
let q = Lockfreeq.create ()

let enquer q first last = 
  for i = first to last do 
    Lockfreeq.enq q i
  done 
;;

let [@warning "-32"] dequer q first last =
  for _ = first to last do
    match Lockfreeq.deq q with
    | None -> failwith "None"
    | Some v -> Printf.printf "%d\n" v
  done 
;;

let[@warning "-32"] pp_int_option_list = Fmt.list ~sep:Fmt.comma (Fmt.option Fmt.int)

module T = Domainslib.Task

let test_fun pool =
  let a1 = T.async pool (fun _ -> enquer q 1 499) in
  let a2 = T.async pool (fun _ -> enquer q 500 1000) in
  List.iter (fun p -> T.await pool p) [a1;a2];
  let rec aux n = 
    match Lockfreeq.deq q with
    | None -> n
    | Some _ -> aux (n+1) 
  in
  aux 0

let t1 () =
  let pool = T.setup_pool ~num_additional_domains:2 () in
  let res = T.run pool (fun _ -> test_fun pool) in
  T.teardown_pool pool;
  res

let test_queue_len () =
  Alcotest.(check int) "No dropped values" 1000 (t1 ())


let () =
let open Alcotest in
run "Utils" [
    "Lockfreeq", [ test_case "Length test" `Quick test_queue_len ];
  ]