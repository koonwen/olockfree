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
  let a1 = T.async pool (fun _ -> enquer q 0 499) in
  let a2 = T.async pool (fun _ -> enquer q 500 1000) in
  List.iter (fun p -> T.await pool p) [a1;a2]

let () =
  let pool = T.setup_pool ~num_additional_domains:2 () in
  let _ = T.run pool (fun _ -> test_fun pool) in
  let res = ref [] in
  for _ = 0 to 1000 do
    res := Lockfreeq.deq q :: !res
  done;
  Format.printf "%a" pp_int_option_list !res;
  T.teardown_pool pool;;