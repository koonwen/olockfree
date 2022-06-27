open Olockfree
module T = Domainslib.Task

module Test_Lockfreeq = struct
  let enquer q ?(first=0) last = 
    for i = first to last do 
      Lockfreeq.enq q i
    done 
  ;;
  let [@warning "-32"] dequer q ?(first=0) last =
  for _ = first to last do
    match Lockfreeq.deq q with
    | None -> failwith "None"
    | Some v -> Printf.printf "%d\n" v
  done 
;;
let[@warning "-32"] pp_int_option_list = Fmt.list ~sep:Fmt.comma (Fmt.option Fmt.int)

let test_len ~num () =
  let queue = Lockfreeq.create () in
  let wrapper pool () =
    let a1 = T.async pool (fun _ -> enquer queue (num/2-1)) in
    let a2 = T.async pool (fun _ -> enquer queue (num/2-1)) in
    List.iter (fun p -> T.await pool p) [a1;a2];
    let rec count n = 
      match Lockfreeq.deq queue with
      | None -> n
      | Some _ -> count (n+1) 
    in
    count 0
  in
  let pool = T.setup_pool ~num_additional_domains:2 () in
  let res = T.run pool (wrapper pool) in
  T.teardown_pool pool;
  res

  let test_speed ~num () =
    let queue1 = Lockfreeq.create () in
    let queue2 = Lockfreeq.create () in
    let seq_time = Utils.time_test (fun _ -> enquer queue1 (num-1)) in
    let par_time = Utils.time_test (fun _ -> enquer queue2 (num-1)) in
    par_time < seq_time

end

let test_queue_len () =
  Alcotest.(check int) "No dropped values" 10_000_000 (Test_Lockfreeq.test_len ~num:10_000_000 ())

let test_speed () =
  Alcotest.(check bool) "Parallel queueing faster than sequential" true (Test_Lockfreeq.test_speed ~num:10_000_000 ())

let () =
let open Alcotest in
run "Utils" [
    "Test_Lockfreeq", [ 
      test_case "Length test" `Quick test_queue_len; 
      test_case "Speed test" `Quick test_speed];
  ]