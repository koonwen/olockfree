open Olockfree
module T = Domainslib.Task

module Test_Lockfreeq = struct
  let enquer q ?(first=0) last = 
    for i = first to last do 
      Lockfreeq.enq q i
    done
  ;;

  let dequer q ?(first=0) last =
    for _ = first to last do
      match Lockfreeq.deq q with
      | None -> failwith "None"
      | Some v -> Printf.printf "%d\n" v
    done
  ;;

  let[@warning "-32"] pp_int_option_list = Fmt.list ~sep:Fmt.comma (Fmt.option Fmt.int)
  let [@warning "-32"] pp_int_list = Fmt.list ~sep:Fmt.comma Fmt.int

  let async_enq num q pool =
    let a1 = T.async pool (fun _ -> enquer q ~first:1 (num/2-1)) in
    let a2 = T.async pool (fun _ -> enquer q ~first:(num/2) num) in
    List.iter (fun p -> T.await pool p) [a1;a2]
  ;;

  let count queue =
    let rec aux n =
      match Lockfreeq.deq queue with
      | None -> n
      | Some _ -> aux (n+1)
    in
    aux 0
  ;;

  let q_to_list queue =
    let rec aux l =
      match Lockfreeq.deq queue with
      | None -> l
      | Some v -> aux (v :: l)
    in
    aux []
  ;;

  let verify_order num queue =
    let split = (num/2) - 1 in
    let rec aux prev_left prev_right =
      match Lockfreeq.deq queue with
      | None -> true
      | Some v -> 
        Printf.printf "%d, %d (%d)\n" prev_left prev_right v;
        if v <= split then
          if v = prev_left+1 then aux v prev_right else false
        else
          if v = prev_right+1 then aux prev_left v else false
      in
      aux 0 split
    ;;

  let test_len ~num () =
    let queue = Lockfreeq.create () in
    let pool = T.setup_pool ~num_additional_domains:2 () in
    T.run pool (fun _ -> async_enq num queue pool);
    T.teardown_pool pool;
    count queue
  ;;

  let test_speed ~num () =
    let queue1 = Lockfreeq.create () in
    let queue2 = Lockfreeq.create () in
    let seq_time = Utils.time_test (fun _ -> enquer queue1 (num-1)) in
    let par_time = Utils.time_test (fun _ -> enquer queue2 (num-1)) in
    par_time < seq_time

  let test_consistency ~num () =
    let queue = Lockfreeq.create () in
    let pool = T.setup_pool ~num_additional_domains:2 () in
    T.run pool (fun _ -> async_enq num queue pool);
    T.teardown_pool pool;
    verify_order num queue
  ;;
  
end

let test_queue_len () =
  Alcotest.(check int) "No dropped values" 10_000_000 (Test_Lockfreeq.test_len ~num:10_000_000 ())

let test_speed () =
  Alcotest.(check bool) "Parallel queueing faster than sequential" true (Test_Lockfreeq.test_speed ~num:10_000_000 ())

let test_consistency () =
  Alcotest.(check bool) "Order is sequentially consistent" true (Test_Lockfreeq.test_consistency ~num:10_000 ())


let () =
let open Alcotest in
run "Utils" [
    "Test_Lockfreeq", [ 
      test_case "Length test" `Quick test_queue_len; 
      test_case "Speed test" `Slow test_speed;
      test_case "Consistency test" `Slow test_consistency
      ];
  ]
