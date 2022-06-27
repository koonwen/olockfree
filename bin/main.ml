let rec incrementer n =
  Unix.sleep 1;
  Printf.printf "%d\n%!" n;
  incrementer (n+1)

let () =
  let d1 = Domain.spawn (fun _ -> incrementer 0) in
  let d2 = Domain.spawn (fun _ -> incrementer 10) in
  List.iter (fun d -> Domain.join d) [d1; d2];