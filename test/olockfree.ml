open Olockfree

let q = Lockfreeq.create ()

let () =
  Lockfreeq.enq q "hello";
  Lockfreeq.enq q "wassup";
  Lockfreeq.enq q "I dont think this will work";
  print_endline @@ (Lockfreeq.deq q |> Option.get);
  print_endline @@ (Lockfreeq.deq q |> Option.get);
  print_endline @@ (Lockfreeq.deq q |> Option.get)
;;
