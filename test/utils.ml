let time_test f =
  let t1 = Unix.time () in
  f ();
  let t2 = Unix.time () in
  t2 -. t1