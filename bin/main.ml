(* open Olockfree

let queue = create ()

let rec producer n =
  if n = 0
  then ()
  else (
    enq queue n;
    Format.printf "Produced %d\n%!" n;
    producer (n - 1))
;;

let rec consumer n acc =
  if n = 0
  then ()
  else (
    match deq queue with
    | None -> consumer n acc
    | Some v ->
      Format.printf "Consumed %d\n%!" v;
      consumer (n - 1) (n + acc))
;;

let () =
  let d1 = Domains.spawn (fun _ -> producer 10) in
  let d2 = Domains.spawn (fun _ -> consumer 10) in
  Domains.join d1;
  Domains.join d2
;; *)

(* let con = Condition.create ()
let mutex = Mutex.create ()
let flag = ref true

let rec r_waiter () =
  Mutex.lock mutex;
  match !flag with
  | true ->
    Condition.wait con mutex;
    r_waiter ()
  | false ->
    Mutex.unlock mutex;
    "DONE"
;;

let signaler () =
  Mutex.lock mutex;
  flag := false;
  Condition.signal con;
  Mutex.unlock mutex
;;

let () =
  let d1 = Domain.spawn (fun _ -> r_waiter ()) in
  let d2 = Domain.spawn (fun _ -> signaler ()) in
  Domain.join d1 |> Format.printf "%s"
;; *)

let n = try int_of_string Sys.argv.(1) with _ -> 10

module Atomic_stack : sig
  type 'a t
  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop  : 'a t -> 'a
end = struct
  type 'a t = {
    mutable contents: 'a list;
    mutex : Mutex.t;
    condition : Condition.t
  }

  let make () = {
    contents = [];
    mutex = Mutex.create ();
    condition = Condition.create ()
  }

  let push r v =
    Mutex.lock r.mutex;
    r.contents <- v::r.contents;
    Condition.signal r.condition;
    Mutex.unlock r.mutex

  let rec pop r = 
    Mutex.lock r.mutex;
    match r.contents with
    | [] ->
      Condition.wait r.condition r.mutex;
      pop r
    | h :: t ->
      r.contents <- t;
      Mutex.unlock r.mutex;
      h
end

let s = Atomic_stack.make ()

let rec producer n =
  if n = 0 then ()
  else begin
    Atomic_stack.push s n;
    Format.printf "Produced %d\n%!" n;
    producer (n-1)
  end
  
let rec consumer n acc =
  if n = 0 then acc
  else begin
    let v = Atomic_stack.pop s in
    Format.printf "Consumed %d\n%!" v;
    consumer (n-1) (n + acc)
  end
    
let main () =
  let c = Domain.spawn (fun _ -> consumer n 0) in
  let p = Domain.spawn (fun _ -> producer n) in
  Domain.join p;
  assert (Domain.join c = n * (n+1) / 2)

let _ = main ()