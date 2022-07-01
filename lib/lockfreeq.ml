(* Internal doubly linked list node structure
type 'a dll =
  | Null
  | Node of
      { mutable prev : 'a dll
      ; payload : 'a
      ; mutable next : 'a dll
      }

(* Queue uses a head and tail terminal *)
(* TODO: Add size *)
type 'a t =
  { mutable hd_term : 'a dll
  ; mutable tl_term : 'a dll
  ; mutex : Mutex.t
  }

let create () = { hd_term = Null; tl_term = Null; mutex = Mutex.create ()}

let enq q x =
  Mutex.lock q.mutex;
  (match q.tl_term with
  | Null ->
    let new_node = Node { prev = Null; payload = x; next = Null } in
    q.hd_term <- new_node;
    q.tl_term <- new_node
  | Node payload as n ->
    let new_node = Node { prev = n; payload = x; next = Null } in
    payload.next <- new_node;
    q.tl_term <- new_node);
  Mutex.unlock q.mutex;
;;

let deq q =
  Mutex.lock q.mutex;
  let res =
    match q.hd_term with
    | Null -> None
    | Node n ->
      q.hd_term <- n.next;
      Some n.payload
    in
  Mutex.unlock q.mutex;
  res
;; *)

type 'a node = 
| Null
| Node of {
  payload : 'a option;
  mutable next : 'a node
}

type 'a t = {
  mutable hd_sentinel: 'a node Atomic.t;
  mutable tl_sentinel: 'a node Atomic.t;
  mutable size: int
}

let create =
  let dummy_node = Node {payload = None; next = Null} |> Atomic.make in
  {hd_sentinel = dummy_node; tl_sentinel = dummy_node; size = 0}

let get_next node = function 
| Node record -> record.next 
| Null -> failwith "Null variant has no next"

let equal_node n1 n2 =
  match n1, n2 with
  | Null, Null -> true
  | Node r1, Node r2 -> r1 == r2
  | _ -> false

let enq q payload = 
  let flag = ref true in
  while !flag do
    let new_node = Node {payload = Some payload; next = Null} in
    let last = q.tl_sentinel |> Atomic.get in
    let next = get_next last in
    (* Check if the truly at the tail *)
    if next = Null then 
      (* Try to add the new node into the queue *)
      if Atomic.compare_and_set (get_next last) next new_node then
        (* Try to update the tail sentinel. No worries if it fails, means some other thread helped out! *)
        let _ = Atomic.compare_and_set tl_sentinel last new_node in 
        flag := false;
      else
        (* Another thread installed their node first, help them complete the task *)
        let _ = Atomic.compare_and_set tl_sentinel last next in ();
  done

let deq q = failwith ""