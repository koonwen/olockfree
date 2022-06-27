(* Internal doubly linked list node structure *)
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
;;
