module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : OrderedType) = struct
  type 'a t = Ord.t list

  let create () = Array.make 10
  let enq q x = failwith "not implemeted"
  let deq q = failwith "not implemented"
end

type 'a dll =
  | Node of 'a dll * 'a option * 'a dll
  | Empty

(* Need to implement a doubly linked list which uses a two terminals *)
type 'a t =
  { s_term : 'a ref
  ; e_term : 'a ref
  }
