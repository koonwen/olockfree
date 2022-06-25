(* This queue is designed to be used in a concurrent setting. 
   It preserves sequential consistency amongst competing threads 
   and is implemented without locks! *)

(** type representing the lock free queue *)
type 'a t

(** [create ()] creates and returns a lock free queue *)
val create : unit -> 'a t

(** [enq q x] inserts the element [x] in FIFO fashion into the queue. 
    This queue is likely unsed in a concurrent setting and therefore
    be mindful that only sequential consistency amongst threads are guranteed*)
val enq : 'a t -> 'a -> unit

(** [deq q] removes the first element from the queue in FIFO fashion. 
    This queue is likely unsed in a concurrent setting and therefore
    be mindful that only sequential consistency amongst threads are guranteed*)
val deq : 'a t -> 'a
