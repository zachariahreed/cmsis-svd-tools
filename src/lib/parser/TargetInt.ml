open Core.Std 

(* FIXME - capture the actual width of the value instead of assuming 64 bits everywhere *)

type t = Int64.t

let zero = 
  Int64.of_int 0

let (+) u v = 
  Int64.(u + v)

let of_int64 v = 
  v

let of_string = 
  Int64.of_string

let compare = 
  Int64.compare

let to_string_hum =
  Printf.sprintf "0x%LX"

let to_string = 
  Int64.to_string

let sexp_of_t v = 
  Sexp.Atom (to_string_hum v)

let t_of_sexp v = 
  failwith "unimplemented"

