open Core.Std 

type t
val zero : t
val (+) : t -> t -> t
val of_int64 : Int64.t -> t
val of_string : string -> t
val compare : t -> t -> int
val to_string_hum : t -> string
val to_string : t -> string
val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t

