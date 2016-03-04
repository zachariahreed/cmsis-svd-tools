(* vim: set foldmethod=marker: *)

open Core.Std
open SVD

(* ********************************************************************************************* *)
module NPath : sig

  type t
  val root : t
  val hash : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val to_string_hum : t -> string
  val append : t -> string -> t
  val parent : t -> t option
  val canonicalize : t -> t -> t
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t

end = struct

  (* {{{ *)

  type t = string list

  let root = []

  let rec hash = function
    | [] -> 0
    | x :: xs -> Int.bit_xor (String.hash x) (hash xs)

  let of_string value =
    String.split ~on:'.' value

  let to_string els =
    String.concat ~sep:"." (List.rev els)

  let to_string_hum =
    function
      | []  -> "<ROOT>"
      | els -> to_string els

  let append base head =
    head :: base

  let parent = function
    | [] -> None
    | _ :: tail -> Some tail

  let canonicalize relto items =
    items @ (List.drop relto (List.length items))

  let compare =
    List.compare String.compare

  let sexp_of_t =
    List.sexp_of_t String.sexp_of_t

  let t_of_sexp =
    List.t_of_sexp String.t_of_sexp

  (* }}} *)

end

module PM = Map.Make (NPath)
module PS = Set.Make (NPath)
module PT = Hashtbl.Make (NPath)
module SM = String.Map

type value_usage =
    ReadOnly
  | WriteOnly
  | ReadWrite

(* ********************************************************************************************* *)
type _ sty =
  | TyAccess             : access sty
  | TyBitRange           : (int * int) sty
  | TyBool               : bool sty
  | TyCType              : ctype sty
  | TyEndianness         : Cpu.endianness sty
  | TyIndex              : RegisterGroup.index sty
  | TyMaskAndValue       : TargetInt.t mask_and_value sty
  | TyModifiedWriteValue : Register.modified_write_values sty
  | TyPath               : NPath.t sty
  | TyProtection         : Register.protection sty
  | TyRangeUsage         : Peripheral.Block.usage sty
  | TyReadAction         : Register.read_action sty
  | TySmallInt           : int sty
  | TyStr                : string sty
  | TyTargetInt          : TargetInt.t sty
  | TyValueUsage         : value_usage sty

type cty =
  | TyAddressBlock
  | TyCluster
  | TyCpu
  | TyDevice
  | TyEnumeratedValue
  | TyEnumeratedValues
  | TyField
  | TyInterrupt
  | TyPeripheral
  | TyRegister

type nil   = Nil
type k_lst = KLst
type k_comp = KComp

type (_,_,_) ty =
  | S : 'a sty        -> (nil,nil,'a) ty
  | C : cty           -> (nil,k_comp,compound) ty
  | L : (nil,_,'a) ty -> (k_lst,nil,'a list) ty

and compound = tagged SM.t

and tagged = V : (_,_,'a) ty * 'a -> tagged

type (_,_) eq = Eq : ('a,'a) eq

(* ****** *)
let eq_sty : type a a'. a sty -> a' sty -> (a,a') eq option =
  fun u v ->
    match (u,v) with
      | TyAccess, TyAccess                         -> Some Eq
      | TyBitRange, TyBitRange                     -> Some Eq
      | TyBool, TyBool                             -> Some Eq
      | TyCType, TyCType                           -> Some Eq
      | TyEndianness, TyEndianness                 -> Some Eq
      | TyIndex, TyIndex                           -> Some Eq
      | TyMaskAndValue, TyMaskAndValue             -> Some Eq
      | TyModifiedWriteValue, TyModifiedWriteValue -> Some Eq
      | TyPath, TyPath                             -> Some Eq
      | TyProtection, TyProtection                 -> Some Eq
      | TyRangeUsage, TyRangeUsage                 -> Some Eq
      | TyReadAction, TyReadAction                 -> Some Eq
      | TySmallInt, TySmallInt                     -> Some Eq
      | TyStr, TyStr                               -> Some Eq
      | TyTargetInt, TyTargetInt                   -> Some Eq
      | TyValueUsage, TyValueUsage                 -> Some Eq
      | _                                          -> None

let rec eq_ty : type k0 k0' k1 k1' a a'. (k0,k1,a) ty -> (k0',k1',a') ty -> (a,a') eq option =
  fun u v ->
    match (u,v) with
      | S u', S v' ->
          eq_sty u' v'
      | C u', C v' when u'=v' ->
          Some Eq
      | L u', L v' ->
          begin
            match eq_ty u' v' with
              | Some Eq -> Some Eq
              | None    -> None
          end
      | _ ->
          None

let scalar_ty_p : type k0 k1 a. (k0,k1,a) ty -> (k0,nil) eq option =
  function
    | S _ -> Some Eq
    | C _ -> Some Eq
    | _   -> None

let string_of_cty : cty -> string =
  function
    | TyAddressBlock       -> "address-block"
    | TyCluster            -> "cluster"
    | TyCpu                -> "cpu"
    | TyDevice             -> "device"
    | TyEnumeratedValue    -> "enumerated-value"
    | TyEnumeratedValues   -> "enumerated-values"
    | TyField              -> "field"
    | TyInterrupt          -> "interrupt"
    | TyPeripheral         -> "peripheral"
    | TyRegister           -> "register"

let rec string_of_sty : type a. a sty -> string =
  function
    | TyAccess             -> "access"
    | TyBitRange           -> "bit-range"
    | TyBool               -> "bool"
    | TyCType              -> "ctype"
    | TyEndianness         -> "endianness"
    | TyIndex              -> "index"
    | TyMaskAndValue       -> "mask-and-value"
    | TyModifiedWriteValue -> "modified-write-value"
    | TyPath               -> "path"
    | TyProtection         -> "protection"
    | TyRangeUsage         -> "range-usage"
    | TyReadAction         -> "read-action"
    | TySmallInt           -> "small-int"
    | TyStr                -> "str"
    | TyTargetInt          -> "target-int"
    | TyValueUsage         -> "value-usage"

let rec string_of_ty : type k0 k1 a. (k0,k1,a) ty -> string =
  function
    | L ty -> (string_of_ty ty) ^ " list"
    | C ty -> string_of_cty ty
    | S ty -> string_of_sty ty


(* operations on "dynamic" types *)
let inj : type a. (_,_,a) ty -> a -> tagged =
  fun t v -> V (t,v)

let proj_exn : type a. (_,_,a) ty -> tagged -> a =
  fun s (V (t,v)) ->
    match eq_ty s t with
      | Some Eq -> v
      | None    -> Printf.failwithf "expected %s - got %s" (string_of_ty s) (string_of_ty t) ()

let lift : type k0 k1 a. (k0,k1,a) ty -> f:(a->a) -> tagged -> tagged =
  fun ty ~f value ->
    value |> proj_exn ty
          |> f
          |> inj ty

(* operations on compounds *)
let opt : type a. compound -> (_,_,a) ty -> string -> a option =
  fun tab ty key ->
    SM.find tab key
      |> Option.map ~f:(proj_exn ty)

let req : type a. compound -> (_,_,a) ty -> string -> a =
  fun tab ty key ->
    opt tab ty key |> Option.value_exn

let dfl : type a. compound -> (_,_,a) ty -> string -> a -> a =
  fun tab ty key default ->
    opt tab ty key |> Option.value ~default

(* ********************************************************************************************* *)
module Injest : sig

  (* parse xml document into a typed "dictionary-of-dictionary" structure *)
  val from_filename : string -> compound

end = struct

  (* {{{ *)
  module P = struct

    (* {{{ *)
    let make_exn_re_pred pat =
      let open Re2.Std in 
        Re2.matches (Re2.create_exn pat)

    let make_exn_re_submatches_exn pat = 
      let open Re2.Std in 
        Re2.find_submatches_exn (Re2.create_exn pat)

    let make_exn_re_submatches pat =
      let open Re2.Std in 
        Re2.find_submatches (Re2.create_exn pat)


    (* ***** *)
    let bool_of_string v : bool =
      match Option.try_with (fun () -> int_of_string v) with
        | Some v -> v <> 0
        | None   -> bool_of_string v

    let target_int_of_string v : TargetInt.t =
      match String.chop_prefix v "#" with
        | None    -> TargetInt.of_string v
        | Some v' -> TargetInt.of_string ("0b" ^ v')

    let mask_and_value_of_string : string -> TargetInt.t mask_and_value =
      let pat = make_exn_re_pred "^#.*x.*$" in
        fun v ->
          if pat v then
            Value (target_int_of_string v)
          else
            MaskedValue (
                target_int_of_string (String.tr 'x' '0' v)
              , target_int_of_string (String.tr 'x' '0' (String.tr '0' '1' v))
              )

    let index_of_string : string -> RegisterGroup.index =
      let p = make_exn_re_submatches "^([0-9]+)-([0-9]+)$" in
        fun v ->
            match p v with
              | Ok [| _; Some m0; Some m1 |] ->
                  RegisterGroup.Range (int_of_string m0, int_of_string m1)
              | _ ->
                  RegisterGroup.List (String.split ~on:',' v)

    let cint_of_string text : cint =
      match String.strip text with
        | "uint8_t"    -> UInt8
        | "uint16_t"   -> UInt16
        | "uint32_t"   -> UInt32
        | "uint64_t"   -> UInt64
        | "int8_t"     -> Int8
        | "int16_t"    -> Int16
        | "int32_t"    -> Int32
        | "int64_t"    -> Int64
        | _            -> failwith "bad cint value"

    let ctype_of_string text : ctype =
      match String.chop_suffix (String.strip text) "*" with
        | None      -> Immediate (cint_of_string text)
        | Some text -> Pointer (cint_of_string text)

    let access_of_string : string -> access =
      function
        | "read-only"       -> ReadOnly
        | "write-only"      -> WriteOnly
        | "read-write"      -> ReadWrite
        | "writeOnce"       -> WriteOnce
        | "read-writeOnce"  -> ReadWriteOnce
        | _                 -> failwith "bad access value"

    let endianness_of_string : string -> Cpu.endianness =
      let open Cpu in
        function
          | "big"        -> Big
          | "little"     -> Little
          | "other"      -> Other
          | "selectable" -> Selectable
          | _            -> failwith "bad endianness value"

    let protection_of_string : string -> Register.protection =
      let open Register in
        function
          | "n"   -> NonSecure
          | "p"   -> Privileged
          | "s"   -> Secure
          | _     -> failwith "bad protection value"

    let range_usage_of_string : string -> Peripheral.Block.usage =
      let open Peripheral.Block in
        function
          | "buffer"    -> Buffer
          | "registers" -> Registers
          | "reserved"  -> Reserved
          | _           -> failwith "bad usage value"

    let modified_write_value_of_string : string -> Register.modified_write_values =
      let open Register in
        function
          | "clear"        -> Clear
          | "modify"       -> Modify
          | "oneToClear"   -> OneToClear
          | "oneToSet"     -> OneToSet
          | "oneToToggle"  -> OneToToggle
          | "set"          -> Set
          | "zeroToClear"  -> ZeroToClear
          | "zeroToSet"    -> ZeroToSet
          | "zeroToToggle" -> ZeroToToggle
          | _              -> failwith "bad modified-write-values value"

    let read_action_of_string : string -> Register.read_action =
      let open Register in
        function
          | "clear"           -> Clear
          | "modify"          -> Modify
          | "modifyExternal"  -> ModifyExternal
          | "set"             -> Set
          | _                 -> failwith "bad read-action value"

    let value_usage_of_string : string -> value_usage =
      function
        | "read"       -> ReadOnly
        | "write"      -> WriteOnly
        | "read-write" -> ReadWrite
        | _            -> failwith "bad usage value"

    let bit_range_of_string : string -> int * int  =
      let p = make_exn_re_submatches_exn "^\\[(\\d+):(\\d+)\\]$" in
        fun v ->
          let m = p v in
            (int_of_string (Option.value_exn m.(2))), (int_of_string (Option.value_exn m.(1)))

    (* }}} *)

  end

  type state =
    | Start   : string -> state
    | F       : cty * string -> state
    | A       : cty * string -> state
    | I       : cty * string * string -> state
    | B       : _ sty -> state
    | Discard : state

  type saction =
      Set
    | App

  type action =
    | Act  : saction * string option * (nil,'k,'a) ty -> action
    | Inl  : action
    | Err  : action
    | Dis  : action
    | Par  : (string -> tagged) -> action

  let field st k     = F (st,k)
  let inline k st k' = I (st,k,k')
  let attrib st k    = A (st,k)

  let par_s ty f     = Par (fun v -> inj (S ty) (f v))
  let app_c ?name ty = Act (App,name,(C ty))
  let app_s ?name ty = Act (App,name,(S ty))
  let set_c ?name ty = Act (Set,name,(C ty))
  let set_s ?name ty = Act (Set,name,(S ty))
  let inl_c          = Inl
  let dis_c          = Dis
  let dis_s          = Dis
  let err            = Err

  let tr : state -> action  =
    function
      | Start( "device" )                        -> set_c TyDevice
      | A (TyDevice, _)                          -> dis_s
      | F (TyDevice,"access")                    -> set_s TyAccess
      | F (TyDevice,"addressUnitBits")           -> set_s TySmallInt
      | F (TyDevice,"cpu")                       -> set_c TyCpu
      | F (TyDevice,"description")               -> set_s TyStr
      | F (TyDevice,"headerDefinitionsPrefix")   -> set_s TyStr
      | F (TyDevice,"headerFile")                -> dis_s
      | F (TyDevice,"headerSystemFilename")      -> set_s TyStr
      | F (TyDevice,"licenseText")               -> set_s TyStr
      | F (TyDevice,"name")                      -> set_s TyStr
      | F (TyDevice,"peripherals")               -> inl_c
      | I (TyDevice,"peripherals","peripheral")  -> app_c ~name:"peripherals" TyPeripheral
      | F (TyDevice,"protection")                -> set_s TyProtection
      | F (TyDevice,"resetMask")                 -> set_s TyTargetInt
      | F (TyDevice,"resetValue")                -> set_s TyTargetInt
      | F (TyDevice,"series")                    -> set_s TyStr
      | F (TyDevice,"size")                      -> set_s TySmallInt
      | F (TyDevice,"vendor")                    -> set_s TyStr
      | F (TyDevice,"vendorExtensions")          -> dis_s
      | F (TyDevice,"vendorID")                  -> set_s TyStr
      | F (TyDevice,"version")                   -> set_s TyStr
      | F (TyDevice,"width")                     -> set_s TySmallInt
      | F (TyCpu,"dcachePresent")                -> set_s TyBool
      | F (TyCpu,"dtcmPresent")                  -> set_s TyBool
      | F (TyCpu,"endian")                       -> set_s TyEndianness
      | F (TyCpu,"fpuDP")                        -> set_s TyBool
      | F (TyCpu,"fpuPresent")                   -> set_s TyBool
      | F (TyCpu,"icachePresent")                -> set_s TyBool
      | F (TyCpu,"itcmPresent")                  -> set_s TyBool
      | F (TyCpu,"mpuPresent")                   -> set_s TyBool
      | F (TyCpu,"name")                         -> set_s TyStr
      | F (TyCpu,"nvicPrioBits")                 -> set_s TySmallInt
      | F (TyCpu,"revision")                     -> set_s TyStr
      | F (TyCpu,"vendorSystickConfig")          -> set_s TyBool
      | F (TyCpu,"vtorPresent")                  -> set_s TyBool
      | F (TyPeripheral,"access")                -> set_s TyAccess
      | F (TyPeripheral,"addressBlock")          -> app_c ~name:"addressBlocks" TyAddressBlock
      | F (TyPeripheral,"alternatePeripheral")   -> set_s TyStr
      | F (TyPeripheral,"appendToName")          -> set_s TyStr
      | F (TyPeripheral,"baseAddress")           -> set_s TyTargetInt
      | F (TyPeripheral,"description")           -> set_s TyStr
      | A (TyPeripheral,"derivedFrom")           -> set_s TyPath
      | F (TyPeripheral,"disableCondition")      -> set_s TyStr
      | F (TyPeripheral,"groupName")             -> set_s TyStr
      | F (TyPeripheral,"headerStructName")      -> set_s TyStr
      | F (TyPeripheral,"interrupt")             -> app_c ~name:"interrupts" TyInterrupt
      | F (TyPeripheral,"name")                  -> set_s TyStr
      | F (TyPeripheral,"prependToName")         -> set_s TyStr
      | F (TyPeripheral,"registers")             -> inl_c
      | I (TyPeripheral,"registers","cluster")   -> app_c ~name:"clusters" TyCluster
      | I (TyPeripheral,"registers","register")  -> app_c ~name:"registers" TyRegister
      | F (TyPeripheral,"resetMask")             -> set_s TyTargetInt
      | F (TyPeripheral,"resetValue")            -> set_s TyTargetInt
      | F (TyPeripheral,"size")                  -> set_s TySmallInt
      | F (TyPeripheral,"version")               -> set_s TyStr
      | F (TyAddressBlock,"offset")              -> set_s TyTargetInt
      | F (TyAddressBlock,"size")                -> set_s TyTargetInt
      | F (TyAddressBlock,"usage")               -> set_s TyRangeUsage
      | F (TyInterrupt,"name")                   -> set_s TyStr
      | F (TyInterrupt,"description")            -> set_s TyStr
      | F (TyInterrupt,"value")                  -> set_s TySmallInt
      | A (TyCluster,"derivedFrom")              -> set_s TyPath
      | F (TyCluster,"addressOffset")            -> set_s TyTargetInt
      | F (TyCluster,"alternateCluster")         -> set_s TyStr
      | F (TyCluster,"cluster")                  -> app_c ~name:"clusters" TyCluster
      | F (TyCluster,"description")              -> set_s TyStr
      | F (TyCluster,"dim")                      -> set_s TySmallInt
      | F (TyCluster,"dimIncrement")             -> set_s TySmallInt
      | F (TyCluster,"dimIndex")                 -> set_s TyIndex
      | F (TyCluster,"headerStructName")         -> set_s TyStr
      | F (TyCluster,"name")                     -> set_s TyStr
      | F (TyCluster,"register")                 -> app_c ~name:"registers" TyRegister
      | A (TyRegister,"derivedFrom")             -> set_s TyPath
      | F (TyRegister,"access")                  -> set_s TyAccess
      | F (TyRegister,"addressOffset")           -> set_s TyTargetInt
      | F (TyRegister,"alternateGroup")          -> set_s TyStr
      | F (TyRegister,"alternateRegister")       -> set_s TyStr
      | F (TyRegister,"dataType")                -> set_s TyCType
      | F (TyRegister,"description")             -> set_s TyStr
      | F (TyRegister,"dim")                     -> set_s TySmallInt
      | F (TyRegister,"dimIncrement")            -> set_s TySmallInt
      | F (TyRegister,"dimIndex")                -> set_s TyIndex
      | F (TyRegister,"displayName")             -> set_s TyStr
      | F (TyRegister,"fields")                  -> inl_c
      | I (TyRegister,"fields","field")          -> app_c ~name:"fields" TyField
      | F (TyRegister,"modifiedWriteValues")     -> set_s TyModifiedWriteValue
      | F (TyRegister,"name")                    -> set_s TyStr
      | F (TyRegister,"protection")              -> set_s TyProtection
      | F (TyRegister,"readAction")              -> set_s TyReadAction
      | F (TyRegister,"resetMask")               -> set_s TyTargetInt
      | F (TyRegister,"resetValue")              -> set_s TyTargetInt
      | F (TyRegister,"size")                    -> set_s TySmallInt
      | F (TyField,"access")                     -> set_s TyAccess
      | F (TyField,"bitOffset")                  -> set_s TySmallInt
      | F (TyField,"bitRange")                   -> set_s TyBitRange
      | F (TyField,"bitWidth")                   -> set_s TySmallInt
      | F (TyField,"description")                -> set_s TyStr
      | F (TyField,"enumeratedValues")           -> app_c TyEnumeratedValues
      | F (TyField,"lsb")                        -> set_s TySmallInt
      | F (TyField,"modifiedWriteValues")        -> set_s TyModifiedWriteValue
      | F (TyField,"msb")                        -> set_s TySmallInt
      | F (TyField,"name")                       -> set_s TyStr
      | F (TyField,"readAction")                 -> set_s TyReadAction
      | F (TyEnumeratedValues,"name")            -> set_s TyStr
      | F (TyEnumeratedValues,"usage")           -> set_s TyValueUsage
      | F (TyEnumeratedValues,"enumeratedValue") -> app_c ~name:"choices" TyEnumeratedValue
      | F (TyEnumeratedValue,"name")             -> set_s TyStr
      | F (TyEnumeratedValue,"description")      -> set_s TyStr
      | F (TyEnumeratedValue,"isDefault")        -> set_s TyBool
      | F (TyEnumeratedValue,"value")            -> set_s TyMaskAndValue
      | B TyAccess                               -> par_s TyAccess P.access_of_string
      | B TyBitRange                             -> par_s TyBitRange P.bit_range_of_string
      | B TyBool                                 -> par_s TyBool P.bool_of_string
      | B TyCType                                -> par_s TyCType P.ctype_of_string
      | B TyEndianness                           -> par_s TyEndianness P.endianness_of_string
      | B TyIndex                                -> par_s TyIndex P.index_of_string
      | B TyMaskAndValue                         -> par_s TyMaskAndValue P.mask_and_value_of_string
      | B TyModifiedWriteValue                   -> par_s TyModifiedWriteValue P.modified_write_value_of_string
      | B TyPath                                 -> par_s TyPath NPath.of_string
      | B TyProtection                           -> par_s TyProtection P.protection_of_string
      | B TyRangeUsage                           -> par_s TyRangeUsage P.range_usage_of_string
      | B TyReadAction                           -> par_s TyReadAction P.read_action_of_string
      | B TySmallInt                             -> par_s TySmallInt int_of_string
      | B TyStr                                  -> par_s TyStr (fun v -> v)
      | B TyTargetInt                            -> par_s TyTargetInt P.target_int_of_string
      | B TyValueUsage                           -> par_s TyValueUsage P.value_usage_of_string
      | _                                        -> err

  (* ***** *)
  module rec E : sig

    (* parses the contents of an element, up to and including the closing tag.
     * expects that the initial element has been already consumed.
     *)

    val parse : (nil,'k,'a) ty -> Xmlm.input -> Xmlm.attribute list -> tagged

  end = struct

    (* {{{ *)

    let vact act tab k k' data =
      let key = Option.value ~default:k k' in

        match act with
          | Set -> 
              SM.add tab ~key ~data 

          | App -> 
              let (V (hty,hval)) = data in
                SM.update tab
                          key
                          (fun tail ->
                            match scalar_ty_p hty with
                              | None    -> failwith "oops"
                              | Some Eq -> let lty  = L hty in
                                           let tval = Option.value_map ~default:[] ~f:(proj_exn lty) tail in
                                             V (lty,hval::tval))


    let parse_discard inp =
      let rec aux depth =
        match (Xmlm.input inp) with
          | `El_end       when depth=0 -> ()
          | `El_end                    -> aux (depth-1)
          | `El_start _                -> aux (depth+1)
          | _                          -> aux (depth)
      in
      aux 0

    let parse_body st v =
      match tr (B st) with
        | Par f -> f v
        | _     -> failwith "expected parser"

    let parse_simple ty inp attrs =

      if not (List.is_empty attrs) then
        failwith "section has unexpected attributes";

      match Xmlm.input inp with
        | `Data d ->
            begin
              match Xmlm.input inp with
                | `El_end -> parse_body ty d
                | _       -> failwith "expected end of element"
            end
        | _ ->
            failwith "expected data"

    let parse_attributes ty attrs state =
      List.fold ~init:state
                ~f:(fun state ((_,k),v) ->
                       match tr (A (ty,k)) with
                         | Dis                  -> state
                         | Act (act,k',(S ty')) -> vact act state k k' (parse_body ty' v)
                         | _                    -> failwith "bad transition while populating attributes")
                attrs

    let rec parse_children inp ty f state =
      match Xmlm.input inp with

        | `El_start (("",k),attrs) -> 

            parse_children inp 
                           ty
                           f
                           begin

                              match tr (f ty k) with

                                | Dis ->
                                    parse_discard inp; state

                                | Inl ->
                                    if not (List.is_empty attrs) then
                                      failwith "section has unexpected attributes";
                                    parse_children inp ty (inline k) state

                                | Act (act,k',ty') ->
                                    vact act state k k' (E.parse ty' inp attrs)

                                | _ -> 
                                    Printf.failwithf "bad transition while populating children of %s: %s" 
                                                     (string_of_ty (C ty)) 
                                                     k 
                                                     ()
                            end

        | `El_end -> 
            state

        | _  -> 
            failwith "oops"

    let reverse_list_elements =
      SM.map ~f:(function
                    | V ((L _) as ty,v) -> V (ty,List.rev v)
                    | v                 -> v)


    let parse_compound ty inp attrs =
         SM.empty
      |> parse_attributes ty attrs
      |> parse_children inp ty field
      |> reverse_list_elements
      |> inj (C ty)

    let parse : type k a. (nil,k,a) ty -> Xmlm.input -> Xmlm.attribute list -> tagged =
      function 
        | S ty -> parse_simple ty
        | C ty -> parse_compound ty

    (* }}} *)

  end

  let dtd inp =
    match Xmlm.peek inp with
      | `Dtd _ -> Some (Xmlm.input inp)
      | _      -> None

  let parse_document inp =
    ignore (dtd inp);
    match Xmlm.input inp with
      | `El_start (("",k),attrs) ->
          begin
            match tr (Start k) with
              | Act (Set,k',ty) ->
                  let result = E.parse ty inp attrs in
                    if not (Xmlm.eoi inp) then
                      failwith "oops - expected EOF";
                    result
              | _ -> failwith "oops - bad transition"
          end
      | _ -> failwith "oops"

  let from_filename filename =
      In_channel.with_file filename 
                           ~f:(fun c ->
                                  Xmlm.make_input ~strip:true (`Channel c)
                                    |> parse_document)
        |> proj_exn (C TyDevice)

  (* }}} *)

end

(* ********************************************************************************************* *)
module Materialize :  sig

  (* resolve any "derivedFrom" references *)
  val apply : compound -> compound

end = struct

  (* {{{ *)
  let list_fold ~f ~items init =
    List.fold ~f ~init items

  let option_fold ~f ~items init =
    Option.fold ~f ~init items


  (* ***** *)
  type status =
      Raw
    | Resolved

  let path_of parent tab =
    NPath.append parent (req tab (S TyStr) "name")

  (* create a mapping of fully qualified entity path names to entity definitions. *)
  let make_entity_map : compound -> (status * cty * tagged SM.t) PT.t =
    let accum = PT.create () in
      
      let rec aux ty pfn tab =

        let path = pfn tab in
        let pfn  = path_of path in

          PT.set accum ~key:path ~data:(Raw,ty,tab);
                                                    
          (* device level *)
          dfl tab (L (C TyPeripheral)) "peripherals" []
            |> List.iter ~f:(aux TyPeripheral pfn);

          (* peripheral and cluster level *)
          dfl tab (L (C TyRegister)) "registers" []
            |> List.iter ~f:(aux TyRegister pfn);

          (* peripheral and cluster level *)
          dfl tab (L (C TyCluster)) "clusters" []
            |> List.iter ~f:(aux TyCluster pfn);

      in

        fun tab -> 
          aux TyDevice (fun _ -> NPath.root) tab;
          accum


  (* Rebuild an entity, recusively pulling in updated children and
   * resolving "derivedFrom" references. Caches the results
   * in the entity map, flipping the status from `Raw` to `Resolved`. 
   *)
  let rec resolve_entity em path ty =
    let status,ty',tab = PT.find_exn em path in

      if ty' <> ty then
        Printf.failwithf "expected %s to be of type %s - got %s"
                         (NPath.to_string_hum path)
                         (string_of_cty ty)
                         (string_of_cty ty')
                         ();

      match status with                         
        | Resolved ->
            tab
        | Raw ->
            tab 

              (* get the final values for an children nodes *)
              |> list_fold ~items:[ TyPeripheral, "peripherals" ;
                                    TyCluster,    "clusters"    ;
                                    TyRegister,   "registers"   ]
                            ~f:(fun tab (ty,key) ->
                                  let aux tab = resolve_entity em (path_of path tab) ty in
                                    SM.change tab 
                                              key 
                                              ~f:(Option.map ~f:(lift (L (C ty)) 
                                                                      ~f:(List.map ~f:aux))))


              (* do the actual derivedFrom processing *)
              |> option_fold ~items:(opt tab (S TyPath) "derivedFrom")
                             ~f:(fun tab parent ->
                                  let parent   = NPath.canonicalize path parent in
                                  let tab'     = resolve_entity em parent ty in
                                    SM.merge tab
                                             tab'
                                             ~f:(fun ~key value ->
                                                  match (key,value) with
                                                    | "derivedFrom",_ -> None
                                                    | _,`Left v       -> Some v
                                                    | _,`Right v      -> Some v
                                                    | _,`Both (v,_)   -> Some v))

              (* cache the updated value *)
              |> (fun tab -> PT.set em ~key:path ~data:(Resolved,ty,tab); tab )


  let apply tab =
    resolve_entity (make_entity_map tab) 
                   NPath.root 
                   TyDevice

  (* }}} *)

end


(* ********************************************************************************************* *)
module Finalize : sig

  (* convert from the dictionary-of-dictionary-based representation 
   * into the record-based representation
   *)
  val apply : compound -> Device.t

end = struct

  (* {{{ *)
  let oor = 
    Option.first_some

  let list_fold ~f ~items init =
    List.fold ~f ~init items

  let option_fold ~f ~items init =
    Option.fold ~f ~init items


  (* ***** *)
  module RegisterProperties = struct

    type t = {
      access       : access option;
      base_address : TargetInt.t;
      protection   : Register.protection option;
      reset_mask   : TargetInt.t option;
      reset_value  : TargetInt.t option;
      size         : int option;
    }

    let merge rp tab = { 
      access       = opt tab (S TyAccess) "access" |> oor rp.access;
      base_address = dfl tab (S TyTargetInt) "baseAddress" rp.base_address;
      protection   = opt tab (S TyProtection) "protection" |> oor rp.protection;
      reset_mask   = opt tab (S TyTargetInt) "resetMask" |> oor rp.reset_mask;
      reset_value  = opt tab (S TyTargetInt) "resetValue" |> oor rp.reset_value;
      size         = opt tab (S TySmallInt) "size" |> oor rp.size;
    }

    let of_tab tab = {
      access       = opt tab (S TyAccess) "access";
      base_address = TargetInt.zero;
      protection   = opt tab (S TyProtection) "protection";
      reset_mask   = opt tab (S TyTargetInt) "resetMask";
      reset_value  = opt tab (S TyTargetInt) "resetValue";
      size         = opt tab (S TySmallInt) "size";
    }

    let address rp tab key =
      let open TargetInt in
        rp.base_address + req tab (S TyTargetInt) key

    let access rp tab =
      opt tab (S TyAccess) "access" |> oor rp.access

    let protection rp tab =
      opt tab (S TyProtection) "protection" |> oor rp.protection

    let reset_mask rp tab =
      opt tab (S TyTargetInt) "resetMask" |> oor rp.reset_mask

    let reset_value rp tab =
      opt tab (S TyTargetInt) "resetValue" |> oor rp.reset_value

    let size rp tab =
      opt tab (S TySmallInt) "size" |> oor rp.size

    let trivial_mask_p rp tab mask =
      (* FIXME *)
      match size rp tab with
        | Some 32 -> mask=(TargetInt.of_int64 (Int64.of_int 0xFFFFFFFF))
        | Some 16 -> mask=(TargetInt.of_int64 (Int64.of_int 0xFFFF))
        | Some 8  -> mask=(TargetInt.of_int64 (Int64.of_int 0xFF))
        | _       -> false

    let reset rp tab =
      reset_value rp tab
        |> Option.map ~f:(fun value ->
                            match reset_mask rp tab with
                              | None -> 
                                  Value value
                              | Some mask when (trivial_mask_p rp tab mask) -> 
                                  Value value
                              | Some mask -> 
                                  MaskedValue (value,mask))

    let maybe_base_address rp tab =
      Option.map ~f:(fun _ -> rp.base_address)
                 (opt tab (S TyTargetInt) "baseAddress")

  end

  (* *** *)
  let strip_array name =
    match String.index name '[' with
      | None   -> name
      | Some i -> String.prefix name i

  let is_array name =
    String.contains name '['

  let make_map_of f items =
    List.filter_map items
                    ~f:(fun tab ->
                          match req tab (S TyStr) "name" with
                            | "RESERVED" -> None
                            | name       -> Some (name,(f tab)))
      |> SM.of_alist_exn

  
  (* *** *)
  let description_of_tab_inl tab =
    let d = opt tab (S TyStr) "description" in
      if d<>(opt tab (S TyStr) "name") then d else None

  let display_name_of_tab_inl tab =
    let d = opt tab (S TyStr) "d" in
      Option.map ~f:strip_array
                 (if d<>(opt tab (S TyStr) "name") then d else None)

  let bits_of_tab_inl tab =
    let bitOffset = opt tab (S TySmallInt) "bitOffset" in
    let bitRange  = opt tab (S TyBitRange) "bitRange" in
    let bitWidth  = opt tab (S TySmallInt) "bitWidth" in
    let lsb       = opt tab (S TySmallInt) "lsb" in
    let msb       = opt tab (S TySmallInt) "msb" in
      match (bitOffset,bitWidth,lsb,msb,bitRange) with
        | (Some i, Some 1, None, None, None)             -> Bit i
        | (Some i, Some w, None, None, None)             -> BitRange (i,w)
        | (None, None, Some l, Some m, None ) when m=l   -> Bit l
        | (None, None, Some l, Some m, None )            -> BitRange (l,m-l+1)
        | (None, None, None, None, Some (l,m) ) when m=l -> Bit l
        | (None, None, None, None, Some (l,m) )          -> BitRange (l,m-l+1)
        | _                                              -> failwith "oops"

  let fpu_of_tab_inl tab =
    let fpuPresent = opt tab (S TyBool) "fpuPresent" in
    let fpuDP      = opt tab (S TyBool) "fpuDP" in
      match fpuPresent,fpuDP with
        | (Some true, Some true) -> Cpu.DoublePrecision
        | (Some true, _)         -> Cpu.SinglePrecision
        | _                      -> Cpu.Absent

  let address_block_of_tab (rp:RegisterProperties.t) tab : Peripheral.Block.t =
    Peripheral.Block.({
      address = RegisterProperties.address rp tab "offset";
      size    = req tab (S TyTargetInt) "size";
      usage   = req tab (S TyRangeUsage) "usage";
    })

  let annotated_of_tab : type a. (_,_,a) ty -> compound -> a annotated =
    fun ty tab -> {
      description = opt tab (S TyStr) "description";
      value       = req tab ty "value";
    }

  let evmap_of_list =
    let open EnumeratedValues in
      List.fold ~init:{ values=SM.empty; default=None }
                ~f:(fun {values;default} tab : evmap ->
                       let description = description_of_tab_inl tab in
                       let is_default  = dfl tab (S TyBool) "is_default" false in
                       let name        = req tab (S TyStr) "name" in
                       let value       = req tab (S TyMaskAndValue) "value" in
                       let values      = SM.add values name { value; description } in
                         match default,is_default with
                          | _,false   -> { values; default }
                          | None,true -> { values; default=Some name }
                          | _         -> failwith "multiple defaults")

  let enumerated_values_of_lst lst =
    let open EnumeratedValues in

      let aux tab =
        let mapping = dfl tab (L (C TyEnumeratedValue)) "choices" [] 
                        |> evmap_of_list in
        let name    = opt tab (S TyStr) "name" in
        let usage   = dfl tab (S TyValueUsage) "usage" ReadWrite in

          match usage with
            | ReadWrite -> name, (ReadWrite mapping)
            | ReadOnly  -> name, (ReadOnly mapping)
            | WriteOnly -> name, (WriteOnly mapping)

      in

      match List.map ~f:aux lst with
        | [] ->
            ReadWrite { values=SM.empty; default=None }
        | [(n,v)] ->
            v
        | [(n0,(ReadOnly v0));(n1,(WriteOnly v1))] when n0=n1 ->
            ReadOnlyWriteOnly (v0,v1)
        | [(n0,(WriteOnly v0));(n1,(ReadOnly v1))] when n0=n1 ->
            ReadOnlyWriteOnly (v1,v0)
        | _ ->
            failwith "oops"

  let register_field_of_tab tab : Register.Field.t =
    Register.Field.({
      access                      = opt tab (S TyAccess) "access";
      bits                        = bits_of_tab_inl tab;
      description                 = description_of_tab_inl tab;
      enumerated_values           = opt tab (L (C TyEnumeratedValues)) "enumeratedValues"
                                      |> Option.map ~f:enumerated_values_of_lst;
      modified_write_values       = opt tab (S TyModifiedWriteValue) "modifiedWriteValues";
      read_action                 = opt tab (S TyReadAction) "readAction";
    })

  let register_of_tab (rp:RegisterProperties.t) tab : Register.t =
    Register.({
      access                    = RegisterProperties.access rp tab;
      address                   = RegisterProperties.address rp tab "addressOffset";
      alternate_group           = opt tab (S TyStr) "alternateGroup";
      alternate_register        = opt tab (S TyStr) "alternateRegister";
      data_type                 = opt tab (S TyCType) "dataType";
      description               = description_of_tab_inl tab;
      display_name              = display_name_of_tab_inl tab;
      fields                    = dfl tab (L (C TyField)) "fields" []
                                    |> make_map_of register_field_of_tab;
      modified_write_values     = opt tab (S TyModifiedWriteValue) "modifiedWriteValues";
      protection                = RegisterProperties.protection rp tab;
      read_action               = opt tab (S TyReadAction) "readAction";
      reset                     = RegisterProperties.reset rp tab;
      size                      = RegisterProperties.size rp tab;
    })

  let cluster_of_tab register_map_of_tab_inl (rp:RegisterProperties.t) (tab:compound) : RegisterGroup.cluster =
    RegisterGroup.({
      alternate_cluster       = opt tab (S TyStr) "alternateCluster";
      base_address            = RegisterProperties.maybe_base_address rp tab;
      description             = description_of_tab_inl tab;
      header_struct_name      = opt tab (S TyStr) "alternatePeripheral";
      registers               = register_map_of_tab_inl rp tab;
    })

  let dim_of_tab tab =
      opt tab (S TySmallInt) "dim"
        |> Option.map 
            ~f:(fun count ->
                let array        = req tab (S TyStr) "name"
                                    |> is_array in
                let index        = opt tab (S TyIndex) "dimIndex" in
                let increment    = req tab (S TySmallInt) "dimIncrement" in
                  
                  (* FIXME - filter out index info if it can be 
                   * derived from the other elements 
                   *)

                  RegisterGroup.{ count; increment; index; array })

  let rg_register_of_tab (rp:RegisterProperties.t) tab : RegisterGroup.t =
    RegisterGroup.R ((register_of_tab rp tab),(dim_of_tab tab))

  let rg_cluster_of_tab register_map_of_tab_inl (rp:RegisterProperties.t) (tab:compound) : RegisterGroup.t =
    RegisterGroup.C ((cluster_of_tab register_map_of_tab_inl rp tab),(dim_of_tab tab))

  let rec register_map_of_tab_inl (rp:RegisterProperties.t) tab : RegisterGroup.t SM.t =

    let aux f items =
      list_fold ~items
                ~f:(fun accum tab ->
                      SM.add accum
                             (strip_array (req tab (S TyStr) "name"))
                             (f tab))

    in                

    SM.empty
      |> (dfl tab (L (C TyRegister)) "registers" []
            |> aux (rg_register_of_tab rp))
      |> (dfl tab (L (C TyCluster)) "clusters" []
            |> aux (rg_cluster_of_tab register_map_of_tab_inl rp))

  let peripheral_of_tab (rp:RegisterProperties.t) (tab:compound) : Peripheral.t =
    let rp = RegisterProperties.merge rp tab in
      Peripheral.({
        address_blocks            = dfl tab (L (C TyAddressBlock)) "addressBlocks" []
                                      |> List.map ~f:(address_block_of_tab rp);
        alternate_peripheral      = opt tab (S TyStr) "alternatePeripheral";
        append_to_name            = opt tab (S TyStr) "appendToName";
        base_address              = opt tab (S TyTargetInt) "baseAddress";
        description               = description_of_tab_inl tab;
        disable_condition         = opt tab (S TyStr) "disableCondition";
        group_name                = opt tab (S TyStr) "groupName";
        header_struct_name        = opt tab (S TyStr) "headerStructName";
        interrupts                = dfl tab (L (C TyInterrupt)) "interrupts" []
                                      |> make_map_of (annotated_of_tab (S TySmallInt));
        name                      = req tab (S TyStr) "name";
        prepend_to_name           = opt tab (S TyStr) "prependToName";
        registers                 = register_map_of_tab_inl rp tab;
        version                   = opt tab (S TyStr) "version";
      })

  let cpu_of_tab tab : Cpu.t =
    Cpu.({
      dcache_present              = dfl tab (S TyBool) "dcachePresent" false;
      dtcm_present                = dfl tab (S TyBool) "dtcmPresent" false;
      endianness                  = opt tab (S TyEndianness) "endian";
      fpu                         = fpu_of_tab_inl tab;
      icache_present              = dfl tab (S TyBool) "icachePresent" false;
      itcm_present                = dfl tab (S TyBool) "itcmPresent" false;
      mpu_present                 = dfl tab (S TyBool) "mpuPresent" false;
      name                        = req tab (S TyStr) "name";
      nvic_prio_bits              = opt tab (S TySmallInt) "nvicPrioBits";
      revision                    = opt tab (S TyStr) "revision";
      vendor_systick_config       = dfl tab (S TyBool) "vendorSystickConfig" false;
      vtor_present                = dfl tab (S TyBool) "vtorPresent" false;
    })

  let apply (tab:compound) : Device.t =
    let rp = RegisterProperties.of_tab tab in
      Device.({
        address_unit_bits         = req tab (S TySmallInt) "addressUnitBits";
        cpu                       = opt tab (C TyCpu) "cpu" 
                                      |> Option.map ~f:cpu_of_tab;
        description               = description_of_tab_inl tab;
        header_definitions_prefix = opt tab (S TyStr) "headerDefinitionsPrefix";
        header_system_filename    = opt tab (S TyStr) "headerSystemFilename";
        name                      = req tab (S TyStr) "name";
        peripherals               = dfl tab (L (C TyPeripheral)) "peripherals" [] 
                                      |> make_map_of (peripheral_of_tab rp);
        series                    = opt tab (S TyStr) "series";
        vendor                    = opt tab (S TyStr) "vendor";
        vendor_id                 = opt tab (S TyStr) "vendorID";
        version                   = req tab (S TyStr) "version";
        width                     = req tab (S TySmallInt) "width";
      })

  (* }}} *)

end

(* ********************************************************************************************* *)
let parse filename =
  Injest.from_filename filename
    |> Materialize.apply
    |> Finalize.apply

