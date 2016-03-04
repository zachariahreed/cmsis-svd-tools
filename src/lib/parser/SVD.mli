open Core.Std 

type bits =
    Bit of int
  | BitRange of int * int
    [@@deriving sexp_of]

type 'a mask_and_value =
    Value of 'a
  | MaskedValue of 'a * 'a
    [@@deriving sexp_of]

type cint =
    UInt8
  | UInt16
  | UInt32
  | UInt64
  | Int8
  | Int16
  | Int32
  | Int64

type ctype =
    Pointer of cint
  | Immediate of cint

type access = 
    ReadOnly
  | WriteOnly 
  | ReadWrite 
  | WriteOnce 
  | ReadWriteOnce  

type 'a annotated = {
  description                  : string option;
  value                        : 'a;
}

module EnumeratedValues : sig 

  type evmap = {
    values                     : TargetInt.t mask_and_value annotated String.Map.t;
    default                    : string option;
  }

  type t =
      ReadWrite of evmap
    | ReadOnly of evmap
    | WriteOnly of evmap
    | ReadOnlyWriteOnly of evmap * evmap

end 

module Register : sig

  type read_action = 
      Clear 
    | Set 
    | Modify 
    | ModifyExternal  

  type modified_write_values = 
      OneToClear 
    | OneToSet 
    | OneToToggle 
    | ZeroToClear 
    | ZeroToSet 
    | ZeroToToggle 
    | Clear 
    | Set 
    | Modify  

  type protection = 
      Secure 
    | NonSecure 
    | Privileged  

  module Field : sig

    type t = {
      description              : string option;
      access                   : access option;
      bits                     : bits;
      enumerated_values        : EnumeratedValues.t option;
      modified_write_values    : modified_write_values option;
      read_action              : read_action option;
    }

  end

  type t = {
    description                : string option;
    access                     : access option;
    address                    : TargetInt.t;
    alternate_group            : string option;
    alternate_register         : string option;
    data_type                  : ctype option;
    display_name               : string option;
    fields                     : Field.t String.Map.t;
    modified_write_values      : modified_write_values option;
    protection                 : protection option;
    read_action                : read_action option;
    reset                      : TargetInt.t mask_and_value sexp_option;
    size                       : int option;
  }

end

module RegisterGroup : sig

  type index =
      Range of int * int
    | List of string list

  type dim = {
    count                      : int;
    increment                  : int;
    index                      : index option;
    array                      : bool;
  }

  type cluster = {
    description                : string option;
    alternate_cluster          : string option;
    header_struct_name         : string option;
    base_address               : TargetInt.t option;
    registers                  : t String.Map.t;
  }

  and t = 
      R of Register.t * (dim option)
    | C of cluster * (dim option)

end


module Peripheral : sig

  module Block : sig

    type usage = 
        Registers
      | Buffer 
      | Reserved  

    type t = {
      address                  : TargetInt.t;
      size                     : TargetInt.t;
      usage                    : usage;
    }

    val usage_to_string : usage -> string

  end

  type t = {
    name                       : string;
    description                : string option;
    address_blocks             : Block.t list;
    append_to_name             : string option;
    base_address               : TargetInt.t option;
    disable_condition          : string option;
    group_name                 : string option;
    interrupts                 : int annotated String.Map.t;
    prepend_to_name            : string option;
    registers                  : RegisterGroup.t String.Map.t;
    version                    : string option;
    alternate_peripheral       : string option;
    header_struct_name         : string option;
  }

end

module Cpu : sig

  type fpu =
      Absent
    | SinglePrecision
    | DoublePrecision

  type endianness = 
      Big 
    | Little 
    | Selectable 
    | Other  

  type t = {
    name                       : string;
    revision                   : string option;
    endianness                 : endianness option;
    fpu                        : fpu;
    dcache_present             : bool;
    dtcm_present               : bool;
    icache_present             : bool;
    itcm_present               : bool;
    mpu_present                : bool;
    nvic_prio_bits             : int option;
    vendor_systick_config      : bool;
    vtor_present               : bool;
  }

end

module Device : sig

  type t = {
    name                       : string;
    version                    : string;
    vendor                     : string option;
    vendor_id                  : string option;
    series                     : string option;
    description                : string option;
    address_unit_bits          : int;
    width                      : int;
    cpu                        : Cpu.t option;
    peripherals                : Peripheral.t String.Map.t;
    header_system_filename     : string option;
    header_definitions_prefix  : string option;
  }

  val sexp_of_t : t -> Sexp.t

end

