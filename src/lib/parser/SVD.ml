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
    [@@deriving sexp_of]

type ctype =
    Pointer of cint
  | Immediate of cint
    [@@deriving sexp_of]

type access = 
    ReadOnly
  | WriteOnly 
  | ReadWrite 
  | WriteOnce 
  | ReadWriteOnce  
    [@@deriving sexp_of]

type 'a annotated = {
  description                : string sexp_option;
  value                      : 'a;
} [@@deriving sexp_of,make]

module EnumeratedValues = struct 

  type evmap = {
    values                     : TargetInt.t mask_and_value annotated String.Map.t;
    default                    : string sexp_option;
  } [@@deriving sexp_of,make]

  type t =
      ReadWrite of evmap
    | ReadOnly of evmap
    | WriteOnly of evmap
    | ReadOnlyWriteOnly of evmap * evmap
      [@@deriving sexp_of]

end 

module Register = struct

  type read_action = 
      Clear 
    | Set 
    | Modify 
    | ModifyExternal  
      [@@deriving sexp_of]

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
      [@@deriving sexp_of]

  type protection = 
      Secure 
    | NonSecure 
    | Privileged  
      [@@deriving sexp_of]

  module Field = struct

    type t = {
      description                : string sexp_option;
      access                     : access sexp_option;
      bits                       : bits;
      enumerated_values          : EnumeratedValues.t sexp_option;
      modified_write_values      : modified_write_values sexp_option;
      read_action                : read_action sexp_option;
    } [@@deriving sexp_of,make]

  end

  type t = {
    description                : string sexp_option;
    access                     : access sexp_option;
    address                    : TargetInt.t;
    alternate_group            : string sexp_option;
    alternate_register         : string sexp_option;
    data_type                  : ctype sexp_option;
    display_name               : string sexp_option;
    fields                     : Field.t String.Map.t;
    modified_write_values      : modified_write_values sexp_option;
    protection                 : protection sexp_option;
    read_action                : read_action sexp_option;
    reset                      : TargetInt.t mask_and_value sexp_option;
    size                       : int sexp_option;
  } [@@deriving sexp_of,make]

end

module RegisterGroup = struct

  type index =
      Range of int * int
    | List of string list
      [@@deriving sexp_of]

  type dim = {
    count                      : int;
    increment                  : int;
    index                      : index sexp_option;
    array                      : bool;
  } [@@deriving sexp_of]

  type cluster = {
    description                : string sexp_option;
    alternate_cluster          : string sexp_option;
    header_struct_name         : string sexp_option;
    base_address               : TargetInt.t sexp_option;
    registers                  : t String.Map.t;
  } [@@deriving sexp_of]

  and t = 
      R of Register.t * (dim option)
    | C of cluster * (dim option)
      [@@deriving sexp_of]

end

module Peripheral = struct

  module Block = struct

    type usage = 
        Registers
      | Buffer 
      | Reserved  
        [@@deriving sexp_of]

    type t = {
      address                    : TargetInt.t;
      size                       : TargetInt.t;
      usage                      : usage;
    } [@@deriving sexp_of,make]

    let usage_to_string =
      function
        | Registers -> "registers"
        | Buffer    -> "buffer"
        | Reserved  -> "reserved"

  end

  type t = {
    name                       : string;
    description                : string sexp_option;
    address_blocks             : Block.t list;
    append_to_name             : string sexp_option;
    base_address               : TargetInt.t sexp_option;
    disable_condition          : string sexp_option;
    group_name                 : string sexp_option;
    interrupts                 : int annotated String.Map.t;
    prepend_to_name            : string sexp_option;
    registers                  : RegisterGroup.t String.Map.t;
    version                    : string sexp_option;
    alternate_peripheral       : string sexp_option;
    header_struct_name         : string sexp_option;
  } [@@deriving sexp_of,make]

end

module Cpu = struct

  type fpu =
      Absent
    | SinglePrecision
    | DoublePrecision
      [@@deriving sexp_of]

  type endianness = 
      Big 
    | Little 
    | Selectable 
    | Other  
      [@@deriving sexp_of]

  type t = {
    name                       : string;
    revision                   : string sexp_option;
    endianness                 : endianness sexp_option;
    fpu                        : fpu;
    dcache_present             : bool;
    dtcm_present               : bool;
    icache_present             : bool;
    itcm_present               : bool;
    mpu_present                : bool;
    nvic_prio_bits             : int sexp_option;
    vendor_systick_config      : bool;
    vtor_present               : bool;
  } [@@deriving sexp_of,make]

end

module Device = struct

  type t = {
    name                       : string;
    version                    : string;
    vendor                     : string sexp_option;
    vendor_id                  : string sexp_option;
    series                     : string sexp_option;
    description                : string sexp_option;
    address_unit_bits          : int;
    width                      : int;
    cpu                        : Cpu.t sexp_option;
    peripherals                : Peripheral.t String.Map.t;
    header_system_filename     : string sexp_option;
    header_definitions_prefix  : string sexp_option;
  } [@@deriving sexp_of,make]

end

