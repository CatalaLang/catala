type constructor = string
class virtual ['a] constructor_map :
  object ('a)
    constraint 'a = < visit_constructor : 'b -> 'c -> 'c; .. >
    method visit_constructor : 'b -> 'c -> 'c
  end
class virtual ['a] constructor_iter :
  object ('a)
    constraint 'a = < visit_constructor : 'b -> 'c -> unit; .. >
    method visit_constructor : 'b -> 'c -> unit
  end
type ident = string
class virtual ['a] ident_map :
  object ('a)
    constraint 'a = < visit_ident : 'b -> 'c -> 'c; .. >
    method visit_ident : 'b -> 'c -> 'c
  end
class virtual ['a] ident_iter :
  object ('a)
    constraint 'a = < visit_ident : 'b -> 'c -> unit; .. >
    method visit_ident : 'b -> 'c -> unit
  end
type qident = ident Utils.Pos.marked list
class virtual ['c] qident_map :
  object ('c)
    constraint 'c =
      < visit_ident : 'd -> 'g -> 'g;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_qident : 'd ->
                       'g Utils.Pos.marked list -> 'g Utils.Pos.marked list;
        .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> 'g -> 'g
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_qident :
      'd -> 'g Utils.Pos.marked list -> 'g Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] qident_iter :
  object ('b)
    constraint 'b =
      < visit_ident : 'c -> 'd -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_qident : 'c -> 'd Utils.Pos.marked list -> unit; .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> 'd -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_qident : 'c -> 'd Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type primitive_typ =
    Integer
  | Decimal
  | Boolean
  | Money
  | Duration
  | Text
  | Date
  | Named of constructor
class virtual ['c] primitive_typ_map :
  object ('c)
    constraint 'c =
      < visit_Boolean : 'd -> primitive_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Text : 'd -> primitive_typ;
        visit_constructor : 'd -> constructor -> constructor;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ; .. >
    method visit_Boolean : 'd -> primitive_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] primitive_typ_iter :
  object ('b)
    constraint 'b =
      < visit_Boolean : 'c -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Integer : 'c -> unit; visit_Money : 'c -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Text : 'c -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit; .. >
    method visit_Boolean : 'c -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type base_typ_data =
    Primitive of primitive_typ
  | Collection of base_typ_data Utils.Pos.marked
class virtual ['c] base_typ_data_map :
  object ('c)
    constraint 'c =
      < visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ; .. >
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] base_typ_data_iter :
  object ('b)
    constraint 'b =
      < visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Decimal : 'c -> unit;
        visit_Duration : 'c -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit; .. >
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type base_typ = Condition | Data of base_typ_data
class virtual ['c] base_typ_map :
  object ('c)
    constraint 'c =
      < visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ; .. >
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] base_typ_iter :
  object ('b)
    constraint 'b =
      < visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Integer : 'c -> unit; visit_Money : 'c -> unit;
        visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit; .. >
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type func_typ = {
  arg_typ : base_typ Utils.Pos.marked;
  return_typ : base_typ Utils.Pos.marked;
}
class virtual ['c] func_typ_map :
  object ('c)
    constraint 'c =
      < visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ; .. >
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] func_typ_iter :
  object ('b)
    constraint 'b =
      < visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Integer : 'c -> unit; visit_Money : 'c -> unit;
        visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit; .. >
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type typ = Base of base_typ | Func of func_typ
class virtual ['c] typ_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] typ_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type struct_decl_field = {
  struct_decl_field_name : ident Utils.Pos.marked;
  struct_decl_field_typ : typ Utils.Pos.marked;
}
class virtual ['c] struct_decl_field_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] struct_decl_field_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type struct_decl = {
  struct_decl_name : constructor Utils.Pos.marked;
  struct_decl_fields : struct_decl_field Utils.Pos.marked list;
}
class virtual ['c] struct_decl_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] struct_decl_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type enum_decl_case = {
  enum_decl_case_name : constructor Utils.Pos.marked;
  enum_decl_case_typ : typ Utils.Pos.marked option;
}
class virtual ['c] enum_decl_case_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] enum_decl_case_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type enum_decl = {
  enum_decl_name : constructor Utils.Pos.marked;
  enum_decl_cases : enum_decl_case Utils.Pos.marked list;
}
class virtual ['c] enum_decl_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] enum_decl_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type match_case_pattern =
    (constructor Utils.Pos.marked option * constructor Utils.Pos.marked) list *
    ident Utils.Pos.marked option
class virtual ['c] match_case_pattern_map :
  object ('c)
    constraint 'c =
      < visit_constructor : 'd -> 'g -> 'g; visit_ident : 'd -> 'h -> 'h;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case_pattern : 'd ->
                                   ('g Utils.Pos.marked option *
                                    'g Utils.Pos.marked)
                                   list * 'h Utils.Pos.marked option ->
                                   ('g Utils.Pos.marked option *
                                    'g Utils.Pos.marked)
                                   list * 'h Utils.Pos.marked option;
        .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> 'g -> 'g
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> 'h -> 'h
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case_pattern :
      'd ->
      ('g Utils.Pos.marked option * 'g Utils.Pos.marked) list *
      'h Utils.Pos.marked option ->
      ('g Utils.Pos.marked option * 'g Utils.Pos.marked) list *
      'h Utils.Pos.marked option
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] match_case_pattern_iter :
  object ('b)
    constraint 'b =
      < visit_constructor : 'c -> 'd -> unit; visit_ident : 'c -> 'f -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case_pattern : 'c ->
                                   ('d Utils.Pos.marked option *
                                    'd Utils.Pos.marked)
                                   list * 'f Utils.Pos.marked option -> 
                                   unit;
        .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> 'd -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> 'f -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case_pattern :
      'c ->
      ('d Utils.Pos.marked option * 'd Utils.Pos.marked) list *
      'f Utils.Pos.marked option -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type op_kind = KInt | KDec | KMoney | KDate | KDuration
class virtual ['a] op_kind_map :
  object ('a)
    constraint 'a =
      < visit_KDate : 'b -> op_kind; visit_KDec : 'b -> op_kind;
        visit_KDuration : 'b -> op_kind; visit_KInt : 'b -> op_kind;
        visit_KMoney : 'b -> op_kind;
        visit_op_kind : 'b -> op_kind -> op_kind; .. >
    method visit_KDate : 'b -> op_kind
    method visit_KDec : 'b -> op_kind
    method visit_KDuration : 'b -> op_kind
    method visit_KInt : 'b -> op_kind
    method visit_KMoney : 'b -> op_kind
    method visit_op_kind : 'b -> op_kind -> op_kind
  end
class virtual ['a] op_kind_iter :
  object ('a)
    constraint 'a =
      < visit_KDate : 'b -> unit; visit_KDec : 'b -> unit;
        visit_KDuration : 'b -> unit; visit_KInt : 'b -> unit;
        visit_KMoney : 'b -> unit; visit_op_kind : 'b -> op_kind -> unit;
        .. >
    method visit_KDate : 'b -> unit
    method visit_KDec : 'b -> unit
    method visit_KDuration : 'b -> unit
    method visit_KInt : 'b -> unit
    method visit_KMoney : 'b -> unit
    method visit_op_kind : 'b -> op_kind -> unit
  end
type binop =
    And
  | Or
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq
class virtual ['a] binop_map :
  object ('a)
    constraint 'a =
      < visit_Add : 'b -> op_kind -> binop; visit_And : 'b -> binop;
        visit_Div : 'b -> op_kind -> binop; visit_Eq : 'b -> binop;
        visit_Gt : 'b -> op_kind -> binop;
        visit_Gte : 'b -> op_kind -> binop; visit_KDate : 'b -> op_kind;
        visit_KDec : 'b -> op_kind; visit_KDuration : 'b -> op_kind;
        visit_KInt : 'b -> op_kind; visit_KMoney : 'b -> op_kind;
        visit_Lt : 'b -> op_kind -> binop;
        visit_Lte : 'b -> op_kind -> binop;
        visit_Mult : 'b -> op_kind -> binop; visit_Neq : 'b -> binop;
        visit_Or : 'b -> binop; visit_Sub : 'b -> op_kind -> binop;
        visit_binop : 'b -> binop -> binop;
        visit_op_kind : 'b -> op_kind -> op_kind; .. >
    method visit_Add : 'b -> op_kind -> binop
    method visit_And : 'b -> binop
    method visit_Div : 'b -> op_kind -> binop
    method visit_Eq : 'b -> binop
    method visit_Gt : 'b -> op_kind -> binop
    method visit_Gte : 'b -> op_kind -> binop
    method visit_KDate : 'b -> op_kind
    method visit_KDec : 'b -> op_kind
    method visit_KDuration : 'b -> op_kind
    method visit_KInt : 'b -> op_kind
    method visit_KMoney : 'b -> op_kind
    method visit_Lt : 'b -> op_kind -> binop
    method visit_Lte : 'b -> op_kind -> binop
    method visit_Mult : 'b -> op_kind -> binop
    method visit_Neq : 'b -> binop
    method visit_Or : 'b -> binop
    method visit_Sub : 'b -> op_kind -> binop
    method visit_binop : 'b -> binop -> binop
    method visit_op_kind : 'b -> op_kind -> op_kind
  end
class virtual ['a] binop_iter :
  object ('a)
    constraint 'a =
      < visit_Add : 'b -> op_kind -> unit; visit_And : 'b -> unit;
        visit_Div : 'b -> op_kind -> unit; visit_Eq : 'b -> unit;
        visit_Gt : 'b -> op_kind -> unit; visit_Gte : 'b -> op_kind -> unit;
        visit_KDate : 'b -> unit; visit_KDec : 'b -> unit;
        visit_KDuration : 'b -> unit; visit_KInt : 'b -> unit;
        visit_KMoney : 'b -> unit; visit_Lt : 'b -> op_kind -> unit;
        visit_Lte : 'b -> op_kind -> unit;
        visit_Mult : 'b -> op_kind -> unit; visit_Neq : 'b -> unit;
        visit_Or : 'b -> unit; visit_Sub : 'b -> op_kind -> unit;
        visit_binop : 'b -> binop -> unit;
        visit_op_kind : 'b -> op_kind -> unit; .. >
    method visit_Add : 'b -> op_kind -> unit
    method visit_And : 'b -> unit
    method visit_Div : 'b -> op_kind -> unit
    method visit_Eq : 'b -> unit
    method visit_Gt : 'b -> op_kind -> unit
    method visit_Gte : 'b -> op_kind -> unit
    method visit_KDate : 'b -> unit
    method visit_KDec : 'b -> unit
    method visit_KDuration : 'b -> unit
    method visit_KInt : 'b -> unit
    method visit_KMoney : 'b -> unit
    method visit_Lt : 'b -> op_kind -> unit
    method visit_Lte : 'b -> op_kind -> unit
    method visit_Mult : 'b -> op_kind -> unit
    method visit_Neq : 'b -> unit
    method visit_Or : 'b -> unit
    method visit_Sub : 'b -> op_kind -> unit
    method visit_binop : 'b -> binop -> unit
    method visit_op_kind : 'b -> op_kind -> unit
  end
type unop = Not | Minus of op_kind
class virtual ['a] unop_map :
  object ('a)
    constraint 'a =
      < visit_KDate : 'b -> op_kind; visit_KDec : 'b -> op_kind;
        visit_KDuration : 'b -> op_kind; visit_KInt : 'b -> op_kind;
        visit_KMoney : 'b -> op_kind; visit_Minus : 'b -> op_kind -> unop;
        visit_Not : 'b -> unop; visit_op_kind : 'b -> op_kind -> op_kind;
        visit_unop : 'b -> unop -> unop; .. >
    method visit_KDate : 'b -> op_kind
    method visit_KDec : 'b -> op_kind
    method visit_KDuration : 'b -> op_kind
    method visit_KInt : 'b -> op_kind
    method visit_KMoney : 'b -> op_kind
    method visit_Minus : 'b -> op_kind -> unop
    method visit_Not : 'b -> unop
    method visit_op_kind : 'b -> op_kind -> op_kind
    method visit_unop : 'b -> unop -> unop
  end
class virtual ['a] unop_iter :
  object ('a)
    constraint 'a =
      < visit_KDate : 'b -> unit; visit_KDec : 'b -> unit;
        visit_KDuration : 'b -> unit; visit_KInt : 'b -> unit;
        visit_KMoney : 'b -> unit; visit_Minus : 'b -> op_kind -> unit;
        visit_Not : 'b -> unit; visit_op_kind : 'b -> op_kind -> unit;
        visit_unop : 'b -> unop -> unit; .. >
    method visit_KDate : 'b -> unit
    method visit_KDec : 'b -> unit
    method visit_KDuration : 'b -> unit
    method visit_KInt : 'b -> unit
    method visit_KMoney : 'b -> unit
    method visit_Minus : 'b -> op_kind -> unit
    method visit_Not : 'b -> unit
    method visit_op_kind : 'b -> op_kind -> unit
    method visit_unop : 'b -> unop -> unit
  end
type builtin_expression = Cardinal | IntToDec | GetDay | GetMonth | GetYear
class virtual ['a] builtin_expression_map :
  object ('a)
    constraint 'a =
      < visit_Cardinal : 'b -> builtin_expression;
        visit_GetDay : 'b -> builtin_expression;
        visit_GetMonth : 'b -> builtin_expression;
        visit_GetYear : 'b -> builtin_expression;
        visit_IntToDec : 'b -> builtin_expression;
        visit_builtin_expression : 'b ->
                                   builtin_expression -> builtin_expression;
        .. >
    method visit_Cardinal : 'b -> builtin_expression
    method visit_GetDay : 'b -> builtin_expression
    method visit_GetMonth : 'b -> builtin_expression
    method visit_GetYear : 'b -> builtin_expression
    method visit_IntToDec : 'b -> builtin_expression
    method visit_builtin_expression :
      'b -> builtin_expression -> builtin_expression
  end
class virtual ['a] builtin_expression_iter :
  object ('a)
    constraint 'a =
      < visit_Cardinal : 'b -> unit; visit_GetDay : 'b -> unit;
        visit_GetMonth : 'b -> unit; visit_GetYear : 'b -> unit;
        visit_IntToDec : 'b -> unit;
        visit_builtin_expression : 'b -> builtin_expression -> unit; .. >
    method visit_Cardinal : 'b -> unit
    method visit_GetDay : 'b -> unit
    method visit_GetMonth : 'b -> unit
    method visit_GetYear : 'b -> unit
    method visit_IntToDec : 'b -> unit
    method visit_builtin_expression : 'b -> builtin_expression -> unit
  end
type literal_date = {
  literal_date_day : int Utils.Pos.marked;
  literal_date_month : int Utils.Pos.marked;
  literal_date_year : int Utils.Pos.marked;
}
class virtual ['c] literal_date_map :
  object ('c)
    constraint 'c =
      < visit_literal_date : 'd -> literal_date -> literal_date;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] literal_date_iter :
  object ('b)
    constraint 'b =
      < visit_literal_date : 'c -> literal_date -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type literal_number = Int of Z.t | Dec of Z.t * Z.t
class virtual ['a] literal_number_map :
  object ('a)
    constraint 'a =
      < visit_Dec : 'b -> Z.t -> Z.t -> literal_number;
        visit_Int : 'b -> Z.t -> literal_number;
        visit_literal_number : 'b -> literal_number -> literal_number; .. >
    method visit_Dec : 'b -> Z.t -> Z.t -> literal_number
    method visit_Int : 'b -> Z.t -> literal_number
    method visit_literal_number : 'b -> literal_number -> literal_number
  end
class virtual ['a] literal_number_iter :
  object ('a)
    constraint 'a =
      < visit_Dec : 'b -> Z.t -> Z.t -> unit; visit_Int : 'b -> Z.t -> unit;
        visit_literal_number : 'b -> literal_number -> unit; .. >
    method visit_Dec : 'b -> Z.t -> Z.t -> unit
    method visit_Int : 'b -> Z.t -> unit
    method visit_literal_number : 'b -> literal_number -> unit
  end
type literal_unit = Percent | Year | Month | Day
class virtual ['a] literal_unit_map :
  object ('a)
    constraint 'a =
      < visit_Day : 'b -> literal_unit; visit_Month : 'b -> literal_unit;
        visit_Percent : 'b -> literal_unit; visit_Year : 'b -> literal_unit;
        visit_literal_unit : 'b -> literal_unit -> literal_unit; .. >
    method visit_Day : 'b -> literal_unit
    method visit_Month : 'b -> literal_unit
    method visit_Percent : 'b -> literal_unit
    method visit_Year : 'b -> literal_unit
    method visit_literal_unit : 'b -> literal_unit -> literal_unit
  end
class virtual ['a] literal_unit_iter :
  object ('a)
    constraint 'a =
      < visit_Day : 'b -> unit; visit_Month : 'b -> unit;
        visit_Percent : 'b -> unit; visit_Year : 'b -> unit;
        visit_literal_unit : 'b -> literal_unit -> unit; .. >
    method visit_Day : 'b -> unit
    method visit_Month : 'b -> unit
    method visit_Percent : 'b -> unit
    method visit_Year : 'b -> unit
    method visit_literal_unit : 'b -> literal_unit -> unit
  end
type money_amount = { money_amount_units : Z.t; money_amount_cents : Z.t; }
class virtual ['a] money_amount_map :
  object ('a)
    constraint 'a =
      < visit_money_amount : 'b -> money_amount -> money_amount; .. >
    method visit_money_amount : 'b -> money_amount -> money_amount
  end
class virtual ['a] money_amount_iter :
  object ('a)
    constraint 'a = < visit_money_amount : 'b -> money_amount -> unit; .. >
    method visit_money_amount : 'b -> money_amount -> unit
  end
type literal =
    LNumber of literal_number Utils.Pos.marked *
      literal_unit Utils.Pos.marked option
  | LBool of bool
  | LMoneyAmount of money_amount
  | LDate of literal_date
class virtual ['c] literal_map :
  object ('c)
    constraint 'c =
      < visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Month : 'd -> literal_unit; visit_Percent : 'd -> literal_unit;
        visit_Year : 'd -> literal_unit;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_money_amount : 'd -> money_amount -> money_amount; .. >
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Month : 'd -> literal_unit
    method visit_Percent : 'd -> literal_unit
    method visit_Year : 'd -> literal_unit
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] literal_iter :
  object ('b)
    constraint 'b =
      < visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Int : 'c -> Z.t -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Month : 'c -> unit; visit_Percent : 'c -> unit;
        visit_Year : 'c -> unit; visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_money_amount : 'c -> money_amount -> unit; .. >
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Month : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_Year : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type aggregate_func =
    AggregateSum of primitive_typ
  | AggregateCount
  | AggregateExtremum of bool * primitive_typ * expression Utils.Pos.marked
  | AggregateArgExtremum of bool * primitive_typ *
      expression Utils.Pos.marked
and collection_op =
    Exists
  | Forall
  | Aggregate of aggregate_func
  | Map
  | Filter
and match_case = {
  match_case_pattern : match_case_pattern Utils.Pos.marked;
  match_case_expr : expression Utils.Pos.marked;
}
and match_cases = match_case Utils.Pos.marked list
and expression =
    MatchWith of expression Utils.Pos.marked * match_cases Utils.Pos.marked
  | IfThenElse of expression Utils.Pos.marked * expression Utils.Pos.marked *
      expression Utils.Pos.marked
  | Binop of binop Utils.Pos.marked * expression Utils.Pos.marked *
      expression Utils.Pos.marked
  | Unop of unop Utils.Pos.marked * expression Utils.Pos.marked
  | CollectionOp of collection_op Utils.Pos.marked * ident Utils.Pos.marked *
      expression Utils.Pos.marked * expression Utils.Pos.marked
  | MemCollection of expression Utils.Pos.marked *
      expression Utils.Pos.marked
  | TestMatchCase of expression Utils.Pos.marked *
      match_case_pattern Utils.Pos.marked
  | FunCall of expression Utils.Pos.marked * expression Utils.Pos.marked
  | Builtin of builtin_expression
  | Literal of literal
  | EnumInject of constructor Utils.Pos.marked option *
      constructor Utils.Pos.marked * expression Utils.Pos.marked option
  | EnumProject of expression Utils.Pos.marked * constructor Utils.Pos.marked
  | StructLit of constructor Utils.Pos.marked *
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list
  | ArrayLit of expression Utils.Pos.marked list
  | Ident of ident
  | Dotted of expression Utils.Pos.marked *
      constructor Utils.Pos.marked option * ident Utils.Pos.marked
class virtual ['c] expression_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop; visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_Or : 'd -> binop; visit_Percent : 'd -> literal_unit;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_unop : 'd -> unop -> unop; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
  end
class virtual ['b] expression_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit; visit_Exists : 'c -> unit;
        visit_Filter : 'c -> unit; visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Int : 'c -> Z.t -> unit; visit_IntToDec : 'c -> unit;
        visit_Integer : 'c -> unit; visit_KDate : 'c -> unit;
        visit_KDec : 'c -> unit; visit_KDuration : 'c -> unit;
        visit_KInt : 'c -> unit; visit_KMoney : 'c -> unit;
        visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_Or : 'c -> unit;
        visit_Percent : 'c -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_unop : 'c -> unop -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
  end
type exception_to =
    NotAnException
  | UnlabeledException
  | ExceptionToLabel of ident Utils.Pos.marked
class virtual ['c] exception_to_map :
  object ('c)
    constraint 'c =
      < visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_NotAnException : 'd -> exception_to;
        visit_UnlabeledException : 'd -> exception_to;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        .. >
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_NotAnException : 'd -> exception_to
    method visit_UnlabeledException : 'd -> exception_to
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_exception_to : 'd -> exception_to -> exception_to
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] exception_to_iter :
  object ('b)
    constraint 'b =
      < visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_NotAnException : 'c -> unit;
        visit_UnlabeledException : 'c -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        .. >
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_NotAnException : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type rule = {
  rule_label : ident Utils.Pos.marked option;
  rule_exception_to : exception_to;
  rule_parameter : ident Utils.Pos.marked option;
  rule_condition : expression Utils.Pos.marked option;
  rule_name : qident Utils.Pos.marked;
  rule_consequence : bool Utils.Pos.marked;
}
class virtual ['c] rule_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule; visit_unop : 'd -> unop -> unop;
        .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
  end
class virtual ['b] rule_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Int : 'c -> Z.t -> unit; visit_IntToDec : 'c -> unit;
        visit_Integer : 'c -> unit; visit_KDate : 'c -> unit;
        visit_KDec : 'c -> unit; visit_KDuration : 'c -> unit;
        visit_KInt : 'c -> unit; visit_KMoney : 'c -> unit;
        visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit; visit_unop : 'c -> unop -> unit;
        .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
  end
type definition = {
  definition_label : ident Utils.Pos.marked option;
  definition_exception_to : exception_to;
  definition_name : qident Utils.Pos.marked;
  definition_parameter : ident Utils.Pos.marked option;
  definition_condition : expression Utils.Pos.marked option;
  definition_expr : expression Utils.Pos.marked;
}
class virtual ['c] definition_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_unop : 'd -> unop -> unop; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
  end
class virtual ['b] definition_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Int : 'c -> Z.t -> unit; visit_IntToDec : 'c -> unit;
        visit_Integer : 'c -> unit; visit_KDate : 'c -> unit;
        visit_KDec : 'c -> unit; visit_KDuration : 'c -> unit;
        visit_KInt : 'c -> unit; visit_KMoney : 'c -> unit;
        visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_unop : 'c -> unop -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
  end
type variation_typ = Increasing | Decreasing
class virtual ['c] variation_typ_map :
  object ('c)
    constraint 'c =
      < visit_Decreasing : 'd -> variation_typ;
        visit_Increasing : 'd -> variation_typ;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Decreasing : 'd -> variation_typ
    method visit_Increasing : 'd -> variation_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] variation_typ_iter :
  object ('b)
    constraint 'b =
      < visit_Decreasing : 'c -> unit; visit_Increasing : 'c -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Decreasing : 'c -> unit
    method visit_Increasing : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type meta_assertion =
    FixedBy of qident Utils.Pos.marked * ident Utils.Pos.marked
  | VariesWith of qident Utils.Pos.marked * expression Utils.Pos.marked *
      variation_typ Utils.Pos.marked option
class virtual ['c] meta_assertion_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop; visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_Or : 'd -> binop; visit_Percent : 'd -> literal_unit;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] meta_assertion_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Decreasing : 'c -> unit; visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit; visit_Exists : 'c -> unit;
        visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_Or : 'c -> unit;
        visit_Percent : 'c -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type assertion = {
  assertion_condition : expression Utils.Pos.marked option;
  assertion_content : expression Utils.Pos.marked;
}
class virtual ['c] assertion_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop; visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_Or : 'd -> binop; visit_Percent : 'd -> literal_unit;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_unop : 'd -> unop -> unop; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
  end
class virtual ['b] assertion_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit; visit_Exists : 'c -> unit;
        visit_Filter : 'c -> unit; visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Int : 'c -> Z.t -> unit; visit_IntToDec : 'c -> unit;
        visit_Integer : 'c -> unit; visit_KDate : 'c -> unit;
        visit_KDec : 'c -> unit; visit_KDuration : 'c -> unit;
        visit_KInt : 'c -> unit; visit_KMoney : 'c -> unit;
        visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_Or : 'c -> unit;
        visit_Percent : 'c -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_unop : 'c -> unop -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
  end
type scope_use_item =
    Rule of rule
  | Definition of definition
  | Assertion of assertion
  | MetaAssertion of meta_assertion
class virtual ['c] scope_use_item_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] scope_use_item_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type scope_use = {
  scope_use_condition : expression Utils.Pos.marked option;
  scope_use_name : constructor Utils.Pos.marked;
  scope_use_items : scope_use_item Utils.Pos.marked list;
}
class virtual ['c] scope_use_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] scope_use_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Date : 'c -> unit; visit_Day : 'c -> unit;
        visit_Dec : 'c -> Z.t -> Z.t -> unit; visit_Decimal : 'c -> unit;
        visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_GetDay : 'c -> unit; visit_GetMonth : 'c -> unit;
        visit_GetYear : 'c -> unit; visit_Gt : 'c -> op_kind -> unit;
        visit_Gte : 'c -> op_kind -> unit; visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type scope_decl_context_scope = {
  scope_decl_context_scope_name : ident Utils.Pos.marked;
  scope_decl_context_scope_sub_scope : constructor Utils.Pos.marked;
}
class virtual ['c] scope_decl_context_scope_map :
  object ('c)
    constraint 'c =
      < visit_constructor : 'd -> constructor -> constructor;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] scope_decl_context_scope_iter :
  object ('b)
    constraint 'b =
      < visit_constructor : 'c -> constructor -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type scope_decl_context_data = {
  scope_decl_context_item_name : ident Utils.Pos.marked;
  scope_decl_context_item_typ : typ Utils.Pos.marked;
}
class virtual ['c] scope_decl_context_data_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method private visit_string : 'env. 'env -> string -> string
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] scope_decl_context_data_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type scope_decl_context_item =
    ContextData of scope_decl_context_data
  | ContextScope of scope_decl_context_scope
class virtual ['c] scope_decl_context_item_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method private visit_string : 'env. 'env -> string -> string
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] scope_decl_context_item_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type scope_decl = {
  scope_decl_name : constructor Utils.Pos.marked;
  scope_decl_context : scope_decl_context_item Utils.Pos.marked list;
}
class virtual ['c] scope_decl_map :
  object ('c)
    constraint 'c =
      < visit_Base : 'd -> base_typ -> typ;
        visit_Boolean : 'd -> primitive_typ;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ;
        visit_Decimal : 'd -> primitive_typ;
        visit_Duration : 'd -> primitive_typ;
        visit_Func : 'd -> func_typ -> typ;
        visit_Integer : 'd -> primitive_typ;
        visit_Money : 'd -> primitive_typ;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Text : 'd -> primitive_typ;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_constructor : 'd -> constructor -> constructor;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_typ : 'd -> typ -> typ; .. >
    method visit_Base : 'd -> base_typ -> typ
    method visit_Boolean : 'd -> primitive_typ
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Decimal : 'd -> primitive_typ
    method visit_Duration : 'd -> primitive_typ
    method visit_Func : 'd -> func_typ -> typ
    method visit_Integer : 'd -> primitive_typ
    method visit_Money : 'd -> primitive_typ
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Text : 'd -> primitive_typ
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_constructor : 'd -> constructor -> constructor
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method private visit_string : 'env. 'env -> string -> string
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] scope_decl_iter :
  object ('b)
    constraint 'b =
      < visit_Base : 'c -> base_typ -> unit; visit_Boolean : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Decimal : 'c -> unit; visit_Duration : 'c -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_Integer : 'c -> unit;
        visit_Money : 'c -> unit; visit_Named : 'c -> constructor -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Text : 'c -> unit; visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_typ : 'c -> typ -> unit; .. >
    method visit_Base : 'c -> base_typ -> unit
    method visit_Boolean : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Decimal : 'c -> unit
    method visit_Duration : 'c -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_Integer : 'c -> unit
    method visit_Money : 'c -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Text : 'c -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_constructor : 'c -> constructor -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type code_item =
    ScopeUse of scope_use
  | ScopeDecl of scope_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl
class virtual ['c] code_item_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Base : 'd -> base_typ -> typ;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumDecl : 'd -> enum_decl -> code_item;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_Func : 'd -> func_typ -> typ;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_ScopeDecl : 'd -> scope_decl -> code_item;
        visit_ScopeUse : 'd -> scope_use -> code_item;
        visit_StructDecl : 'd -> struct_decl -> code_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_code_item : 'd -> code_item -> code_item;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Base : 'd -> base_typ -> typ
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumDecl : 'd -> enum_decl -> code_item
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Func : 'd -> func_typ -> typ
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_ScopeDecl : 'd -> scope_decl -> code_item
    method visit_ScopeUse : 'd -> scope_use -> code_item
    method visit_StructDecl : 'd -> struct_decl -> code_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_code_item : 'd -> code_item -> code_item
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] code_item_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Base : 'c -> base_typ -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Decimal : 'c -> unit; visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumDecl : 'c -> enum_decl -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_GetDay : 'c -> unit;
        visit_GetMonth : 'c -> unit; visit_GetYear : 'c -> unit;
        visit_Gt : 'c -> op_kind -> unit; visit_Gte : 'c -> op_kind -> unit;
        visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_ScopeDecl : 'c -> scope_decl -> unit;
        visit_ScopeUse : 'c -> scope_use -> unit;
        visit_StructDecl : 'c -> struct_decl -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_code_item : 'c -> code_item -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Base : 'c -> base_typ -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumDecl : 'c -> enum_decl -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_ScopeDecl : 'c -> scope_decl -> unit
    method visit_ScopeUse : 'c -> scope_use -> unit
    method visit_StructDecl : 'c -> struct_decl -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_code_item : 'c -> code_item -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type code_block = code_item Utils.Pos.marked list
class virtual ['c] code_block_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Base : 'd -> base_typ -> typ;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumDecl : 'd -> enum_decl -> code_item;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_Func : 'd -> func_typ -> typ;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_ScopeDecl : 'd -> scope_decl -> code_item;
        visit_ScopeUse : 'd -> scope_use -> code_item;
        visit_StructDecl : 'd -> struct_decl -> code_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_code_block : 'd ->
                           code_item Utils.Pos.marked list ->
                           code_item Utils.Pos.marked list;
        visit_code_item : 'd -> code_item -> code_item;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Base : 'd -> base_typ -> typ
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumDecl : 'd -> enum_decl -> code_item
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Func : 'd -> func_typ -> typ
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_ScopeDecl : 'd -> scope_decl -> code_item
    method visit_ScopeUse : 'd -> scope_use -> code_item
    method visit_StructDecl : 'd -> struct_decl -> code_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_code_block :
      'd ->
      code_item Utils.Pos.marked list -> code_item Utils.Pos.marked list
    method visit_code_item : 'd -> code_item -> code_item
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] code_block_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Base : 'c -> base_typ -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Decimal : 'c -> unit; visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumDecl : 'c -> enum_decl -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_GetDay : 'c -> unit;
        visit_GetMonth : 'c -> unit; visit_GetYear : 'c -> unit;
        visit_Gt : 'c -> op_kind -> unit; visit_Gte : 'c -> op_kind -> unit;
        visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_ScopeDecl : 'c -> scope_decl -> unit;
        visit_ScopeUse : 'c -> scope_use -> unit;
        visit_StructDecl : 'c -> struct_decl -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit;
        visit_code_item : 'c -> code_item -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Base : 'c -> base_typ -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumDecl : 'c -> enum_decl -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_ScopeDecl : 'c -> scope_decl -> unit
    method visit_ScopeUse : 'c -> scope_use -> unit
    method visit_StructDecl : 'c -> struct_decl -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit
    method visit_code_item : 'c -> code_item -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type source_repr = string Utils.Pos.marked
class virtual ['c] source_repr_map :
  object ('c)
    constraint 'c =
      < visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_source_repr : 'd -> 'g Utils.Pos.marked -> 'g Utils.Pos.marked;
        .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_source_repr :
      'd -> 'g Utils.Pos.marked -> 'g Utils.Pos.marked
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] source_repr_iter :
  object ('b)
    constraint 'b =
      < visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_source_repr : 'c -> 'd Utils.Pos.marked -> unit; .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_source_repr : 'c -> 'd Utils.Pos.marked -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type law_article = {
  law_article_name : string Utils.Pos.marked;
  law_article_id : string option;
  law_article_expiration_date : string option;
}
class virtual ['c] law_article_map :
  object ('c)
    constraint 'c =
      < visit_law_article : 'd -> law_article -> law_article;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_article : 'd -> law_article -> law_article
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] law_article_iter :
  object ('b)
    constraint 'b =
      < visit_law_article : 'c -> law_article -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_article : 'c -> law_article -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type law_include =
    PdfFile of string Utils.Pos.marked * int option
  | CatalaFile of string Utils.Pos.marked
  | LegislativeText of string Utils.Pos.marked
class virtual ['c] law_include_map :
  object ('c)
    constraint 'c =
      < visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include;
        visit_LegislativeText : 'd -> string Utils.Pos.marked -> law_include;
        visit_PdfFile : 'd ->
                        string Utils.Pos.marked -> int option -> law_include;
        visit_law_include : 'd -> law_include -> law_include;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        .. >
    method visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include
    method visit_LegislativeText :
      'd -> string Utils.Pos.marked -> law_include
    method visit_PdfFile :
      'd -> string Utils.Pos.marked -> int option -> law_include
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_include : 'd -> law_include -> law_include
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] law_include_iter :
  object ('b)
    constraint 'b =
      < visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit;
        visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit;
        visit_PdfFile : 'c -> string Utils.Pos.marked -> int option -> unit;
        visit_law_include : 'c -> law_include -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        .. >
    method visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit
    method visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit
    method visit_PdfFile :
      'c -> string Utils.Pos.marked -> int option -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_include : 'c -> law_include -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type law_article_item =
    LawText of string
  | CodeBlock of code_block * source_repr
class virtual ['c] law_article_item_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Base : 'd -> base_typ -> typ;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CodeBlock : 'd ->
                          code_block ->
                          string Utils.Pos.marked -> law_article_item;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumDecl : 'd -> enum_decl -> code_item;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_Func : 'd -> func_typ -> typ;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ; visit_KDate : 'd -> op_kind;
        visit_KDec : 'd -> op_kind; visit_KDuration : 'd -> op_kind;
        visit_KInt : 'd -> op_kind; visit_KMoney : 'd -> op_kind;
        visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_LawText : 'd -> string -> law_article_item;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_Percent : 'd -> literal_unit;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_ScopeDecl : 'd -> scope_decl -> code_item;
        visit_ScopeUse : 'd -> scope_use -> code_item;
        visit_StructDecl : 'd -> struct_decl -> code_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_code_block : 'd ->
                           code_item Utils.Pos.marked list ->
                           code_item Utils.Pos.marked list;
        visit_code_item : 'd -> code_item -> code_item;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_law_article_item : 'd -> law_article_item -> law_article_item;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_source_repr : 'd ->
                            string Utils.Pos.marked ->
                            string Utils.Pos.marked;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Base : 'd -> base_typ -> typ
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CodeBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_article_item
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumDecl : 'd -> enum_decl -> code_item
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Func : 'd -> func_typ -> typ
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_LawText : 'd -> string -> law_article_item
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_Percent : 'd -> literal_unit
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_ScopeDecl : 'd -> scope_decl -> code_item
    method visit_ScopeUse : 'd -> scope_use -> code_item
    method visit_StructDecl : 'd -> struct_decl -> code_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_code_block :
      'd ->
      code_item Utils.Pos.marked list -> code_item Utils.Pos.marked list
    method visit_code_item : 'd -> code_item -> code_item
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_article_item :
      'd -> law_article_item -> law_article_item
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method visit_source_repr :
      'd -> string Utils.Pos.marked -> string Utils.Pos.marked
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] law_article_item_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Base : 'c -> base_typ -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CodeBlock : 'c -> code_block -> string Utils.Pos.marked -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Decimal : 'c -> unit; visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumDecl : 'c -> enum_decl -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_GetDay : 'c -> unit;
        visit_GetMonth : 'c -> unit; visit_GetYear : 'c -> unit;
        visit_Gt : 'c -> op_kind -> unit; visit_Gte : 'c -> op_kind -> unit;
        visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_LawText : 'c -> string -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit; visit_Percent : 'c -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_ScopeDecl : 'c -> scope_decl -> unit;
        visit_ScopeUse : 'c -> scope_use -> unit;
        visit_StructDecl : 'c -> struct_decl -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit;
        visit_code_item : 'c -> code_item -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_law_article_item : 'c -> law_article_item -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_source_repr : 'c -> string Utils.Pos.marked -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Base : 'c -> base_typ -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CodeBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumDecl : 'c -> enum_decl -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_LawText : 'c -> string -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_Percent : 'c -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_ScopeDecl : 'c -> scope_decl -> unit
    method visit_ScopeUse : 'c -> scope_use -> unit
    method visit_StructDecl : 'c -> struct_decl -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit
    method visit_code_item : 'c -> code_item -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_article_item : 'c -> law_article_item -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method visit_source_repr : 'c -> string Utils.Pos.marked -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type law_heading = {
  law_heading_name : string;
  law_heading_precedence : int;
}
class virtual ['c] law_heading_map :
  object ('c)
    constraint 'c =
      < visit_law_heading : 'd -> law_heading -> law_heading; .. >
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method private visit_bool : 'env. 'env -> bool -> bool
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method private visit_float : 'env. 'env -> float -> float
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_heading : 'd -> law_heading -> law_heading
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method private visit_string : 'env. 'env -> string -> string
    method private visit_unit : 'env. 'env -> unit -> unit
  end
class virtual ['b] law_heading_iter :
  object ('b)
    constraint 'b = < visit_law_heading : 'c -> law_heading -> unit; .. >
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_heading : 'c -> law_heading -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
  end
type law_structure =
    LawInclude of law_include
  | LawHeading of law_heading * law_structure list
  | LawArticle of law_article * law_article_item list
  | MetadataBlock of code_block * source_repr
  | IntermediateText of string
class virtual ['c] law_structure_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Base : 'd -> base_typ -> typ;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include;
        visit_CodeBlock : 'd ->
                          code_block ->
                          string Utils.Pos.marked -> law_article_item;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumDecl : 'd -> enum_decl -> code_item;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_Func : 'd -> func_typ -> typ;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ;
        visit_IntermediateText : 'd -> string -> law_structure;
        visit_KDate : 'd -> op_kind; visit_KDec : 'd -> op_kind;
        visit_KDuration : 'd -> op_kind; visit_KInt : 'd -> op_kind;
        visit_KMoney : 'd -> op_kind; visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_LawArticle : 'd ->
                           law_article ->
                           law_article_item list -> law_structure;
        visit_LawHeading : 'd ->
                           law_heading -> law_structure list -> law_structure;
        visit_LawInclude : 'd -> law_include -> law_structure;
        visit_LawText : 'd -> string -> law_article_item;
        visit_LegislativeText : 'd -> string Utils.Pos.marked -> law_include;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_MetadataBlock : 'd ->
                              code_block ->
                              string Utils.Pos.marked -> law_structure;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_PdfFile : 'd ->
                        string Utils.Pos.marked -> int option -> law_include;
        visit_Percent : 'd -> literal_unit;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_ScopeDecl : 'd -> scope_decl -> code_item;
        visit_ScopeUse : 'd -> scope_use -> code_item;
        visit_StructDecl : 'd -> struct_decl -> code_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_code_block : 'd ->
                           code_item Utils.Pos.marked list ->
                           code_item Utils.Pos.marked list;
        visit_code_item : 'd -> code_item -> code_item;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_law_article : 'd -> law_article -> law_article;
        visit_law_article_item : 'd -> law_article_item -> law_article_item;
        visit_law_heading : 'd -> law_heading -> law_heading;
        visit_law_include : 'd -> law_include -> law_include;
        visit_law_structure : 'd -> law_structure -> law_structure;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_source_repr : 'd ->
                            string Utils.Pos.marked ->
                            string Utils.Pos.marked;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Base : 'd -> base_typ -> typ
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include
    method visit_CodeBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_article_item
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumDecl : 'd -> enum_decl -> code_item
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Func : 'd -> func_typ -> typ
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_IntermediateText : 'd -> string -> law_structure
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_LawArticle :
      'd -> law_article -> law_article_item list -> law_structure
    method visit_LawHeading :
      'd -> law_heading -> law_structure list -> law_structure
    method visit_LawInclude : 'd -> law_include -> law_structure
    method visit_LawText : 'd -> string -> law_article_item
    method visit_LegislativeText :
      'd -> string Utils.Pos.marked -> law_include
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_MetadataBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_structure
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_PdfFile :
      'd -> string Utils.Pos.marked -> int option -> law_include
    method visit_Percent : 'd -> literal_unit
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_ScopeDecl : 'd -> scope_decl -> code_item
    method visit_ScopeUse : 'd -> scope_use -> code_item
    method visit_StructDecl : 'd -> struct_decl -> code_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_code_block :
      'd ->
      code_item Utils.Pos.marked list -> code_item Utils.Pos.marked list
    method visit_code_item : 'd -> code_item -> code_item
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_article : 'd -> law_article -> law_article
    method visit_law_article_item :
      'd -> law_article_item -> law_article_item
    method visit_law_heading : 'd -> law_heading -> law_heading
    method visit_law_include : 'd -> law_include -> law_include
    method visit_law_structure : 'd -> law_structure -> law_structure
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method visit_source_repr :
      'd -> string Utils.Pos.marked -> string Utils.Pos.marked
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] law_structure_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Base : 'c -> base_typ -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit;
        visit_CodeBlock : 'c -> code_block -> string Utils.Pos.marked -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Decimal : 'c -> unit; visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumDecl : 'c -> enum_decl -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_GetDay : 'c -> unit;
        visit_GetMonth : 'c -> unit; visit_GetYear : 'c -> unit;
        visit_Gt : 'c -> op_kind -> unit; visit_Gte : 'c -> op_kind -> unit;
        visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_IntermediateText : 'c -> string -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_LawArticle : 'c -> law_article -> law_article_item list -> unit;
        visit_LawHeading : 'c -> law_heading -> law_structure list -> unit;
        visit_LawInclude : 'c -> law_include -> unit;
        visit_LawText : 'c -> string -> unit;
        visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_MetadataBlock : 'c ->
                              code_block -> string Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit;
        visit_PdfFile : 'c -> string Utils.Pos.marked -> int option -> unit;
        visit_Percent : 'c -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_ScopeDecl : 'c -> scope_decl -> unit;
        visit_ScopeUse : 'c -> scope_use -> unit;
        visit_StructDecl : 'c -> struct_decl -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit;
        visit_code_item : 'c -> code_item -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_law_article : 'c -> law_article -> unit;
        visit_law_article_item : 'c -> law_article_item -> unit;
        visit_law_heading : 'c -> law_heading -> unit;
        visit_law_include : 'c -> law_include -> unit;
        visit_law_structure : 'c -> law_structure -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_source_repr : 'c -> string Utils.Pos.marked -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Base : 'c -> base_typ -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit
    method visit_CodeBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumDecl : 'c -> enum_decl -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_IntermediateText : 'c -> string -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_LawArticle :
      'c -> law_article -> law_article_item list -> unit
    method visit_LawHeading : 'c -> law_heading -> law_structure list -> unit
    method visit_LawInclude : 'c -> law_include -> unit
    method visit_LawText : 'c -> string -> unit
    method visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_MetadataBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_PdfFile :
      'c -> string Utils.Pos.marked -> int option -> unit
    method visit_Percent : 'c -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_ScopeDecl : 'c -> scope_decl -> unit
    method visit_ScopeUse : 'c -> scope_use -> unit
    method visit_StructDecl : 'c -> struct_decl -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit
    method visit_code_item : 'c -> code_item -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_article : 'c -> law_article -> unit
    method visit_law_article_item : 'c -> law_article_item -> unit
    method visit_law_heading : 'c -> law_heading -> unit
    method visit_law_include : 'c -> law_include -> unit
    method visit_law_structure : 'c -> law_structure -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method visit_source_repr : 'c -> string Utils.Pos.marked -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type program_item = LawStructure of law_structure
class virtual ['c] program_item_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Base : 'd -> base_typ -> typ;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include;
        visit_CodeBlock : 'd ->
                          code_block ->
                          string Utils.Pos.marked -> law_article_item;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumDecl : 'd -> enum_decl -> code_item;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_Func : 'd -> func_typ -> typ;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ;
        visit_IntermediateText : 'd -> string -> law_structure;
        visit_KDate : 'd -> op_kind; visit_KDec : 'd -> op_kind;
        visit_KDuration : 'd -> op_kind; visit_KInt : 'd -> op_kind;
        visit_KMoney : 'd -> op_kind; visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_LawArticle : 'd ->
                           law_article ->
                           law_article_item list -> law_structure;
        visit_LawHeading : 'd ->
                           law_heading -> law_structure list -> law_structure;
        visit_LawInclude : 'd -> law_include -> law_structure;
        visit_LawStructure : 'd -> law_structure -> program_item;
        visit_LawText : 'd -> string -> law_article_item;
        visit_LegislativeText : 'd -> string Utils.Pos.marked -> law_include;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_MetadataBlock : 'd ->
                              code_block ->
                              string Utils.Pos.marked -> law_structure;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_PdfFile : 'd ->
                        string Utils.Pos.marked -> int option -> law_include;
        visit_Percent : 'd -> literal_unit;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_ScopeDecl : 'd -> scope_decl -> code_item;
        visit_ScopeUse : 'd -> scope_use -> code_item;
        visit_StructDecl : 'd -> struct_decl -> code_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_code_block : 'd ->
                           code_item Utils.Pos.marked list ->
                           code_item Utils.Pos.marked list;
        visit_code_item : 'd -> code_item -> code_item;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_law_article : 'd -> law_article -> law_article;
        visit_law_article_item : 'd -> law_article_item -> law_article_item;
        visit_law_heading : 'd -> law_heading -> law_heading;
        visit_law_include : 'd -> law_include -> law_include;
        visit_law_structure : 'd -> law_structure -> law_structure;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_program_item : 'd -> program_item -> program_item;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_source_repr : 'd ->
                            string Utils.Pos.marked ->
                            string Utils.Pos.marked;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Base : 'd -> base_typ -> typ
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include
    method visit_CodeBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_article_item
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumDecl : 'd -> enum_decl -> code_item
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Func : 'd -> func_typ -> typ
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_IntermediateText : 'd -> string -> law_structure
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_LawArticle :
      'd -> law_article -> law_article_item list -> law_structure
    method visit_LawHeading :
      'd -> law_heading -> law_structure list -> law_structure
    method visit_LawInclude : 'd -> law_include -> law_structure
    method visit_LawStructure : 'd -> law_structure -> program_item
    method visit_LawText : 'd -> string -> law_article_item
    method visit_LegislativeText :
      'd -> string Utils.Pos.marked -> law_include
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_MetadataBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_structure
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_PdfFile :
      'd -> string Utils.Pos.marked -> int option -> law_include
    method visit_Percent : 'd -> literal_unit
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_ScopeDecl : 'd -> scope_decl -> code_item
    method visit_ScopeUse : 'd -> scope_use -> code_item
    method visit_StructDecl : 'd -> struct_decl -> code_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_code_block :
      'd ->
      code_item Utils.Pos.marked list -> code_item Utils.Pos.marked list
    method visit_code_item : 'd -> code_item -> code_item
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_article : 'd -> law_article -> law_article
    method visit_law_article_item :
      'd -> law_article_item -> law_article_item
    method visit_law_heading : 'd -> law_heading -> law_heading
    method visit_law_include : 'd -> law_include -> law_include
    method visit_law_structure : 'd -> law_structure -> law_structure
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_program_item : 'd -> program_item -> program_item
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method visit_source_repr :
      'd -> string Utils.Pos.marked -> string Utils.Pos.marked
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] program_item_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Base : 'c -> base_typ -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit;
        visit_CodeBlock : 'c -> code_block -> string Utils.Pos.marked -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Decimal : 'c -> unit; visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumDecl : 'c -> enum_decl -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_GetDay : 'c -> unit;
        visit_GetMonth : 'c -> unit; visit_GetYear : 'c -> unit;
        visit_Gt : 'c -> op_kind -> unit; visit_Gte : 'c -> op_kind -> unit;
        visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_IntermediateText : 'c -> string -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_LawArticle : 'c -> law_article -> law_article_item list -> unit;
        visit_LawHeading : 'c -> law_heading -> law_structure list -> unit;
        visit_LawInclude : 'c -> law_include -> unit;
        visit_LawStructure : 'c -> law_structure -> unit;
        visit_LawText : 'c -> string -> unit;
        visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_MetadataBlock : 'c ->
                              code_block -> string Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit;
        visit_PdfFile : 'c -> string Utils.Pos.marked -> int option -> unit;
        visit_Percent : 'c -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_ScopeDecl : 'c -> scope_decl -> unit;
        visit_ScopeUse : 'c -> scope_use -> unit;
        visit_StructDecl : 'c -> struct_decl -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit;
        visit_code_item : 'c -> code_item -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_law_article : 'c -> law_article -> unit;
        visit_law_article_item : 'c -> law_article_item -> unit;
        visit_law_heading : 'c -> law_heading -> unit;
        visit_law_include : 'c -> law_include -> unit;
        visit_law_structure : 'c -> law_structure -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_program_item : 'c -> program_item -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_source_repr : 'c -> string Utils.Pos.marked -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Base : 'c -> base_typ -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit
    method visit_CodeBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumDecl : 'c -> enum_decl -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_IntermediateText : 'c -> string -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_LawArticle :
      'c -> law_article -> law_article_item list -> unit
    method visit_LawHeading : 'c -> law_heading -> law_structure list -> unit
    method visit_LawInclude : 'c -> law_include -> unit
    method visit_LawStructure : 'c -> law_structure -> unit
    method visit_LawText : 'c -> string -> unit
    method visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_MetadataBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_PdfFile :
      'c -> string Utils.Pos.marked -> int option -> unit
    method visit_Percent : 'c -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_ScopeDecl : 'c -> scope_decl -> unit
    method visit_ScopeUse : 'c -> scope_use -> unit
    method visit_StructDecl : 'c -> struct_decl -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit
    method visit_code_item : 'c -> code_item -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_article : 'c -> law_article -> unit
    method visit_law_article_item : 'c -> law_article_item -> unit
    method visit_law_heading : 'c -> law_heading -> unit
    method visit_law_include : 'c -> law_include -> unit
    method visit_law_structure : 'c -> law_structure -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_program_item : 'c -> program_item -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method visit_source_repr : 'c -> string Utils.Pos.marked -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type program = {
  program_items : program_item list;
  program_source_files : string list;
}
class virtual ['c] program_map :
  object ('c)
    constraint 'c =
      < visit_Add : 'd -> op_kind -> binop;
        visit_Aggregate : 'd -> aggregate_func -> collection_op;
        visit_AggregateArgExtremum : 'd ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked ->
                                     aggregate_func;
        visit_AggregateCount : 'd -> aggregate_func;
        visit_AggregateExtremum : 'd ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked ->
                                  aggregate_func;
        visit_AggregateSum : 'd -> primitive_typ -> aggregate_func;
        visit_And : 'd -> binop;
        visit_ArrayLit : 'd -> expression Utils.Pos.marked list -> expression;
        visit_Assertion : 'd -> assertion -> scope_use_item;
        visit_Base : 'd -> base_typ -> typ;
        visit_Binop : 'd ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> expression;
        visit_Boolean : 'd -> primitive_typ;
        visit_Builtin : 'd -> builtin_expression -> expression;
        visit_Cardinal : 'd -> builtin_expression;
        visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include;
        visit_CodeBlock : 'd ->
                          code_block ->
                          string Utils.Pos.marked -> law_article_item;
        visit_Collection : 'd ->
                           base_typ_data Utils.Pos.marked -> base_typ_data;
        visit_CollectionOp : 'd ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> expression;
        visit_Condition : 'd -> base_typ;
        visit_ContextData : 'd ->
                            scope_decl_context_data ->
                            scope_decl_context_item;
        visit_ContextScope : 'd ->
                             scope_decl_context_scope ->
                             scope_decl_context_item;
        visit_Data : 'd -> base_typ_data -> base_typ;
        visit_Date : 'd -> primitive_typ; visit_Day : 'd -> literal_unit;
        visit_Dec : 'd -> Z.t -> Z.t -> literal_number;
        visit_Decimal : 'd -> primitive_typ;
        visit_Decreasing : 'd -> variation_typ;
        visit_Definition : 'd -> definition -> scope_use_item;
        visit_Div : 'd -> op_kind -> binop;
        visit_Dotted : 'd ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> expression;
        visit_Duration : 'd -> primitive_typ;
        visit_EnumDecl : 'd -> enum_decl -> code_item;
        visit_EnumInject : 'd ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> expression;
        visit_EnumProject : 'd ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> expression;
        visit_Eq : 'd -> binop;
        visit_ExceptionToLabel : 'd -> ident Utils.Pos.marked -> exception_to;
        visit_Exists : 'd -> collection_op;
        visit_Filter : 'd -> collection_op;
        visit_FixedBy : 'd ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> meta_assertion;
        visit_Forall : 'd -> collection_op;
        visit_FunCall : 'd ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> expression;
        visit_Func : 'd -> func_typ -> typ;
        visit_GetDay : 'd -> builtin_expression;
        visit_GetMonth : 'd -> builtin_expression;
        visit_GetYear : 'd -> builtin_expression;
        visit_Gt : 'd -> op_kind -> binop;
        visit_Gte : 'd -> op_kind -> binop;
        visit_Ident : 'd -> ident -> expression;
        visit_IfThenElse : 'd ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> expression;
        visit_Increasing : 'd -> variation_typ;
        visit_Int : 'd -> Z.t -> literal_number;
        visit_IntToDec : 'd -> builtin_expression;
        visit_Integer : 'd -> primitive_typ;
        visit_IntermediateText : 'd -> string -> law_structure;
        visit_KDate : 'd -> op_kind; visit_KDec : 'd -> op_kind;
        visit_KDuration : 'd -> op_kind; visit_KInt : 'd -> op_kind;
        visit_KMoney : 'd -> op_kind; visit_LBool : 'd -> bool -> literal;
        visit_LDate : 'd -> literal_date -> literal;
        visit_LMoneyAmount : 'd -> money_amount -> literal;
        visit_LNumber : 'd ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> literal;
        visit_LawArticle : 'd ->
                           law_article ->
                           law_article_item list -> law_structure;
        visit_LawHeading : 'd ->
                           law_heading -> law_structure list -> law_structure;
        visit_LawInclude : 'd -> law_include -> law_structure;
        visit_LawStructure : 'd -> law_structure -> program_item;
        visit_LawText : 'd -> string -> law_article_item;
        visit_LegislativeText : 'd -> string Utils.Pos.marked -> law_include;
        visit_Literal : 'd -> literal -> expression;
        visit_Lt : 'd -> op_kind -> binop;
        visit_Lte : 'd -> op_kind -> binop; visit_Map : 'd -> collection_op;
        visit_MatchWith : 'd ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> expression;
        visit_MemCollection : 'd ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> expression;
        visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item;
        visit_MetadataBlock : 'd ->
                              code_block ->
                              string Utils.Pos.marked -> law_structure;
        visit_Minus : 'd -> op_kind -> unop;
        visit_Money : 'd -> primitive_typ; visit_Month : 'd -> literal_unit;
        visit_Mult : 'd -> op_kind -> binop;
        visit_Named : 'd -> constructor -> primitive_typ;
        visit_Neq : 'd -> binop; visit_Not : 'd -> unop;
        visit_NotAnException : 'd -> exception_to; visit_Or : 'd -> binop;
        visit_PdfFile : 'd ->
                        string Utils.Pos.marked -> int option -> law_include;
        visit_Percent : 'd -> literal_unit;
        visit_Primitive : 'd -> primitive_typ -> base_typ_data;
        visit_Rule : 'd -> rule -> scope_use_item;
        visit_ScopeDecl : 'd -> scope_decl -> code_item;
        visit_ScopeUse : 'd -> scope_use -> code_item;
        visit_StructDecl : 'd -> struct_decl -> code_item;
        visit_StructLit : 'd ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> expression;
        visit_Sub : 'd -> op_kind -> binop;
        visit_TestMatchCase : 'd ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked ->
                              expression;
        visit_Text : 'd -> primitive_typ;
        visit_UnlabeledException : 'd -> exception_to;
        visit_Unop : 'd ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> expression;
        visit_VariesWith : 'd ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option ->
                           meta_assertion;
        visit_Year : 'd -> literal_unit;
        visit_aggregate_func : 'd -> aggregate_func -> aggregate_func;
        visit_assertion : 'd -> assertion -> assertion;
        visit_base_typ : 'd -> base_typ -> base_typ;
        visit_base_typ_data : 'd -> base_typ_data -> base_typ_data;
        visit_binop : 'd -> binop -> binop;
        visit_builtin_expression : 'd ->
                                   builtin_expression -> builtin_expression;
        visit_code_block : 'd ->
                           code_item Utils.Pos.marked list ->
                           code_item Utils.Pos.marked list;
        visit_code_item : 'd -> code_item -> code_item;
        visit_collection_op : 'd -> collection_op -> collection_op;
        visit_constructor : 'd -> constructor -> constructor;
        visit_definition : 'd -> definition -> definition;
        visit_enum_decl : 'd -> enum_decl -> enum_decl;
        visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case;
        visit_exception_to : 'd -> exception_to -> exception_to;
        visit_expression : 'd -> expression -> expression;
        visit_func_typ : 'd -> func_typ -> func_typ;
        visit_ident : 'd -> ident -> ident;
        visit_law_article : 'd -> law_article -> law_article;
        visit_law_article_item : 'd -> law_article_item -> law_article_item;
        visit_law_heading : 'd -> law_heading -> law_heading;
        visit_law_include : 'd -> law_include -> law_include;
        visit_law_structure : 'd -> law_structure -> law_structure;
        visit_literal : 'd -> literal -> literal;
        visit_literal_date : 'd -> literal_date -> literal_date;
        visit_literal_number : 'd -> literal_number -> literal_number;
        visit_literal_unit : 'd -> literal_unit -> literal_unit;
        visit_marked : 'a.
                         ('d -> 'a -> 'a) ->
                         'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked;
        visit_match_case : 'd -> match_case -> match_case;
        visit_match_case_pattern : 'd ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option;
        visit_match_cases : 'd -> match_cases -> match_cases;
        visit_meta_assertion : 'd -> meta_assertion -> meta_assertion;
        visit_money_amount : 'd -> money_amount -> money_amount;
        visit_op_kind : 'd -> op_kind -> op_kind;
        visit_primitive_typ : 'd -> primitive_typ -> primitive_typ;
        visit_program : 'd -> program -> program;
        visit_program_item : 'd -> program_item -> program_item;
        visit_qident : 'd ->
                       ident Utils.Pos.marked list ->
                       ident Utils.Pos.marked list;
        visit_rule : 'd -> rule -> rule;
        visit_scope_decl : 'd -> scope_decl -> scope_decl;
        visit_scope_decl_context_data : 'd ->
                                        scope_decl_context_data ->
                                        scope_decl_context_data;
        visit_scope_decl_context_item : 'd ->
                                        scope_decl_context_item ->
                                        scope_decl_context_item;
        visit_scope_decl_context_scope : 'd ->
                                         scope_decl_context_scope ->
                                         scope_decl_context_scope;
        visit_scope_use : 'd -> scope_use -> scope_use;
        visit_scope_use_item : 'd -> scope_use_item -> scope_use_item;
        visit_source_repr : 'd ->
                            string Utils.Pos.marked ->
                            string Utils.Pos.marked;
        visit_struct_decl : 'd -> struct_decl -> struct_decl;
        visit_struct_decl_field : 'd ->
                                  struct_decl_field -> struct_decl_field;
        visit_typ : 'd -> typ -> typ; visit_unop : 'd -> unop -> unop;
        visit_variation_typ : 'd -> variation_typ -> variation_typ; .. >
    method visit_Add : 'd -> op_kind -> binop
    method visit_Aggregate : 'd -> aggregate_func -> collection_op
    method visit_AggregateArgExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateCount : 'd -> aggregate_func
    method visit_AggregateExtremum :
      'd ->
      bool -> primitive_typ -> expression Utils.Pos.marked -> aggregate_func
    method visit_AggregateSum : 'd -> primitive_typ -> aggregate_func
    method visit_And : 'd -> binop
    method visit_ArrayLit :
      'd -> expression Utils.Pos.marked list -> expression
    method visit_Assertion : 'd -> assertion -> scope_use_item
    method visit_Base : 'd -> base_typ -> typ
    method visit_Binop :
      'd ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Boolean : 'd -> primitive_typ
    method visit_Builtin : 'd -> builtin_expression -> expression
    method visit_Cardinal : 'd -> builtin_expression
    method visit_CatalaFile : 'd -> string Utils.Pos.marked -> law_include
    method visit_CodeBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_article_item
    method visit_Collection :
      'd -> base_typ_data Utils.Pos.marked -> base_typ_data
    method visit_CollectionOp :
      'd ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Condition : 'd -> base_typ
    method visit_ContextData :
      'd -> scope_decl_context_data -> scope_decl_context_item
    method visit_ContextScope :
      'd -> scope_decl_context_scope -> scope_decl_context_item
    method visit_Data : 'd -> base_typ_data -> base_typ
    method visit_Date : 'd -> primitive_typ
    method visit_Day : 'd -> literal_unit
    method visit_Dec : 'd -> Z.t -> Z.t -> literal_number
    method visit_Decimal : 'd -> primitive_typ
    method visit_Decreasing : 'd -> variation_typ
    method visit_Definition : 'd -> definition -> scope_use_item
    method visit_Div : 'd -> op_kind -> binop
    method visit_Dotted :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option ->
      ident Utils.Pos.marked -> expression
    method visit_Duration : 'd -> primitive_typ
    method visit_EnumDecl : 'd -> enum_decl -> code_item
    method visit_EnumInject :
      'd ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> expression
    method visit_EnumProject :
      'd ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked -> expression
    method visit_Eq : 'd -> binop
    method visit_ExceptionToLabel :
      'd -> ident Utils.Pos.marked -> exception_to
    method visit_Exists : 'd -> collection_op
    method visit_Filter : 'd -> collection_op
    method visit_FixedBy :
      'd ->
      qident Utils.Pos.marked -> ident Utils.Pos.marked -> meta_assertion
    method visit_Forall : 'd -> collection_op
    method visit_FunCall :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Func : 'd -> func_typ -> typ
    method visit_GetDay : 'd -> builtin_expression
    method visit_GetMonth : 'd -> builtin_expression
    method visit_GetYear : 'd -> builtin_expression
    method visit_Gt : 'd -> op_kind -> binop
    method visit_Gte : 'd -> op_kind -> binop
    method visit_Ident : 'd -> ident -> expression
    method visit_IfThenElse :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_Increasing : 'd -> variation_typ
    method visit_Int : 'd -> Z.t -> literal_number
    method visit_IntToDec : 'd -> builtin_expression
    method visit_Integer : 'd -> primitive_typ
    method visit_IntermediateText : 'd -> string -> law_structure
    method visit_KDate : 'd -> op_kind
    method visit_KDec : 'd -> op_kind
    method visit_KDuration : 'd -> op_kind
    method visit_KInt : 'd -> op_kind
    method visit_KMoney : 'd -> op_kind
    method visit_LBool : 'd -> bool -> literal
    method visit_LDate : 'd -> literal_date -> literal
    method visit_LMoneyAmount : 'd -> money_amount -> literal
    method visit_LNumber :
      'd ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> literal
    method visit_LawArticle :
      'd -> law_article -> law_article_item list -> law_structure
    method visit_LawHeading :
      'd -> law_heading -> law_structure list -> law_structure
    method visit_LawInclude : 'd -> law_include -> law_structure
    method visit_LawStructure : 'd -> law_structure -> program_item
    method visit_LawText : 'd -> string -> law_article_item
    method visit_LegislativeText :
      'd -> string Utils.Pos.marked -> law_include
    method visit_Literal : 'd -> literal -> expression
    method visit_Lt : 'd -> op_kind -> binop
    method visit_Lte : 'd -> op_kind -> binop
    method visit_Map : 'd -> collection_op
    method visit_MatchWith :
      'd ->
      expression Utils.Pos.marked ->
      match_cases Utils.Pos.marked -> expression
    method visit_MemCollection :
      'd ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression
    method visit_MetaAssertion : 'd -> meta_assertion -> scope_use_item
    method visit_MetadataBlock :
      'd -> code_block -> string Utils.Pos.marked -> law_structure
    method visit_Minus : 'd -> op_kind -> unop
    method visit_Money : 'd -> primitive_typ
    method visit_Month : 'd -> literal_unit
    method visit_Mult : 'd -> op_kind -> binop
    method visit_Named : 'd -> constructor -> primitive_typ
    method visit_Neq : 'd -> binop
    method visit_Not : 'd -> unop
    method visit_NotAnException : 'd -> exception_to
    method visit_Or : 'd -> binop
    method visit_PdfFile :
      'd -> string Utils.Pos.marked -> int option -> law_include
    method visit_Percent : 'd -> literal_unit
    method visit_Primitive : 'd -> primitive_typ -> base_typ_data
    method visit_Rule : 'd -> rule -> scope_use_item
    method visit_ScopeDecl : 'd -> scope_decl -> code_item
    method visit_ScopeUse : 'd -> scope_use -> code_item
    method visit_StructDecl : 'd -> struct_decl -> code_item
    method visit_StructLit :
      'd ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list ->
      expression
    method visit_Sub : 'd -> op_kind -> binop
    method visit_TestMatchCase :
      'd ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> expression
    method visit_Text : 'd -> primitive_typ
    method visit_UnlabeledException : 'd -> exception_to
    method visit_Unop :
      'd ->
      unop Utils.Pos.marked -> expression Utils.Pos.marked -> expression
    method visit_VariesWith :
      'd ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> meta_assertion
    method visit_Year : 'd -> literal_unit
    method visit_aggregate_func : 'd -> aggregate_func -> aggregate_func
    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    method visit_assertion : 'd -> assertion -> assertion
    method visit_base_typ : 'd -> base_typ -> base_typ
    method visit_base_typ_data : 'd -> base_typ_data -> base_typ_data
    method visit_binop : 'd -> binop -> binop
    method private visit_bool : 'env. 'env -> bool -> bool
    method visit_builtin_expression :
      'd -> builtin_expression -> builtin_expression
    method private visit_bytes : 'env. 'env -> bytes -> bytes
    method private visit_char : 'env. 'env -> char -> char
    method visit_code_block :
      'd ->
      code_item Utils.Pos.marked list -> code_item Utils.Pos.marked list
    method visit_code_item : 'd -> code_item -> code_item
    method visit_collection_op : 'd -> collection_op -> collection_op
    method visit_constructor : 'd -> constructor -> constructor
    method visit_definition : 'd -> definition -> definition
    method visit_enum_decl : 'd -> enum_decl -> enum_decl
    method visit_enum_decl_case : 'd -> enum_decl_case -> enum_decl_case
    method visit_exception_to : 'd -> exception_to -> exception_to
    method visit_expression : 'd -> expression -> expression
    method private visit_float : 'env. 'env -> float -> float
    method visit_func_typ : 'd -> func_typ -> func_typ
    method visit_ident : 'd -> ident -> ident
    method private visit_int : 'env. 'env -> int -> int
    method private visit_int32 : 'env. 'env -> int32 -> int32
    method private visit_int64 : 'env. 'env -> int64 -> int64
    method visit_law_article : 'd -> law_article -> law_article
    method visit_law_article_item :
      'd -> law_article_item -> law_article_item
    method visit_law_heading : 'd -> law_heading -> law_heading
    method visit_law_include : 'd -> law_include -> law_include
    method visit_law_structure : 'd -> law_structure -> law_structure
    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    method visit_literal : 'd -> literal -> literal
    method visit_literal_date : 'd -> literal_date -> literal_date
    method visit_literal_number : 'd -> literal_number -> literal_number
    method visit_literal_unit : 'd -> literal_unit -> literal_unit
    method visit_marked :
      ('d -> 'a -> 'a) -> 'd -> 'a Utils.Pos.marked -> 'a Utils.Pos.marked
    method visit_match_case : 'd -> match_case -> match_case
    method visit_match_case_pattern :
      'd ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option
    method visit_match_cases : 'd -> match_cases -> match_cases
    method visit_meta_assertion : 'd -> meta_assertion -> meta_assertion
    method visit_money_amount : 'd -> money_amount -> money_amount
    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
    method visit_op_kind : 'd -> op_kind -> op_kind
    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    method visit_primitive_typ : 'd -> primitive_typ -> primitive_typ
    method visit_program : 'd -> program -> program
    method visit_program_item : 'd -> program_item -> program_item
    method visit_qident :
      'd -> ident Utils.Pos.marked list -> ident Utils.Pos.marked list
    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    method private visit_result :
      'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
    method visit_rule : 'd -> rule -> rule
    method visit_scope_decl : 'd -> scope_decl -> scope_decl
    method visit_scope_decl_context_data :
      'd -> scope_decl_context_data -> scope_decl_context_data
    method visit_scope_decl_context_item :
      'd -> scope_decl_context_item -> scope_decl_context_item
    method visit_scope_decl_context_scope :
      'd -> scope_decl_context_scope -> scope_decl_context_scope
    method visit_scope_use : 'd -> scope_use -> scope_use
    method visit_scope_use_item : 'd -> scope_use_item -> scope_use_item
    method visit_source_repr :
      'd -> string Utils.Pos.marked -> string Utils.Pos.marked
    method private visit_string : 'env. 'env -> string -> string
    method visit_struct_decl : 'd -> struct_decl -> struct_decl
    method visit_struct_decl_field :
      'd -> struct_decl_field -> struct_decl_field
    method visit_typ : 'd -> typ -> typ
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'd -> unop -> unop
    method visit_variation_typ : 'd -> variation_typ -> variation_typ
  end
class virtual ['b] program_iter :
  object ('b)
    constraint 'b =
      < visit_Add : 'c -> op_kind -> unit;
        visit_Aggregate : 'c -> aggregate_func -> unit;
        visit_AggregateArgExtremum : 'c ->
                                     bool ->
                                     primitive_typ ->
                                     expression Utils.Pos.marked -> unit;
        visit_AggregateCount : 'c -> unit;
        visit_AggregateExtremum : 'c ->
                                  bool ->
                                  primitive_typ ->
                                  expression Utils.Pos.marked -> unit;
        visit_AggregateSum : 'c -> primitive_typ -> unit;
        visit_And : 'c -> unit;
        visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit;
        visit_Assertion : 'c -> assertion -> unit;
        visit_Base : 'c -> base_typ -> unit;
        visit_Binop : 'c ->
                      binop Utils.Pos.marked ->
                      expression Utils.Pos.marked ->
                      expression Utils.Pos.marked -> unit;
        visit_Boolean : 'c -> unit;
        visit_Builtin : 'c -> builtin_expression -> unit;
        visit_Cardinal : 'c -> unit;
        visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit;
        visit_CodeBlock : 'c -> code_block -> string Utils.Pos.marked -> unit;
        visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit;
        visit_CollectionOp : 'c ->
                             collection_op Utils.Pos.marked ->
                             ident Utils.Pos.marked ->
                             expression Utils.Pos.marked ->
                             expression Utils.Pos.marked -> unit;
        visit_Condition : 'c -> unit;
        visit_ContextData : 'c -> scope_decl_context_data -> unit;
        visit_ContextScope : 'c -> scope_decl_context_scope -> unit;
        visit_Data : 'c -> base_typ_data -> unit; visit_Date : 'c -> unit;
        visit_Day : 'c -> unit; visit_Dec : 'c -> Z.t -> Z.t -> unit;
        visit_Decimal : 'c -> unit; visit_Decreasing : 'c -> unit;
        visit_Definition : 'c -> definition -> unit;
        visit_Div : 'c -> op_kind -> unit;
        visit_Dotted : 'c ->
                       expression Utils.Pos.marked ->
                       constructor Utils.Pos.marked option ->
                       ident Utils.Pos.marked -> unit;
        visit_Duration : 'c -> unit;
        visit_EnumDecl : 'c -> enum_decl -> unit;
        visit_EnumInject : 'c ->
                           constructor Utils.Pos.marked option ->
                           constructor Utils.Pos.marked ->
                           expression Utils.Pos.marked option -> unit;
        visit_EnumProject : 'c ->
                            expression Utils.Pos.marked ->
                            constructor Utils.Pos.marked -> unit;
        visit_Eq : 'c -> unit;
        visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit;
        visit_Exists : 'c -> unit; visit_Filter : 'c -> unit;
        visit_FixedBy : 'c ->
                        qident Utils.Pos.marked ->
                        ident Utils.Pos.marked -> unit;
        visit_Forall : 'c -> unit;
        visit_FunCall : 'c ->
                        expression Utils.Pos.marked ->
                        expression Utils.Pos.marked -> unit;
        visit_Func : 'c -> func_typ -> unit; visit_GetDay : 'c -> unit;
        visit_GetMonth : 'c -> unit; visit_GetYear : 'c -> unit;
        visit_Gt : 'c -> op_kind -> unit; visit_Gte : 'c -> op_kind -> unit;
        visit_Ident : 'c -> ident -> unit;
        visit_IfThenElse : 'c ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           expression Utils.Pos.marked -> unit;
        visit_Increasing : 'c -> unit; visit_Int : 'c -> Z.t -> unit;
        visit_IntToDec : 'c -> unit; visit_Integer : 'c -> unit;
        visit_IntermediateText : 'c -> string -> unit;
        visit_KDate : 'c -> unit; visit_KDec : 'c -> unit;
        visit_KDuration : 'c -> unit; visit_KInt : 'c -> unit;
        visit_KMoney : 'c -> unit; visit_LBool : 'c -> bool -> unit;
        visit_LDate : 'c -> literal_date -> unit;
        visit_LMoneyAmount : 'c -> money_amount -> unit;
        visit_LNumber : 'c ->
                        literal_number Utils.Pos.marked ->
                        literal_unit Utils.Pos.marked option -> unit;
        visit_LawArticle : 'c -> law_article -> law_article_item list -> unit;
        visit_LawHeading : 'c -> law_heading -> law_structure list -> unit;
        visit_LawInclude : 'c -> law_include -> unit;
        visit_LawStructure : 'c -> law_structure -> unit;
        visit_LawText : 'c -> string -> unit;
        visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit;
        visit_Literal : 'c -> literal -> unit;
        visit_Lt : 'c -> op_kind -> unit; visit_Lte : 'c -> op_kind -> unit;
        visit_Map : 'c -> unit;
        visit_MatchWith : 'c ->
                          expression Utils.Pos.marked ->
                          match_cases Utils.Pos.marked -> unit;
        visit_MemCollection : 'c ->
                              expression Utils.Pos.marked ->
                              expression Utils.Pos.marked -> unit;
        visit_MetaAssertion : 'c -> meta_assertion -> unit;
        visit_MetadataBlock : 'c ->
                              code_block -> string Utils.Pos.marked -> unit;
        visit_Minus : 'c -> op_kind -> unit; visit_Money : 'c -> unit;
        visit_Month : 'c -> unit; visit_Mult : 'c -> op_kind -> unit;
        visit_Named : 'c -> constructor -> unit; visit_Neq : 'c -> unit;
        visit_Not : 'c -> unit; visit_NotAnException : 'c -> unit;
        visit_Or : 'c -> unit;
        visit_PdfFile : 'c -> string Utils.Pos.marked -> int option -> unit;
        visit_Percent : 'c -> unit;
        visit_Primitive : 'c -> primitive_typ -> unit;
        visit_Rule : 'c -> rule -> unit;
        visit_ScopeDecl : 'c -> scope_decl -> unit;
        visit_ScopeUse : 'c -> scope_use -> unit;
        visit_StructDecl : 'c -> struct_decl -> unit;
        visit_StructLit : 'c ->
                          constructor Utils.Pos.marked ->
                          (ident Utils.Pos.marked *
                           expression Utils.Pos.marked)
                          list -> unit;
        visit_Sub : 'c -> op_kind -> unit;
        visit_TestMatchCase : 'c ->
                              expression Utils.Pos.marked ->
                              match_case_pattern Utils.Pos.marked -> unit;
        visit_Text : 'c -> unit; visit_UnlabeledException : 'c -> unit;
        visit_Unop : 'c ->
                     unop Utils.Pos.marked ->
                     expression Utils.Pos.marked -> unit;
        visit_VariesWith : 'c ->
                           qident Utils.Pos.marked ->
                           expression Utils.Pos.marked ->
                           variation_typ Utils.Pos.marked option -> unit;
        visit_Year : 'c -> unit;
        visit_aggregate_func : 'c -> aggregate_func -> unit;
        visit_assertion : 'c -> assertion -> unit;
        visit_base_typ : 'c -> base_typ -> unit;
        visit_base_typ_data : 'c -> base_typ_data -> unit;
        visit_binop : 'c -> binop -> unit;
        visit_builtin_expression : 'c -> builtin_expression -> unit;
        visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit;
        visit_code_item : 'c -> code_item -> unit;
        visit_collection_op : 'c -> collection_op -> unit;
        visit_constructor : 'c -> constructor -> unit;
        visit_definition : 'c -> definition -> unit;
        visit_enum_decl : 'c -> enum_decl -> unit;
        visit_enum_decl_case : 'c -> enum_decl_case -> unit;
        visit_exception_to : 'c -> exception_to -> unit;
        visit_expression : 'c -> expression -> unit;
        visit_func_typ : 'c -> func_typ -> unit;
        visit_ident : 'c -> ident -> unit;
        visit_law_article : 'c -> law_article -> unit;
        visit_law_article_item : 'c -> law_article_item -> unit;
        visit_law_heading : 'c -> law_heading -> unit;
        visit_law_include : 'c -> law_include -> unit;
        visit_law_structure : 'c -> law_structure -> unit;
        visit_literal : 'c -> literal -> unit;
        visit_literal_date : 'c -> literal_date -> unit;
        visit_literal_number : 'c -> literal_number -> unit;
        visit_literal_unit : 'c -> literal_unit -> unit;
        visit_marked : 'a.
                         ('c -> 'a -> unit) ->
                         'c -> 'a Utils.Pos.marked -> unit;
        visit_match_case : 'c -> match_case -> unit;
        visit_match_case_pattern : 'c ->
                                   (constructor Utils.Pos.marked option *
                                    constructor Utils.Pos.marked)
                                   list * ident Utils.Pos.marked option ->
                                   unit;
        visit_match_cases : 'c -> match_cases -> unit;
        visit_meta_assertion : 'c -> meta_assertion -> unit;
        visit_money_amount : 'c -> money_amount -> unit;
        visit_op_kind : 'c -> op_kind -> unit;
        visit_primitive_typ : 'c -> primitive_typ -> unit;
        visit_program : 'c -> program -> unit;
        visit_program_item : 'c -> program_item -> unit;
        visit_qident : 'c -> ident Utils.Pos.marked list -> unit;
        visit_rule : 'c -> rule -> unit;
        visit_scope_decl : 'c -> scope_decl -> unit;
        visit_scope_decl_context_data : 'c -> scope_decl_context_data -> unit;
        visit_scope_decl_context_item : 'c -> scope_decl_context_item -> unit;
        visit_scope_decl_context_scope : 'c ->
                                         scope_decl_context_scope -> unit;
        visit_scope_use : 'c -> scope_use -> unit;
        visit_scope_use_item : 'c -> scope_use_item -> unit;
        visit_source_repr : 'c -> string Utils.Pos.marked -> unit;
        visit_struct_decl : 'c -> struct_decl -> unit;
        visit_struct_decl_field : 'c -> struct_decl_field -> unit;
        visit_typ : 'c -> typ -> unit; visit_unop : 'c -> unop -> unit;
        visit_variation_typ : 'c -> variation_typ -> unit; .. >
    method visit_Add : 'c -> op_kind -> unit
    method visit_Aggregate : 'c -> aggregate_func -> unit
    method visit_AggregateArgExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateCount : 'c -> unit
    method visit_AggregateExtremum :
      'c -> bool -> primitive_typ -> expression Utils.Pos.marked -> unit
    method visit_AggregateSum : 'c -> primitive_typ -> unit
    method visit_And : 'c -> unit
    method visit_ArrayLit : 'c -> expression Utils.Pos.marked list -> unit
    method visit_Assertion : 'c -> assertion -> unit
    method visit_Base : 'c -> base_typ -> unit
    method visit_Binop :
      'c ->
      binop Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Boolean : 'c -> unit
    method visit_Builtin : 'c -> builtin_expression -> unit
    method visit_Cardinal : 'c -> unit
    method visit_CatalaFile : 'c -> string Utils.Pos.marked -> unit
    method visit_CodeBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Collection : 'c -> base_typ_data Utils.Pos.marked -> unit
    method visit_CollectionOp :
      'c ->
      collection_op Utils.Pos.marked ->
      ident Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Condition : 'c -> unit
    method visit_ContextData : 'c -> scope_decl_context_data -> unit
    method visit_ContextScope : 'c -> scope_decl_context_scope -> unit
    method visit_Data : 'c -> base_typ_data -> unit
    method visit_Date : 'c -> unit
    method visit_Day : 'c -> unit
    method visit_Dec : 'c -> Z.t -> Z.t -> unit
    method visit_Decimal : 'c -> unit
    method visit_Decreasing : 'c -> unit
    method visit_Definition : 'c -> definition -> unit
    method visit_Div : 'c -> op_kind -> unit
    method visit_Dotted :
      'c ->
      expression Utils.Pos.marked ->
      constructor Utils.Pos.marked option -> ident Utils.Pos.marked -> unit
    method visit_Duration : 'c -> unit
    method visit_EnumDecl : 'c -> enum_decl -> unit
    method visit_EnumInject :
      'c ->
      constructor Utils.Pos.marked option ->
      constructor Utils.Pos.marked ->
      expression Utils.Pos.marked option -> unit
    method visit_EnumProject :
      'c ->
      expression Utils.Pos.marked -> constructor Utils.Pos.marked -> unit
    method visit_Eq : 'c -> unit
    method visit_ExceptionToLabel : 'c -> ident Utils.Pos.marked -> unit
    method visit_Exists : 'c -> unit
    method visit_Filter : 'c -> unit
    method visit_FixedBy :
      'c -> qident Utils.Pos.marked -> ident Utils.Pos.marked -> unit
    method visit_Forall : 'c -> unit
    method visit_FunCall :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Func : 'c -> func_typ -> unit
    method visit_GetDay : 'c -> unit
    method visit_GetMonth : 'c -> unit
    method visit_GetYear : 'c -> unit
    method visit_Gt : 'c -> op_kind -> unit
    method visit_Gte : 'c -> op_kind -> unit
    method visit_Ident : 'c -> ident -> unit
    method visit_IfThenElse :
      'c ->
      expression Utils.Pos.marked ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_Increasing : 'c -> unit
    method visit_Int : 'c -> Z.t -> unit
    method visit_IntToDec : 'c -> unit
    method visit_Integer : 'c -> unit
    method visit_IntermediateText : 'c -> string -> unit
    method visit_KDate : 'c -> unit
    method visit_KDec : 'c -> unit
    method visit_KDuration : 'c -> unit
    method visit_KInt : 'c -> unit
    method visit_KMoney : 'c -> unit
    method visit_LBool : 'c -> bool -> unit
    method visit_LDate : 'c -> literal_date -> unit
    method visit_LMoneyAmount : 'c -> money_amount -> unit
    method visit_LNumber :
      'c ->
      literal_number Utils.Pos.marked ->
      literal_unit Utils.Pos.marked option -> unit
    method visit_LawArticle :
      'c -> law_article -> law_article_item list -> unit
    method visit_LawHeading : 'c -> law_heading -> law_structure list -> unit
    method visit_LawInclude : 'c -> law_include -> unit
    method visit_LawStructure : 'c -> law_structure -> unit
    method visit_LawText : 'c -> string -> unit
    method visit_LegislativeText : 'c -> string Utils.Pos.marked -> unit
    method visit_Literal : 'c -> literal -> unit
    method visit_Lt : 'c -> op_kind -> unit
    method visit_Lte : 'c -> op_kind -> unit
    method visit_Map : 'c -> unit
    method visit_MatchWith :
      'c ->
      expression Utils.Pos.marked -> match_cases Utils.Pos.marked -> unit
    method visit_MemCollection :
      'c ->
      expression Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_MetaAssertion : 'c -> meta_assertion -> unit
    method visit_MetadataBlock :
      'c -> code_block -> string Utils.Pos.marked -> unit
    method visit_Minus : 'c -> op_kind -> unit
    method visit_Money : 'c -> unit
    method visit_Month : 'c -> unit
    method visit_Mult : 'c -> op_kind -> unit
    method visit_Named : 'c -> constructor -> unit
    method visit_Neq : 'c -> unit
    method visit_Not : 'c -> unit
    method visit_NotAnException : 'c -> unit
    method visit_Or : 'c -> unit
    method visit_PdfFile :
      'c -> string Utils.Pos.marked -> int option -> unit
    method visit_Percent : 'c -> unit
    method visit_Primitive : 'c -> primitive_typ -> unit
    method visit_Rule : 'c -> rule -> unit
    method visit_ScopeDecl : 'c -> scope_decl -> unit
    method visit_ScopeUse : 'c -> scope_use -> unit
    method visit_StructDecl : 'c -> struct_decl -> unit
    method visit_StructLit :
      'c ->
      constructor Utils.Pos.marked ->
      (ident Utils.Pos.marked * expression Utils.Pos.marked) list -> unit
    method visit_Sub : 'c -> op_kind -> unit
    method visit_TestMatchCase :
      'c ->
      expression Utils.Pos.marked ->
      match_case_pattern Utils.Pos.marked -> unit
    method visit_Text : 'c -> unit
    method visit_UnlabeledException : 'c -> unit
    method visit_Unop :
      'c -> unop Utils.Pos.marked -> expression Utils.Pos.marked -> unit
    method visit_VariesWith :
      'c ->
      qident Utils.Pos.marked ->
      expression Utils.Pos.marked ->
      variation_typ Utils.Pos.marked option -> unit
    method visit_Year : 'c -> unit
    method visit_aggregate_func : 'c -> aggregate_func -> unit
    method private visit_array :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    method visit_assertion : 'c -> assertion -> unit
    method visit_base_typ : 'c -> base_typ -> unit
    method visit_base_typ_data : 'c -> base_typ_data -> unit
    method visit_binop : 'c -> binop -> unit
    method private visit_bool : 'env. 'env -> bool -> unit
    method visit_builtin_expression : 'c -> builtin_expression -> unit
    method private visit_bytes : 'env. 'env -> bytes -> unit
    method private visit_char : 'env. 'env -> char -> unit
    method visit_code_block : 'c -> code_item Utils.Pos.marked list -> unit
    method visit_code_item : 'c -> code_item -> unit
    method visit_collection_op : 'c -> collection_op -> unit
    method visit_constructor : 'c -> constructor -> unit
    method visit_definition : 'c -> definition -> unit
    method visit_enum_decl : 'c -> enum_decl -> unit
    method visit_enum_decl_case : 'c -> enum_decl_case -> unit
    method visit_exception_to : 'c -> exception_to -> unit
    method visit_expression : 'c -> expression -> unit
    method private visit_float : 'env. 'env -> float -> unit
    method visit_func_typ : 'c -> func_typ -> unit
    method visit_ident : 'c -> ident -> unit
    method private visit_int : 'env. 'env -> int -> unit
    method private visit_int32 : 'env. 'env -> int32 -> unit
    method private visit_int64 : 'env. 'env -> int64 -> unit
    method visit_law_article : 'c -> law_article -> unit
    method visit_law_article_item : 'c -> law_article_item -> unit
    method visit_law_heading : 'c -> law_heading -> unit
    method visit_law_include : 'c -> law_include -> unit
    method visit_law_structure : 'c -> law_structure -> unit
    method private visit_lazy_t :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    method private visit_list :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    method visit_literal : 'c -> literal -> unit
    method visit_literal_date : 'c -> literal_date -> unit
    method visit_literal_number : 'c -> literal_number -> unit
    method visit_literal_unit : 'c -> literal_unit -> unit
    method visit_marked :
      ('c -> 'a -> unit) -> 'c -> 'a Utils.Pos.marked -> unit
    method visit_match_case : 'c -> match_case -> unit
    method visit_match_case_pattern :
      'c ->
      (constructor Utils.Pos.marked option * constructor Utils.Pos.marked)
      list * ident Utils.Pos.marked option -> unit
    method visit_match_cases : 'c -> match_cases -> unit
    method visit_meta_assertion : 'c -> meta_assertion -> unit
    method visit_money_amount : 'c -> money_amount -> unit
    method private visit_nativeint : 'env. 'env -> nativeint -> unit
    method visit_op_kind : 'c -> op_kind -> unit
    method private visit_option :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    method visit_primitive_typ : 'c -> primitive_typ -> unit
    method visit_program : 'c -> program -> unit
    method visit_program_item : 'c -> program_item -> unit
    method visit_qident : 'c -> ident Utils.Pos.marked list -> unit
    method private visit_ref :
      'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    method private visit_result :
      'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) Result.result -> unit
    method visit_rule : 'c -> rule -> unit
    method visit_scope_decl : 'c -> scope_decl -> unit
    method visit_scope_decl_context_data :
      'c -> scope_decl_context_data -> unit
    method visit_scope_decl_context_item :
      'c -> scope_decl_context_item -> unit
    method visit_scope_decl_context_scope :
      'c -> scope_decl_context_scope -> unit
    method visit_scope_use : 'c -> scope_use -> unit
    method visit_scope_use_item : 'c -> scope_use_item -> unit
    method visit_source_repr : 'c -> string Utils.Pos.marked -> unit
    method private visit_string : 'env. 'env -> string -> unit
    method visit_struct_decl : 'c -> struct_decl -> unit
    method visit_struct_decl_field : 'c -> struct_decl_field -> unit
    method visit_typ : 'c -> typ -> unit
    method private visit_unit : 'env. 'env -> unit -> unit
    method visit_unop : 'c -> unop -> unit
    method visit_variation_typ : 'c -> variation_typ -> unit
  end
type source_file_or_master =
    SourceFile of program_item list
  | MasterFile of string Utils.Pos.marked list
