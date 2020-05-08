open Types
open Typed
open Prog
open Value

exception BadEnvironment of string
exception UnboundName of name

module EvalEnv : sig
  type t [@@deriving sexp,yojson]

  val empty_eval_env : t

  val get_toplevel : t -> t
  val get_val_firstlevel : t -> (string * value) list

  val get_namespace : t -> string
  val set_namespace : string -> t -> t

  val insert_val_bare : string -> value -> t -> t
  val insert_decl_bare : string -> Declaration.t -> t -> t
  val insert_typ_bare : string -> Type.t -> t -> t

  val insert_val : name -> value -> t -> t
  val insert_decl: name -> Declaration.t -> t -> t
  val insert_typ : name -> Type.t -> t -> t

  val insert_vals_bare : (string * value) list -> t -> t
  val insert_decls_bare : (string  * Declaration.t) list -> t ->t
  val insert_typs_bare : (string * Type.t) list -> t -> t

  val insert_vals : (name * value) list -> t -> t
  val insert_decls: (name * Declaration.t) list -> t ->t
  val insert_typs : (name * Type.t) list -> t -> t

  val find_val : name -> t -> value
  val find_decl : name -> t -> Declaration.t
  val find_typ : name -> t -> Type.t

  val push_scope : t -> t
  val pop_scope : t -> t

  val print_env : t -> unit
end

module CheckerEnv : sig
  type t [@@deriving sexp,yojson]

  val empty_t : t

  val resolve_type_name_opt : name -> t -> Typed.Type.t option
  val resolve_type_name : name -> t -> Typed.Type.t
  val find_type_of_opt : name -> t -> (Typed.Type.t * Typed.direction) option
  val find_type_of : name -> t -> Typed.Type.t * Typed.direction
  val find_types_of : name -> t -> (Typed.Type.t * Typed.direction) list

  val insert_type : name -> Typed.Type.t -> t -> t
  val insert_type_of : name -> Typed.Type.t -> t -> t
  val insert_dir_type_of : name -> Typed.Type.t -> Typed.direction -> t -> t
  val insert_type_var : name -> t -> t
  val push_scope : t -> t
  val pop_scope : t -> t

  val resolve_type_name_opt : name -> t -> Typed.Type.t option
  val resolve_type_name : name -> t -> Typed.Type.t
  val find_type_of_opt : name -> t -> (Typed.Type.t * Typed.direction) option
  val find_type_of : name -> t -> Typed.Type.t * Typed.direction
  val find_const : name -> t -> value
  val find_const_opt : name -> t -> value option

  val insert_type : name -> Typed.Type.t -> t -> t
  val insert_types : (string * Typed.Type.t) list -> t -> t
  val insert_type_of : name -> Typed.Type.t -> t -> t
  val insert_dir_type_of : name -> Typed.Type.t -> Typed.direction -> t -> t
  val insert_type_var : name -> t -> t
  val insert_type_vars : string list -> t -> t
  val insert_const : name -> value -> t -> t
  val push_scope : t -> t
  val pop_scope : t -> t

  val eval_env_of_t : t -> EvalEnv.t
end
