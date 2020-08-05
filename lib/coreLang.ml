(* open Typed *)
(* open Util *)

module P4Int = Types.P4Int

type datatype =
  | TInteger
  | TBitstring of int
  | TError
  | TMatchKind
  | TRecord of { fields : (string * datatype) list; }
  | THeader of { fields : (string * datatype) list; }
  | TTypeName of string

and parsertype = unit (* TODO *)

and controltype = unit (* TODO *)

and tabletype = unit (* TODO *)

and coretype =
  | TDataType of datatype
  | TParser of parsertype
  | TControl of controltype
  | TTable of tabletype
  | TFunction of {
    typ_params : string list;
    params : (dir * string * datatype) list;
    ret : datatype;
  }
  | TConstructor of {
    params : (string * datatype) list;
    ret : coretype;
  }

and dir =
  | In
  | Out
  | InOut

and uop = unit (* TODO *)

and bop = unit (* TODO *)

and expr =
  | Integer of {
    v : int
  }
  | Bitstring of {
    w : int;
    v : Bigint.t;
  }
  | Var of {
    name : string
  }
  | Uop of {
    op : uop;
    expr : expr
  }
  | Bop of {
    op : bop;
    lhs : expr;
    rhs : expr;
  }
  | Cast of {
    typ : datatype;
    expr : expr;
  }
  | Record of {
    fields : (string * expr) list;
  }
  | ExprMember of {
    expr : expr;
    mem : string;
  }
  | Error of {
    name : string
  }
  | FunctionCall of {
    expr : expr;
    typ_args : datatype list;
    args : expr list;
  }

and ctrl_stmt =
  | CMethodCall of {
    expr : expr;
    typ_args : datatype list;
    args : expr list;
  }
  | CAssign of {
    lhs : expr;
    rhs : expr;
  }
  | Conditional of {
    guard : expr;
    t : ctrl_stmt;
    f : ctrl_stmt;
  }
  | Block of {
    blk : ctrl_stmt list;
  }
  | Exit
  | Return of {
    v : expr option;
  }
  | CVarDecl of {
    decl : var_decl
  }

and prsr_stmt =
  | PAssign of {
    lhs : expr;
    rhs : expr;
  }
  | PVarDecl of {
    decl : var_decl
  }
  | PMethodCall of {
    expr : expr;
    typ_args : datatype list;
    args : expr list;
  }
  | Select of {
    cases : select;
  }

and select = unit (* TODO *)

and var_decl =
  | Variable of {
    typ : datatype;
    var : string;
    expr : expr option;
  }
  | Instantiation of {
    typ_name : string;
    args : expr list;
    var : string;
  }

and error_decl = string list

and matchkind_decl = string list

and typ_decl = {
  typ : coretype;
  name : string;
}

and obj_decl =
  | Table of table_decl
  | Control of control_decl
  | Parser of parser_decl
  | Function of function_decl

and table_decl = unit (* TODO *)

and control_decl = unit (* TODO *)

and parser_decl = unit (* TODO *)

and function_decl = unit (* TODO *)

and extern_decl = unit (* TODO *)

and program = {
  error : error_decl;
  matchkind : matchkind_decl;
  typs : typ_decl list;
  externs : extern_decl list;
  objs : obj_decl list;
  main : ctrl_stmt list;
}