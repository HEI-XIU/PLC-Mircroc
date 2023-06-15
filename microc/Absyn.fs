(* File MicroC/Absyn.fs
   Abstract syntax of micro-C, an imperative language.
   sestoft@itu.dk 2009-09-25

   Must precede Interp.fs, Comp.fs and Contcomp.fs in Solution Explorer
 *)
//抽象语法树
module Absyn

// 基本类型
// 注意，数组、指针是递归类型
// 这里没有函数类型，注意与上次课的 MicroML 对比
type typ =
  | TypI                             (* Type int                    *)
  | TypC                             (* Type char                   *)
  | TypB                             (*Type boolean                 *)
  | TypS                             (* Type string                 *)
  | TypF                             (* Type float                  *)
  | TypA of typ * int option         (* Array type                  *)
  | TypP of typ                      (* Pointer type                *)
  | TypeStruct of string             (* Struct type                *)
  | Lambda of  typ option * string * (typ * string) list * stmt
  
                                                                
and expr =                           // 表达式，右值   
                                           
  | Access of access                 (* x    or  *p    or  a[e]     *) //访问左值（右值）
  | Assign of access * expr          (* x=e  or  *p=e  or  a[e]=e   *)
  | Addr of access                   (* &x   or  &*p   or  &a[e]    *)

  | CstI of int                      (* Constant  int               *)
  | CstF of float                    (* Constant  foat              *)
  | CstB of bool                     (* Constant  Bool              *)
  | CstS of string                   (* constant string             *)
  | CstC of char                     (* constant char               *) 

  | ToInt of expr                    (* 类型转换 int                 *)
  | ToChar of expr                   (* 类型转换 char                *)
  | ToFloat of expr                  (* 类型转换 foat                *)
  | ToString of expr                 (* 类型转换 string              *)

  | Prim1 of string * expr           (* Unary primitive operator    *)//一元基本算子
  | Prim2 of string * expr * expr    (* Binary primitive operator   *)//二元基本算子
  | Prim3 of expr * expr * expr      (* 三目运算符                   *)
 
  | PlusAssign of access * expr      (* x += a                      *)
  | MinusAssign of access * expr     (* x -= a                      *)
  | TimesAssign of access * expr     (* x *=  a                     *)
  | DivAssign of access * expr       (* x /= a                      *)
  | MODASSIGN of access * expr       (* x %= a                      *)

  | PrePlus of string * access       (* ++x                         *)
  | RearPlus of access * string      (* x++                         *)
  | PreMinus of string * access      (* --x                         *)
  | RearMinus of access * string     (* x--                         *)

//???这个怎么重复定义了？
  | ModAssign of access * expr      (* x %= a                       *)
  | Print of string * expr
  | Typeof of expr                  (* 类型，但后期似乎没用到，不确定删不删*)
  | Sizeof of expr                  (* 结构体获取大小使用             *)


  | Andalso of expr * expr           (* Sequential and              *)
  | Orelse of expr * expr            (* Sequential or               *)
  | Call of string * expr list       (* Function call f(...)        *)
                                                                   
and access =                         //左值，存储的位置                                            
  | AccVar of string                 (* Variable access        x    *) 
  | AccDeref of expr                 (* Pointer dereferencing  *p   *)
  | AccIndex of access * expr        (* Array indexing         a[e] *)
  | AccStruct of access * access     (* Struct结构体                *)

                                                                   
and stmt =                           //语句                           
  | If of expr * stmt * stmt         (* Conditional                 *)
  | While of expr * stmt             (* While循环                   *)
  | Expr of expr                     (* Expression statement   e;   *)
  | Return of expr option            (* Return from method          *)
  | Block of stmtordec list          (* Block: grouping and scope   *)
  | For of expr * expr * expr * stmt (* for循环                     *)
  | Forin of access * expr * expr * stmt(* forin语法                *)
  | DoWhile of stmt * expr           (* DoWhile循环                 *)
  | DoUntil of stmt * expr           (* DoUntil循环                 *)
  | Switch of expr * stmt list       (* Switch语法                  *)
  | Case of expr * stmt              (* Case功能语法                *)
  | Default of stmt                  (* Default语法                 *)
  | Match of expr * stmt list        (* 模式匹配语法                 *)
  | Pattern of expr * stmt           (* 模式匹配各个模式功能         *)
  | MatchAll of stmt                 (* 模式匹配中的‘_’字符功能      *)
  | Break                            (*break功能                    *)
  | Continue                         (*continue功能                 *)
  // 语句块内部，可以是变量声明 或语句的列表                                                              

//语句或声明
and stmtordec =                                                    
  | Dec of typ * string              (* Local variable declaration  *)
  | Stmt of stmt                     (* A statement                 *)
  | DecAndAssign of typ * string * expr // Assign variable when (*变量初始化*)

  
// 顶级声明 可以是函数声明或变量声明
and topdec = 
  | Fundec of typ option * string * (typ * string) list * stmt
  | Vardec of typ * string
  | Structdec of string * (typ * string) list        (*struct结构体功能                 *)
  | VariableDeclareAndAssign of typ * string * expr  (*未明确其功能                     *)

// 程序是顶级声明的列表
and program = 
  | Prog of topdec list
