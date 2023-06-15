(* File MicroC/Comp.fs
   A compiler from micro-C, a sublanguage of the C language, to an
   abstract machine.  Direct (forwards) compilation without
   optimization of jumps to jumps, tail-calls etc.
   sestoft@itu.dk * 2009-09-23, 2011-11-10

   A value is an integer; it may represent an integer or a pointer,
   where a pointer is just an address in the store (of a variable or
   pointer or the base address of an array).

   The compile-time environment maps a global variable to a fixed
   store address, and maps a local variable to an offset into the
   current stack frame, relative to its bottom.  The run-time store
   maps a location to an integer.  This freely permits pointer
   arithmetics, as in real C.  A compile-time function environment
   maps a function name to a code label.  In the generated code,
   labels are replaced by absolute code addresses.

   Expressions can have side effects.  A function takes a list of
   typed arguments and may optionally return a result.

   Arrays can be one-dimensional and constant-size only.  For
   simplicity, we represent an array as a variable which holds the
   address of the first array element.  This is consistent with the
   way array-type parameters are handled in C, but not with the way
   array-type variables are handled.  Actually, this was how B (the
   predecessor of C) represented array variables.

   The store behaves as a stack, so all data except global variables
   are stack allocated: variables, function parameters and arrays.
*)

module Comp

open System.IO
open Absyn
open StackMachine
open Debug
open Backend

(* ------------------------------------------------------------------- *)

// 带类型的数据
type memoryData =
    | INT of int
    | BOOL of bool
    | CHAR of char
    | POINTER of int
    | FLOAT of float
    | STRING of string

    member this.pointer =
        match this with
        | POINTER i -> i
        | INT i -> i
        | _ -> failwith ("not a pointer")

    member this.int =
        match this with
        | INT i -> i
        | POINTER i -> i
        | FLOAT i -> int i
        | BOOL i -> if i then 1 else 0
        | _ -> failwith ("not int")

    member this.char =
        match this with
        | CHAR i -> i
        | INT i -> char i
        | _ -> failwith ("not char")

    member this.bool =
        match this with
        | BOOL i -> i
        | _ -> failwith ("not bool")

    member this.float =
        match this with
        | FLOAT i -> i
        | INT i -> float i
        | _ -> failwith ("not float")

    member this.string =
        match this with
        | INT i -> string i
        | BOOL i -> string i
        | CHAR i -> string i
        | POINTER i -> string i
        | FLOAT i -> string i
        | STRING i -> string i

    member this.typeName =
        match this with
        | INT i -> "int"
        | BOOL i -> "bool"
        | CHAR i -> "char"
        | POINTER i -> "pointer"
        | FLOAT i -> "float"
        | STRING i -> "string"

(* Simple environment operations *)

type 'data Env = (string * 'data) list

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: yr -> if x = y then v else lookup yr x

(* A global variable has an absolute address, a local one has an offset: *)
//在环境env上查找名称为x的结构体
let rec structLookup env x =
    match env with
    | []                            -> failwith(x + " not found")
    | (name, arglist, size)::rhs    -> if x = name then (name, arglist, size) else structLookup rhs x


type Var =
    | Glovar of int (* absolute address in stack           *)
    | Locvar of int (* address relative to bottom of frame *)
    | StructMemberLoc of int

let rec structLookupVar env x lastloc =
    match env with
    | []                            -> failwith(x + " not found")
    | (name, (loc, typ))::rhs         -> 
        if x = name then 
            match typ with
            | TypA (_, _)  -> StructMemberLoc (lastloc+1)
            | _                 -> loc 
        else
        match loc with
        | StructMemberLoc lastloc1 -> structLookupVar rhs x lastloc1

(* The variable environment keeps track of global and local variables, and
   keeps track of next available offset for local variables

ex1.c下面的的全局声明

int g ;
int h[3]

构造的环境如下：

h 是整型数组，长度为 3，g是整数，下一个空闲位置是 5

([("h", (Glovar 4, TypA (TypI, Some 3)));
 ("g", (Glovar 0, TypI))], 5)

实际存储布局如下：
 (0,0)(1,0)(2,0)(3,0) (4,1) ......
*)

type VarEnv = (Var * typ) Env * int

(* The function environment maps function name to label and parameter decs *)

type Paramdecs = (typ * string) list

type StructTypeEnv = (string * (Var * typ) Env * int) list

type FunEnv = (label * typ option * Paramdecs) Env

let isX86Instr = ref false

(* Bind declared variable in env and generate code to allocate it: *)
// kind : Glovar / Locvar
let rec allocateWithMsg (kind: int -> Var) (typ, x) (varEnv: VarEnv) (structEnv : StructTypeEnv) =
    let varEnv, instrs = allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv) (structEnv : StructTypeEnv)

    msg
    <| "\nalloc\n"
       + sprintf "%A\n" varEnv
       + sprintf "%A\n" instrs

    (varEnv, instrs)

and allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv) (structEnv : StructTypeEnv) : VarEnv * instr list =

    msg $"allocate called!{(x, typ)}"
    // let defaultValue typ =
    //     match typ with
    //     | TypI -> INT(0)
    //     | TypC -> CHAR(' ')
    //     | TypB -> BOOL(false)
    //     | TypF -> FLOAT(0.0)
    //     | TypP i -> POINTER(-1)
    //     | TypS -> STRING("")

    // newloc 下个空闲存储位置
    let (env, newloc) = varEnv

    match typ with
    | TypA (TypA _, _) -> raise (Failure "allocate: array of arrays not permitted")
    | TypA (t, Some i) ->
        let newEnv = ((x, (kind (newloc + i), typ)) :: env, newloc + i + 1) //数组内容占用 i个位置,数组变量占用1个位置

        let code = [ INCSP i; GETSP; OFFSET(i - 1); SUB ]
        // info (fun () -> printf "new varEnv: %A\n" newEnv)
        (newEnv, code)
    | TypS ->
        let newEnv = ((x, (kind (newloc+128), typ)) :: env, newloc+128+1)
        let code = [INCSP 128; GETSP; CSTI (128-1); SUB]
        (newEnv, code)
    | TypeStruct structName ->
        let (name, argslist, size) = structLookup structEnv structName
        let code = [INCSP (size + 1); GETSP; CSTI (size); SUB]
        let newEnvr = ((x, (kind (newloc + size + 1), typ)) :: env, newloc+size+1+1)
        (newEnvr, code)
    | _ ->
        let newEnv = ((x, (kind (newloc), typ)) :: env, newloc + 1)

        let code = [ INCSP 1 ]

        // info (fun () -> printf "new varEnv: %A\n" newEnv) // 调试 显示分配后环境变化

        (newEnv, code)

(* Bind declared parameters in env: *)

let bindParam (env, newloc) (typ, x) : VarEnv =
    ((x, (Locvar newloc, typ)) :: env, newloc + 1)

let bindParams paras ((env, newloc): VarEnv) : VarEnv = List.fold bindParam (env, newloc) paras

(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)

// let makeGlobalEnvs (topdecs: topdec list) : VarEnv * FunEnv * instr list =
//     let rec addv decs varEnv funEnv =

//         msg $"\nGlobal varEnv:\n{varEnv}\n"
//         msg $"\nGlobal funEnv:\n{funEnv}\n"

//         match decs with
//         | [] -> (varEnv, funEnv, [])
//         | dec :: decr ->
//             match dec with
//             | Vardec (typ, var) ->
//                 let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv
//                 let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv
//                 (varEnvr, funEnvr, code1 @ coder)
//             | Fundec (tyOpt, f, xs, body) -> addv decr varEnv ((f, ($"{newLabel ()}_{f}", tyOpt, xs)) :: funEnv)

//     addv topdecs ([], 0) []


(*
    生成 x86 代码，局部地址偏移 *8 ，因为 x86栈上 8个字节表示一个 堆栈的 slot槽位
    栈式虚拟机 无须考虑，每个栈位保存一个变量
*)
let x86patch code =
    if isX86Instr.Value then
        code @ [ CSTI -8; MUL ] // x86 偏移地址*8
    else
        code
(* ------------------------------------------------------------------- *)

(* Compiling micro-C statements:
   * stmt    is the statement to compile
   * varenv  is the local and global variable environment
   * funEnv  is the global function environment
*)
let mutable lablist : label list = []

let rec headlab labs = 
    match labs with
        | lab :: tr -> lab
        | []        -> failwith "Error: unknown break"
let rec dellab labs =
    match labs with
        | lab :: tr ->   tr
        | []        ->   []
let rec cStmt stmt (varEnv: VarEnv) (funEnv: FunEnv) (structEnv : StructTypeEnv) : instr list =
    match stmt with
    | If (e, stmt1, stmt2) ->
        let labelse = newLabel () //生成else语句的标签
        let labend = newLabel () //生成end语句的标签

        cExpr e varEnv funEnv structEnv //编译表达式e
        @ [ IFZERO labelse ] //如果表达式e等于0，跳到else标签
          @ cStmt stmt1 varEnv funEnv structEnv //编译语句stmt1
            @ [ GOTO labend ] //跳转到end标签
              @ [ Label labelse ] //else标签开始的地方
                @ cStmt stmt2 varEnv funEnv structEnv @ [ Label labend ] //编译语句stmt2，并连上end标签，编译结束
    | While (e, body) ->
        let labbegin = newLabel ()
        let labtest = newLabel ()

        let labend = newLabel ()
        lablist <- [labend; labtest; labbegin]

        [ GOTO labtest; Label labbegin ]
        @ cStmt body varEnv funEnv structEnv
          @ [ Label labtest ]
            // @ cExpr e varEnv funEnv @ [ IFNZRO labbegin ]
            @ cExpr e varEnv funEnv structEnv@ [ IFNZRO labbegin; Label labend ]
    | Expr e -> cExpr e varEnv funEnv structEnv@ [ INCSP -1 ]
    | Block stmts ->

        let rec loop stmts varEnv =
            match stmts with
            | [] -> (snd varEnv, [])
            | s1 :: sr ->
                let (varEnv1, code1) = cStmtOrDec s1 varEnv funEnv structEnv
                let (fdepthr, coder) = loop sr varEnv1
                (fdepthr, code1 @ coder)

        let (fdepthend, code) = loop stmts varEnv

        code @ [ INCSP(snd varEnv - fdepthend) ]

    | Return None -> [ RET(snd varEnv - 1) ]
    | Return (Some e) -> cExpr e varEnv funEnv structEnv @ [ RET(snd varEnv) ]

    |DoWhile (stmt1, e) -> //dowhile循环
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签

        let labend = newLabel ()
        lablist <- [labend; labtest; labbegin]
        cStmt stmt1 varEnv funEnv structEnv//先编译语句stmt
        @ [ GOTO labtest; Label labbegin ] //跳转到test标签；begin标签开始的地方
        @ cStmt stmt1 varEnv funEnv structEnv//编译语句stmt
          @ [ Label labtest ] //test标签
            // @ cExpr e varEnv funEnv @ [ IFNZRO labbegin ] //编译表达式e；如果不等于0跳转到begin，实现循环
            @ cExpr e varEnv funEnv structEnv @ [ IFNZRO labbegin; Label labend ]

    | DoUntil (stmt1, e) -> //dountil循环
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签

        let labend = newLabel ()
        lablist <- [labend; labtest; labbegin]
        cStmt stmt1 varEnv funEnv structEnv//先编译语句stmt
        @ [ GOTO labtest; Label labbegin ] //跳转到test标签；begin标签开始的地方
        @ cStmt stmt1 varEnv funEnv structEnv//编译语句stmt
          @ [ Label labtest ] //test标签
            // @ cExpr e varEnv funEnv @ [ IFZERO labbegin ] //编译表达式e；如果等于0跳转到begin，实现循环
            @ cExpr e varEnv funEnv structEnv @ [ IFNZRO labbegin; Label labend ]
            
    | For (e1, e2, e3, body) -> //for循环
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签

        // 把for循环转换为while循环进行理解
        cExpr e1 varEnv funEnv structEnv//先编译初始化表达式e1
        @ [ INCSP -1 ]//释放空间
          @ [ GOTO labtest; Label labbegin ]//跳转到test标签；begin标签开始的地方
            @ cStmt body varEnv funEnv structEnv//编译函数体语句
              @ cExpr e3 varEnv funEnv structEnv//编译循环后的操作表达式
                @ [ INCSP -1 ]//释放空间
                  @ [ Label labtest ]//test标签
                    @ cExpr e2 varEnv funEnv structEnv//编译条件表达式e2 
                      @ [IFNZRO labbegin]//如果e2不为0，就跳转到begin标签进行循环
    | Switch (e, stmt1) -> //switch语句
        
        //定义辅助函数cases
        let rec cases stmt1 =
            match stmt1 with
            | Case(e2, stmt2) :: stmts -> //匹配到case语句
                // 标签要在Case里面，因为每条case的标签是不一样的
                let labend = newLabel () //生成end标签
                let labnext = newLabel () //生成next标签

                [ DUP ]//复制一个栈顶
                @ cExpr e2 varEnv funEnv structEnv//编译case常量表达式
                  @ [ EQ ]//判断switch表达式和case常量表达式是否相等
                    @ [ IFZERO labend ]//不相等，就跳转到end标签
                      @ cStmt stmt2 varEnv funEnv structEnv //相等，就编译case中的语句
                        @ [ GOTO labnext; Label labend ]//跳转到最后的next标签；end标签
                          @ cases stmts//编译剩下的case语句
                            @ [ Label labnext ]//next标签

            | _ -> [] //未匹配任何case

        cExpr e varEnv funEnv structEnv//编译switch表达式
        @ cases stmt1//编译case语句
          @ [ INCSP -1 ]//释放空间（因为复制一个栈顶元素）
    | Break -> 
    //     let labend = newLabel ()
        let labend = headlab lablist
        [GOTO labend]
    //continue功能解读
    | Continue -> 
    //     // let labbegin = newLabel ()
        let lablist   = dellab lablist
        let labbegin = headlab lablist
        [GOTO labbegin]
    | Try(stmt,catchs)  ->
        let exns = [Exception "ArithmeticalExcption"]
        let rec lookupExn e1 (es:excep list) exdepth=
            match es with
            | hd :: tail -> if e1 = hd then exdepth else lookupExn e1 tail exdepth+1
            | []-> -1
        // let (labend, C1) = addLabel C
        let labend = newLabel ()
        // let lablist = labend :: lablist
        // lablist <- [ labend ] @ lablist
        let (env, fdepth) = varEnv
        let varEnv = (env, fdepth+3*catchs.Length)
        let (tryinstr,varEnv) = tryStmt stmt varEnv funEnv structEnv
        let rec everycatch c  = 
            match c with
            | [Catch(exn, body)] -> 
                let exnum = lookupExn exn exns 1
                let catchcode = cStmt body varEnv funEnv structEnv
                let labcatch = 
                    // addLabel( cStmt body varEnv funEnv lablist structEnv [])
                    newLabel()
                // let lablist = label :: lablist

                let trycode = PUSHHDLR (exnum, labcatch) :: tryinstr @ [POPHDLR; Label labcatch]
                (catchcode, trycode)
            | Catch(exn,body) :: tr->
                let exnum = lookupExn exn exns 1
                let (C2, C3) = everycatch tr
                // let (label, Ccatch) = addLabel( cStmt body varEnv funEnv lablist structEnv C2)
                let catchcode = C2 @ cStmt body varEnv funEnv structEnv
                let labcatch = newLabel()
                // let trycode = PUSHHDLR (exnum, labcatch) :: C3 @ [POPHDLR]
                let trycode = PUSHHDLR (exnum, labcatch) :: C3 @ [POPHDLR; Label labcatch]
                (catchcode, trycode @ [Label labcatch])
            | [] -> ([], tryinstr)
        let (catchcode, trycode) = everycatch catchs
        trycode @ catchcode @ [Label labend]

and tryStmt tryBlock (varEnv : VarEnv) (funEnv : FunEnv) (structEnv : StructTypeEnv) : instr list * VarEnv = 
    match tryBlock with
    | Block stmts ->

        let rec loop stmts varEnv =
            match stmts with
            | [] -> (snd varEnv, [], varEnv)
            | s1 :: sr ->
                let (varEnv1, code1) = cStmtOrDec s1 varEnv funEnv structEnv
                let (fdepthr, coder, varEnv2) = loop sr varEnv1
                (fdepthr, code1 @ coder, varEnv2)

        let (fdepthend, code, varEnv1) = loop stmts varEnv

        code @ [ INCSP(snd varEnv - fdepthend) ], varEnv1
//语句 或 声明
and cStmtOrDec stmtOrDec (varEnv: VarEnv) (funEnv: FunEnv) (structEnv : StructTypeEnv) : VarEnv * instr list =
    match stmtOrDec with
    | Stmt stmt -> (varEnv, cStmt stmt varEnv funEnv structEnv ) //语句
    | Dec (typ, x) -> allocateWithMsg Locvar (typ, x) varEnv structEnv //调用allocateWithMsg函数为局部变量分配空间
    | DecAndAssign (typ, x, expr) ->
        let (varEnv1,code) = allocateWithMsg Locvar (typ, x) varEnv structEnv //调用allocateWithMsg函数为局部变量分配空间
        let (code2) = cExpr (Assign (AccVar x, expr)) varEnv1 funEnv structEnv //获取表达式expr给该变量x赋值的汇编指令
        let res = code @ code2 @ [INCSP -1] //返回varEnv1这个变量环境 和 两个汇编指令列表的拼接，最后释放空间
        (varEnv1, res)//返回环境变量和汇编指令列表

(* Compiling micro-C expressions:

   * e       is the expression to compile
   * varEnv  is the local and gloval variable environment
   * funEnv  is the global function environment

   Net effect principle: if the compilation (cExpr e varEnv funEnv) of
   expression e returns the instruction sequence instrs, then the
   execution of instrs will leave the rvalue of expression e on the
   stack top (and thus extend the current stack frame with one element).
*)
//编译右值表达式
and cExpr (e: expr) (varEnv: VarEnv) (funEnv: FunEnv) (structEnv : StructTypeEnv) : instr list =        //参数：表达式e，变量环境varEnv，函数环境funEnv，返回汇编指令列表
    match e with
    | Access acc -> cAccess acc varEnv funEnv structEnv @ [ LDI ]
    | Assign (acc, e) ->
        cAccess acc varEnv funEnv structEnv 
        @ cExpr e varEnv funEnv structEnv @ [ STI ]
    | CstI i -> [ CSTI i ]//整型
    | CstC c -> //字符
        let c = (int c)
        [ CSTI c ]
    | CstF f -> //浮点数
        let bytes = System.BitConverter.GetBytes(float32(f))
        let v = System.BitConverter.ToInt32(bytes, 0)
        [ CSTI v ]
    | Print(s,e)     ->  
      cExpr e varEnv funEnv structEnv
      @ (match s with
         | "%d"      -> [PRINTI]
         | "%c"      -> [PRINTC]
        //  | "%f"      -> [printf]
         | _        -> raise (Failure "unknown primitive 1"))
    | Addr acc -> cAccess acc varEnv funEnv structEnv
    | Prim1 (ope, e1) -> //一元表达式
        cExpr e1 varEnv funEnv structEnv
        @ (match ope with //操作符模式匹配
           | "!" -> [ NOT ]
           | "printi" -> [ PRINTI ]
           | "printc" -> [ PRINTC ]
        //    | "printf" -> [ PRINTC ]
        //    | "~" -> [ BITNOT ]
           | _ -> raise (Failure "unknown primitive 1"))
    | Prim2 (ope, e1, e2) -> //二元表达式
        cExpr e1 varEnv funEnv structEnv //计算e1表达式
        @ cExpr e2 varEnv funEnv structEnv//计算e2表达式
          @ (match ope with //匹配操作符
             | "*" -> [ MUL ]
             | "+" -> [ ADD ]
             | "-" -> [ SUB ]
             | "/" -> [ DIV ]
             | "%" -> [ MOD ]
             | "==" -> [ EQ ]
             | "!=" -> [ EQ; NOT ]
             | "<" -> [ LT ]
             | ">=" -> [ LT; NOT ]
             | ">" -> [ SWAP; LT ]
             | "<=" -> [ SWAP; LT; NOT ] //指令顺序：从左往右
            //  | "<<" -> [ BITLEFT ]
            //  | ">>" -> [ BITRIGHT ]
            //  | "&" -> [ BITAND ]
            //  | "|" -> [ BITOR ]
            //  | "^" -> [ BITXOR ]
            // 需要修改Backend.fs，但个人能力，时间有限，无法完成
             | _ -> raise (Failure "unknown primitive 2"))

    | Prim3 (e1, e2, e3) -> //三目运算
        let labelse = newLabel ()
        let labend = newLabel ()

        cExpr e1 varEnv funEnv structEnv
        @ [ IFZERO labelse ]
          @ cExpr e2 varEnv funEnv structEnv
            @ [ GOTO labend ]
              @ [ Label labelse ]
                @ cExpr e3 varEnv funEnv structEnv @ [ Label labend ]
    
    //复合运算
    | PlusAssign (acc, e) ->
        cAccess acc varEnv funEnv structEnv
        @[DUP;LDI]
        @ cExpr e varEnv funEnv structEnv
        @ [ ADD;STI ]

    | MinusAssign (acc, e) ->
        cAccess acc varEnv funEnv structEnv
        @[DUP;LDI]
        @ cExpr e varEnv funEnv structEnv
        @ [ SUB;STI ]

    | TimesAssign (acc, e) ->
        cAccess acc varEnv funEnv structEnv
        @[DUP;LDI]
        @ cExpr e varEnv funEnv structEnv
        @ [ MUL;STI ]

    | DivAssign (acc, e) ->
        cAccess acc varEnv funEnv structEnv
        @[DUP;LDI]
        @ cExpr e varEnv funEnv structEnv
        @ [ DIV;STI ]


    | ModAssign (acc, e) ->
        cAccess acc varEnv funEnv structEnv
        @[DUP;LDI]
        @ cExpr e varEnv funEnv structEnv
        @ [MOD;STI]
    
     | PrePlus (ope, acc) -> //前置自增
        cAccess acc varEnv funEnv structEnv
        @ [ DUP; LDI; CSTI 1; ADD; STI ]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //ADD:栈顶的acc地址的值+1
                                                        //STI:将 上一步+1后的值 写入栈顶，即set s[s[sp-1]]

    | PreMinus (ope, acc) -> //前置自减
        cAccess acc varEnv funEnv structEnv
        @ [ DUP; LDI; CSTI 1; SUB; STI ]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //SUB:栈顶的acc地址的值-1
                                                        //STI:将 上一步-1后的值 写入栈顶，即set s[s[sp-1]]

    | RearPlus (acc, ope) -> //后置自增
        cAccess acc varEnv funEnv structEnv
        @ [ DUP; LDI; SWAP; DUP; LDI; CSTI 1; ADD; STI ; INCSP -1]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:将复制后的栈顶的acc地址的值入栈，即s[sp]=s[s[sp]]
                                                        //SWAP:交换栈顶和复制前的元素，交换后靠栈底的那个是左值acc原来的值
                                                        //DUP:复制栈顶的acc地址
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //ADD:栈顶的acc地址的值+1
                                                        //STI:将 上一步+1后的值 写入栈顶，即set s[s[sp-1]]，因为s[sp]=s[s[sp]]，故也就是把新值赋值给一开始的acc
                                                        //INCSP -1:释放空间

    | RearMinus (acc, ope) -> //后置自减
        cAccess acc varEnv funEnv structEnv
        @ [ DUP; LDI; SWAP; DUP; LDI; CSTI 1; SUB; STI ; INCSP -1]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:将复制后的栈顶的acc地址的值入栈，即s[sp]=s[s[sp]]
                                                        //SWAP:交换栈顶和复制前的元素，交换后靠栈底的那个是左值acc原来的值
                                                        //DUP:复制栈顶的acc地址
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //SUB:栈顶的acc地址的值-1
                                                        //STI:将 上一步-1后的值 写入栈顶，即set s[s[sp-1]]，因为s[sp]=s[s[sp]]，故也就是把新值赋值给一开始的acc
                                                        //INCSP -1:释放空间


    | Andalso (e1, e2) ->
        let labend = newLabel ()
        let labfalse = newLabel ()

        cExpr e1 varEnv funEnv structEnv
        @ [ IFZERO labfalse ]
          @ cExpr e2 varEnv funEnv structEnv
            @ [ GOTO labend
                Label labfalse
                CSTI 0
                Label labend ]
    | Orelse (e1, e2) ->
        let labend = newLabel ()
        let labtrue = newLabel ()

        cExpr e1 varEnv funEnv structEnv
        @ [ IFNZRO labtrue ]
          @ cExpr e2 varEnv funEnv structEnv
            @ [ GOTO labend
                Label labtrue
                CSTI 1
                Label labend ]
    | Call (f, es) -> callfun f es varEnv funEnv structEnv





and structAllocateDef(kind : int -> Var) (structName : string) (typ : typ) (varName : string) (structTypEnv : StructTypeEnv) : StructTypeEnv = 
    match structTypEnv with
    | lhs :: rhs ->
        let (name, env, depth) = lhs
        if name = structName 
        then 
            match typ with
            | TypA (TypA _, _)    -> failwith "Warning: allocate-arrays of arrays not permitted" 
            | TypA (t, Some i)         ->
                let newEnv = env @ [(varName, (kind (depth+i), typ))]
                (name, newEnv, depth + i) :: rhs
            | _ ->
                let newEnv = env @ [(varName, (kind (depth+1), typ))] 
                (name, newEnv, depth + 1) :: rhs
        else structAllocateDef kind structName typ varName rhs
    | [] -> 
        match typ with
            | TypA (TypA _, _)    -> failwith "Warning: allocate-arrays of arrays not permitted" 
            | TypA (t, Some i)         ->
                let newEnv = [(varName, (kind (i), typ))]
                (structName, newEnv, i) :: structTypEnv
            | _ ->
                let newEnv = [(varName, (kind (0), typ))]
                (structName, newEnv, 0) :: structTypEnv

and makeStructEnvs(structName : string) (structEntry :(typ * string) list ) (structTypEnv : StructTypeEnv) : StructTypeEnv = 
    let rec addm structName structEntry structTypEnv = 
        match structEntry with
        | [] -> structTypEnv
        | lhs::rhs ->
            match lhs with
            | (typ, name)   -> 
                let structTypEnv1 = structAllocateDef StructMemberLoc structName typ name structTypEnv
                let structTypEnvr = addm structName rhs structTypEnv1
                structTypEnvr

    addm structName structEntry structTypEnv

(* Build environments for global variables and functions *)
// and makeGlobalEnvs (topdecs: topdec list) : VarEnv * FunEnv * StructTypeEnv * instr list =
//     let rec addv decs varEnv funEnv =

//         msg $"\nGlobal funEnv:\n{funEnv}\n"

//         match decs with
//         | [] -> (varEnv, funEnv, structTypEnv, [])
//         | dec :: decr ->
//             match dec with
//             | Vardec (typ, var) ->
//                 let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv structTypEnv
//                 let (varEnvr, funEnvr, structTypEnvr coder) = addv decr varEnv1 funEnv structTypEnv
//                 (varEnvr, funEnvr, code1 @ coder)
//             | VardecAndAssign (typ, var, e) ->
//                 let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv structEnv
//                 let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv
//                 let code2 = cAccess (AccVar var) varEnvr funEnvr @ (structEnv (cExpr e varEnvr funEnvr [] structTypEnv (STI :: (addINCSP -1 coder))))
//                 (varEnvr, funEnvr, code1 @ coder @ code2)
//             |Structdec (typName, typEntry) -> 
//                 let structTypEnv1 = makeStructEnvs typName typEntry structTypEnv
//                 let (varEnvr, funEnvr, structTypEnvr, coder) = addv decr varEnv funEnv structTypEnv1
//                 (varEnvr, funEnvr, structTypEnvr, coder)
//             | Fundec (tyOpt, f, xs, body) -> addv decr varEnv ((f, ($"{newLabel ()}_{f}", tyOpt, xs)) :: funEnv)

//     addv topdecs ([], 0) [] []
and makeGlobalEnvs(topdecs : topdec list) : VarEnv * FunEnv * StructTypeEnv * instr list =
    let rec addv decs varEnv funEnv structTypEnv =
        match decs with
        | [] -> (varEnv, funEnv, structTypEnv, [])
        | dec::decr ->
            match dec with
            | Vardec (typ, x) -> 
                let (varEnv1, code1) = allocate Glovar (typ, x) varEnv structTypEnv
                let (varEnvr, funEnvr, structTypEnvr, coder) = addv decr varEnv1 funEnv structTypEnv
                (varEnvr, funEnvr, structTypEnvr, code1 @ coder)
                //显示没有定义，暂时先不写
            | VariableDeclareAndAssign (typ, x, e) -> 
                // let (varEnv1, code1) = allocate Glovar (typ, x) varEnv structTypEnv
                // let (varEnvr, funEnvr, structTypEnvr, coder) = addv decr varEnv1 funEnv structTypEnv
                // // let code2 = cAccess (AccVar x) varEnvr funEnvr structTypEnv 
                // // let code3 = cExpr e varEnvr funEnvr [] structTypEnv (STI :: (addINCSP -1 coder))
                // (varEnvr, funEnvr, structTypEnvr, code1 @ (cAccess (AccVar(x)) varEnvr funEnvr [] structTypEnv (cExpr e varEnvr funEnvr [] structTypEnv (STI :: (addINCSP -1 coder)))))
                let (varEnv1, code1) = allocateWithMsg Glovar (typ, x) varEnv structTypEnv
                let (varEnvr, funEnvr, structTypEnvr, coder) = addv decr varEnv1 funEnv structTypEnv
                let code2 = cAccess (AccVar x) varEnvr funEnvr structTypEnv
                (varEnvr, funEnvr, structTypEnvr, code1 @ coder @ code2)
            | Fundec (tyOpt, f, xs, body) ->
                addv decr varEnv ((f, (newLabel(), tyOpt, xs)) :: funEnv) structTypEnv
            | Structdec (typName, typEntry) -> 
                let structTypEnv1 = makeStructEnvs typName typEntry structTypEnv
                let (varEnvr, funEnvr, structTypEnvr, coder) = addv decr varEnv funEnv structTypEnv1
                (varEnvr, funEnvr, structTypEnvr, coder)
                
    addv topdecs ([], 0) [] []


(* Generate code to access variable, dereference pointer or index array.
   The effect of the compiled code is to leave an lvalue on the stack.   *)

and cAccess access varEnv funEnv structEnv : instr list =
    match access with
    | AccVar x ->
        match lookup (fst varEnv) x with
        // x86 虚拟机指令 需要知道是全局变量 [GVAR addr]
        // 栈式虚拟机Stack VM 的全局变量的地址是 栈上的偏移 用 [CSTI addr] 表示
        // F# ! 操作符 取引用类型的值
        | Glovar addr, _ ->
            if isX86Instr.Value then
                [ GVAR addr ]
            else
                [ CSTI addr ]
        | Locvar addr, _ -> [ GETBP; OFFSET addr; ADD ]

     | AccStruct (AccVar stru, AccVar memb) ->
        let (loc, TypeStruct structname)   = lookup (fst varEnv) stru
        let (name, argslist, size) = structLookup structEnv structname
        match structLookupVar argslist memb 0 with
        | StructMemberLoc varLocate ->
            match lookup (fst varEnv) stru with
            | Glovar addr, _ -> 
                let a = (addr - (size+1) + varLocate)
                [ CSTI a ]
            | Locvar addr, _ -> 
                // GETBP :: addCST (addr - (size+1) + varLocate) (ADD ::  C)
                let a = addr - (size+1) + varLocate
                [ GETBP; OFFSET a; ADD ]
    | AccDeref e ->
        match e with
        | Access _ -> (cExpr e varEnv funEnv structEnv)
        | Addr _ -> (cExpr e varEnv funEnv structEnv)
        | _ ->
            printfn "WARN: x86 pointer arithmetic not support!"
            (cExpr e varEnv funEnv structEnv)
    | AccIndex (acc, idx) ->
        cAccess acc varEnv funEnv structEnv
        @ [ LDI ]
          @ x86patch (cExpr idx varEnv funEnv structEnv) @ [ ADD ]

(* Generate code to evaluate a list es of expressions: *)

and cExprs es varEnv funEnv structEnv: instr list =
    List.concat (List.map (fun e -> cExpr e varEnv funEnv structEnv) es)

(* Generate code to evaluate arguments es and then call function f: *)

and callfun f es varEnv funEnv structEnv: instr list =
    let (labf, tyOpt, paramdecs) = lookup funEnv f
    let argc = List.length es

    if argc = List.length paramdecs then
        cExprs es varEnv funEnv structEnv @ [ CALL(argc, labf) ]
    else
        raise (Failure(f + ": parameter/argument mismatch"))


(* Compile a complete micro-C program: globals, call to main, functions *)
let argc = ref 0

let cProgram (Prog topdecs) : instr list =
    let _ = resetLabels ()
    let ((globalVarEnv, _), funEnv,structEnv, globalInit) = makeGlobalEnvs topdecs

    let compilefun (tyOpt, f, xs, body) =
        let (labf, _, paras) = lookup funEnv f
        let paraNums = List.length paras
        let (envf, fdepthf) = bindParams paras (globalVarEnv, 0)
        let code = cStmt body (envf, fdepthf) funEnv structEnv

        [ FLabel(paraNums, labf) ]
        @ code @ [ RET(paraNums - 1) ]

    let functions =
        List.choose
            (function
            | Fundec (rTy, name, argTy, body) -> Some(compilefun (rTy, name, argTy, body))
            | Vardec _ -> None
            | VariableDeclareAndAssign _ -> None
            | Structdec _ -> None)
            topdecs

    let (mainlab, _, mainparams) = lookup funEnv "main"
    argc.Value <- List.length mainparams

    globalInit
    @ [ LDARGS argc.Value
        CALL(argc.Value, mainlab)
        STOP ]
      @ List.concat functions

(* Compile a complete micro-C and write the resulting instruction list
   to file fname; also, return the program as a list of instructions.
 *)

let intsToFile (inss: int list) (fname: string) =
    File.WriteAllText(fname, String.concat " " (List.map string inss))

let writeInstr fname instrs =
    let ins = String.concat "\n" (List.map string instrs)

    File.WriteAllText(fname, ins)
    printfn $"VM instructions saved in file:\n\t{fname}"


let compileToFile program fname =

    msg <| sprintf "program:\n %A" program

    let instrs = cProgram program

    msg <| sprintf "\nStack VM instrs:\n %A\n" instrs

    writeInstr (fname + ".ins") instrs

    let bytecode = code2ints instrs

    msg
    <| sprintf "Stack VM numeric code:\n %A\n" bytecode

    // 面向 x86 的虚拟机指令 略有差异，主要是地址偏移的计算方式不同
    // 单独生成 x86 的指令
    isX86Instr.Value <- true
    let x86instrs = cProgram program
    writeInstr (fname + ".insx86") x86instrs

    let x86asmlist = List.map emitx86 x86instrs
    let x86asmbody = List.fold (fun asm ins -> asm + ins) "" x86asmlist

    let x86asm = (x86header + beforeinit argc.Value + x86asmbody)

    printfn $"x86 assembly saved in file:\n\t{fname}.asm"
    File.WriteAllText(fname + ".asm", x86asm)

    // let deinstrs = decomp bytecode
    // printf "deinstrs: %A\n" deinstrs
    intsToFile bytecode (fname + ".out")

    instrs

(* Example programs are found in the files ex1.c, ex2.c, etc *)
