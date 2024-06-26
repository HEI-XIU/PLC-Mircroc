(* File MicroC/Interp.c
   Interpreter for micro-C, a fraction of the C language
   sestoft@itu.dk * 2010-01-07, 2014-10-18

   A value is an integer; it may represent an integer or a pointer,
   where a pointer is just an address in the store (of a variable or
   pointer or the base address of an array).  The environment maps a
   variable to an address (location), and the store maps a location to
   an integer.  This freely permits pointer arithmetics, as in real C.
   Expressions can have side effects.  A function takes a list of
   typed arguments and may optionally return a result.

   For now, arrays can be one-dimensional only.  For simplicity, we
   represent an array as a variable which holds the address of the
   first array element.  This is consistent with the way array-type
   parameters are handled in C (and the way that array-type variables
   were handled in the B language), but not with the way array-type
   variables are handled in C.

   The store behaves as a stack, so all data are stack allocated:
   variables, function parameters and arrays.

   The return statement is not implemented (for simplicity), so all
   functions should have return type void.  But there is as yet no
   typecheck, so be careful.
 *)

module Interp

open Absyn
open Debug

// 带类型的数据
type memoryData =
    | INT of int
    | BOOL of bool
    | CHAR of char
    | POINTER of int
    | FLOAT of float
    | STRING of string
    | STRUCT of string*int*int
    | ARRAY of typ*int*int

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
        | STRUCT (s,i,size) -> i
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
        | _ ->   failwith ("not string")

    member this.typeName =
        match this with
        // | INT i -> "int"
        // | BOOL i -> "bool"
        // | CHAR i -> "char"
        // | POINTER i -> "pointer"
        // | FLOAT i -> "float"
        // | STRING i -> "string"
        // | STRUCT (s,i,size) -> "struct"
        | INT i -> TypI
        | FLOAT f -> TypF
        | CHAR c -> TypC
        | BOOL b -> TypB
        | STRING s -> TypS
        | ARRAY(typ,i,size) ->  TypA(typ,Some size)
        | STRUCT (s,i,size) -> TypeStruct s
        | _ ->   failwith (" 该类型暂不支持")

(* Simple environment operations *)
// 多态类型 env
// 环境 env 是 元组 ("name",data) 的列表 ，名称是字符串 string 值 'data 可以是任意类型
//  名称 ---> 数据 名称与数据绑定关系的 键-值 对  key-value pairs
// [("x",9);("y",8)]: int env

type 'data env = (string * 'data) list

//环境查找函数
//在环境 env上查找名称为 x 的值
// let rec lookup env x =
//     match env with
//     | [] -> failwith (x + " not found")
//     | (y, v) :: yr -> if x = y then v else lookup yr x

(* A local variable environment also knows the next unused store location *)

// ([("x",9);("y",8)],10)
// x 在位置9,y在位置8,10--->下一个空闲空间位置10
type locEnv = int env * int

(* A function environment maps a function name to parameter list and body *)
//函数参数例子:
//void func (int a , int *p)
// 参数声明列表为: [(TypI,"a");(TypP(TypI) ,"p")]
type paramdecs = (typ * string) list

(* 函数环境列表
  [("函数名", ([参数元组(类型,"名称")的列表],函数体AST)),....]

  //main (i){
  //  int r;
  //    fac (i, &r);
  //    print r;
  // }
  [ ("main",
   ([(TypI, "i")],
    Block
      [Dec (TypI,"r");
       Stmt (Expr (Call ("fac",[Access (AccVar "i"); Addr (AccVar "r")])));
       Stmt (Expr (Prim1 ("printi",Access (AccVar "r"))))]))]

函数环境 是 多态类型  'data env ---(string * 'data ) list 的一个 具体类型 ⭐⭐⭐
    类型变量 'data  具体化为  (paramdecs * stmt)
    (string * (paramdecs * stmt)) list
*)

type funEnv = (paramdecs * stmt) env

(* A global environment consists of a global variable environment
   and a global function environment
 *)

// 全局环境是 变量声明环境 和 函数声明环境的元组
// 两个列表的元组
// ([var declares...],[fun declares..])
// ( [ ("x" ,1); ("y",2) ], [("main",mainAST);("fac",facAST)] )
// mainAST,facAST 分别是main 与fac 的抽象语法树

type gloEnv = int env * funEnv

type structEnv = (string * paramdecs * int ) list

//环境查找函数
//查找名称为 x 的值的地址
let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: yr ->
        if x = y then
            POINTER(v)
        else
            lookup yr x

//查找名称为 x 的函数
let rec lookupFunc env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: yr -> if x = y then v else lookupFunc yr x

//查找称为 x结构体
let rec structLookup env x index=
    match env with
    | []                            -> failwith(x + " not found")
    | (name, arglist, size)::rhs    -> if x = name then (index, arglist, size) else structLookup rhs x (index+1)
(* The store maps addresses (ints) to values (ints): *)

//地址是store上的的索引值
type address = int

// store 是一个 地址到值的映射，是对内存的抽象 ⭐⭐⭐
// store 是可更改的数据结构，特定位置的值可以修改，注意与环境的区别
// map{(0,3);(1,8) }
// 位置 0 保存了值 3
// 位置 1 保存了值 8

type store = Map<address, memoryData>

//空存储
// let emptyStore = Map.empty<address, int>
//空存储
let emptyStore = Map.empty<address, memoryData>

//保存value到存储store
let setSto (store: store) addr value = store.Add(addr, value)
// let setSto (store: store) addr (value: memoryData) =
//     match store.TryFind addr with
//     | Some (data) ->
//         if data.typeName = value.typeName then
//             store.Add(addr, value)
//         else
//             failwith (
//                 "type error cant assign "
//                 + value.typeName
//                 + " to "
//                 + data.typeName
//             )
//     | None -> store.Add(addr, value)

//输入addr 返回存储的值value
let getSto (store: store) addr = store.Item addr

// store上从loc开始分配n个值的空间
// 用于数组分配
let rec initSto loc n store initValue =
    if n = 0 then
        store
    else // 默认值 0
        initSto (loc + 1) (n - 1) (setSto store loc initValue) initValue

(* Combined environment and store operations *)

(* Extend local variable environment so it maps x to nextloc
   (the next store location) and set store[nextloc] = v.

locEnv结构是元组 : (绑定环境env,下一个空闲地址nextloc)
store结构是Map<string,int>

扩展环境 (x nextloc) :: env ====> 新环境 (env1,nextloc+1)
变更store (nextloc) = v
 *)

// 绑定一个值 x,v 到环境
// 环境是非更改数据结构，只添加新的绑定（变量名称，存储位置），注意与store 的区别⭐⭐⭐
// 返回新环境 locEnv,更新store,
// nextloc是store上下一个空闲位置
(*

// variable.c
int g ;
int h[3];
void main (int n){
n = 8;
}
上面c程序的解释环境如下：

 环境：locEnv:
    ([(n, 5); (n, 4); (g, 0)], 6)

存储：store:
    (0, 0)  (1, 0)(2, 0)(3, 0)(4, 1)  (5, 8)
     ^^^^    ^^^^^^^^^^^^^^^^^^^^^^    ^^^^
       g               h                n

   变量 地址 值
   n--->5--->8
   h--->4--->1
   g--->0--->0

   下一个待分配位置是 6
*)

//将多个值 xs vs绑定到环境
//遍历 xs vs 列表,然后调用 bindVar实现单个值的绑定
let store2str store =
    String.concat "" (List.map string (Map.toList store))
    //将一个映射(Map)数据结构store转换为一个字符串。
    //List.map string (Map.toList store)：将Map中的每一个(key,value)键值对转化成字符串形式，生成一个字符串列表
    //String.concat “”：将字符串列表中的所有元素连接起来，组成一个新的字符串，使用空字符串""作为分隔符，即不插入任何分隔符。

//将一个变量名x和它的值v进行绑定，把它们添加到环境(env)和存储器(store)中。
let bindVar x v (env, nextloc) store : locEnv * store =
    let env1 = (x, nextloc) :: env
    msg $"bindVar:\n%A{env1}\n"

    //返回新环境，新的待分配位置+1，设置当前存储位置为值 v
    let ret = ((env1, nextloc + 1), setSto store nextloc v)
    
    msg $"locEnv:\n {fst ret}\n"
    msg $"Store:\n {store2str (snd ret)}\n"

    ret 

//将多个变量名xs和变量值vs进行绑定。该函数通过调用一个名为bindVar的函数实现将一个变量名和变量值进行绑定，并将结果添加到环境和存储器
let rec bindVars xs vs locEnv store : locEnv * store =
    let res =
        match (xs, vs) with
        | ([], []) -> (locEnv, store)
        | (x1 :: xr, v1 :: vr) ->
            let (locEnv1, sto1) = bindVar x1 v1 locEnv store
            bindVars xr vr locEnv1 sto1
        | _ -> failwith "parameter/argument mismatch"

    msg "\nbindVars:\n"
    msg $"\nlocEnv:\n{locEnv}\n"
    msg $"\nStore:\n"
    store2str store |> msg
    res
(* Allocate variable (int or pointer or array): extend environment so
   that it maps variable to next available store location, and
   initialize store location(s).
 *)
//用来实现内存分配和更新内存状态的功能。
//函数传入了一些参数，包括要分配的变量的类型(typ)、变量的名称(name)、变量的初始值(value，如果有的话)、当前的环境(currenEnv)、下一个可用内存位置(nextloc)
//以及当前存储状态(currStore)。函数会返回一个新的环境(locEnv)和新的存储状态(store)，这些新的环境和存储状态包含了新分配的变量。
let rec allocate (typ: typ, name: string, value: memoryData option) (currenEnv, nextloc: int) structEnv currStore : locEnv * store =

    let defaultValue typ =
        match typ with
        | TypI -> INT(0)
        | TypC -> CHAR(' ')
        | TypB -> BOOL(false)
        | TypF -> FLOAT(0.0)
        | TypP i -> POINTER(-1)
        | TypS -> STRING("")
        | TypeStruct s-> STRUCT(s,0,0)
        | _ -> failwith ("cant init")
//给定一个类型(typ)和下一个可用内存位置(nextloc)，根据不同的类型分配内存并返回分配内存后的新状态。代码使用了OCaml的模式匹配来区分不同的类型并进行相应的处理。
    let (newNextloc: int, value: memoryData, newStore: store) =
        match typ with
            //新的内存位置、值和状态被存储在一个元组(newNextloc, value, newStore)中返回
            | TypA (t, Some i) -> (nextloc+i, (ARRAY(t,nextloc,i)), initSto nextloc i currStore (defaultValue t))
            | TypA (t, None) -> (nextloc, (ARRAY(typ,nextloc,0)), currStore)
            // 默认值是 -1
            | TypeStruct s -> 
                            let (index,arg,size) = structLookup structEnv s 0
                            (nextloc+size, (STRUCT (s,index,size)), initSto nextloc size currStore (defaultValue typ) )
            | TypB   -> (nextloc,  (BOOL false), currStore)
            | TypI   -> (nextloc,  (INT 0),currStore)
            | TypP i -> (nextloc,  (POINTER 0),currStore)
            | TypC  -> (nextloc,  (CHAR (char 0)),currStore)
            | TypS   -> (nextloc, (STRING ""),currStore)
            | TypF   -> (nextloc, (FLOAT 0.0),currStore)
            | _ -> (nextloc,  (INT -1), currStore)  
 
        //数组 调用 initSto 分配 i 个空间
        // | TypA (t, Some i) -> (nextloc + i, POINTER(nextloc), initSto nextloc i currStore (defaultValue t))
        // // 常规变量默认值是 0
        // | _ ->
        //     (nextloc,
        //      (match value with
        //       | Some (x) -> x
        //       | None -> defaultValue typ),
        //      currStore)

    msg $"\nalloc:\n {((typ, name), (currenEnv, nextloc), currStore)}"
    bindVar name value (currenEnv, newNextloc) newStore

(* Build global environment of variables and functions.  For global
   variables, store locations are reserved; for global functions, just
   add to global function environment.
*)

let allsize typ = 
    match typ with
    |  TypA (t, Some i) -> i
    |  _ -> 1

(* ------------------------------------------------------------------- *)
(* Interpreting micro-C statements *)

let rec exec stmt (locEnv: locEnv) (gloEnv: gloEnv) (structEnv:structEnv) (store: store) : store =
    match stmt with
    | If (e, stmt1, stmt2) ->
        let (v, store1) = eval e locEnv gloEnv structEnv store

        if v.bool then
            exec stmt1 locEnv gloEnv structEnv store1 //True分支
        else
            exec stmt2 locEnv gloEnv structEnv store1 //False分支

    | While (e, body) ->

        //定义 While循环辅助函数 loop
        let rec loop store1 =
            //求值 循环条件,注意变更环境 store
            // let (v, store2) = eval e locEnv gloEnv store1
            let (resCmped, store2) = eval e locEnv gloEnv structEnv store1
            // 求值  就是在更新变量，比如 while(i++) 就是i++ 的更新操作
            // 虽然看太不懂 但是盲猜是在干这个
            // 继续循环
            // if v <> 0 then
            if resCmped.bool then
            // 返回的值 去做loop
                loop (exec body locEnv gloEnv structEnv store2)
            else
                store2 //退出循环返回 环境store2

        loop store

    | Expr e ->
        // _ 表示丢弃e的值,返回 变更后的环境store1
        let (_, store1) = eval e locEnv gloEnv structEnv store
        store1

    | Block stmts ->
        // 语句块 解释辅助函数 loop
        let rec loop ss (locEnv, store) =
            match ss with
            | [] -> store
            //语句块,解释 第1条语句s1
            // 调用loop 用变更后的环境 解释后面的语句 sr.
            | s1 :: sr -> loop sr (stmtordec s1 locEnv gloEnv store structEnv)

        loop stmts (locEnv, store)

    // | Return _ -> failwith "return not implemented" // 解释器没有实现 return
    | Return e ->  match e with
                  | Some e1 -> let (res ,store0) = eval e1 locEnv gloEnv structEnv store;
                               let st = store0.Add(-1, res);
                               (st)                     
                  | None -> store
    | For(assignedStmt,cmpStmt,updateStmt,body) -> 
        let (resAssigned ,storeAssigned) = eval assignedStmt locEnv gloEnv structEnv store
        // 对 assignedStmt 进行求值,通过let关键字将求值结果中的语句部分赋值给resAssigned变量，将新的存储状态赋值给storeAssigned变量，从而分别存储这两个值。
        // 获得初始值
        let rec loop storeOrigin =
                //storeOrigin是递归函数loop的输入参数
                //求值 循环条件,注意变更环境 store
                // 这里是做判断 是for 的第二个参数  i<n
            let (resCmped, storeCmped) = eval cmpStmt locEnv gloEnv structEnv storeOrigin
                // body 里面可能也会改变变量的 比如 
                // for(i=0;i<n;i++){
                //     i++
                // }
                // 所以要返回body里面改变过的变量 
                // 去做一个更新操作 
            if resCmped.bool then 
                //如果是0,就停止
                let (updatedRes ,updatedStore) = eval updateStmt locEnv gloEnv structEnv (exec body locEnv gloEnv structEnv storeCmped)
                //  这里做了第三个参数的i++ 
                //  然后这个值可以放到loop里去做循环
                //  用更新的变量去做body 里的事情
                loop updatedStore
            else storeCmped  
        //执行语句
        loop storeAssigned
    | Forin(acc,e1,e2,body) -> //实现的应该是js语法而不是Python语法的for in range
          let (loc, store1) = access acc locEnv gloEnv structEnv store
          let (re, store2) = eval e1 locEnv gloEnv structEnv store1
          let (re2,store3) = eval e2 locEnv gloEnv structEnv store2
          match e1 with
          | CstI i -> let rec loop i stores =
                          if i<>(re2.int+1) then loop (i+1) (exec body locEnv gloEnv structEnv (setSto stores loc.int (INT i)) )
                                    else (stores)
                      loop re.int store3 
          | _ ->   failwith ("Forin语法错误")
                      
    | DoWhile(body,e) -> 
    //body为函数体，e为while()括号内判断表达式，用于判定是否结束
      let rec loop store1 =
                //求值 循环条件,注意变更环境 store
              let (v, store2) = eval e locEnv gloEnv structEnv store1
                // 继续循环
              if v.bool then loop (exec body locEnv gloEnv structEnv store2)
                      else store2  //退出循环返回 环境store2
      loop (exec body locEnv gloEnv structEnv store)

    | DoUntil(body,e) -> 
    //body为函数体，e为until()括号内判断表达式，用于判定是否结束
      let rec loop store1 =
                //求值 循环条件,注意变更环境 store
              let (v, store2) = eval e locEnv gloEnv structEnv store1
                // 继续循环
              if v.bool then loop (exec body locEnv gloEnv structEnv store2)
                      else store2  //退出循环返回 环境store2
      loop (exec body locEnv gloEnv structEnv store)

    | Switch(e,body) ->  
              let (res, store1) = eval e locEnv gloEnv structEnv store
              //对e进行求值然后赋值给res
              let rec choose list =
                match list with
                | Case(e1,body1) :: tail -> 
                //使用::符号对Case(e1, body1)和tail进行模式匹配，匹配成功时执行下面的代码块。
                    let (res2, store2) = eval e1 locEnv gloEnv structEnv store1
                    if res2=res then exec body1 locEnv gloEnv structEnv store2
                                else choose tail
                | [] -> store1
                | Default( body1 ) :: tail -> 
                    let (store2) = exec body1 locEnv gloEnv structEnv store1
                    choose tail
                | _ ->   failwith ("Switch语法错误")
                
              (choose body)
              //对choose函数的调用，并以body参数作为输入
    | Case(e,body) -> exec body locEnv gloEnv structEnv store
    | Match(e,body) ->  
    //与switch类似
              let (res, store1) = eval e locEnv gloEnv structEnv store
              let rec choose list =
                match list with
                | Pattern(e1,body1) :: tail -> 
                    let (res2, store2) = eval e1 locEnv gloEnv structEnv store1
                    if res2 = res  then exec body1 locEnv gloEnv structEnv store2
                                   else choose tail
                | [] -> store1 
                | MatchAll( body1) :: tail ->
                    let (store2) = exec body1 locEnv gloEnv structEnv store1
                    choose tail
                | _ ->   failwith ("Match语法错误")

              (choose body)
    | Pattern(e,body) -> exec body locEnv gloEnv structEnv store
    | MatchAll (body )-> exec body locEnv gloEnv structEnv store
    | Break -> store
    | Continue -> store
    | _ ->  failwith ("exec 内部错误")
and stmtordec stmtordec locEnv gloEnv store structEnv =
    match stmtordec with
    | Stmt stmt -> (locEnv, exec stmt locEnv gloEnv structEnv store )
    | Dec (typ, x) -> allocate (typ, x, None) locEnv structEnv store
    | DecAndAssign (typ, name, expr) -> allocate (typ, name, Some(fst (eval expr locEnv gloEnv structEnv store))) locEnv structEnv store
(* Evaluating micro-C expressions *)

and eval e locEnv gloEnv  structEnv store : memoryData * store =
    match e with
    | Sizeof e -> let (res,s1) = eval e locEnv gloEnv structEnv store
                  match res with
                  | STRUCT (s,i,size) -> (INT size,s1)
                  | ARRAY (typ , i,size) -> (INT size,s1)
                  | STRING s -> (INT s.Length,s1)
                  | _ -> (INT 1,s1)
    //获取变量代码

    // | Typeof e -> let (res,s) = eval e locEnv gloEnv structEnv store
    //               match res.checktype with
    //               | TypB   -> (STRING "Bool",s)
    //               | TypI   -> (STRING "Int",s)
    //               | TypP i -> (STRING "Pointer",s)
    //               | TypC   -> (STRING "Char",s)
    //               | TypS   -> (STRING "String",s)
    //               | TypF   -> (STRING "Float",s)
    //               | TypA (typ,i) -> (STRING "Array",s)
    //               | TypeStruct str  -> (STRING ("Struct "+str),s)

    //类型的强制转换
    | ToInt e -> match e with
                    | CstC c -> (INT( int c-48),store)
                    | CstF f -> (FLOAT(f), store)
                    | _ -> failwith ("The input type is incorrect")
    | ToChar e -> match e with
                    | CstI i -> (CHAR(char i + char '0'), store)
                    | _ -> failwith ("The input type is incorrect")
    | ToFloat e -> match e with
                    | CstI i -> (INT(i), store)
                    | _ -> failwith ("The input type is incorrect")
    | ToString e -> match e with
                    | CstI i -> (STRING(string i), store)
                    | CstF f -> (STRING(string f), store)
                    | CstC c -> (STRING(string c), store)
                    | _ -> failwith ("The input type is incorrect")

    | Access acc ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        (getSto store1 loc.pointer, store1)
    | Assign (acc, e) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let (res, store2) = eval e locEnv gloEnv structEnv store1
        (res, setSto store2 loc.pointer res)
    // 对常量表达式(CstI, CstB, CstF, CstS, CstC)和地址表达式(Addr)的求值。                 
    | CstI i -> (INT(i), store)
    | CstB i -> (BOOL(i), store)
    | CstF i -> (FLOAT(i), store)
    | CstS i -> (STRING(i), store)
    | CstC i -> (CHAR(i),store)
    | Addr acc -> access acc locEnv gloEnv structEnv store
    //一元基本算子
    | Prim1 (ope, e1) ->
        let (i1, store1) = eval e1 locEnv gloEnv structEnv store

        let res =
            match ope with
            | "!" -> BOOL(not i1.bool)
            | "printi" ->
                (printf "%d " i1.int
                 i1)
            | "printc" ->
                (printf "%c" i1.char
                 i1)
            | _ -> failwith ("unknown primitive " + ope)

        (res, store1)
    //打印输出
    | Print (ope , e1) ->
        let (i1,store1) = eval e1 locEnv gloEnv structEnv store

        let res = 
            match ope with
            | "%c"   -> (printf "%c " i1.char; i1)
            | "%d"   -> (printf "%d " i1.int;i1)  
            | "%s"   -> (printf "%s " i1.string;i1) 
            | "%f"   -> (printf "%f " i1.float;i1)
            | "%x"   -> (printf "%x"  i1.int;i1)
            | "%o"   -> (printf "%o"  i1.int;i1) 
            | _      -> (printf "%s " i1.string;i1) 

        (res, store1)
    //二元基本算子
    | Prim2 (ope, e1, e2) ->

        let (i1, store1) = eval e1 locEnv gloEnv structEnv store
        let (i2, store2) = eval e2 locEnv gloEnv structEnv store1

        let res =
            match ope with
            | "*" ->
                match (i1) with
                | INT i -> 
                    if(i2.typeName = i1.typeName) then INT(i1.int * i2.int)
                    else FLOAT(i1.float * i2.float)
                | FLOAT i -> 
                    if(i2.typeName = i1.typeName) then FLOAT(i1.float * i2.float)
                    else FLOAT(i1.float * i2.float)
                | _ -> failwith ("cant TIMES")
            | "+" ->
                match (i1, i2) with
                | (INT i1, INT i2) -> INT(i1 + i2)
                | (FLOAT i1, _) -> FLOAT(i1 + i2.float)
                | (_, FLOAT i2) -> FLOAT(i1.float + i2)
                | (STRING i1, STRING i2) -> STRING(i1 + i2)
                | _ -> failwith ("type error: cant calu")
                //有待修改
            | "-" ->
                match (i1, i2) with
                | (INT i1, INT i2) -> INT(i1 - i2)
                | (FLOAT i1, _) -> FLOAT(i1 - i2.float)
                | (_, FLOAT i2) -> FLOAT(i1.float - i2)
                | _ -> failwith ("type error: cant calu")
            | "/" ->
                match i1 with
                | INT i -> 
                    if(i2.typeName = i1.typeName) then INT(i1.int / i2.int)
                    else FLOAT(i1.float / i2.float)
                | FLOAT i -> 
                    if(i2.typeName = i1.typeName) then FLOAT(i1.float / i2.float)
                    else FLOAT(i1.float / i2.float)
                | _ -> failwith ("cant DIV")
            | "%" -> 
                match i2 with
                | INT i -> 
                    if(i2.typeName = i1.typeName) then INT(i1.int % i2.int)
                    else failwith ("cant MOD")
                | _ -> failwith ("cant MOD")
            | "==" ->
                if i1 = i2 then
                    BOOL(true)
                else
                    BOOL(false)
            | "!=" ->
                if i1 <> i2 then
                    BOOL(true)
                else
                    BOOL(false)
            | "<" ->
                if i1 < i2 then
                    BOOL(true)
                else
                    BOOL(false)
            | "<=" ->
                if i1 <= i2 then
                    BOOL(true)
                else
                    BOOL(false)
            | ">=" ->
                if i1 >= i2 then
                    BOOL(true)
                else
                    BOOL(false)
            | ">" ->
                if i1 > i2 then
                    BOOL(true)
                else
                    BOOL(false)
            | _ -> failwith ("unknown primitive " + ope)

        (res, store2)
    //三目运算
    | Prim3 (cond, e1, e2) ->
        let (v1, store1) = eval cond locEnv gloEnv structEnv store

        if v1.bool then
            let (v2, store2) = eval e1 locEnv gloEnv structEnv store1
            (v2, store2)
        else
            let (v3, store3) = eval e2 locEnv gloEnv structEnv store1
            (v3, store3)

    | Andalso (e1, e2) ->
        let (i1, store1) as res = eval e1 locEnv gloEnv structEnv store

        if i1.bool then
            eval e2 locEnv gloEnv structEnv store1
        else
            res
    //+=操作
    | PlusAssign (acc, e) ->
    // 根据变量的信息，可以通过访问(locEnv/gloEnv/structEnv)来查找该变量在存储状态(store)中所表示的地址(loc），最终通过访问该地址获取变量的值，并返回该地址和经过存储状态修改的存储结果。
    // access函数的参数包括一个表示变量的访问路径(acc)，当前环境(locEnv)、全局环境(gloEnv)、结构体环境(structEnv)以及当前存储状态(store)。
    // 具体的访问路径是一个由多个部分构成的列表，每个部分描述了变量被访问的方式。例如，访问全局变量a的访问路径为[Var "a"]；访问结构体MyStruct中的变量field1的访问路径为[Var "MyStruct"; Var "field1"]。
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let (res, store2) = eval e locEnv gloEnv structEnv store1
        let tmp = getSto store1 loc.pointer
        //获取值的类型

        let var =
            match tmp with
            | INT i -> 
                if(tmp.typeName = res.typeName) then INT(tmp.int + res.int)
                else FLOAT(tmp.float + res.float)
            | FLOAT i -> FLOAT(tmp.float + res.float)
            | _ -> failwith ("please input int or float")

        (var, setSto store2 loc.pointer var)
    //-=操作
    | MinusAssign (acc, e) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let (res, store2) = eval e locEnv gloEnv structEnv store1
        let tmp = getSto store1 loc.pointer

        let var =
            match tmp with
            | INT i -> 
                if(tmp.typeName = res.typeName) then INT(tmp.int - res.int)
                else FLOAT(tmp.float - res.float)
            | FLOAT i -> FLOAT(tmp.float - res.float)
            | _ -> failwith ("please input int or float")

        (var, setSto store2 loc.pointer var)
    //*=操作
    | TimesAssign (acc, e) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let (res, store2) = eval e locEnv gloEnv structEnv store1
        let tmp = getSto store1 loc.pointer

        let var =
            match tmp with
            | INT i -> 
                if(tmp.typeName = res.typeName) then INT(tmp.int * res.int)
                else FLOAT(tmp.float * res.float)
            | FLOAT i -> FLOAT(tmp.float * res.float)
            | _ -> failwith ("please input int or float")

        (var, setSto store2 loc.pointer var)
    // /=操作
    | DivAssign (acc, e) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let (res, store2) = eval e locEnv gloEnv structEnv store1
        let tmp = getSto store1 loc.pointer

        let var =
            match tmp with
            | INT i -> 
                if(tmp.typeName = res.typeName) then INT(tmp.int / res.int)
                else FLOAT(tmp.float / res.float)
            | FLOAT i -> FLOAT(tmp.float / res.float)
            | _ -> failwith ("please input int or float")

        (var, setSto store2 loc.pointer var)
    // %=操作
    | ModAssign (acc, e) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let (res, store2) = eval e locEnv gloEnv structEnv store1
        let tmp = getSto store1 loc.pointer

        let var =
            match tmp with
            | INT i -> 
                if(tmp.typeName = res.typeName) then INT(tmp.int % res.int)
                else failwith ("please input int or float")
            | _ -> failwith ("please input int or float")

        (var, setSto store2 loc.pointer var)
    //前++操作
    | PrePlus (ope, acc) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let tmp = getSto store1 loc.pointer
        let var =
            match tmp with
            | INT i -> INT(tmp.int + 1)
            | FLOAT i -> FLOAT(tmp.float+1.0)
            | _ -> failwith ("please input int or float")
        (var, setSto store loc.pointer var)
    //后++操作
    | RearPlus (acc, ope) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let tmp = getSto store1 loc.pointer
        let var =
            match tmp with
            | INT i -> INT(tmp.int + 1)
            | FLOAT i -> FLOAT(tmp.float+1.0)
            | _ -> failwith ("please input int or float")
        (var, setSto store loc.pointer var)
    //前--操作
    | PreMinus (ope, acc) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let tmp = getSto store1 loc.pointer
        let var =
            match tmp with
            | INT i -> INT(tmp.int - 1)
            | FLOAT i -> FLOAT(tmp.float-1.0)
            | _ -> failwith ("please input int or float")
        (var, setSto store loc.pointer var)
    //后--操作
    | RearMinus (acc, ope) ->
        let (loc, store1) = access acc locEnv gloEnv structEnv store
        let tmp = getSto store1 loc.pointer
        let var =
            match tmp with
            | INT i -> INT(tmp.int - 1)
            | FLOAT i -> FLOAT(tmp.float-1.0)
            | _ -> failwith ("please input int or float")
        (var, setSto store loc.pointer var)
        
    | Orelse (e1, e2) ->
        let (i1, store1) as res = eval e1 locEnv gloEnv structEnv store

        if i1.bool then
            res
        else
            eval e2 locEnv gloEnv structEnv store1
    | Call (f, es) -> callfun f es locEnv gloEnv structEnv store
    | _ -> failwith ("unknown primitive ")

and access acc locEnv gloEnv structEnv store : memoryData * store =
    match acc with
    | AccVar x -> (lookup (fst locEnv) x, store)
    | AccDeref e -> eval e locEnv gloEnv structEnv store 
    | AccIndex(acc, idx) -> 
      let (a, store1) = access acc locEnv gloEnv structEnv store
      let aval = getSto store1 a.int
      let (i, store2) = eval idx locEnv gloEnv structEnv store1
      let size = 
        match aval with
        | ARRAY (name,i,size) -> size
        | _ -> failwith ("ARRAY unknown primitive")
      if(i.int>=size) then  failwith( " index out of size" )
      elif(i.int<0) then failwith( " index out of size" )
      else (INT(aval.int + i.int), store2) 
    //表达式类型AccStruct(结构访问)的求值部分。描述了对结构体中某个成员的访问。
    | AccStruct(acc,acc2) ->  let (b, store1) = access acc locEnv gloEnv structEnv store
    //从存储状态得到该结构体变量所存储地址的信息，并将该地址进行一定的处理，读出结构体的具体信息，包括成员参数列表(param)和该结构体的大小(size)。
    //然后，通过计算当前访问成员之前所占用的空间，即(a = b.int - sizestruct)，找到该结构体成员在内存中的起始位置。
                              let aval = getSto store1 b.int
                              let list = structEnv.[aval.int]
                              let param =
                                  match list with 
                                  | (string,paramdecs,int) -> paramdecs
                              let sizestruct =
                                  match list with 
                                  | (string,paramdecs,i) -> i
                              let a =b.int - sizestruct;
                            // 该代码使用了递归的方式，根据访问的成员的类型（类型包括访问单个变量与访问数组中的某个位置）
                            // 和访问的成员在结构体中的位置（即已占用空间的大小）来确定所求变量在存储状态中的位置。
                            // 在该过程中，需要针对访问单个变量或访问数组中某个位置进行不同的处理
                              let rec lookupidx list index = 
                                  match list with
                                  | [] -> failwith("can not find ")
                                  | (typ , name ) ::tail -> match acc2 with
                                                            | AccVar x -> if x = name then index 
                                                                                      else lookupidx tail ( index + ( allsize typ) )
                                                            | AccIndex( acc3, idx ) ->  match acc3 with
                                                                                        | AccVar y ->  if name = y then 
                                                                                                          let size = 
                                                                                                            match typ with
                                                                                                            | TypA(typ,Some i) -> i
                                                                                                            | TypA(typ,None) -> 0
                                                                                                            | _ -> failwith ("AccStruct acc3 typ unknown primitive")
                                                                                                          let (i, store2) = eval idx locEnv gloEnv structEnv store1
                                                                                                          if(i.int>=size) then  failwith( " index out of size" )
                                                                                                          elif(i.int<0) then failwith( " index out of size" )
                                                                                                                        else (index + i.int)
                                                                                                       else lookupidx tail (index + (allsize typ))
                                                                                        | _ -> failwith ("AccStruct acc2 acc3 unknown primitive")
                                                            | _ -> failwith ("AccStruct acc2 unknown primitive")
                              (INT(a+(lookupidx param 0)),store1)

and evals es locEnv gloEnv structEnv store : memoryData list * store =
    match es with
    | [] -> ([], store)
    | e1 :: er ->
        let (v1, store1) = eval e1 locEnv gloEnv structEnv store
        let (vr, storer) = evals er locEnv gloEnv structEnv store1
        (v1 :: vr, storer)

and callfun f es locEnv gloEnv structEnv store : memoryData * store =
    msg
    <| sprintf "callfun: %A\n" (f, locEnv, gloEnv,structEnv, store)

    let (_, nextloc) = locEnv
    let (varEnv, funEnv) = gloEnv
    let (paramdecs, fBody) = lookupFunc funEnv f
    let (vs, store1) = evals es locEnv gloEnv structEnv store

    let (fBodyEnv, store2) =
        bindVars (List.map snd paramdecs) vs (varEnv, nextloc) store1

    let store3 = exec fBody fBodyEnv gloEnv structEnv store2
    let res = store3.TryFind(-1) 
    let restore = store3.Remove(-1)
    match res with
    | None -> (INT(0),restore)
    | Some i -> (i,restore)



//初始化 解释器环境和store
let initEnvAndStore (topdecs: topdec list) : locEnv * funEnv * structEnv * store =

    //包括全局函数和全局变量
    msg $"\ntopdecs:\n{topdecs}\n"

    let rec addv decs locEnv funEnv structEnv store =
        match decs with
        | [] -> (locEnv, funEnv,structEnv, store)

        // 全局变量声明  调用allocate 在store上给变量分配空间
        | Vardec (typ, x) :: decr ->
            let (locEnv1, sto1) = allocate (typ, x, None) locEnv structEnv store
            addv decr locEnv1 funEnv structEnv sto1

        //全局函数 将声明(f,(xs,body))添加到全局函数环境 funEnv
        | Fundec (_, f, xs, body) :: decr -> addv decr locEnv ((f, (xs, body)) :: funEnv) structEnv store
        | Structdec (name,list) :: decr ->
          let rec sizeof list all = 
            match list with
            | [] -> all
            | ( typ ,string ):: tail -> sizeof tail ((allsize typ) + all)
          let fin = sizeof list 0
          addv decr locEnv funEnv ((name,list, fin) :: structEnv) store
        | VariableDeclareAndAssign (typ,x,e) :: decr ->
          let (locEnv1, sto1) = allocate (typ, x, None) locEnv structEnv store
          addv decr locEnv1 funEnv structEnv sto1 
    // ([], 0) []  默认全局环境
    // locEnv ([],0) 变量环境 ，变量定义为空列表[],下一个空闲地址为0
    // ([("n", 1); ("r", 0)], 2)  表示定义了 变量 n , r 下一个可以用的变量索引是 2
    // funEnv []   函数环境，函数定义为空列表[]
    addv topdecs ([], 0) [] [] emptyStore
(* Interpret a complete micro-C program by initializing the store
   and global environments, then invoking its `main' function.
 *)
// run 返回的结果是 代表内存更改的 store 类型
// vs 参数列表 [8,2,...]
// 可以为空 []
let run (Prog topdecs) vs =
    //
    let ((varEnv, nextloc), funEnv,structEnv, store0) = initEnvAndStore topdecs

    // mainParams 是 main 的参数列表
    //
    let (mainParams, mainBody) = lookupFunc funEnv "main"

    let (mainBodyEnv, store1) =
        bindVars (List.map snd mainParams) vs (varEnv, nextloc) store0
    msg
    <|
    //以ex9.c为例子
    // main的 AST
    sprintf "\nmainBody:\n %A\n" mainBody
    +
    //局部环境
    // 如
    // i 存储在store位置0,store中下个空闲位置是1
    //([("i", 0)], 1)
    sprintf "\nmainBodyEnv:\n %A\n" mainBodyEnv
    +

    //全局环境 (变量,函数定义)
    // fac 的AST
    // main的 AST
    sprintf $"\n varEnv:\n {varEnv} \nfunEnv:\n{funEnv}\n"
    +

    //当前存储
    // store 中 0 号 位置存储值为8
    // map [(0, 8)]
    sprintf "\nstore1:\n %A\n" store1

    let endstore =
        exec mainBody mainBodyEnv (varEnv, funEnv) structEnv  store1

    msg $"\nvarEnv:\n{varEnv}\n"
    msg $"\nStore:\n"
    msg <| store2str endstore

    endstore
(* Example programs are found in the files ex1.c, ex2.c, etc *)