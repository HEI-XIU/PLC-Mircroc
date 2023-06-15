# PLC_MicoC

编译原理大作业

| 姓名   | 学号     | 班级       | 任务 | 权重   |
| ------ | -------- | ---------- | ---- | ------ |
| Name   | No.      | Class      | Task | Factor |
| 朱凯   | 32001129 | 计算机2002 | 0.95 | 0.95   |
| 杨晨露 | 32001298 | 计算机2001 | 0.95 | 0.95   |



##  可选改进列表

### 词法

- 手动实现词法分析

### 语法

- do while/until/for 语法
- 三目运算 ? :
- 变量初始化
- 字符串支持
  - 模板字符串
  - 单引号，双引号嵌套
  - [三引号字符串字面量](https://gitee.com/link?target=https%3A%2F%2Fcn.julialang.org%2FJuliaZH.jl%2Flatest%2Fmanual%2Fstrings%2F%23%E4%B8%89%E5%BC%95%E5%8F%B7%E5%AD%97%E7%AC%A6%E4%B8%B2%E5%AD%97%E9%9D%A2%E9%87%8F)
- python语法，无须"{}" 无须";"
- 给出 递归下降分析的实现

### 语义

- 类型检查
- 类型推理
- 多态类型
- 安全数组，越界检查

### 特性

- [位运算](https://gitee.com/link?target=https%3A%2F%2Fcn.julialang.org%2FJuliaZH.jl%2Flatest%2Fmanual%2Fmathematical-operations%2F%23%E4%BD%8D%E8%BF%90%E7%AE%97%E7%AC%A6)
- 布尔类型，逻辑运算
- [复数，有理数](https://gitee.com/link?target=https%3A%2F%2Fcn.julialang.org%2FJuliaZH.jl%2Flatest%2Fmanual%2Fcomplex-and-rational-numbers%2F)
- [链式比较](https://gitee.com/link?target=https%3A%2F%2Fcn.julialang.org%2FJuliaZH.jl%2Flatest%2Fmanual%2Fmathematical-operations%2F%23%E9%93%BE%E5%BC%8F%E6%AF%94%E8%BE%83)
- [正则表达式](https://gitee.com/link?target=https%3A%2F%2Fcn.julialang.org%2FJuliaZH.jl%2Flatest%2Fmanual%2Fstrings%2F%23%E6%AD%A3%E5%88%99%E8%A1%A8%E8%BE%BE%E5%BC%8F)
- 复合数据类型
  - 列表 list cons ，记录 record，元组 tuple
- 模式匹配
- 宏，[元编程](https://gitee.com/link?target=https%3A%2F%2Fcn.julialang.org%2FJuliaZH.jl%2Flatest%2Fmanual%2Fmetaprogramming%2F)
- immutable 机制
- 异常
- 高阶函数，lamda表达式
- 协程
- 生成器
- 异步
- 面向对象
- 逻辑式
- 关系式

## 错误处理

- 提示错误行，列

### 运行时

- 垃圾回收
- 语言库

### 代码生成

- x86,llvm,risc-v
- wasm3虚拟机

### 链接

- 多模块机制，多文件编译

### 其他语言

- decaf
- 类 java ,python,lua,v等语言

# 报告内容

1. 成员代码提交日志 

2. 项目自评等级:(1-5) 请根据自己项目情况填写下表

   | 解释器                   | 完善程度 | 难度 |                             备注                             |      |
   | ------------------------ | -------- | ---- | :----------------------------------------------------------: | ---- |
   | 各类数组，数组检查       | 0        |      |                         未实现该功能                         |      |
   | bool类型                 | 5        | 2    |                                                              | z    |
   | 数强制转型               | 4        | 2    | string，int,float 间的转化较完善，与char与int的转化实现的一般 |      |
   | 字符类型                 | 5        | 2    |                                                              | z    |
   | dountil循环              | 5        | 1    |                                                              | z    |
   | dowhile循环              | 5        | 1    |                                                              | z    |
   | float类型                | 5        | 4    |                                                              | z    |
   | for循环                  | 5        | 1    |                                                              | z    |
   | forin（支持数组和纯数字) |          |      |                         未实现改功能                         |      |
   | （\*……\*）注释的实现     | 5        | 1    |                                                              |      |
   | 按照进制创建整数         | 3        | 3    |                  未实现二进制的进制整数创建                  | z    |
   | 数据初值定义             |          | 1    |                   不太确定是不是自己实现的                   |      |
   | 模式匹配                 | 5        | 1    |                                                              |      |
   | 三目运算                 | 5        | 1    |                                                              | z    |
   | += 等语法糖              | 5        | 1    |                                                              | z    |
   | 自增自减（++/--)         | 5        | 1    |                                                              | z    |
   | 字符串                   | 5        | 3    |                                                              | z    |
   | 结构体                   | 0        |      |                         未实现改功能                         |      |
   | switch case default      | 5        | 1    |                                                              | z    |
   | 类型检查                 | 0        |      |                         未实现改功能                         |      |
   | 变量名称检查             | 0        |      |                         未实现改功能                         |      |
   | break，continue          | 5        | 1    |                                                              |      |
   
| 编译器              | 完善程度 | 难度 | 备注 |      |
| ------------------- | -------- | ---- | ---- | ---- |
| 各类数组，数组检查  |          |      |      |      |
| bool类型            |          |      |      |      |
| 数强制转型          |          |      |      |      |
| 字符类型            |          |      |      |      |
| dountil循环         | 5        | 1    |      |      |
| dowhile循环         | 5        | 1    |      |      |
| float类型           |          |      |      |      |
| for循环             | 5        | 1    |      |      |
| forin（支持纯数字)  |          |      |      |      |
| 函数返回值          |          |      |      |      |
| 按照进制创建整数    |          |      |      |      |
| 数据初值定义        |          |      |      |      |
| 模式匹配            |          |      |      |      |
| 三目运算            | 5        | 1    |      |      |
| += 等语法糖         |          |      |      |      |
| 自增自减（++/--)    |          |      |      |      |
| 结构体              |          |      |      |      |
| switch case default |          |      |      |      |
| 变量名称检查        |          |      |      |      |
| trycatch            |          |      |      |      |
| break,continue      | 5        | 2    |      |      |

| 词法                                      | 评分 | 备注 |      |
| ----------------------------------------- | ---- | ---- | ---- |
| 注释 // /**/                              | ⭐⭐⭐  |      |      |
| 字符串常量 单引号' ' 双引号 "" 三引号 ''' | ⭐    |      |      |
| 数值常量 0b0101， 八进制0o777 十六0xFFDA  | ⭐    |      |      |
| 语法                                      | ⭐    |      |      |
| if的多种方式 switch case                  | ⭐    |      |      |
| 循环 for / while / do while/ until        | ⭐    |      |      |
| for in 表达式                             |      |      |      |
|                                           |      |      |      |
| 语义                                      | ⭐    |      |      |
| 动态作用域，静态作用域                    | ⭐    |      |      |
| 闭包支持                                  |      |      |      |
| 模式匹配支持                              | ⭐    |      |      |
| 中间代码生成 AST，四元式，三元式，llvm    |      |      |      |
| 生成器 generator, yield                   | ⭐    |      |      |

1. 项目说明
   - 项目 是基于现有的MicoC代码
     - 改进 xxx模块 功能1
     - 改进 xxx模块 功能2
     - 。。。。
     
   - 项目 独立开发
     
     - 独立开发了 xx 模块
     - 。。。。
     
   - 项目运行
     
     - **解释器：**
       
       dotnet restore interpc.fsproj //可选
       
       dotnet clean interpc.fsproj //可选
       
       dotnet build -v n interpc.fsproj //构建，-v n查看详细生成过程
       
       ./bin/Debug/net5.0/interpc.exe 测试的文件 参数
       
       dotnet run -p interpc.fsproj 测试的文件 参数
       
       dotnet run -p interpc.fsproj -g 测试的文件 参数 //显示token AST 等调试信息
       
     -  **编译器：**
       
       dotnet restore microcc.fsproj
       
       dotnet clean microcc.fsproj
       
       dotnet build microcc.fsproj //构建编译器
       
       dotnet run -p microcc.fsproj 测试的文件 //执行编译器
       
       ./bin/Debug/net5.0/microcc.exe 测试的文件 //直接执行
       
       
   
2. 解决技术要点说明
   - 解决 xxx 问题1， 关键代码与步骤如下
     - 。。。
     - 。。。
   - 解决 xxx 问题2， 关键代码与步骤如下
     - 。。。
     - 。。。
3. 心得体会（结合自己情况具体说明）
   - 大项目开发过程心得
     - 遇到哪些困难，经历哪里过程，有哪些收获
     - 。。。
     - 。。。
   - 本课程建议
     - 课程难度方面，进度方面，课程内容，授课方式等，给出你的意见
     - 。。。
     - 。。。