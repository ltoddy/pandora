- 符号

```
(something [id ...+] an-expr ...)
```

斜体的元变量在本规范中，如`id`和`an-expr`，使用Racket标识符的语法，所以an-expr是一元变量。命名约定隐式定义了许多元变量的含义：
1、以id结尾的元变量表示标识符，如X或my-favorite-martian。
2、一元标识符以keyword结束代表一个关键字，如#:tag。
3、一元标识符以expr结束表达代表任何子表，它将被解析为一个表达式。
4、一元标识符以body结束代表任何子表；它将被解析为局部定义或表达式。只有在没有任何表达式之前，一个body才能解析为一个定义，而最后一个body必须是一个表达式；参见《内部定义》（Internal Definitions）部分。

在语法的方括号表示形式的括号序列，其中方括号通常用于（约定）。也就是说，方括号并不意味着是句法表的可选部分。

…表示前一个表的零个或多个重复，…+表示前面数据的一个或多个重复。否则，非斜体标识代表自己。

根据上面的语法，这里有一些something的合乎逻辑的用法：

```
(something [x])
(something [x] (+ 1 2))
(something [x my-favorite-martian x] (+ 1 2) #f)
```

一些语法表规范指的是不隐式定义而不是预先定义的元变量。这样的元变量在主表定义后面使用BNF-like格式提供选择：

```
(something-else [thing ...+] an-expr ...)
```

```
thing	 	=	 	thing-id
 	 	|	 	thing-keyword
```
    
上面的例子表明，在其它的表中，一个thing要么是标识符要么是关键字。

- 标识符和绑定

表达式的上下文决定表达式中出现的标识符的含义。特别是，用语言racket开始一个模块时，如：

> #lang racket

意味着，在模块中，本指南中描述的标识符从这里描述的含义开始：cons指创建一个配对的函数，car指的是提取一个配对的第一个元素的函数，等等。

诸如像 define、lambda和let的表，并让一个意义与一个或多个标识符相关联，也就是说，它们绑定标识符。绑定应用的程序的一部分是绑定的范围。对给定表达式有效的绑定集是表达式的环境。

例如，有以下内容：

```racket
#lang racket

(define f
  (lambda (x)
    (let ([y 5])
      (+ x y))))

(f 10)
```

define是f的绑定，lambda有一个对x的绑定，let有一个对y的绑定，对f的绑定范围是整个模块；x绑定的范围是(let ([y 5]) (+ x y))；y绑定的范围仅仅是 (+ x y)。(+ x y)的环境包括对y、x和f的绑定，以及所有在racket中的绑定。

模块级的define只能绑定尚未定义或被require进入模块的标识符。但是，局部define或其他绑定表可以为已有绑定的标识符提供新的局部绑定；这样的绑定会对现有绑定屏蔽（shadow）。

例如：

```racket
(define f
  (lambda (append)
    (define cons (append "ugly" "confusing"))
    (let ([append 'this-was])
      (list append cons))))

> (f list)

'(this-was ("ugly" "confusing"))
```

类似地，模块级define可以从模块的语言中shadow一个绑定。例如，一个racket模块里的(define cons 1)屏蔽被racket所提供的cons。有意屏蔽一个语言绑定是一个绝佳的主意——尤其是像cons这种被广泛使用的绑定——但是屏蔽消除了程序员应该避免使用的语言提供的所有模糊绑定。

即使像define和lambda这些从绑定中得到它们的含义，尽管它们有转换（transformer）绑定（这意味着它们表示语法表）而不是值绑定。由于define具有一个转换绑定，因此标识符本身不能用于获取值。但是，对define的常规绑定可以被屏蔽。

例如：

```
> define

eval:1:0: define: bad syntax

  in: define
> (let ([define 5]) define)

5
```

同样，用这种方式来隐藏标准绑定是一个绝佳主意，但这种可能性是Racket灵活性的与生俱来的部分。

- 函数调用（过程程序）

一个表表达式：

```
(proc-expr arg-expr ...)
```

是一个函数调用——也被称为一个应用程序——`proc-expr`不是标识符，而是作为一个语法翻译器（如`if`或`define`）。

  - 赋值顺序和数量

  一个函数调用求值是首先求值proc-expr和为所有arg-expr（由左至右）。然后，如果proc-expr产生一个函数接受arg-expr提供的所有参数，这个函数被调用。否则，将引发异常。

  例如：
  ```racket
  > (cons 1 null)
  '(1)
  > (+ 1 2 3)
  6
  > (cons 1 2 3)
  cons: arity mismatch;
  the expected number of arguments does not match the given
  number
    expected: 2
    given: 3
    arguments...:
    1
    2
    3
  > (1 2 3)
  application: not a procedure;
  expected a procedure that can be applied to arguments
    given: 1
    arguments...:
    2
    3
  ```

  某些函数，如`cons`，接受固定数量的参数。某些函数，如`+`或`list`，接受任意数量的参数。一些函数接受一系列参数计数；例如`substring`接受两个或三个参数。一个函数的元数（`arity`）是它接受参数的数量。

  - 关键字参数

  除了通过位置参数外，有些函数接受关键字参数。因此，`arg`可以是一个`arg-keyword` `arg-expr`序列而不只是一个`arg-expr`：

  ```
  (proc-expr arg ...)
  arg	 	=	 	arg-expr
   	 	|	 	arg-keyword arg-expr
  ```

  例如：

  ```
  (go "super.rkt" #:mode 'fast)
  ```

  调用函数绑定到"super.rkt" 作为位置参数，并用'fast通过#:mode关键字作为相关参数。关键字隐式地与后面的表达式配对。

  既然关键字本身不是一个表达式，那么

  ```
  (go "super.rkt" #:mode #:fast)
  ```

  就是语法错误。#:mode关键字必须跟着一个表达式以产生一个参数值，并#:fast不是一个表达式。

  关键字arg的顺序决定arg-expr的求值顺序，而一个函数接受关键字参数与在参数列表中的位置无关。上面对go的调用可以等价地写为：

  ```
  (go #:mode 'fast "super.rkt")
  ```

  - apply函数

  函数调用的语法支持任意数量的参数，但是一个特定的调用总是指定一个固定数量的参数。因此，一个带参数列表的函数不能直接将一个类似于+的函数应用到列表中的所有项中：

  ```racket
  (define (avg lst) ; doesn’t work...
  (/ (+ lst) (length lst)))

  > (avg '(1 2 3))
  +: contract violation
    expected: number?
    given: '(1 2 3)

  (define (avg lst) ; doesn’t always work...
    (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
      (length lst)))

  > (avg '(1 2 3))
  2
  > (avg '(1 2))
  list-ref: index too large for list
    index: 2
    in: '(1 2)
  ```

  apply函数提供了一种绕过这种限制的方法。它使用一个函数和一个list参数，并将函数应用到列表中的值：

  ```racket
  (define (avg lst)
  (/ (apply + lst) (length lst)))

  > (avg '(1 2 3))
  2
  > (avg '(1 2))
  3/2
  > (avg '(1 2 3 4))
  5/2
  ```

  为方便起见，apply函数接受函数和列表之间的附加参数。额外的参数被有效地加入参数列表：

  ```racket
  (define (anti-sum lst)
  (apply - 0 lst))

  > (anti-sum '(1 2 3))
  -6
  ```

  apply函数也接受关键字参数，并将其传递给调用函数：

  ```racket
  (apply go #:mode 'fast '("super.rkt"))
  (apply go '("super.rkt") #:mode 'fast)
  ```

  包含在apply列表参数中的关键字不算作调用函数的关键字参数；相反，这个列表中的所有参数都被位置参数处理。要将一个关键字参数列表传递给函数，使用keyword-apply函数，它接受一个要应用的函数和三个列表。前两个列表是平行的，其中第一个列表包含关键字（按keyword<?排序），第二个列表包含每个关键字的对应参数。第三个列表包含位置函数参数，就像apply。

```racket
(keyword-apply go
               '(#:mode)
               '(fast)
               '("super.rkt"))
```

- lambda函数（程序）

lambda表达式创建函数。在最简单的情况，lambda表达式具有形式：

```
(lambda (arg-id ...)
  body ...+)
```

具有n个arg-ids的lambda表接受n个参数：

```racket
> ((lambda (x) x)
   1)
1

> ((lambda (x y) (+ x y))
   1 2)
3

> ((lambda (x y) (+ x y))
   1)
#<procedure>: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: 2
  given: 1
  arguments...:
   1
```

  - 申明剩余（rest）参数

  lambda表达式也可以有这种形式：

  ```racket
  (lambda rest-id
  body ...+)
  ```

  也就是说，lambda表达式可以有一个没有被圆括号包围的单个rest-id。所得到的函数接受任意数目的参数，并且这个参数放入一个绑定到rest-id的列表：

  例如：

  ```racket
  > ((lambda x x)
    1 2 3)
  '(1 2 3)
  > ((lambda x x))
  '()
  > ((lambda x (car x))
    1 2 3)
  1
  ```

  带有一个rest-id的函数经常使用apply函数调用另一个函数，它接受任意数量的参数。

  例如：

  ```
  (define max-mag
    (lambda nums
      (apply max (map magnitude nums))))
  
  > (max 1 -2 0)
  1
  > (max-mag 1 -2 0)
  2
  ```

  lambda表还支持必需参数与rest-id相结合：

  ```racket
  (lambda (arg-id ...+ . rest-id)
    body ...+)
  ```

  这个表的结果是一个函数，它至少需要与arg-id一样多的参数，并且还接受任意数量的附加参数。

  例如：

  ```
  (define max-mag
    (lambda (num . nums)
      (apply max (map magnitude (cons num nums)))))
  
  > (max-mag 1 -2 0)
  2
  > (max-mag)
  max-mag: arity mismatch;
  the expected number of arguments does not match the given
  number
    expected: at least 1
    given: 0
  ```

  `rest-id`变量有时称为`rest`参数，因为它接受函数参数的“rest”。


  - 声明可选（optional）参数

  不只是标识符，一个lambda表的参数（不仅是剩余参数）可以用标识符和缺省值指定：

  ```racket
  (lambda gen-formals
    body ...+)
 
  gen-formals	 	=	 	(arg ...)
      |	 	rest-id
      |	 	(arg ...+ . rest-id)
          
  arg	 	=	 	arg-id
      |	 	[arg-id default-expr]
  ```

  表的参数[`arg-id` `default-expr`]是可选的。当参数不在应用程序中提供，`default-expr`产生默认值。`default-expr`可以引用任何前面的`arg-id`，并且下面的每个`arg-id`也必须应该有一个默认值。

  例如：
  ```
  (define greet
    (lambda (given [surname "Smith"])
      (string-append "Hello, " given " " surname)))
    
  > (greet "John")
  "Hello, John Smith"
  > (greet "John" "Doe")
  "Hello, John Doe"

  (define greet
    (lambda (given [surname (if (equal? given "John")
                                "Doe"
                                "Smith")])
      (string-append "Hello, " given " " surname)))

  > (greet "John")
  "Hello, John Doe"
  > (greet "Adam")
  "Hello, Adam Smith"
  ```

  - 声明关键字（keyword）参数

  lambda表可以声明要通过关键字传递的参数，而不是位置。关键字参数可以与位置参数混合，也可以为两种参数提供默认值表达式：

  ```racket
  (lambda gen-formals
    body ...+)
  
  gen-formals	 	=	 	(arg ...)
      |	 	rest-id
      |	 	(arg ...+ . rest-id)
          
  arg	 	=	 	arg-id
      |	 	[arg-id default-expr]
      |	 	arg-keyword arg-id
      |	 	arg-keyword [arg-id default-expr]
  ```

  由一个应用程序使用同一个`arg-keyword`关键字提供一个参数，该参数指定为`arg-keyword` `arg-id`。在参数列表中关键字标识符对的位置与应用程序中的参数匹配并不重要，因为它将通过关键字而不是位置与参数值匹配。

  ```
  (define greet
    (lambda (given #:last surname)
      (string-append "Hello, " given " " surname)))
      
  > (greet "John" #:last "Smith")
  "Hello, John Smith"
  > (greet #:last "Doe" "John")
  "Hello, John Doe"
  ```

  `arg-keyword` [`arg-id` `default-expr`]参数指定一个带默认值的关键字参数。

  例如：

  ```racket
  (define greet
    (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
      (string-append hi ", " given " " surname)))
      
  > (greet "John")
  "Hello, John Smith"
  > (greet "Karl" #:last "Marx")
  "Hello, Karl Marx"
  > (greet "John" #:hi "Howdy")
  "Howdy, John Smith"
  > (greet "Karl" #:last "Marx" #:hi "Guten Tag")
  "Guten Tag, Karl Marx"
  ```

  `lambda`表不支持创建一个接受“`rest`”关键字的函数。要构造一个接受所有关键字参数的函数，请使用`make-keyword-procedure`函数。这个函数支持`make-keyword-procedure`通过前两个（位置）参数中的并行列表接受关键字参数，然后由应用程序的所有位置参数作为剩余位置参数。

  例如：

  ```racket
  (define (trace-wrap f)
    (make-keyword-procedure
    (lambda (kws kw-args . rest)
      (printf "Called with ~s ~s ~s\n" kws kw-args rest)
      (keyword-apply f kws kw-args rest))))
  
  > ((trace-wrap greet) "John" #:hi "Howdy")
  Called with (#:hi) ("Howdy") ("John")
  "Howdy, John Smith"
  ```

  - 多解函数：case-lambda

  case-lambda表创建一个函数，该函数可以根据所提供的参数的数量具有完全不同的行为。case-lambda表达式有以下形式：

```racket
(case-lambda
  [formals body ...+]
  ...)
 
formals	 	=	 	(arg-id ...)
 	 	|	 	rest-id
 	 	|	 	(arg-id ...+ . rest-id)
```

  每个[`formals` `body` `...+`]类似于(`lambda` `formals` `body` `...+`)。通过`case-lambda`应用函数生成类似于应用一个`lambda`匹配给定参数数量的第一种情况。

  例如：

  ```racket
  (define greet
    (case-lambda
      [(name) (string-append "Hello, " name)]
      [(given surname) (string-append "Hello, " given " " surname)]))
  
  > (greet "John")
  "Hello, John"
  > (greet "John" "Smith")
  "Hello, John Smith"
  > (greet)
  greet: arity mismatch;
  the expected number of arguments does not match the given
  number
    given: 0
  ```

  lambda函数不能直接支持可选参数或关键字参数。

- 定义：define

基本定义具为如下形式：

```racket
(define id expr)
```

在这种情况下，id绑定到了expr的结果。

例如：

```racket
(define salutation (list-ref '("Hi" "Hello") (random 2)))
 
> salutation
"Hi"
```

  - 函数速记法

  定义表还支持函数定义的简写：

  ```racket
  (define (id arg ...) body ...+)
  ```

  这是以下内容的简写：

  ```racket
  (define id (lambda (arg ...) body ...+))
  ```

  例如：

  ```racket
  (define (greet name)
    (string-append salutation ", " name))

  > (greet "John")
  "Hi, John"

  (define (greet first [surname "Smith"] #:hi [hi salutation])
    (string-append hi ", " first " " surname))

  > (greet "John")
  "Hi, John Smith"
  > (greet "John" #:hi "Hey")
  "Hey, John Smith"
  > (greet "John" "Doe")
  "Hi, John Doe"
  ```

  通过define这个函数简写也支持一个剩余参数（rest argument）（即，一个额外参数用于在列表中收集最后参数）：

  ```racket
  (define (id arg ... . rest-id) body ...+)
  ```

  这是以下内容的简写：

  ```racket
  (define id (lambda (arg ... . rest-id) body ...+))
  ```

  例如：

  ```racket
  (define (avg . l)
    (/ (apply + l) (length l)))
  
  > (avg 1 2 3)
  2
  ```

  - 咖喱函数简写

  注意下面的`make-add-suffix`函数接收一个字符串并返回另一个带字符串的函数：

  ```racket
  (define make-add-suffix
    (lambda (s2)
      (lambda (s) (string-append s s2))))
  ```

  虽然不常见，但make-add-suffix的结果可以直接调用，就像这样：

  ```racket
  > ((make-add-suffix "!") "hello")
  "hello!"
  ```

  从某种意义上说，make-add-suffix是一个函数，需要两个参数，但每次只需要一个参数。一个函数带有一些参数并返回一个函数会消费更多，有时被称为一个咖喱函数（curried function）。

  使用define的函数简写形式，make-add-suffix可以等效地写成：

  ```racket
  (define (make-add-suffix s2)
    (lambda (s) (string-append s s2)))
  ```

  这个简写反映了(make-add-suffix "!")函数调用的形状。define表更进一步支持定义反映嵌套函数调用的咖喱函数简写：

```racket
(define ((make-add-suffix s2) s)
  (string-append s s2))

> ((make-add-suffix "!") "hello")
"hello!"

(define louder (make-add-suffix "!"))
(define less-sure (make-add-suffix "?"))

> (less-sure "really")
"really?"
> (louder "really")
"really!"
```

  define函数简写的完整语法如下所示：

  ```racket
  (define (head args) body ...+)
  
  head	 	=	 	id
      |	 	(head args)
          
  args	 	=	 	arg ...
      |	 	arg ... . rest-id
  ```

  这个简写的扩展有一个给定义中的每个head的嵌套lambda表，最里面的head与最外面的lambda通信。

  - 多值和define-values

  Racket表达式通常产生一个单独的结果，但有些表达式可以产生多个结果。例如，quotient（商）和remainder（余数）各自产生一个值，但quotient/remainder同时产生相同的两个值：

  ```racket
  > (quotient 13 3)
  4
  > (remainder 13 3)
  1
  > (quotient/remainder 13 3)
  4
  1
  ```

  如上所示，REPL在自己的行打印每一结果值。

  多值函数可以用values函数来实现，它接受任意数量的值，并将它们作为结果返回：

  ```racket
  > (values 1 2 3)
  1
  2
  3

  (define (split-name name)
    (let ([parts (regexp-split " " name)])
      (if (= (length parts) 2)
          (values (list-ref parts 0) (list-ref parts 1))
          (error "not a <first> <last> name"))))

  
  > (split-name "Adam Smith")
  "Adam"
  "Smith"
  ```

  define-values表同时将多个标识符绑定到多个结果产生单个表达式：

  ```racket
  (define-values (id ...) expr)
  ```

  由expr产生的结果数必须与id的数相匹配。

  例如：

  ```racket
  (define-values (given surname) (split-name "Adam Smith"))
  
  > given
  "Adam"
  > surname
  "Smith"
  ```

  一个define表（不是一个函数简写）等价于一个带有单个id的define-values表。

  - 内部定义

  当句法表的语法指定body，那相应的表可以是定义或表达式。作为一个body的定义是一个内部定义（internal definition）。

  一个body序列中的表达式和内部定义可以混合，只要最后一个body是表达式。

  例如，lambda的语法是：

  ```racket
  (lambda gen-formals
    body ...+)
  ```
  
  下面是语法的有效实例：

  ```racket
  (lambda (f)                ; 没有定义
    (printf "running\n")
    (f 0))
  
  (lambda (f)                ; 一个定义
    (define (log-it what)
      (printf "~a\n" what))
    (log-it "running")
    (f 0)
    (log-it "done"))
  
  (lambda (f n)              ; 两个定义
    (define (call n)
      (if (zero? n)
          (log-it "done")
          (begin
            (log-it "running")
            (f n)
            (call (- n 1)))))
    (define (log-it what)
      (printf "~a\n" what))
    (call n))
  ```

  特定的body序列中的内部定义是相互递归的，也就是说，只要引用在定义发生之前没有实际求值，那么任何定义都可以引用任何其他定义。如果过早引用定义，则会出现错误。

  例如：

  ```racket
  (define (weird)
    (define x x)
    x)
  
  > (weird)
  x: undefined;
  cannot use before initialization
  ```

  一系列的内部定义只使用define很容易转换为等效的letrec表（如同在下一节介绍的内容）。然而，其他的定义表可以表现为一个body，包括define-values、 struct（见《程序员定义的数据类型》（Programmer-Defined Datatypes））或define-syntax（见《宏》（Macros））。

- 局部绑定

虽然内部define可用于局部绑定，Racket提供了三种形式给予程序员在绑定方面的更多控制：let、let*和letrec。

  - 并行绑定：let

  let表绑定一组标识符，每个标识符都是某个表达式的结果，用于let主体：

  ```racket
  (let ([id expr] ...) body ...+)
  ```

  id绑定处于”平行”状态，即对于任何id，没有一个id绑定到右边的expr，但都可在body内找到。id必须被定义为彼此不同的形式。

  例如：

  ```racket
  > (let ([me "Bob"])
      me)
  "Bob"

  > (let ([me "Bob"]
          [myself "Robert"]
          [I "Bobby"])
      (list me myself I))
  '("Bob" "Robert" "Bobby")

  > (let ([me "Bob"]
          [me "Robert"])
      me)
  eval:3:0: let: duplicate identifier
    at: me
    in: (let ((me "Bob") (me "Robert")) me)
  ```

  事实上，一个id的expr不会明白自己的绑定通常对封装有用，必须转回到旧的值：

  ```racket
  > (let ([me "Tarzan"]
          [you "Jane"])
      (let ([me you]
            [you me])
        (list me you)))
  '("Jane" "Tarzan")
  ```

  - 顺序绑定：let*

  let*的语法和let的一样：

  ```racket
  (let* ([id expr] ...) body ...+)
  ```

  不同的是，每个id可用于以后的expr，以及body内。此外，id不需要有区别，最新的绑定可见。

  例如：

  ```racket
  > (let* ([x (list "Burroughs")]
          [y (cons "Rice" x)]
          [z (cons "Edgar" y)])
      (list x y z))

  '(("Burroughs") ("Rice" "Burroughs") ("Edgar" "Rice" "Burroughs"))
  > (let* ([name (list "Burroughs")]
          [name (cons "Rice" name)]
          [name (cons "Edgar" name)])
      name)

  '("Edgar" "Rice" "Burroughs")
  ```

  换言之， let*表是相当于嵌套的let表，每一个都有一个单独的绑定：

  ```racket
  > (let ([name (list "Burroughs")])
      (let ([name (cons "Rice" name)])
        (let ([name (cons "Edgar" name)])
          name)))

  '("Edgar" "Rice" "Burroughs")
  ```

  - 递归绑定：letrec

  letrec的语法也和let相同：

  ```racket
  (letrec ([id expr] ...) body ...+)
  ```

  而let使其其绑定只在body内被提供，let*使其绑定提供给任何后来的绑定expr，letrec使其绑定提供给所有其他expr，甚至更早的。换句话说，letrec绑定是递归的。

  在一个letrec表中的expr经常大都是递归或互相递归的lambda表函数：

  ```racket
  > (letrec ([swing
              (lambda (t)
                (if (eq? (car t) 'tarzan)
                    (cons 'vine
                          (cons 'tarzan (cddr t)))
                    (cons (car t)
                          (swing (cdr t)))))])
      (swing '(vine tarzan vine vine)))

  '(vine vine tarzan vine)

  > (letrec ([tarzan-near-top-of-tree?
              (lambda (name path depth)
                (or (equal? name "tarzan")
                    (and (directory-exists? path)
                        (tarzan-in-directory? path depth))))]
            [tarzan-in-directory?
              (lambda (dir depth)
                (cond
                  [(zero? depth) #f]
                  [else
                  (ormap
                    (λ (elem)
                      (tarzan-near-top-of-tree? (path-element->string elem)
                                                (build-path dir elem)
                                                (- depth 1)))
                    (directory-list dir))]))])
      (tarzan-near-top-of-tree? "tmp"
                                (find-system-path 'temp-dir)
                                4))

  directory-list: could not open directory
    path: /var/tmp/abrt/Python-2013-12-05-03:18:26-13782
    system error: Permission denied; errno=13
  ```

  而一个letrec表的expr是典型的lambda表达式，它们可以是任何表达式。表达式按顺序求值，在获得每个值之后，它立即与相应的id相关联。如果id在其值准备就绪之前被引用，则会引发一个错误，就像内部定义一样。

  ```racket
  > (letrec ([quicksand quicksand])
      quicksand)

  quicksand: undefined;
  cannot use before initialization
  ```

  - 命名let

  命名let是一个迭代和递归表。它使用与局部绑定相同的语法关键字let，但在let之后的标识符（而不是一个括号）触发不同的解析。

  ```racket
  (let proc-id ([arg-id init-expr] ...)
    body ...+)
  ```

  命名的let表等效于

  ```racket
  (letrec ([proc-id (lambda (arg-id ...)
                      body ...+)])
    (proc-id init-expr ...))
  ```

  也就是说，一个命名的let绑定一个只在函数体中可见的函数标识符，并且用一些初始表达式的值隐式调用函数。

  例如：

  ```racket
  (define (duplicate pos lst)
    (let dup ([i 0]
              [lst lst])
    (cond
      [(= i pos) (cons (car lst) lst)]
      [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))
  
  > (duplicate 1 (list "apple" "cheese burger!" "banana"))
  '("apple" "cheese burger!" "cheese burger!" "banana")
  ```

  - 多值绑定：let-values，let*-values，letrec-values

  以define-values同样的方式绑定定义的多个结果（见《多值和define-values》）（Multiple Values and define-values），let-values、let*-values和letrec-values值绑定多个局部结果。

  ```racket
  (let-values ([(id ...) expr] ...)
    body ...+)

  (let*-values ([(id ...) expr] ...)
    body ...+)

  (letrec-values ([(id ...) expr] ...)
    body ...+)
  ```

  每个expr必须产生许多值作为id的对应。绑定的规则是和没有-values的形式的表相同：let-values的id只绑定在body里，let*-value的id绑定在后面从句的expr里，letrec-value的id绑定是针对对所有的expr。

  例如：

  ```racket
  > (let-values ([(q r) (quotient/remainder 14 3)])
      (list q r))

  '(4 2)
  ```

- 条件分支

大多数函数都可用于分支，如<和string?，结果要么产生#t要么产生#f。Racket的分支表，无论什么情况，对待任何非#f值为真。我们说一个真值（true value）意味着其它为任何非#f值。

本约定的“真值（true value）”在#f能够代替故障或表明不提供一个可选的值的地方与协议完全吻合 。（谨防过度使用这一技巧，记住一个异常通常是一个更好的机制来报告故障。）

例如，member函数具有双重职责；它可以用来查找从一个特定项目开始的列表的尾部，或者它可以用来简单地检查一个项目是否存在于列表中：

```racket
> (member "Groucho" '("Harpo" "Zeppo"))
#f
> (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
'("Groucho" "Zeppo")
> (if (member "Groucho" '("Harpo" "Zeppo"))
      'yep
      'nope)
'nope
> (if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
      'yep
      'nope)
'yep
```
  - 简单分支：if

  在if表：

  ```racket
  (if test-expr then-expr else-expr)
  ```

  test-expr总是求值。如果它产生任何非#f值，然后对then-expr求值。否则，else-expr被求值。

  if表必须既有一个then-expr也有一个else-expr；后者不是可选的。执行（或跳过）基于一个test-expr的副作用，使用when或unless，将在后边《顺序》（Sequencing）部分描述。

  - 组合测试：and和or

  Racket的and和or是语法形式，而不是函数。不像一个函数，如果前边的一个求值确定了答案，and和or表会忽略后边的表达式求值。

  ```racket
  (and expr ...)
  ```

  如果其所有的expr产生#f，or表产生#f。否则，它从它的expr第一个非#f值产生结果值。作为一个特殊的情况，(or)产生#f。

  例如：

  ```racket
  > (define (got-milk? lst)
      (and (not (null? lst))
          (or (eq? 'milk (car lst))
              (got-milk? (cdr lst))))) ;仅在需要时发生
  > (got-milk? '(apple banana))
  #f
  > (got-milk? '(apple milk banana))
  #t
  ```

  如果求值达到and或or表的最后一个expr，那么expr的值直接决定and或or的结果。因此，最后一个expr是在尾部的位置，这意味着上面got-milk?函数在固定空间中运行。

  - 约束测试：cond

  cond表链接了一系列的测试以选择一个表达式结果。一个最近似的情况，cond语法如下：

  ```racket
  (cond [test-expr body ...+]
        ...)
  ```

  每个test-expr求值顺序求值。如果它产生#f，相应的body被忽略，求值进程进入下一个test-expr。当一个test-expr产生一个真值，它的body求值产生的结果作为cond表的结果。并不再进一步对test-expr求值。

  在cond最后的test-expr可用else代替。在求值条件里，else作为一个#t的同义词提供。但它阐明了最后的从句是为了获取所有剩余的事例。如果else没有被使用，而且可能没有test-expr产生真值；在这种情况下，该cond表达式的结果是#<void>。

  例如：

  ```racket
  > (cond
    [(= 2 3) (error "wrong!")]
    [(= 2 2) 'ok])
  'ok

  > (cond
    [(= 2 3) (error "wrong!")])
  > (cond
    [(= 2 3) (error "wrong!")]
    [else 'ok])
  'ok

  (define (got-milk? lst)
    (cond
      [(null? lst) #f]
      [(eq? 'milk (car lst)) #t]
      [else (got-milk? (cdr lst))]))

  > (got-milk? '(apple banana))
  #f
  > (got-milk? '(apple milk banana))
  #t
  ```

  包括以上两种从句的cond的完整语法：

  ```racket
  (cond cond-clause ...)
  
  cond-clause	 	=	 	[test-expr then-body ...+]
      |	 	[else then-body ...+]
      |	 	[test-expr => proc-expr]
      |	 	[test-expr]
  ```

  =>变体获取test-expr真的结果并且传递给proc-expr的结果，proc-expr必须是有一个参数的函数。


  例如：

  ```racket
  > (define (after-groucho lst)
      (cond
        [(member "Groucho" lst) => cdr]
        [else (error "not there")]))
  > (after-groucho '("Harpo" "Groucho" "Zeppo"))
  '("Zeppo")
  > (after-groucho '("Harpo" "Zeppo"))
  not there
  ```

  只包括一个test-expr的从句是很少使用的。它捕获test-expr的真值的结果，并简单地返回这个结果给整个cond表达式。

- 排序

Racket程序员喜欢编写尽可能少的带副作用的程序，因为纯粹的函数代码更容易测试和组成更大的程序。

然而，与外部环境的交互需要进行排序，例如在向显示器写入、打开图形窗口或在磁盘上操作文件时。 

  - 前置效应：begin

  begin表达式排序表达式：

```racket
(begin expr ...+)
```

  expr被顺序求值，并且除最后的expr结果外所有都被忽视。来自最后一个expr的结果作为begin表的结果，它是相对于begin表位于尾部的位置。

  例如：

  ```racket
  (define (print-triangle height)
    (if (zero? height)
        (void)
        (begin
          (display (make-string height #\*))
          (newline)
          (print-triangle (sub1 height)))))
  
  > (print-triangle 4)
  ****
  ***
  **
  *
  ```

  有多种表，比如lambda或cond支持一系列表达式甚至没有begin。这样的状态有时被叫做有一个隐含的begin（implicit begin）。

  例如：

  ```racket
  (define (print-triangle height)
    (cond
      [(positive? height)
      (display (make-string height #\*))
      (newline)
      (print-triangle (sub1 height))]))
  
  > (print-triangle 4)
  ****
  ***
  **
  *
  ```

  begin表在顶层（top level）、模块级（module level）或仅在内部定义之后作为body是特殊的。在这些状态下，begin的上下文被拼接到周围的上下文中，而不是形成一个表达式。
 
  例如：

  ```racket
  > (let ([curly 0])
      (begin
        (define moe (+ 1 curly))
        (define larry (+ 1 moe)))
      (list larry curly moe))

  '(2 0 1)
  ```

  这种拼接行为主要用于宏（macro），我们稍后将在《宏》（macro）中讨论它。

  - 后置效应：begin0

  一个begin0表达式与有一个begin表达式有相同的语法：

  ```racket
  (begin0 expr ...+)
  ```

  不同的是begin0返回第一个expr的结果，而不是最后一个expr的结果。begin0表对于实现发生在一个计算之后的副作用是有用的，尤其是在计算产生了一个未知的数值结果的情况下。

  例如：

  ```racket
  (define (log-times thunk)
    (printf "Start: ~s\n" (current-inexact-milliseconds))
    (begin0
      (thunk)
      (printf "End..: ~s\n" (current-inexact-milliseconds))))
  
  > (log-times (lambda () (sleep 0.1) 0))
  Start: 1509391508010.048
  End..: 1509391508110.237
  0
  > (log-times (lambda () (values 1 2)))
  Start: 1509391508110.958
  End..: 1509391508111.068
  1
  2
  ```

  - if影响：when和unless

  when表将if样式条件与“then”子句（并且没有“else”子句）的排序相结合：
 
  ```racket
  (when test-expr then-body ...+)
  ```
 
  如果test-expr产生一个真值，那么所有的then-body被求值。最后一个then-body的结果是when表的结果。否则，没有then-body被求值而且结果是#<void>。

  unless是相似的：

  ```racket
  (unless test-expr then-body ...+)
  ```

  不同的是，test-expr结果是相反的：如果test-expr结果为#f时then-body被求值。

  例如：

  ```racket
  (define (enumerate lst)
    (if (null? (cdr lst))
        (printf "~a.\n" (car lst))
        (begin
          (printf "~a, " (car lst))
          (when (null? (cdr (cdr lst)))
            (printf "and "))
          (enumerate (cdr lst)))))

  > (enumerate '("Larry" "Curly" "Moe"))
  Larry, Curly, and Moe.

  (define (print-triangle height)
    (unless (zero? height)
      (display (make-string height #\*))
      (newline)
      (print-triangle (sub1 height))))

  > (print-triangle 4)
  ****
  ***
  **
  *
  ```

- 赋值：set!

使用set!赋值给变量：

```racket
(set! id expr)
```

一个set!表达式对expr求值并改变id（它必须限制在闭括号的环境内）为结果值。set!表达式自己返回的结果是#<void>。

例如：

```racket
(define greeted null)

(define (greet name)
  (set! greeted (cons name greeted))
  (string-append "Hello, " name))

> (greet "Athos")
"Hello, Athos"
> (greet "Porthos")
"Hello, Porthos"
> (greet "Aramis")
"Hello, Aramis"
> greeted
'("Aramis" "Porthos" "Athos")

(define (make-running-total)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))
(define win (make-running-total))
(define lose (make-running-total))

> (win)
1
> (win)
2
> (lose)
1
> (win)
3
```

  - 使用的指导原则

  虽然使用set!有时是适当的，Racket风格通常建议不使用set!。下面的准则有助于解释什么时候使用set!是适当的。

  1、与任何现代语言一样，分配给共享标识符不是将参数传递给过程或获得结果的替代。

  事实上很糟糕的示例：

  ```racket
  (define name "unknown")
  (define result "unknown")
  (define (greet)
    (set! result (string-append "Hello, " name)))
  
  > (set! name "John")
  > (greet)
  > result
  "Hello, John"
  ```

  合适的示例：

  ```racket
  (define (greet name)
    (string-append "Hello, " name))

  > (greet "John")
  "Hello, John"
  > (greet "Anna")
  "Hello, Anna"
  ```

  2、对局部变量的赋值序列远比嵌套绑定差。

  差的示例：

  ```racket
  > (let ([tree 0])
      (set! tree (list tree 1 tree))
      (set! tree (list tree 2 tree))
      (set! tree (list tree 3 tree))
      tree)

  '(((0 1 0) 2 (0 1 0)) 3 ((0 1 0) 2 (0 1 0)))
  ```

  好的示例：

  ```racket
  > (let* ([tree 0]
          [tree (list tree 1 tree)]
          [tree (list tree 2 tree)]
          [tree (list tree 3 tree)])
      tree)

  '(((0 1 0) 2 (0 1 0)) 3 ((0 1 0) 2 (0 1 0)))
  ```

  3、使用赋值来从迭代中积累结果是不好的风格。通过循环参数积累更好。

  略差的示例：

  ```racket
  (define (sum lst)
    (let ([s 0])
      (for-each (lambda (i) (set! s (+ i s)))
                lst)
      s))
  
  > (sum '(1 2 3))
  6
  ```

  好的示例：

  ```racket
  (define (sum lst)
    (let loop ([lst lst] [s 0])
      (if (null? lst)
          s
          (loop (cdr lst) (+ s (car lst))))))

  > (sum '(1 2 3))
  6
  ```

  更好（使用现有函数）示例：

  ```racket
  (define (sum lst)
    (apply + lst))

  > (sum '(1 2 3))
  6
  ```

  好的（一般的途径）示例：

  ```racket
  (define (sum lst)
    (for/fold ([s 0])
              ([i (in-list lst)])
      (+ s i)))
  
  > (sum '(1 2 3))
  6
  ```

  4、对于有状态的对象的情况是必要的或合适的，那么用set!实现对象的状态是比较好的。

  合适的实例：

  ```racket
  (define next-number!
    (let ([n 0])
      (lambda ()
        (set! n (add1 n))
        n)))
  
  > (next-number!)
  1
  > (next-number!)
  2
  > (next-number!)
  3
  ```

  所有其它的情况都相同，则不使用赋值或变化的程序总是优于使用赋值或变化的程序。虽然应该避免副作用，但如果结果代码可读性更高，或者实现了更好的算法，则应该使用这些副作用。

  使用可变值，如向量和哈希表，对程序风格的疑虑会比直接使用set!要少。不过，在程序里简单地用vector-set!替换set!显然没有改善程序的风格。

  - 多值赋值：set!-values

  set!-values表一次赋值给多个变量，给出一个生成适当的数值的表达式：
 
  ```racket
  (set!-values (id ...) expr)
  ```

  这个表等价于使用let-values从expr接收多个结果，然后将结果使用set!单独赋值给id。

  例如：

  ```racket
  (define game
    (let ([w 0]
          [l 0])
      (lambda (win?)
        (if win?
            (set! w (+ w 1))
            (set! l (+ l 1)))
        (begin0
          (values w l)
          ; swap sides...
          (set!-values (w l) (values l w))))))
  
  > (game #t)
  1
  0
  > (game #t)
  1
  1
  > (game #f)
  1
  2
  ```

- 引用：quote和'

引用表产生一个常数：

```racket
(quote datum)
```

datum的语法在技术上被指定为read函数解析为单个元素的任何内容。quote表的值与read将产生给定的datum的值相同。

datum可以是一个符号、一个布尔值、一个数字、一个（字符或字节）字符串、一个字符、一个关键字、一个空列表、一个包含更多类似值的配对（或列表），一个包含更多类似值的向量，一个包含更多类似值的哈希表，或者一个包含其它类似值的格子。

例如：

```racket
> (quote apple)
'apple
> (quote #t)
#t
> (quote 42)
42
> (quote "hello")
"hello"
> (quote ())
'()
> (quote ((1 2 3) #("z" x) . the-end))
'((1 2 3) #("z" x) . the-end)
> (quote (1 2 . (3)))
'(1 2 3)
```

正如上面最后一个示例所示，datum不需要匹配一个值的格式化的打印表。一个datum不能作为从#<开始的打印呈现，所以不能是#<void>、#<undefined>或一个程序。

quote表很少用于datum的布尔值、数字或字符串本身，因为这些值的打印表可以用作常量。quote表更常用于符号和列表，当没有被引用时，它具有其他含义（标识符、函数调用等）。

表达式：

```racket
'datum
```
是

```racket
(quote datum)
```

的简写。

这个简写几乎总是用来代替quote。简写甚至应用于datum中，因此它可以生成包含quote的列表。

例如：

```racket
> 'apple
'apple
> '"hello"
"hello"
> '(1 2 3)
'(1 2 3)
> (display '(you can 'me))
(you can 'me)
```

- 准引用：quasiquote和`

quasiquote表类似于quote：

```racket
(quasiquote datum)
```

然而，对于出现在datum的每个(unquote expr)，expr被求值并产生一个替代unquote子表的值。

例如：

```racket
> (quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
'(1 2 3 4)
```

此表可用于编写根据特定模式生成列表的函数。

例如：

```racket
> (define (deep n)
    (cond
      [(zero? n) 0]
      [else
       (quasiquote ((unquote n) (unquote (deep (- n 1)))))]))
> (deep 8)
'(8 (7 (6 (5 (4 (3 (2 (1 0))))))))
```

甚至可以以编程方式廉价地构造表达式。（当然，第9次就超出了10，你应该使用一个macro来做这个（第10次是当你学习了一本像PLAI那样的教科书）。）

例如：

```racket
> (define (build-exp n)
    (add-lets n (make-sum n)))
> (define (add-lets n body)
    (cond
      [(zero? n) body]
      [else
       (quasiquote
        (let ([(unquote (n->var n)) (unquote n)])
          (unquote (add-lets (- n 1) body))))]))
> (define (make-sum n)
    (cond
      [(= n 1) (n->var 1)]
      [else
       (quasiquote (+ (unquote (n->var n))
                      (unquote (make-sum (- n 1)))))]))
> (define (n->var n) (string->symbol (format "x~a" n)))
> (build-exp 3)

'(let ((x3 3)) (let ((x2 2)) (let ((x1 1)) (+ x3 (+ x2 x1)))))
```

unquote-splicing表和unquote相似，但其expr必须产生一个列表，而且unquote-splicing表必须出现在一个产生一个列表或向量的上下文里。顾名思义，生成的列表被拼接到它自己使用的上下文中。

例如：

```racket
> (quasiquote (1 2 (unquote-splicing (list (+ 1 2) (- 5 1))) 5))
'(1 2 3 4 5)
```

使用拼接，我们可以修改上面的示例表达式的构造，只需要一个let表达式和一个单个+表达式。

例如：

```racket
> (define (build-exp n)
    (add-lets
     n
     (quasiquote (+ (unquote-splicing
                     (build-list
                      n
                      (λ (x) (n->var (+ x 1)))))))))
> (define (add-lets n body)
    (quasiquote
     (let (unquote
           (build-list
            n
            (λ (n)
              (quasiquote
               [(unquote (n->var (+ n 1))) (unquote (+ n 1))]))))
       (unquote body))))
> (define (n->var n) (string->symbol (format "x~a" n)))
> (build-exp 3)

'(let ((x1 1) (x2 2) (x3 3)) (+ x1 x2 x3))
```

如果一个quasiquote表出现在一个封闭的quasiquote表里，那这个内部的quasiquote有效地删除unquote和unquote-splicing表的一层，结果第二层unquote和unquote-splicing表是必要的。

例如：

```racket
> (quasiquote (1 2 (quasiquote (unquote (+ 1 2)))))
'(1 2 (quasiquote (unquote (+ 1 2))))
> (quasiquote (1 2 (quasiquote (unquote (unquote (+ 1 2))))))
'(1 2 (quasiquote (unquote 3)))
> (quasiquote (1 2 (quasiquote ((unquote (+ 1 2)) (unquote (unquote (- 5 1)))))))
'(1 2 (quasiquote ((unquote (+ 1 2)) (unquote 4))))
```

上面的求值实际上不会像显示那样打印。相反，quasiquote和unquote的简写形式将被使用：`（即一个反引号）和,（即一个逗号）。同样的简写可在表达式中使用：

例如：

```racket
> `(1 2 `(,(+ 1 2) ,,(- 5 1)))
'(1 2 `(,(+ 1 2) ,4))
```

unquote-splicing简写形式是,@：

例如：

```racket
> `(1 2 ,@(list (+ 1 2) (- 5 1)))
'(1 2 3 4)
```

- 简单调度：case

通过将表达式的结果与子句的值相匹配，case表分派一个子句：

```racket
(case expr
  [(datum ...+) body ...+]
  ...)
```

每个datum将使用equal?对比expr的结果，然后相应的body被求值。case表可以为N个datum在O(log N)时间内分派正确的从句。

可以给每个从句提供多个datum，而且如果任何一个datum匹配，那么相应的body被求值。

例如：

```racket
> (let ([v (random 6)])
    (printf "~a\n" v)
    (case v
      [(0) 'zero]
      [(1) 'one]
      [(2) 'two]
      [(3 4 5) 'many]))
      
0
'zero
```

一个case表最后一个从句可以使用else，就像cond那样：

例如：

```racket
> (case (random 6)
    [(0) 'zero]
    [(1) 'one]
    [(2) 'two]
    [else 'many])

'many
```

对于更一般的模式匹配（但没有分派时间保证），使用match，这个会在《模式匹配》（Pattern Matching）中介绍。


- 动态绑定：parameterize

parameterize表把一个新值和body表达式求值过程中的一个参数（parameter）相结合：

```racket
(parameterize ([parameter-expr value-expr] ...)
  body ...+)
```

例如，error-print-width参数控制在错误消息中打印一个值的字符数：

```racket
> (parameterize ([error-print-width 5])
    (car (expt 10 1024)))

car: contract violation
  expected: pair?
  given: 10...
  
> (parameterize ([error-print-width 10])
    (car (expt 10 1024)))

car: contract violation
  expected: pair?
  given: 1000000..
```
  
一般来说，参数实现了一种动态绑定。make-parameter函数接受任何值并返回一个初始化为给定值的新参数。应用参数作为一个函数返回它的其当前值：

```racket
> (define location (make-parameter "here"))
> (location)

"here"
```

parameterize表不是一个像let那样的绑定表；每次location的使用向上都直接指向原来的定义。在parameterize主体被求值的整个时间内parameterize表调整参数的值，甚至是文本以外的parameterize主体的参数使用：

```racket
> (define (would-you-could-you?)
    (and (not (equal? (location) "here"))
         (not (equal? (location) "there"))))
> (would-you-could-you?)

#f

> (parameterize ([location "on a bus"])
    (would-you-could-you?))

#t
```

如果参数的使用是在parameterize主体内部进行的，但是在parameterize表产生一个值之前没有被求值，那么这个用法看不到parameterize表所安装的值：

```racket
> (let ([get (parameterize ([location "with a fox"])
               (lambda () (location)))])
    (get))

"here"
```

参数的当前绑定可以通过将该参数作为具有值的函数进行调用以进行必要的调整。 如果parameterize已经调整了参数的值，那么直接应用参数过程只会影响与活动parameterize相关的值：

```racket
> (define (try-again! where)
    (location where))
> (location)
"here"

> (parameterize ([location "on a train"])
    (list (location)
          (begin (try-again! "in a boat")
                 (location))))
'("on a train" "in a boat")

> (location)
"here"
```

使用parameterize通常更适合于更新参数值，这与使用let绑定新变量的原因相同，最好使用set!（见《赋值：set!》）。

似乎变量和set!可以解决很多参数解决的相同问题。 例如，lokation可以被定义为一个字符串，以及set!可以用来调整它的价值：

```racket
> (define lokation "here")
> (define (would-ya-could-ya?)
    (and (not (equal? lokation "here"))
         (not (equal? lokation "there"))))
> (set! lokation "on a bus")
> (would-ya-could-ya?)

#t
```

然而，参数与set!相比，参数提供了几个关键的优点：

1、parameterize表有助于在正确避免异常时自动重置参数的值。 添加异常处理程序和其它表去实现转回一个set!是比较繁琐的。

2、参数可以和tail调用很好的一致（请参阅《尾递归》）（Tail Recursion）。 parameterize表中的最后一个body相对于parameterize表处于尾部位置。

3、参数与线程正常工作（请参阅《线程》）（Threads）。 parameterize表仅调整当前线程中的参数值，以避免与其他线程竞争。
