- 异常

每当发生运行时错误时，就会引发异常（exception）。除非捕获异常，然后通过打印与异常相关联的消息来处理，然后从计算中逃逸。

```racket
> (/ 1 0)

/: division by zero
> (car 17)

car: contract violation

  expected: pair?

  given: 17
```

  若要捕获异常，请使用with-handlers表：

```racket
 (with-handlers ([predicate-expr handler-expr] ...)
  body ...+)
```

在处理中的每个predicate-expr确定一种异常，它是由with-handlers表捕获，代表异常的值传递给处理器程序由handler-expr生成。handler-expr的结果即with-handlers表达式的结果。

例如，零做除数错误创建了exn:fail:contract:divide-by-zero结构类型：

```racket
> (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])
    (/ 1 0))

+inf.0
> (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])
    (car 17))

car: contract violation

  expected: pair?

  given: 17
```

error函数是引起异常的一种方法。它打包一个错误信息及其它信息进入到exn:fail结构：

```racket
> (error "crash!")

crash!
> (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
    (error "crash!"))

'air-bag
```
 exn:fail:contract:divide-by-zero和exn:fail结构类型是exn结构类型的子类型。核心表和核心函数引起的异常总是创建exn的或其子类的一个实例，但异常不必通过结构表示。raise函数允许你创建任何值作为异常：

```racket
> (raise 2)

uncaught exception: 2
> (with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) 'two)])
    (raise 2))

'two
> (with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) 'two)])
    (/ 1 0))

/: division by zero
```

在一个with-handlers表里的多个predicate-expr让你在不同的途径处理各种不同的异常。判断按顺序进行尝试，如果没有匹配，则将异常传播到封闭上下文中。

```racket
> (define (always-fail n)
    (with-handlers ([even? (lambda (v) 'even)]
                    [positive? (lambda (v) 'positive)])
      (raise n)))
> (always-fail 2)

'even
> (always-fail 3)

'positive
> (always-fail -3)

uncaught exception: -3
> (with-handlers ([negative? (lambda (v) 'negative)])
   (always-fail -3))

'negative
````

使用(lambda (v) #t)作为一个判断捕获所有异常，当然：

```racket
> (with-handlers ([(lambda (v) #t) (lambda (v) 'oops)])
    (car 17))

'oops
```

然而，捕获所有异常通常是个坏主意。如果用户在一个终端窗口键入ctl-c或者在DrRacket点击停止按钮（Stop）中断计算，然后通常exn:break异常不应该被捕获。仅仅应该抓取具有代表性的错误，使用exn:fail?作为判断：

```racket
> (with-handlers ([exn:fail? (lambda (v) 'oops)])
    (car 17))

'oops
> (with-handlers ([exn:fail? (lambda (v) 'oops)])
    (break-thread (current-thread)) ; 模拟Ctl-C
    (car 17))

user break
```

- 提示和中止

当一个异常被引发时，控制将从一个任意深度的求值上下文逃逸到异常被捕获的位置——或者如果没有捕捉到异常，那么所有的出路都会消失：

```racket
> (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (/ 1 0)))))))

/: division by zero
```

但如果控制逃逸”所有的出路“，为什么REPL在一个错误被打印之后能够继续运行？你可能会认为这是因为REPL把每一个互动封装进了with-handlers表里，它抓取了所有的异常，但这确实不是原因。

实际的原因是，REPL用一个提示封装了互动，有效地用一个逃逸位置标记求值上下文。如果一个异常没有被捕获，那么关于异常的信息被打印印刷，然后求值中止到最近的封闭提示。更确切地说，每个提示有提示标签（prompt tag），并有指定的默认提示标签，未捕获的异常处理程序使用中止。

call-with-continuation-prompt函数用一个给定的提示标签设置提示，然后在提示符下对一个给定的thunk求值。default-continuation-prompt-tag函数返回默认提示标记。abort-current-continuation函数转义到具有给定提示标记的最近的封闭提示符。

```racket
> (define (escape v)
    (abort-current-continuation
     (default-continuation-prompt-tag)
     (lambda () v)))
> (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))

0
> (+ 1
     (call-with-continuation-prompt
      (lambda ()
        (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0))))))))
      (default-continuation-prompt-tag)))

1
```

在上面的escape中，值V被封装在一个过程中，该过程在转义到封闭提示符后被调用。

提示和中止看起来非常像异常处理和引发。事实上，提示和中止本质上是一种更原始的异常形式，与with-handlers和raise都是按提示执行和中止。更原始形式的权力与操作符名称中的“延续（continuation）”一词有关，我们将在下一节中讨论。

- 延续

延续（continuation）是一个值，该值封装了表达式的求值上下文。call-with-composable-continuation函数从当前函数调用和运行到最近的外围提示捕获当前延续。（记住，每个REPL互动都是隐含地封装在REPL提示中。）

例如，在下面内容里

(+ 1 (+ 1 (+ 1 0)))

在求值0的位置时，表达式上下文包含三个嵌套的加法表达式。我们可以通过更改0来获取上下文，然后在返回0之前获取延续：

> (define saved-k #f)
> (define (save-it!)
    (call-with-composable-continuation
     (lambda (k) ; k is the captured continuation
       (set! saved-k k)
       0)))
> (+ 1 (+ 1 (+ 1 (save-it!))))

3

保存在save-k中的延续封装程序上下文(+ 1 (+ 1 (+ 1 ?)))，?代表插入结果值的位置——因为在save-it!被调用时这是表达式上下文。延续被封装从而其行为类似于函数(lambda (v) (+ 1 (+ 1 (+ 1 v))))：

> (saved-k 0)

3
> (saved-k 10)

13
> (saved-k (saved-k 0))

6

通过call-with-composable-continuation捕获延续是动态确定的，没有语法。例如，用

> (define (sum n)
    (if (zero? n)
        (save-it!)
        (+ n (sum (sub1 n)))))
> (sum 5)

15

在saved-k里延续成为(lambda (x) (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 x))))))：

> (saved-k 0)

15
> (saved-k 10)

25

在Racket（或Scheme）中较传统的延续运算符是call-with-current-continuation，它通常缩写为call/cc。这是像call-with-composable-continuation，但应用捕获的延续在还原保存的延续前首先中止（对于当前提示）。此外，Scheme系统传统上支持程序启动时的单个提示符，而不是通过call-with-continuation-prompt允许新提示。在Racket中延续有时被称为分隔的延续（delimited continuations），因为一个程序可以引入新定义的提示，并且作为call-with-composable-continuation捕获的延续有时被称为组合的延续（composable continuations），因为他们没有一个内置的abort。
