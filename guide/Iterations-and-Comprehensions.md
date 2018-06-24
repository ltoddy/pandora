- 迭代和推导

用于语法形式的for家族支持对序列进行迭代。
 列表，向量，字符串，字节字符串，输入端口和散列表都可以用作序列，
 像in-range的构造函数可以提供更多类型的序列。

for的变种累积迭代结果以不同的方式，但它们都具有相同的语法形状。 现在简化了，for的语法是

```racket
(for ([id sequence-expr] ...)
  body ...+)
```

for循环遍历由sequence-expr生成的序列。 对于序列的每个元素，将元素绑定到id，
然后输出bodys的效果。

示列：

```racket
> (for ([i '(1 2 3)])
    (display i))

123
> (for ([i "abc"])
    (printf "~a..." i))

a...b...c...
> (for ([i 4])
    (display i))

0123
```

for的for / list变体更像Racket。 它将body结果累积到一个列表中，
而不是输出body仅仅一种效果。 在更多的技术术语中，for / list实现了列表内容的理解。

示列：

```racket
> (for/list ([i '(1 2 3)])
    (* i i))

'(1 4 9)
> (for/list ([i "abc"])
    i)

'(#\a #\b #\c)
> (for/list ([i 4])
    i)

'(0 1 2 3)
```

for的完整语法可容纳多个序列并行迭代，for\*变体可以嵌套迭代，而不是并行运行。
for和for *积累body的更多变体以不同的方式产生。
在所有这些变体中，包含迭代的谓词都可以包含在绑定中。

- 序列构造

in-range 函数生成数字序列，给定一个可选的起始数字（默认为0），序列结束前的数字和一个可选的步长（默认为1）。 直接使用非负整数k作为序列是（范围内k）的简写。
例如：

```racket
> (for ([i 3])
    (display i))

012
> (for ([i (in-range 3)])
    (display i))

012
> (for ([i (in-range 1 4)])
    (display i))

123
> (for ([i (in-range 1 4 2)])
    (display i))

13
> (for ([i (in-range 4 1 -1)])
    (display i))

432
> (for ([i (in-range 1 4 1/2)])
    (printf " ~a " i))

 1  3/2  2  5/2  3  7/2 
```

in-naturals函数是相似的，除了起始数字必须是一个确切的非负整数（默认为0），步长总是1，没有上限。 for循环只使用in-naturals将永远不会终止，除非正文表达引发异常或以其他方式退出。
例如：

```racket
> (for ([i (in-naturals)])
    (if (= i 10)
        (error "too much!")
        (display i)))

0123456789
too much!
```

stop-before和stop-after函数构造一个给定序列和判断式的新序列。 新序列就像给定的序列，但是在判断式返回true的第一个元素之前或之后立即被截断。
例如：

```racket
> (for ([i (stop-before "abc def"
                        char-whitespace?)])
    (display i))

abc
```

像in-list，in-vector和in-string这样的序列构造函数只是简单地使用list，vector或string作为序列。 和in-range一样，这些构造函数在给定错误类型的值时会引发异常，并且由于它们会避免运行时调度来确定序列类型，因此可以实现更高效的代码生成; 有关更多信息，请参阅迭代性能。
例如：

```racket
> (for ([i (in-string "abc")])
    (display i))

abc
> (for ([i (in-string '(1 2 3))])
    (display i))

in-string: contract violation

  expected: string

  given: '(1 2 3)
```

- for和for\*

更完整的语法是

```racket
(for (clause ...)
  body ...+)
 
clause	 	=	 	[id sequence-expr]
 	 	|	 	#:when boolean-expr
 	 	|	 	#:unless boolean-expr
```

当多个[id sequence-expr]从句在for表里提供时，相应的序列并行遍历：

```racket
> (for ([i (in-range 1 4)]
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "Chapter ~a. ~a\n" i chapter))

Chapter 1. Intro

Chapter 2. Details

Chapter 3. Conclusion
```

对于并行序列，for表达式在任何序列结束时停止迭代。这种行为允许in-naturals创造数值的无限序列，
可用于索引：

```racket
> (for ([i (in-naturals 1)]
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "Chapter ~a. ~a\n" i chapter))

Chapter 1. Intro

Chapter 2. Details

Chapter 3. Conclusion
```
for\*表具有与for语法相同的语法，嵌套多个序列，而不是并行运行它们：

```racket
> (for* ([book '("Guide" "Reference")]
         [chapter '("Intro" "Details" "Conclusion")])
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Details

Guide Conclusion

Reference Intro

Reference Details

Reference Conclusion
```

因此，for\*是对嵌套for的一个简写，以同样的方式let\*是一个let嵌套的简写。

一个clause的#:when boolean-expr表是另一个简写。
仅当boolean-expr产生一个真值时它允许body求值：

```racket
> (for* ([book '("Guide" "Reference")]
         [chapter '("Intro" "Details" "Conclusion")]
         #:when (not (equal? chapter "Details")))
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Conclusion

Reference Intro

Reference Conclusion
```

一个带#:when的boolean-expr可以适用于任何上述迭代绑定。
在一个for表里，仅仅如果在前面绑定的迭代测试是嵌套的时，这个范围是有意义的；
因此，用#:when隔离绑定是多重嵌套的，而不是平行的，甚至于用for也一样。

```racket
> (for ([book '("Guide" "Reference" "Notes")]
        #:when (not (equal? book "Notes"))
        [i (in-naturals 1)]
        [chapter '("Intro" "Details" "Conclusion" "Index")]
        #:when (not (equal? chapter "Index")))
    (printf "~a Chapter ~a. ~a\n" book i chapter))

Guide Chapter 1. Intro

Guide Chapter 2. Details

Guide Chapter 3. Conclusion

Reference Chapter 1. Intro

Reference Chapter 2. Details

Reference Chapter 3. Conclusion
```

一个#:unless从句和一个#:when从句是类似的，但仅当boolean-expr产生一个非值时body求值。

- for/list和for\*/list

for/list表具有与for相同的语法，它对body求值以获取进入新构造列表的值：

```racket
> (for/list ([i (in-naturals 1)]
             [chapter '("Intro" "Details" "Conclusion")])
    (string-append (number->string i) ". " chapter))

'("1. Intro" "2. Details" "3. Conclusion")
```

在一个for-list表中的一个#:when从句随着body的求值修整结果列表：

```racket
> (for/list ([i (in-naturals 1)]
             [chapter '("Intro" "Details" "Conclusion")]
             #:when (odd? i))
    chapter)

'("Intro" "Conclusion")
```

\#:when的这种修剪行为使用for/list比for更有用。而对for来说扁平的when表通常是满足需要的，
一个for/list里的一个when表达表会导致结果列表包含 #<void>以代替省略列表元素。

 for\*/list表类似于for\*，嵌套多个迭代：

```racket 
> (for*/list ([book '("Guide" "Ref.")]
              [chapter '("Intro" "Details")])
    (string-append book " " chapter))

'("Guide Intro" "Guide Details" "Ref. Intro" "Ref. Details")
```

for*/list表与嵌套for/list表不太一样。嵌套for/list将生成一个列表的列表，而不是一个扁平列表。
非常像#:when，那么，for*/list的嵌套比for\*的嵌套更有用。

- for/vector和for\*/vector

for/vector表可以使用与for/list表相同的语法，但是对body的求值进入一个新构造的向量而不是列表：

```racket
> (for/vector ([i (in-naturals 1)]
               [chapter '("Intro" "Details" "Conclusion")])
    (string-append (number->string i) ". " chapter))

'#("1. Intro" "2. Details" "3. Conclusion")
```

for\*/vector表的行为类似，但迭代嵌套和for*一样。

for/vector和for\*/vector表也允许构造向量的长度，在预先提供的情况下。
由此产生的迭代可以比for/vector或for*/vector更有效地执行：
 
```racket
> (let ([chapters '("Intro" "Details" "Conclusion")])
    (for/vector #:length (length chapters) ([i (in-naturals 1)]
                                            [chapter chapters])
      (string-append (number->string i) ". " chapter)))

'#("1. Intro" "2. Details" "3. Conclusion")
```

如果提供了一个长度，当vector被填充或被请求完成时迭代停止，而无论哪个先来。
如果所提供的长度超过请求的迭代次数，则向量中的剩余槽被初始化为make-vector的缺省参数。

- for/and和for/or

for/and表用and组合迭代结果，一旦遇到#f就停止：

```racket
> (for/and ([chapter '("Intro" "Details" "Conclusion")])
    (equal? chapter "Intro"))

#f
```

for/or表用or组合迭代结果，一旦遇到真值立即停止：

```racket
> (for/or ([chapter '("Intro" "Details" "Conclusion")])
    (equal? chapter "Intro"))

#t
```

与通常一样，for\*/and和for\*/or表提供与嵌套迭代相同的功能。

- for/first和for/last

for/first表返回第一次对body进行求值的结果，跳过了进一步的迭代。
这个带有一个#:when从句的表是最非常有用的。

```racket
> (for/first ([chapter '("Intro" "Details" "Conclusion" "Index")]
              #:when (not (equal? chapter "Intro")))
    chapter)

"Details"
```

如果body求值进行零次，那么结果是#f。

for/last表运行所有迭代，返回最后一次迭代的值（或如果没有迭代运行返回#f）：

```racket
> (for/last ([chapter '("Intro" "Details" "Conclusion" "Index")]
              #:when (not (equal? chapter "Index")))
    chapter)

"Conclusion"
```

通常， for\*/first和for\*/last表提供和嵌套迭代相同的工具：

```racket
> (for*/first ([book '("Guide" "Reference")]
               [chapter '("Intro" "Details" "Conclusion" "Index")]
               #:when (not (equal? chapter "Intro")))
    (list book chapter))

'("Guide" "Details")
> (for*/last ([book '("Guide" "Reference")]
              [chapter '("Intro" "Details" "Conclusion" "Index")]
              #:when (not (equal? chapter "Index")))
    (list book chapter))

'("Reference" "Conclusion")
```

- for/fold和for\*fold

for/fold表是合并迭代结果的一种非常通用的方法。它的语法与原来的for语法略有不同，
因为必须在开始时声明累积变量：

```racket
(for/fold ([accum-id init-expr] ...)
          (clause ...)
  body ...+)
```

在简单的情况下，仅提供一个 [accum-id init-expr]，那么for/fold的结果是accum-id的最终值，
并启动了init-expr的值。在clause和body、accum-id可参照获得其当前值，
并且最后的body为下一次迭代的提供accum-id值。

例如：

```racket
> (for/fold ([len 0])
            ([chapter '("Intro" "Conclusion")])
    (+ len (string-length chapter)))

15
> (for/fold ([prev #f])
            ([i (in-naturals 1)]
             [chapter '("Intro" "Details" "Details" "Conclusion")]
             #:when (not (equal? chapter prev)))
    (printf "~a. ~a\n" i chapter)
    chapter)

1. Intro

2. Details

4. Conclusion

"Conclusion"
```

当多个accum-id被指定，那么最后的body必须产生多值，每一个对应accum-id.
for/fold的表达本身产生多值给结果。

例如:

```racket
> (for/fold ([prev #f]
             [counter 1])
            ([chapter '("Intro" "Details" "Details" "Conclusion")]
             #:when (not (equal? chapter prev)))
    (printf "~a. ~a\n" counter chapter)
    (values chapter
            (add1 counter)))

1. Intro

2. Details

3. Conclusion

"Conclusion"

4
```

- 多值序列

同样，一个函数或表达式可以生成多个值，序列的单个迭代可以生成多个元素。
例如，作为一个序列的哈希表生成两个迭代的两个值：一个键和一个值。

同样，let-values将多个结果绑定到多个标识符，for能将多个序列元素绑定到多个迭代标识符：

```racket
> (for ([(k v) #hash(("apple" . 1) ("banana" . 3))])
    (printf "~a count: ~a\n" k v))

apple count: 1

banana count: 3
```

这种扩展到多值绑定对所有变量都适用。例如，对于列表嵌套迭代，构建一个列表，也可以处理多值序列：

```racket
> (for*/list ([(k v) #hash(("apple" . 1) ("banana" . 3))]
              [(i) (in-range v)])
    k)

'("apple" "banana" "banana" "banana")
```

- 打断迭代

一个更完整的for的语法是

```racket
(for (clause ...)
  body-or-break ... body)
 
clause	 	=	 	[id sequence-expr]
 	 	|	 	#:when boolean-expr
 	 	|	 	#:unless boolean-expr
 	 	|	 	break
 	 	 	 	 
body-or-break	 	=	 	body
 	 	|	 	break
 	 	 	 	 
break	 	=	 	#:break boolean-expr
 	 	|	 	#:final boolean-expr
```

那是，一个#:break或#:final从句可以包括绑定从句和body之间的迭代。在绑定从句之间，当它的boolean-expr为真时，#:break像#:unless，在for之间的所有序列停止。处在body内，除了当boolean-expr是真时，#:break对序列有一样的效果，并且它也阻止随后的body从当前迭代的求值。

例如，当在有效跳跃后的序列以及主体之间使用#:unless，

```racket
> (for ([book '("Guide" "Story" "Reference")]
        #:unless (equal? book "Story")
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Details

Guide Conclusion

Reference Intro

Reference Details

Reference Conclusion

使用#:break致使整个for迭代终止：

> (for ([book '("Guide" "Story" "Reference")]
        #:break (equal? book "Story")
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Details

Guide Conclusion
> (for* ([book '("Guide" "Story" "Reference")]
         [chapter '("Intro" "Details" "Conclusion")])
    #:break (and (equal? book "Story")
                 (equal? chapter "Conclusion"))
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Details

Guide Conclusion

Story Intro

Story Details
```

一个#:final从句类似于#:break，但它不立即终止迭代。相反，它最多地允许为每一个序列和最多再一个body的求值绘制再一个元素。

```racket
> (for* ([book '("Guide" "Story" "Reference")]
         [chapter '("Intro" "Details" "Conclusion")])
    #:final (and (equal? book "Story")
                 (equal? chapter "Conclusion"))
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Details

Guide Conclusion

Story Intro

Story Details

Story Conclusion
> (for ([book '("Guide" "Story" "Reference")]
        #:final (equal? book "Story")
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "~a ~a\n" book chapter))

Guide Intro

Guide Details

Guide Conclusion

Story Intro
```
