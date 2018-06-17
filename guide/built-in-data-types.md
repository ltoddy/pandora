内置的数据类型

## 布尔值(Boolean)

Racket有表示布尔值的两个常数：#f表示真，#f表示假。大写的#T和#F在语法上描述为同样的值，但小写形式是首选。

boolean? 过程识别两个布尔常量。然而，在对if、cond、and、or等等的测试表达式的结果里，除了#f外，任何值都是记为真。

比如：

```scheme
> (= 2 (+ 1 1))
#t

> (boolean? #t)
#t

> (boolean? #f)
#t

> (boolean? "no")
#f

> (if "no" 1 0)
1
```

## 数值(Number)

Racket的数值（number）可以是精确的也可以是不精确的：

1. 精确的数字是：
    1. 一个任意大的或小的整数，比如：5，99999999999999999，或-17；
    2. 一个有理数，即精确的两个任意小的或大的整数比，比如：1/2，99999999999999999/2 -3/4；
    3. 一个复数，带有精确的实部和虚部（即虚部不为零），比如：1+2i或1/2+3/4i。

2. 一个不精确的数字是：
    1. 一个数的一个IEEE浮点表示，比如：2.0或3.14e+87，其中IEEE无穷大和非数书写为：+inf.0，-inf.0和+nan.0（或-nan.0）；
    2. 一个带有实部和虚部配对的复数的IEEE浮点表示，比如：2.0+3.0i或-inf.0+nan.0i；一种特殊情况是，一个不精确的复数可以有一个精确的零实部和一个不精确的虚部。

计算涉及到精确的数值产生不精确的结果，这样的情况对数据造成一种污染。注意，然而，Racket没有提供"不精确的布尔值"，所以对不精确的数字的比较分支计算却能产生精确的结果。exact->inexact和inexact->exact程序在两种类型的数值之间转换。

例如：

```scheme
> (/ 1 2)
1/2

> (/ 1 2.0)
0.5

> (if (= 3.0 2.999) 1 2)
2

> (inexact->exact 0.1)
3602879701896397/36028797018963968
```

在针对通常的number?的加法中，整数类、有理数类、实类（总是有理数）和复数都以通常的方式定义，并被程序integer?、rational?、real?和complex?所识别。一些数学过程只接受实数，但大多数实现了对复数的标准扩展。

例如：

```scheme
> (integer? 5)
#t

> (complex? 5)
#t

> (integer? 5.0)
#t

> (integer? 1+2i)
#f

> (complex? 1+2i)
#t

> (complex? 1.0+2.0i)
#t

> (abs -5)
5

> (abs -5+2i)
abs: contract violation
  expected: real?
  given: -5+2i

> (sin -5+2i)
3.6076607742131563+1.0288031496599335i
```

`=`过程比较数值相等的数值。如果给定不精确和精确的数字进行比较，它实际上会在比较之前将不精确数字转换为精确数字。`eqv?`(乃至`equal?`)程序，相反，程序比较既是精确数而且数值上相等的数值。

例如：

```scheme
> (= 1 1.0)
#t

> (eqv? 1 1.0)
#f
```

## 字符(Character)

Racket字符对应于Unicode标量值。粗略地说，一个标量值是一个无符号整数，它的表示适合21位，并且映射到某种自然语言字符或字符块的某些概念。

虽然每个Racket字符对应一个整数，但字符数据类型和数值是有区别的。char->integer和integer->char程序在标量值和相应字符之间转换。

一个可打印字符通常在以#\作为代表字符后打印。一个不可打印字符通常在以#\u开始十六进制数的标量值打印。几个字符特殊打印；例如，空格和换行字符分别打印为#\space和#\newline。

例如：

```scheme
> (integer->char 65)
#\A

> (char->integer #\A)
65

> #\λ
#\λ

> #\u03BB
#\λ

> (integer->char 17)
#\u0011

> (char->integer #\space)
32
```

Racket提供了几种分类和转换字符的程序.注意,然而,某些Unicode字符要如人类希望的那样转换只有在一个字符串中才行(例如,"ß"的大写转换或者"Σ"的小写转换)。

例如：

```scheme
> (char-alphabetic? #\A)
#t
> (char-numeric? #\0)
#t
> (char-whitespace? #\newline)
#t
> (char-downcase #\A)
#\a
> (char-upcase #\ß)
#\ß
```

char=?程序比较两个或多个字符，char-ci=?比较忽略字符大小写。eqv?和equal?程序的行为与char=?在字符方面的表现一样；当更具体地声明正在比较的值是字符时使用char=?.

例如：

```scheme
> (char=? #\a #\A)
#f
> (char-ci=? #\a #\A)
#t
> (eqv? #\a #\A)
#f
```

## 字符串(Unicode Strings)

字符串是固定长度的字符(characters)数组。它打印使用双引号，双引号和反斜杠字符在字符串中是用反斜杠转义。其他常见的字符串转义是支持的，包括\n换行，\r回车，使用\后边跟随三个八进制数字实现八进制转义，使用\u（达四位数）实现十六进制转义。在打印字符串时通常用\u显示字符串中的不可打印字符。

例如：

```scheme
> "Apple"
"Apple"
> "\u03BB"
"λ"
> (display "Apple")
Apple
> (display "a \"quoted\" thing")
a "quoted" thing
> (display "two\nlines")
two
lines
> (display "\u03BB")
λ
```

字符串可以是可变的，也可以是不可变的；作为表达式直接写入的字符串是不可变的，但大多数其他字符串是可变的。 `make-string`程序创建一个给定长度和可选填充字符的可变字符串。`string-ref`程序从字符串（用0字符串集索引）存取一个字符。`string-set!`程序更改可变字符串中的一个字符。

例如：
```scheme
> (string-ref "Apple" 0)
#\A
> (define s (make-string 5 #\.))
> s
"....."
> (string-set! s 2 #\λ)
> s
"..λ.."
```

字符串排序和状态操作通常是区域无关(locale-independent)的，也就是说，它们对所有用户都是相同的。提供了一些与区域相关(locale-dependent)的操作，允许字符串折叠和排序的方式取决于最终用户的区域设置。如果你在排序字符串，例如，如果排序结果应该在机器和用户之间保持一致，使用`string<?`或者`string-ci<?`，但如果排序纯粹是为最终用户订购字符串，使用`string-locale<?`或者`string-locale-ci<?`。

例如：

```scheme
> (string<? "apple" "Banana")
#f
> (string-ci<? "apple" "Banana")
#t
> (string-upcase "Straße")
"STRASSE"
> (parameterize ([current-locale "C"])
    (string-locale-upcase "Straße"))
"STRAßE"
```

## 字节(Byte)和字节字符串(Byte String)

字节是包含0到255之间的精确整数。byte?判断表示字节的数字。

例如：

```scheme
> (byte? 0)
#t
> (byte? 256)
#f
```

字节字符串类似于字符，但它的内容是字节序列而不是字符。字节字符串可用于处理纯ASCII而不是Unicode文本的应用程序中。一个字节的字符串打印形式特别支持这样的用途，因为一个字节的字符串打印的ASCII的字节字符串解码，但有一个#前缀。在字节字符串不可打印的ASCII字符或非ASCII字节用八进制表示法。

例如：

```scheme
> #"Apple"
#"Apple"

> (bytes-ref #"Apple" 0)
65

> (make-bytes 3 65)
#"AAA"

> (define b (make-bytes 2 0))
> b
#"\0\0"

> (bytes-set! b 0 1)
> (bytes-set! b 1 255)
> b
#"\1\377"
```

字符串和字节字符串之间的显式转换，Racket直接支持三种编码：UTF-8，Latin-1，和当前的本地编码。字节到字节的通用转换器(特别是从UTF-8)弥合了支持任意字符串编码的差异分歧。

例如：

```scheme
> (bytes->string/utf-8 #"\316\273")
"λ"

> (bytes->string/latin-1 #"\316\273")
"Î»"

> (parameterize ([current-locale "C"])  ; C locale supports ASCII,
    (bytes->string/locale #"\316\273")) ; only, so...
bytes->string/locale: byte string is not a valid encoding
for the current locale
  byte string: #"\316\273"
  
> (let ([cvt (bytes-open-converter "cp1253" ; Greek code page
                                   "UTF-8")]
        [dest (make-bytes 2)])
    (bytes-convert cvt #"\353" 0 1 dest)
    (bytes-close-converter cvt)
    (bytes->string/utf-8 dest))
    
"λ"
```

## 符号(Symbol)

符号是一个原子值，它像前面的标识符那样以'前缀打印。一个表达式以'开始并以标识符继续表达式产生一个符号值。

例如：

```scheme
> 'a
'a
> (symbol? 'a)
#t
```

对于任何字符序列，一个相应的符号被保留；调用string->symbol程序，或读入一个语法标识，产生一个保留符号。由于互联网的符号可以方便地用eq?（或这样：eqv?或equal?）进行比较，所以他们作为一个易于使用的标签和枚举值提供。

符号是区分大小写的。通过使用一个#ci前缀或其他方式，在读者保留默认情况下，读者可以将大小写字符序列生成一个符号。

例如：
```scheme
> (eq? 'a 'a)
#t
> (eq? 'a (string->symbol "a"))
#t
> (eq? 'a 'b)
#f
> (eq? 'a 'A)
#f
> #ci'A
'a
```

任何字符串（即，任何字符序列）都可以提供给string->symbol以获得相应的符号。读者输入任何字符都可以直接出现在一个标识符里，除了空白和以下特殊字符：

> ( ) [ ] { } ” , “ ' ` ; # | \

`gensym`和`string->uninterned-symbol`程序产生新的非保留(uninterned)符号，那不等同(比照`eq?`)于任何先前的保留或非保留符号。非保留符号是可用的新标签，不能与任何其它值混淆。

例如：

```scheme
> (define s (gensym))
> s
'g42
> (eq? s 'g42)
#f
> (eq? 'a (string->uninterned-symbol "a"))
#f
```

## 关键字(Keyword)

一个关键字值是类似于一个符号，但它的印刷形式是用前缀#:。

例如：

```scheme
> (string->keyword "apple")
'#:apple

> '#:apple
'#:apple

> (eq? '#:apple (string->keyword "apple"))
#t
```

## 配对(Pair)和列表(List)

一个配对把两个任意值结合。`cons`程序构建配对，`car`和`cdr`程序分别提取配对的第一和第二个成员。`pair?`谓词确认配对。

例如：

```scheme
> (cons 1 2)
'(1 . 2)
> (cons (cons 1 2) 3)
'((1 . 2) . 3)
> (car (cons 1 2))
1
> (cdr (cons 1 2))
2
> (pair? (cons 1 2))
#t
```

列表是创建链表的配对的组合。更确切地说，一个列表要么是空列表`null`，要么是个配对(其第一个元素是列表元素，第二个元素是一个列表)。`list?`谓词识别列表。`null?`谓词识别空列表。

例如：

```scheme
> null
'()
> (cons 0 (cons 1 (cons 2 null)))
'(0 1 2)
> (list? null)
#t
> (list? (cons 1 (cons 2 null)))
#t
> (list? (cons 1 2))
#f
```

当一个列表或配对的一个元素不能写成引用值时，使用`list`或`cons`打印。例如，一个用`srcloc`构建的值不能使用`quote`来写，应该使用`srcloc`来写：

```scheme
> (srcloc "file.rkt" 1 0 1 (+ 4 4))
(srcloc "file.rkt" 1 0 1 8)

> (list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
(list 'here (srcloc "file.rkt" 1 0 1 8) 'there)

> (cons 1 (srcloc "file.rkt" 1 0 1 8))
(cons 1 (srcloc "file.rkt" 1 0 1 8))

> (cons 1 (cons 2 (srcloc "file.rkt" 1 0 1 8)))
(list* 1 2 (srcloc "file.rkt" 1 0 1 8))
```

如最后一个例子所示，list*是用来缩略一系列的不能使用list缩略的cons。

列表中最重要的预定义程序是遍历列表元素的那些程序：

```scheme
> (map (lambda (i) (/ 1 i))
       '(1 2 3))
'(1 1/2 1/3)

> (andmap (lambda (i) (i . < . 3))
         '(1 2 3))
#f

> (ormap (lambda (i) (i . < . 3))
         '(1 2 3))
#t

> (filter (lambda (i) (i . < . 3))
          '(1 2 3))
'(1 2)

> (foldl (lambda (v i) (+ v i))
         10
         '(1 2 3))
16

> (for-each (lambda (i) (display i))
            '(1 2 3))
123

> (member "Keys"
          '("Florida" "Keys" "U.S.A."))
'("Keys" "U.S.A.")

> (assoc 'where
         '((when "3:30") (where "Florida") (who "Mickey")))
'(where "Florida")
```

配对是不可变的（与Lisp传统相反），pair?、list?仅识别不可变的配对和列表。mcons程序创建一个可变的配对，用set-mcar!和set-mcdr!，及mcar和mcdr进行操作。一个可变的配对用mcons打印，而write和display使用{和}打印可变配对：

例如：

```scheme
> (define p (mcons 1 2))
> p
(mcons 1 2)

> (pair? p)
#f

> (mpair? p)
#t

> (set-mcar! p 0)
> p
(mcons 0 2)

> (write p)
{0 . 2}
```

## 向量(Vector)

向量是任意值的固定长度数组。与列表不同，向量支持常量时间访问和元素更新。

向量打印类似列表——作为其元素的括号序列——但向量要在'之后加前缀#，或如果某个元素不能用引号则使用vector表示。

例如：

```scheme
> #("a" "b" "c")
'#("a" "b" "c")

> #(name (that tune))
'#(name (that tune))

> #4(baldwin bruce)
'#(baldwin bruce bruce bruce)

> (vector-ref #("a" "b" "c") 1)
"b"

> (vector-ref #(name (that tune)) 1)
'(that tune)
```

像字符串一样，向量要么是可变的，要么是不可变的，直接作为表达式编写的向量是不可变的。

向量可以通过`vector->list`和`list->vector`转换成列表，反之亦然。这种转换与列表中预定义的程序相结合特别有用。当分配额外的列表似乎太昂贵时，考虑使用像`for/fold`的循环形式，它像列表一样识别向量。

例如：

```scheme
> (list->vector (map string-titlecase
                     (vector->list #("three" "blind" "mice"))))

'#("Three" "Blind" "Mice")
```

## 哈希表(Hash Table)

## 格子(Box)

## 无效值(Void)和未定义值(Undefined)

