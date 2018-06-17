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
## 符号(Symbol)
## 关键字(Keyword)
## 配对(Pair)和列表(List)
## 向量(Vector)
## 哈希表(Hash Table)
## 格子(Box)
## 无效值(Void)和未定义值(Undefined)

