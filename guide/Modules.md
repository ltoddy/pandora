- 模块

模块让你把Racket代码组织成多个文件和可重用的库。

- 组织模块

“cake.rkt”和“random-cake.rkt”示例演示如何组织一个程序模块的最常用的方法：把所有的模块文件在一个目录（也许是子目录），然后有模块通过相对路径相互引用。模块目录可以作为一个项目，因为它可以在文件系统上移动或复制到其它机器上，而相对路径则保存模块之间的连接。

另一个例子，如果你正在开发一个糖果分类程序，你可能有一个主要的“sort.rkt”模块，使用其他模块访问糖果数据库和控制分拣机。如果糖果数据库模块本身被组织成子模块以处理条码和厂家信息，那么数据库模块可以是“db/lookup.rkt”，它使用辅助模块“db/barcodes.rkt”和“db/makers.rkt”。同样，分拣机驱动程序“machine/control.rkt“可能会使用辅助模块”machine/sensors.rkt“和“machine/actuators.rkt”。

![](https://docs.racket-lang.org/guide/pict.png)

“sort.rkt”模块使用相对路径“db/lookup.rkt”和“machine/control.rkt”从数据库和机器控制库导入：

"sort.rkt"

```racket
#lang racket
(require "db/lookup.rkt" "machine/control.rkt")
....
```

“db/lookup.rkt”模块类似地使用相对路径给它自己的源码以访问“db/barcodes.rkt”和“db/makers.rkt”模块：

"db/lookup.rkt"

```racket
#lang racket
(require "barcode.rkt" "makers.rkt")
....
``

同上，“machine/control.rkt”：

"machine/control.rkt"

```racket
#lang racket
(require "sensors.rkt" "actuators.rkt")
....
```

Racket工具所有运行都自动使用相对路径。例如，

racket sort.rkt

在命令行运行“sort.rkt”程序和自动加载并编译所需的模块。对于一个足够大的程序，从源码编译可能需要很长时间，所以使用

raco make sort.rkt

编译“sort.rkt”及其所有依赖成为字节码文件。如果字节码文件存在，运行racket sort.rkt，将自动使用字节码文件。

- 库集合

一个集合（collection）是已安装的库模块的按等级划分的组。一个集合中的模块通过一个引号引用，无后缀路径。例如，下面的模块引用“date.rkt”库，它是部分“racket”集合的一部分：

```racket
#lang racket
 
(require racket/date)
 
(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
```

当搜索在线Racket文档时，搜索结果显示提供每个绑定的模块。或者，如果通过单击超链接到达绑定文档，则可以在绑定名称上悬停以查找哪些模块提供了它。

一个模块的引用，像racket/date，看起来像一个标识符，但它并不是和printf或date->string相同的方式对待。相反，当require发现一个被引号包括的模块的引用，它转化这个引用为基于集合的路径：

1. 首先，如果这个引用路径不包含/，那么require自动添加一个“/main”给参考。例如，(require slideshow)相当于(require slideshow/main)。

2. 其次，require隐式添加”.rkt”后缀给路径。

3. 最后，require通过在已安装的集合中搜索路径来决定路径，而不是将路径处理为相对于封闭模块的路径。

作为一个最近似情况，集合作为文件系统目录实现。例如，“racket”集合大多位于Racket安装的“collects”目录中的“racket”目录中，如以下报告：

```racket
#lang racket
 
(require setup/dirs)
 
(build-path (find-collects-dir) ;主集合的目录
            "racket")
```

然而，Racket安装的“collects”目录仅仅是一个require寻找目录集合的地方。其它地方包括用户指定的通过(find-user-collects-dir)报告的目录以及通过PLTCOLLECTS搜索路径配置的目录。最后，最典型的是，通过安装包（packages）找到集合。

- 包和集合

一个包（package）是通过Racket包管理器安装的一组库（或者预先安装在Racket分发中）。例如，racket/gui库是由“gui”包提供的，而parser-tools/lex是由“parser-tools”库提供的。

Racket程序不直接针对包。相反，程序通过集合针对库，添加或删除一个包会改变可用的基于集合的库集。单个包可以为多个集合提供库，两个不同的包可以在同一集合中提供库（但不是同一个库，并且包管理器确保安装的包在该层级不冲突）。

- 添加集合

回顾组织模块部分的糖果排序示例，假设“db/”和“machine/”中的模块需要一组常见的助手函数集。辅助函数可以放在一个“utils/”目录，同时模块“分db/”或“machine/”可以以开始于”../utils/”的相对路径访问公用模块 。只要一组模块在一个项目中协同工作，最好保持相对路径。程序员可以在不知道你的Racket配置的情况下跟踪相关路径的引用。

有些库是用于跨多个项目的，因此将库的源码保存在目录中使用是没有意义的。在这种情况下，最好的选择是添加一个新的集合。有了在一个集合中的库后，它可以通过一个封闭路径引用，就像是包括了Racket发行库的库一样。

你可以通过将文件放置在Racket安装包里或通过(get-collects-search-dirs)报告的一个目录下添加一个新的集合。或者，你可以通过设置PLTCOLLECTS环境变量添加到搜索目录列表。但最好的选择，是添加一个包。

创建包并不意味着您必须注册一个包服务器，或者执行一个将源代码复制到归档格式中的绑定步骤。创建包只意味着使用包管理器将你的库的本地访问作为当前源码位置的集合。

例如，假设你有一个目录“/usr/molly/bakery”，它包含“cake.rkt”模块（来自于本节的开始部分）和其它相关模块。为了使模块可以作为一个“bakery”集合获取，或者

1. 使用raco pkg命令行工具：

> raco pkg install --link /usr/molly/bakery

当所提供的路径包含目录分隔符时，实际上不需要--link标记。

2. 从“File”（文件）菜单使用DrRacket的DrRacket’s Package Manager（包管理器）项。在Do What I Mean面板，点击Browse...（浏览），选择“/usr/molly/bakery”目录，然后单击安装。

后来，(require bakery/cake)从任何模块将从”/usr/molly/bakery/cake.rkt“输入print-cake函数。

默认情况下，你安装的目录的名称既用作包名称，又用作包提供的集合。而且，包管理器通常默认只为当前用户安装，而不是在Racket安装的所有用户。有关更多信息，请参阅《Racket中的包管理》（Package Management in Racket）。

如果打算将库分发给其他人，请仔细选择集合和包名称。集合名称空间是分层的，但顶级集合名是全局的，包名称空间是扁平的。考虑将一次性库放在一些顶级名称，像“bakery”这种标识制造者。在制作烘焙食品库的最终集合时，使用像“bakery”这样的集合名。

在你的库之后被放入一个集合，你仍然可以使用raco make以编译库源，但更好而且更方便的是使用raco setup。raco setup命令取得一个集合名（而不是文件名）并编译集合内所有的库。此外，raco setup可以建立文档，并收集和添加文档到文档的索引，通过集合中的一个“info.rkt”模块做详细说明。有关raco setup的详细信息请看《raco setup：安装管理器》（raco setup: Installation Management）。

- 模块基础知识

每个Racket模块通常驻留在自己的文件中。例如，假设文件“cake.rkt”包含以下模块：

"cake.rkt"

```racket
#lang racket

(provide print-cake)
    
; draws a cake with n candles
(define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
    
(define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline))
```

然后，其他模块可以导入“cake.rkt”以使用print-cake的函数，因为“cake.rkt”的provide行明确导出了print-cake的定义。show函数对"cake.rkt"是私有的（即它不能从其他模块被使用），因为show没有被导出。

下面的“random-cake.rkt”模块导入“cake.rkt”：

"random-cake.rkt"

```racket
#lang racket
    
(require "cake.rkt")

(print-cake (random 30))
```

相对在导入(require "cake.rkt")内的引用“cake.rkt”的运行来说，如果“cake.rkt”和“random-cake.rkt”模块在同一个目录里。UNIX样式的相对路径用于所有平台上的相对模块引用，就像HTML页面中的相对的URL一样。

- module表

一个模块声明的普通写法形式，既可在REPL又可在一个文件中执行的是

```racket
(module name-id initial-module-path
  decl ...)
```

其中的name-id是一个模块名，initial-module-path是一个初始的输入口，每个decl是一个输入口. 输出口. 定义或表达式。在文件的情况下，name-id通常与包含文件的名称相匹配，减去其目录路径或文件扩展名，但在模块通过其文件路径require时name-id被忽略。

initial-module-path是必需的，因为即使是require表必须导入，以便在模块主体中进一步使用。换句话说，initial-module-path导入在主体内可供使用的引导语法。最常用的initial-module-path是racket，它提供了本指南中描述的大部分绑定，包括require. define和provide。另一种常用的initial-module-path是racket/base，它提供了较少的函数，但仍然是大多数最常用的函数和语法。

例如，前面一节里的“cake.rkt”例子可以写为

```racket
(module cake racket
  (provide print-cake)
 
  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
 
  (define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline)))
```

此外，module表可以在REPL中求值以申明一个cake模块，不与任何文件相关联。为指向是这样一个独立模块，这样引用模块名称：

例如：

```racket
> (require 'cake)
> (print-cake 3)

   ...   
 .-|||-. 
 |     | 
---------
```

声明模块不会立即求值这个模块的主体定义和表达式。这个模块必须在顶层明确地被require以来触发求值。在求值被触发一次之后，后续的require不会重新对模块主体求值。

例如：

```racket
> (module hi racket
    (printf "Hello\n"))
> (require 'hi)
Hello

> (require 'hi)
```

- #lang简写

#lang简写的主体没有特定的语法，因为语法是由如下#lang语言名称确定的。

在#lang racket的情况下，语法为：

#lang racket
decl ...

其读作如下同一内容：

```racket
(module name racket
  decl ...)
```

这里的name是来自包含#lang表的文件名称。

#lang racket/base表具有和#lang racket同样的语法，除了普通写法的扩展使用racket/base而不是racket。#lang scribble/manual表相反，有一个完全不同的语法，甚至看起来不像Racket，在这个指南里我们不准备去描述。

除非另有规定，一个模块是一个文档，它作为“语言”使用#lang标记法表示将以和#lang racket同样的方式扩大到module中。文档的语言名也可以直接使用module或require。

- 子模块

一个module表可以被嵌套在一个模块内，在这种情况下，这个嵌套module表声明一个子模块。子模块可以通过外围模块使用一个引用名称直接引用。下面的例子通过从zoo子模块导入tiger打印“Tony”：

"park.rkt"

```racket
#lang racket
    
(module zoo racket
    (provide tiger)
    (define tiger "Tony"))
    
(require 'zoo)
    
tiger
```

运行一个模块不是必须运行子模块。在上面的例子中，运行“park.rkt”运行它的子模块zoo仅因为“park.rkt”模块require了这个zoo子模块。否则，一个模块及其子模块可以独立运行。此外，如果“park.rkt“被编译成字节码文件（通过raco make），那么“park.rkt”代码或zoo代码可以独立下载。

子模块可以嵌套子模块，而且子模块可以被一个模块通过使用子模块路径直接引用，不同于它的外围模块。

 一个module*表类似于一个嵌套的module表：
 
 ```racket
 (module* name-id initial-module-path-or-#f
  decl ...)
 ```

module*表不同于module，它反转这个对于子模块和外围模块的参考的可能性：

1. 用module申明的一个子模块模块可通过其外围模块require，但子模块不能require外围模块或在词法上参考外围模块的绑定。

2. 用module*申明的一个子模块可以require其外围模块，但外围模块不能require子模块。

此外，一个module*表可以在initial-module-path的位置指定#f，在这种情况下，所有外围模块的绑定对子模块可见——包括没有使用provide输出的绑定。

用module*和#f申明的子模块的一个应用是通过子模块输出附加绑定，那不是通常的从模块输出：

"cake.rkt"

```racket
#lang racket
    
(provide print-cake)
    
(define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
    
(define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline))
    
(module* extras #f
    (provide show))
```

在这个修订的“cake.rkt”模块，show不是被一个模块输入，它采用(require "cake.rkt")，因为大部分“cake.rkt“的用户不想要那些额外的函数。一个模块可以要求extra子模块使用(require (submod "cake.rkt" extras))访问另外的隐藏的show函数。

- main和test子模块

下面“cake.rkt”的变体包括一个main子模块，它调用print-cake：

"cake.rkt"

```racket
#lang racket
    
(define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
    
(define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline))
    
(module* main #f
    (print-cake 10))
```

运行一个模块不会运行module*定义的子模块。尽管如此，还是可以通过racket或DrRacket运行上面的模块打印一个带10支蜡烛的蛋糕，因为main子模块是一个特殊情况。

当一个模块作为一个可执行程序的名称提供给racket在DrRacket中直接运行或执行在，如果模块有一个main子模块，main子模块会在其外围模块之后运行。当一个模块直接运行时，声明一个main子模块从而指定额外的行为去被执行，以代替require作为在一个较大程序里的一个库。

一个main子模块不必用module*声明。如果main模块不需要使用其外围模块的绑定，则可以用module声明它。更通常的是，main使用module+声明：

```racket
(module+ name-id
  decl ...)
```

用module+申明的一个子模块就像一个由module*用#f代替initial-module-path申明的模块。此外，多个module+表可以指定相同的子模块名称，在这种情况下，module+表的主体被组合起来以创建一个单独的子模块。

 module+的组合行为对定义一个test子模块是非常有用的，它可以方便地使用raco test运行，用同样的方式main也可以方便地使用racket运行。例如，下面的“physics.rkt”模块输出drop和to-energy函数，它定义了一个test模块支持单元测试：
 
 "physics.rkt"

```racket
#lang racket
(module+ test
    (require rackunit)
    (define ε 1e-10))
    
(provide drop
            to-energy)
    
(define (drop t)
    (* 1/2 9.8 t t))
    
(module+ test
    (check-= (drop 0) 0 ε)
    (check-= (drop 10) 490 ε))
    
(define (to-energy m)
    (* m (expt 299792458.0 2)))
    
(module+ test
    (check-= (to-energy 0) 0 ε)
    (check-= (to-energy 1) 9e+16 1e+15))
```

引入“physics.rkt”到一个更大的程序不会运行drop和to-energy测试——即使引发这个测试代码的加载，如果模块被编译——但在运行raco test physics.rkt的时候会同时运行这个测试。

上述“physics.rkt”模块相当于使用module*：

"physics.rkt"

```racket
#lang racket
    
(provide drop
            to-energy)
    
(define (drop t)
    (* 1/2 49/5 t t))
    
(define (to-energy m)
    (* m (expt 299792458 2)))
    
(module* test #f
    (require rackunit)
    (define ε 1e-10)
    (check-= (drop 0) 0 ε)
    (check-= (drop 10) 490 ε)
    (check-= (to-energy 0) 0 ε)
    (check-= (to-energy 1) 9e+16 1e+15))
```

使用module+代替module*允许测试与函数定义交叉。

module+的组合行为有时对main模块也有帮助。即使组合是不需要的，(module+ main ....)仍是首选，因为它比(module* main #f ....)更具可读性。

- 模块的语法

#lang在一个模块文件的开始，它开始一个对module表的简写，就像'是一种对quote表的简写。不同于'，#lang简写在REPL内不能正常执行，部分是因为它必须由end-of-file终止，也因为#lang的普通写法依赖于封闭文件的名称。

- 模块的路径

模块路径（module path）是对模块的引用，作为require的使用，或者作为module表中的initial-module-path。它可以是几种形式中的任意一种：

1. (quote id)

引用标识符的模块路径指的是使用标识符的非文件module声明。这种模块引用形式做多的场景是在REPL。

例如：

```racket
> (module m racket
    (provide color)
    (define color "blue"))
> (module n racket
    (require 'm)
    (printf "my favorite color is ~a\n" color))
> (require 'n)

my favorite color is blue
```

2. rel-string

字符串模块路径是使用UNIX样式约定的相对路径：/是路径分隔符，..指父目录，.指同一目录。rel-string不必以路径分隔符开始或结束。如果路径没有后缀，“.rkt”会自动添加。

路径是相对于封闭文件，如果有的话，或者是相对于当前目录。（更确切地说，路径是相对于(current-load-relative-directory)的值），这是在加载文件时设置的。

模块基础（Module Basics）使用相对路径显示了示例。

如果一个相对路径以”.ss”后缀结尾，它会被转换成”.rkt”。如果实现引用模块的文件实际上以”.SS”结束，当试图加载文件（但“.rkt”后缀优先）时后缀将被改回来。这种双向转换提供了与Racket旧版本的兼容。

3. id

一个模块的路径是一个引用标识符，引用一个已经安装的库。id约束只包含ASCII字母. ASCII数字. +. -. _和/，/分隔标识符内的路径元素。元素指的是集合（collection）和子集合（sub-collection），而不是目录和子目录。

这种形式的一个例子是racket/date。它是指模块的源是“racket”集合中的“date.rkt”文件，它被安装为Racket的一部分。“.rkt”后缀被自动添加。

这种形式的另一个例子是racket，在初始引入时它通常被使用。路径racket是对racket/main的简写；当一个id没有/，那么/main自动被添加到结尾。因此，racket或racket/main是指其源是“racket”集合里的“main.rkt”文件的模块。

例如：

```racket
> (module m racket
    (require racket/date)
  
    (printf "Today is ~s\n"
            (date->string (seconds->date (current-seconds)))))
> (require 'm)

Today is "Monday, October 30th, 2017"
```

当一个模块的完整路径以”.rkt”结束，如果没有这样的文件存在但有一个“.ss”后缀的文件存在，那么这个“.ss”后缀是是自动替代的。这种转换提供了与旧版本的Racket的兼容。

4. (lib rel-string)

像一个不带引号的标识符的路径，但表示为一个字符串而不是标识符。另外，rel-string可以以一个文件的后缀结束，在这种情况下，“.rkt”不是自动添加。

这种形式的例子包括(lib "racket/date.rkt")和(lib "racket/date")，这是相当于racket/date。其他的例子包括(lib "racket")和(lib "racket/main")，都相当于racket。

例如：

```racket
> (module m (lib "racket")
    (require (lib "racket/date.rkt"))
  
    (printf "Today is ~s\n"
            (date->string (seconds->date (current-seconds)))))
> (require 'm)

Today is "Monday, October 30th, 2017"
```

5. (planet id)

访问通过行星服务器分发的第三方库。首先需要下载库，然后使用本地副本。

id编码了用/分隔的几条信息：包所有者，然后是可选的版本信息的包名，以及一个特定的库与包的可选路径。像id作为一个lib路径的简写，一个".rkt"后缀被自动添加，并且当子路径没有提供时/main用作路径。

例如：

```racket
> (module m (lib "racket")
    ; Use "schematics"'s "random.plt" 1.0, file "random.rkt":
    (require (planet schematics/random:1/random))
    (display (random-gaussian)))
> (require 'm)

0.9050686838895684
```

与其它形式，一个用“.ss”作为文件结尾的实现可以自动取代如果没有用".rkt"执行文件结尾存在。

6. (planet package-string)

就像planet的符号形式，但使用的是字符串而不是标识符。另外，package-string可以一个文件的后缀结束，在这种情况下，“.rkt”不添加。

与其他形式一样，当以”.SS”文件结尾的实现可以自动取代时，如果没有以”.rkt”执行文件结尾存在，“.SS”扩展为“.rkt”。

7. (planet rel-string (user-string pkg-string vers ...))
 
```
vers	 	=	 	nat
 	 	|	 	(nat nat)
 	 	|	 	(= nat)
 	 	|	 	(+ nat)
 	 	|	 	(- nat)
```

从行星服务器访问库的更一般形式。在这种一般形式中，行星引用开始时像一个相对路径的库引用，但路径后面是关于库的生产者. 包和版本的信息。指定的包是按需下载和安装的。

verse在包的可接受版本中指定了一个约束，其中版本号是非负整数序列，约束确定序列中每个元素的允许值。如果没有为特定元素提供约束，则允许任何版本；特别是，省略所有vers意味着任何版本都可以接受。至少指定一个ver用于强烈推荐。

对于版本约束，普通nat与(+ nat)相同，对应于版本号的相应元素的nat或更高的nat。(start-nat end-nat)匹配范围内的任何start-nat到end-nat，包括，一个(= nat)完全匹配nat。一个(- nat)匹配nat或更低。

例如：

```racket
> (module m (lib "racket")
    (require (planet "random.rkt" ("schematics" "random.plt" 1 0)))
    (display (random-gaussian)))
> (require 'm)

0.9050686838895684
```

自动的".ss"和".rkt"转换作为其它形式添加。

8. (file string)

指定一个文件，其string是一个使用当前平台的约定的相对或绝对路径。此表单不可移植，并且不应当使用一个扁平的. 轻便的rel-string满足使用。

自动的".ss"和".rkt"转换作为其它形式添加。

```racket
(submod base element ...+)
 
base	 	=	 	module-path
 	 	|	 	"."
 	 	|	 	".."
 	 	 	 	 
element	 	=	 	id
 	 	|	 	".."
```

是指一个base子模块。elements序列在submod指定了一个子模块名称的路径以到达最终的子模块之间。

例如：

```racket
> (module zoo racket
    (module monkey-house racket
      (provide monkey)
      (define monkey "Curious George")))
> (require (submod 'zoo monkey-house))
> monkey

"Curious George"
```

使用“.”作为base在submod代表的外围模块之间。使用“..”作为base相当于使用“.”后跟一个额外的“..”。当一个路径的表(quote id)是指一个子模块，它相当于(submod "." id)。

使用“..”作为一种element取消一个子模块的步骤，有效指定外围模块。例如，(submod "..")是指封闭的子模块的模块，路径出现在其中。

例如：

```racket
> (module zoo racket
    (module monkey-house racket
      (provide monkey)
      (define monkey "Curious George"))
    (module crocodile-house racket
      (require (submod ".." monkey-house))
      (provide dinner)
      (define dinner monkey)))
> (require (submod 'zoo crocodile-house))
> dinner

"Curious George"
```

- 导入：require

从另一个模块导入require表。一个require表可以出现在一个模块中，在这种情况下，它将指定模块的绑定引入到导入的模块中。一个require表也可以出现在顶层，在这种情况下，既导入绑定也实例化指定的模块；即，它对指定模块的主体和表达式求值，如果他们还没有被求值。

单个的require可以同时指定多个导入：

```racket
(require require-spec ...)
```

在一个单一的require表里指定多个require-spec，从本质上与使用多个require，每个单独包含一个单一的require-spec是相同的。区别很小，且局限于顶层：一个独立的require可以导入一个给定标识符最多一次，而一个单独的require可以代替以前require的绑定（都是只局限于顶层，在一个模块之外）。

require-spec的允许形态是递归定义的：

1. module-path

在最简单的形式中，require-spec是一个module-path（如前一节《模块路径》（Module Paths）中定义的）。在这种情况下，require所引入的绑定通过provide声明来确定，其中在每个模块通过各个module-path引用。

例如：

```racket
> (module m racket
    (provide color)
    (define color "blue"))
> (module n racket
    (provide size)
    (define size 17))
> (require 'm 'n)
> (list color size)

'("blue" 17)
```
2. 

```racket
(only-in require-spec id-maybe-renamed ...)
 
id-maybe-renamed	 	=	 	id
 	 	|	 	[orig-id bind-id]
```

一个only-in表限制绑定设置，它将通过require-spec引入。此外，only-in选择重命名每个绑定，它被保护：在[orig-id bind-id]表里，orig-id是指一个被require-spec隐含的绑定，并且bind-id是这个在导入上下文中将被绑定的名称，以代替orig-id。

例如：

```racket
> (module m (lib "racket")
    (provide tastes-great?
             less-filling?)
    (define tastes-great? #t)
    (define less-filling? #t))
> (require (only-in 'm tastes-great?))
> tastes-great?

#t
> less-filling?
less-filling?: undefined;
 cannot reference undefined identifier
 
> (require (only-in 'm [less-filling? lite?]))
> lite?
#t
```

3. `(except-in require-spec id ...)`

这种形式是only-in的补充：它从以require-spec指定的集合中排除指定的绑定。

4. `(rename-in require-spec [orig-id bind-id] ...)`

这种形式支持类似于only-in的重命名，但从require-spec中分离单独的标识符，它们没有作为一个orig-id提交。

5.`(prefix-in prefix-id require-spec)`

这是一个重命名的简写，prefix-id添加到用require-spec指定的每个标识符的前面。

除了only-in. except-in. rename-in和prefix-in表可以嵌套以实现更复杂的导入绑定操作。例如,

```racket
(require (prefix-in m: (except-in 'm ghost)))
```

导入m输出的所有绑定，除ghost绑定之外，并带用m前缀的局部名字：

等价地，prefix-in可以被应用在except-in之前，只是带except-in的省略是用m前缀指定：

```racket
(require (except-in (prefix-in m: 'm) m:ghost))
```

- 输出：provide

默认情况下，一个模块的所有定义对模块都是私有的。provide表指定定义，以使在模块require的地方可获取。

```racket
(provide provide-spec ...)
```

一个provide表只能出现在模块级（即一个module的当前主体）中。在一个单一的provide中指定多个provide-spec，那和使用多个provide，其每一个有单一的provide-spec，明显是一样的。

每个标识符最多可以从模块中导出一次，遍及模块中的所有provide。更确切地说，每个导出的外部名称必须是不同的；相同的内部绑定可以用不同的外部名称多次导出。

允许的provide-spec形式是递归定义的：

1. identifier

在最简单的形式中，provide-spec标明一个绑定，它在被导出的模块内。绑定可以来自于局部定义，也可以来自于一个导入。

```racket
(rename-out [orig-id export-id] ...)
```

一个rename-out表类似于只指定一个标识符，但这个导出绑定orig-id是给定一个不同的名称，export-id，给导入模块。

2. `(struct-out struct-id)`

一个结构表导出由(struct struct-id ....)创建的绑定。

3. `(all-defined-out)`

all-defined-out简写导出所有的绑定，其定义在导出模块中（与导入相反）。

all-defined-out简写的使用通常是被阻止的，因为它不太清楚模块的实际导出，而且因为Racket程序员习惯于认为可以自由地将定义添加到模块，而不影响其公共接口（在all-defined-out被使用时候都不是这样）。

4. `(all-from-out module-path)`

all-from-out简写输出模块中的所有绑定，该模块使用一个基于module-path的require-spec导入。

尽管不同的module-path可以引用同一个基于文件的模块，但是带all-from-out的重复导出是明确基于module-path引用，而不是实际引用的模块。

5. `(except-out provide-spec id ...)`

就像provide-spec，但省略每个id的导出，其中id是要省略的绑定的外部名称。

6. `(prefix-out prefix-id provide-spec)`

就像provide-spec，但为每个导出的绑定添加prefix-id到外部名称的开头。

- 赋值和重定义

在一个模块的变量定义上的set!使用，仅限于定义模块的主体。也就是说，一个模块可以改变它自己定义的值，这样的变化对于导入模块是可见的。但是，一个导入上下文不允许更改导入绑定的值。

例如：

```racket
> (module m racket
    (provide counter increment!)
    (define counter 0)
    (define (increment!)
      (set! counter (add1 counter))))
> (require 'm)
> counter
0

> (increment!)
> counter
1

> (set! counter -1)
set!: cannot mutate module-required identifier
  in: counter
```

在上述例子中，一个模块可以给别人提供一个改变其输出的能力，通过提供一个修改函数实现，如increment!。

禁止导入变量的分配有助于支持程序的模块化推理。例如，在模块中，

```racket
(module m racket
  (provide rx:fish fishy-string?)
  (define rx:fish #rx"fish")
  (define (fishy-string? s)
    (regexp-match? rx:fish s)))
```

fishy-string?函数将始终匹配包含“fish”的字符串，不管其它模块如何使用rx:fish绑定。从本质上来说，它帮助程序员的原因是，禁止对导入的赋值也允许更有效地执行许多程序。

在同一行中，当一个模块不包含set!，在模块中定义的特定标识符，那该标识符被认为是一个常数（constant），不能更改——即使重新声明该模块。

因此，通常不允许重新声明模块。对于基于文件的模块，简单地更改文件不会导致任何情况下的重新声明，因为基于文件的模块是按需加载的，而先前加载的声明满足将来的请求。它可以使用Racket的反射支持重新声明一个模块，而非文件模块可以重新在REPL中声明；在这种情况下，如果涉及一个以前的静态绑定，重新申明可能失败。

```racket
> (module m racket
    (define pie 3.141597))
> (require 'm)
> (module m racket
    (define pie 3))

define-values: assignment disallowed;
 cannot re-define a constant
  constant: pie
  in module: 'm

作为探索和调试目的，Racket反射层提供了一个compile-enforce-module-constants参数来使常量的执行无效。

> (compile-enforce-module-constants #f)
> (module m2 racket
    (provide pie)
    (define pie 3.141597))
> (require 'm2)
> (module m2 racket
    (provide pie)
    (define pie 3))
> (compile-enforce-module-constants #t)
> pie

3
```










