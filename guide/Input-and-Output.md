- 输入和输出

Racket端口对应Unix中流的概念

一个Racket端口代表一个数据源或数据池，例如一个文件、一个终端、一个TCP连接或者一个内存中字符串。
端口提供顺序的访问，在那里数据能够被分批次地读或写，而不需要数据被一次性接受或生成。
更具体地，一个输入端口（input port）代表一个程序能从中读取数据的源，一个输出端口（output port）代表一个程序能够向其中输出的数据池。

- 端口的种类

不同的函数可创建不同类型的端口，这里有一些例子：

文件：函数open-output-file打开一个可供写入的文件，而open-input-file打开一文件以读取其内容。

例子：

```racket
> (define out (open-output-file "data"))
> (display "hello" out)
> (close-output-port out)
> (define in (open-input-file "data"))
> (read-line in)
"hello"
> (close-input-port in)
```

如果一个文件已经存在，open-output-file的默认行为是抛出一个异常。
提供了选项如#:exists 'truncate 或 #:exists 'update来重写或更新文件。

例子：

```racket
> (define out (open-output-file "data" #:exists 'truncate))
> (display "howdy" out)
> (close-output-port out)
```

作为不得不将打开/关闭函数调用匹配起来的替代，
绝大多数Racket程序员会使用call-with-input-file和call-with-output-file，
接收一个函数作为参数以执行希望的操作。这个函数仅获取端口参数，操作将自动打开及关闭（端口）。

例子：

```racket
> (call-with-output-file "data"
                          #:exists 'truncate
                          (lambda (out)
                            (display "hello" out)))
> (call-with-input-file "data"
                        (lambda (in)
                          (read-line in)))
"hello"
```

字符串：open-output-string创建一个将数据堆入字符串的端口，
get-output-string将累积而成的字符串解压。
open-input-string创建一个用于从字符串读取的端口。

例子:

```racket
> (define p (open-output-string))
> (display "hello" p)
> (get-output-string p)
"hello"
> (read-line (open-input-string "goodbye\nfarewell"))
"goodbye"
```

TCP连接：tcp-connect为客户端的TCP通信创建了输入与输出端口。
tcp-listen创建了经由tcp-accept接收连接的服务器。

例子：

```racket
> (define server (tcp-listen 12345))
> (define-values (c-in c-out) (tcp-connect "localhost" 12345))
> (define-values (s-in s-out) (tcp-accept server))
> (display "hello\n" c-out)
> (close-output-port c-out)
> (read-line s-in)
"hello"
> (read-line s-in)
#<eof>
```

进程管道：subprocess启动一操作系统级进程并返回与对应子进程stdin/stdout/stderr的端口。
（这三种端口是连接到子进程的确定已存在的端口，不需要创建。）

例子：

```racket
> (define-values (p stdout stdin stderr)
    (subprocess #f #f #f "/usr/bin/wc" "-w"))
> (display "a b c\n" stdin)
> (close-output-port stdin)
> (read-line stdout)
"       3"
> (close-input-port stdout)
> (close-input-port stderr)
```

内部管道：make-pipe返回两个端口代表一个管道的双端。这种类型的管道属于Racket内部，
与用于不同进程间通信的系统级管道无关。

例子：

```racket
> (define-values (in out) (make-pipe))
> (display "garbage" out)
> (close-output-port out)
> (read-line in)
"garbage"
```

- 默认端口

对于大多数简单IO函数，目标端口是一可选参数，默认值为当前的输入/输出端口。
此外，错误信息被写入当前错误端口，这也是一个输出端口。
current-input-port, current-output-port和current-error-port返回当前相关端口。

例子：

```racket
> (display "Hi")
Hi
> (display "Hi" (current-output-port)) ; the same
Hi
```

如果你通过终端打开racket程序，当前输入、输出、错误端口会连接至终端。
更一般地，它们会连接到系统级的输入、输出、错误。在本指引中，例子将输出以紫色显示，
错误信息以红色斜体显示。

例子：

```racket
(define (swing-hammer)
  (display "Ouch!" (current-error-port)))
 
> (swing-hammer)
Ouch!
```

当前端口这类函数实际上是参数，代表它们的值能够通过parameterize设置。

查看 动态绑定：parameterize 一节以获取参数的介绍

例子：

```racket
> (let ([s (open-output-string)])
    (parameterize ([current-error-port s])
      (swing-hammer)
      (swing-hammer)
      (swing-hammer))
    (get-output-string s))
"Ouch!Ouch!Ouch!"
```

- 读和写Racket数据

就像在内建数据类型中提到的，Racket提供三种方式打印内建值类型的实例：

print, 以在REPL环境下的结果打印其值
write, 以在输出上调用read反向产生打印值
display, 缩小待输出值，至少对以字符/字节为主的数据类型，仅保留字符/字节部分，
否则行为等同于write

这里有一些例子：

```racket
> (print 1/2)
1/2
> (print #\x)
#\x
> (print "hello")
"hello"
> (print #"goodbye")
#"goodbye"
> (print '|pea pod|)
'|pea pod|
> (print '("i" pod))
'("i" pod)
> (print write)
#<procedure:write>
 
> (write 1/2)
1/2
> (write #\x)
#\x
> (write "hello")
"hello"
> (write #"goodbye")
#"goodbye"
> (write '|pea pod|)
|pea pod|
> (write '("i" pod))
("i" pod)
> (write write)
#<procedure:write>
 
> (display 1/2)
1/2
> (display #\x)
x
> (display "hello")
hello
> (display #"goodbye")
goodbye
> (display '|pea pod|)
pea pod
> (display '("i" pod))
(i pod)
> (display write)
#<procedure:write>
```

总的来说，print对应Racket语法的表达层，write对应阅读层，display大致对应字符层。

printf支持数据与文本的简单格式。在printf支持的格式字符串中，~a display下一个参数，
~s write下一个参数，而~v print下一个参数。

例子：

```racket
(define (deliver who when what)
  (printf "Items ~a for shopper ~s: ~v" who when what))
 
> (deliver '("list") '("John") '("milk"))
Items (list) for shopper ("John"): '("milk")
```

使用write后，与display和read不同的是，许多类型的数据可以经由read重新读入。
相同类型经print处理的值也能够被read解析，但是结果包含额外的引号形式，
因为经print后的形式会被以表达式形式读入。

例子：

```racket
> (define-values (in out) (make-pipe))
> (write "hello" out)
> (read in)
"hello"
> (write '("alphabet" soup) out)
> (read in)
'("alphabet" soup)
> (write #hash((a . "apple") (b . "banana")) out)
> (read in)
'#hash((a . "apple") (b . "banana"))
> (print '("alphabet" soup) out)
> (read in)
''("alphabet" soup)
> (display '("alphabet" soup) out)
> (read in)
'(alphabet soup)
```

- 字符类型和序列化

prefab类型（查看预置结构类型章节）自动支持序列化：它们可被写入输出流，
其副本可被由输入流读入：

```racket
> (define-values (in out) (make-pipe))
> (write #s(sprout bean) out)
> (read in)
'#s(sprout bean)
```

使用struct创建的其它结构类型，提供较prefab类型更多的抽象，通常使用#<....>记号
（对于不透明结构类型）或使用#(....)矢量记号（对于透明结构类型）作为输出。
两种的输出结果都不能以结构类型反向读入。

```racket
> (struct posn (x y))
> (write (posn 1 2))
#<posn>
> (define-values (in out) (make-pipe))
> (write (posn 1 2) out)
> (read in)
UNKNOWN::0: read: bad syntax `#<'
> (struct posn (x y) #:transparent)
> (write (posn 1 2))
#(struct:posn 1 2)
> (define-values (in out) (make-pipe))
> (write (posn 1 2) out)
> (define v (read in))
> v
'#(struct:posn 1 2)
> (posn? v)
#f
> (vector? v)
#t
```

serializable-struct定义了能够被序列化为值，可供write/read写入或存储的类型。
序列化的结果可被反序列化为原始结构类的实例。序列化形式与函数经由racket/serialize提供。

例子：

```racket
> (require racket/serialize)
> (serializable-struct posn (x y) #:transparent)
> (deserialize (serialize (posn 1 2)))
(posn 1 2)
> (write (serialize (posn 1 2)))
((3) 1 ((#f . deserialize-info:posn-v0)) 0 () () (0 1 2))
> (define-values (in out) (make-pipe))
> (write (serialize (posn 1 2)) out)
> (deserialize (read in))
(posn 1 2)
```

除了struct绑定的名字外，serializable-struct绑定具有反序列化信息的标识符，
会自动由模块上下文提供反序列化标识符。当值被反序列化时，反序列化标识符会经由反射被访问。

- 字节、字符和编码

类似read-line, read, display, write这样的函数的工作以字符为单位（对应Unicode标量值）。
概念上来说，它们经由read-char与write-char实现。

更初级一点，端口读写字节而非字符。read-byte与write-byte读写原始字节。
其它函数，例如read-bytes-line，建立在字节操作而非字符操作。

事实上，read-char和write-char函数概念上由read-byte和write-byte实现。
当一个字节的值小于128，它将对应于一个ASCII字符。任何其它的字节会被视为UTF-8序列的一部分，
而UTF-8则是字节形式编码Unicode标量值的标准方式之一（具有将ASCII字符原样映射的优点）。
此外，一个单次的read-char可能会调用多次read-byte，一个标准的write-char可能生成多个输出字节。

read-char和write-char操作总使用UTF-8编码。如果你有不同编码的文本流，或想以其它编码生成文本流，
使用reencode-input-port或reencode-output-port。reencode-input-port将一种你指定编码的
输入流转换为UTF-8流；以这种方式，read-char能够察觉UTF-8编码，即使原始编码并非如此。
应当注意，read-byte也看到重编码后的数据，而非原始字节流。

- IO 模式

如果你想处理文件中独立的各行，你可以使用in-lines：

```racket
> (define (upcase-all in)
    (for ([l (in-lines in)])
      (display (string-upcase l))
      (newline)))
> (upcase-all (open-input-string
               (string-append
                "Hello, World!\n"
                "Can you hear me, now?")))
HELLO, WORLD!
CAN YOU HEAR ME, NOW?
```

如果你想确定“hello”是否在文件中存在，你可以搜索独立各行，
但是更简便的方法是对流应用一正则表达式（查看正则表达式章节）：

```racket
> (define (has-hello? in)
    (regexp-match? #rx"hello" in))
> (has-hello? (open-input-string "hello"))
#t
> (has-hello? (open-input-string "goodbye"))
#f
```

如果你想将一个端口拷贝至另一个，使用来自racket/port的copy-port，它能够在很多的数据可用时有效传输大的块，也能够在小的块全部就绪时立刻传输：

```racket
> (define o (open-output-string))
> (copy-port (open-input-string "broom") o)
> (get-output-string o)
"broom"
```
