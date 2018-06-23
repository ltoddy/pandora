- 写regexp模式

一个字符串或字节字符串可以直接用作一个正则表达式模式，也可以#rx形成字面上的正则表达式值。
例如，#rx"abc"是一个基于正则表达式值的字符串，并且#rx#"abc"是一个基于正则表达式值的字节字符串。
或者，一个字符串或字节字符串可以以#px做前缀，如在#px"abc"中一样，稍微扩展字符串中模式的语法。

在一个正则表达式模式的大多数角色都是相匹配的文本字符串中出现的自己。
因此，该模式#rx"abc"匹配在演替中的一个字符串中包含的字符a、b和C。
其它角色扮演的元字符（metacharacters）和字符序列作为元序列（metasequences）。
也就是说，它们指定的东西不是字面上的自我。例如，在模式#rx"a.c"中，字符a和C代表它们自己，
但元字符.可以匹配任何字符。因此，该模式#rx"a.c"在演替里匹配一个a,任意字符,和C。

如果我们需要匹配字符.本身，我们可以在它前面加上一个\。字符序列\.结果就是一个元序列，
因为它不匹配它本身而只是.。所以，在演替里匹配a、.和C，我们使用正则表达式模式#rx"a\\.c"。
C”；双\字符是一个Racket字符串神器，它不是正则表达式模式自己的。

正则表达式函数接受一个字符串或字节字符串并产生一个正则表达式的值。
当你使用正则表达式构建模式以匹配多个字符串，因为一个模式在它可以被使用在一个匹配之前被编译成了
一个正则表达式的值。这个pregexp函数就像regexp，但使用扩展语法。
正则表达式值做为#rx或#px的字面形式，被编译一次，尽管当它们可读时。

regexp-quote函数接受任意的字符串并返回一个模式匹配原始字符串。
特别是，在输入字符串中的字符，可以作为正则表达式元字符用一个反斜杠转义，
所以只有它们自己使他们安全地匹配。

```racket
> (regexp-quote "cons")

"cons"
> (regexp-quote "list?")

"list\\?"
```

regexp-quote函数在从一个混合的正则表达式字符串和字面的字符串构建一个完整的正则表达式是有用的。

- 匹配正则表达式模式

regexp-match-positions函数接受一个正则表达模式和一个文本字符串，
如果正则表达式匹配（某部分）文本字符串则返回一个匹配，或如果正则表达式不匹配字符串则返回#f。
成功匹配生成一个索引对列表。

例如：

```racket
> (regexp-match-positions #rx"brain" "bird")

#f
> (regexp-match-positions #rx"needle" "hay needle stack")

'((4 . 10))
```

在第二个例子中，整数4和10确定的子字符串匹配。4是起始（含）索引，和10是结束（唯一的）
匹配的子字符串的索引：

```racket
> (substring "hay needle stack" 4 10)

"needle"
```

第一个例子中，regexp-match-positions的返回列表只包含一个索引对，
和这索引对代表由正则表达式匹配整个字符串。当我们论述子模式后，
我们将看到一个匹配操作可以产生一个列表的子匹配（submatche）。

regexp-match-positions函数需要可选第三和第四个参数指定的文本字符串的匹配应该发生的指标。

```racket
> (regexp-match-positions
    #rx"needle"
    "his needle stack -- my needle stack -- her needle stack"
    20 39)

'((23 . 29))
```

注意，返回的索引仍然与全文字符串相对应。

regexp-match函数类似于regexp-match-positions，但它不是返回索引对，它返回匹配的子字符串：

```racket
> (regexp-match #rx"brain" "bird")

#f
> (regexp-match #rx"needle" "hay needle stack")

'("needle")
```

当regexp-match使用字节字符串表达式，结果是一个匹配的字节串：

```racket
> (regexp-match #rx#"needle" #"hay needle stack")

'(#"needle")
```

如果在端口中有数据，则无需首先将其读取到字符串中。像regexp-match函数可以直接匹配端口：

```racket
> (define-values (i o) (make-pipe))
> (write "hay needle stack" o)
> (close-output-port o)
> (regexp-match #rx#"needle" i)

'(#"needle")
```

regexp-match?函数类似于regexp-match-positions，但只简单地返回一个布尔值，
以指示是否匹配成功：

```racket
> (regexp-match? #rx"brain" "bird")

#f
> (regexp-match? #rx"needle" "hay needle stack")

#t
```

regexp-split函数有两个参数，一个正则表达式模式和一个文本字符串，
并返回一个文本字符串的子串列表；这个模式识别分隔子字符串的分隔符。

```racket
> (regexp-split #rx":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")

'("/bin" "/usr/bin" "/usr/bin/X11" "/usr/local/bin")
> (regexp-split #rx" " "pea soup")

'("pea" "soup")
```

如果第一个参数匹配空字符串，那么返回所有的单个字符的子字符串列表。

```racket
> (regexp-split #rx"" "smithereens")

'("" "s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s" "")
```

因此，确定一个或多个空格作为分隔符，请注意使用正则表达式#rx” +“，而不是#rx” *”。

```racket
> (regexp-split #rx" +" "split pea     soup")

'("split" "pea" "soup")
> (regexp-split #rx" *" "split pea     soup")

'("" "s" "p" "l" "i" "t" "" "p" "e" "a" "" "s" "o" "u" "p" "")
```

regexp-replace函数用另一个字符串替换文本字符串匹配的部分。
第一个参数是模式，第二个参数是文本字符串，第三个参数是要插入的字符串，
或者一个将匹配转换为插入字符串的过程。

```racket
> (regexp-replace #rx"te" "liberte" "ty")

"liberty"
> (regexp-replace #rx"." "racket" string-upcase)

"Racket"
```

如果该模式没有出现在这个文本字符串中，返回的字符串与文本字符串相同。

regexp-replace*函数用插入字符串替换文本字符串中的所有匹配：

```racket
> (regexp-replace* #rx"te" "liberte egalite fraternite" "ty")

"liberty egality fratyrnity"
> (regexp-replace* #rx"[ds]" "drracket" string-upcase)

"Drracket"
```

- 基本申明

论断（assertions）^和$分别标识文本字符串的开头和结尾，
它们确保对它们临近的一个或其它文本字符串的结束正则表达式匹配：

```racket
> (regexp-match-positions #rx"^contact" "first contact")

#f
```

以上正则表达式匹配失败是因为contact没有出现在文本字符串的开始。在.

```racket
> (regexp-match-positions #rx"laugh$" "laugh laugh laugh laugh")

'((18 . 23))
```

中，正则表达式匹配的最后的laugh。

元序列\b简称一个字的范围存在，但这元序列只能与#px语法一起工作。在

```racket
> (regexp-match-positions #px"yack\\b" "yackety yack")

'((8 . 12))
```

在yackety的yack不在字边界结束，所以不匹配。第二yack在字边界结束，所以匹配。

元序列\B（也只有#px）对\b有相反的影响；它断言字边界不存在。在

```racket
> (regexp-match-positions #px"an\\B" "an analysis")

'((3 . 5))
```

an不在单词边界结束，是匹配的。

- 常用的字符类

在#px语法里，一些标准的字符类可以方便地表示为元序列以代替明确的括号内的表达式：
\d匹配一个数字（与[0-9]同样）；\s匹配一个ASCII空白字符；
而\w匹配一个可以是“字（word）”的一部分的字符。

这些元序列的大写版本代表相应的字符类的反转：\D匹配一个非数字，\S匹配一个非空格字符，
而\W匹配一个非“字”字符。

在把这些元序列放进一个Racket字符串里时，记得要包含一个双反斜杠：

```racket
> (regexp-match #px"\\d\\d"
   "0 dear, 1 have 2 read catch 22 before 9")

'("22")
```

这些字符类可以在括号表达式中使用。比如，#px"[a-z\\d]"匹配一个小写字母或数字。

- POSIX字符类

POSIX（可移植性操作系统接口）字符类是一种特殊形式的形如[:...:]的元序列，
它只能用在#px语法中的一个括号表达式内。POSIX类支持

[:alnum:] — ASCII字母和数字

[:alpha:] — ASCII字母

[:ascii:] — ASCII字符

[:blank:] — ASCII等宽的空格：空格和tab

[:cntrl:] — “控制”字符：ASCII 0到32

[:digit:] — ASCII数字，像\d

[:graph:] — ASCII图形字符

[:lower:] — ASCII小写字母

[:print:] — ASCII图形字符加等宽空白

[:space:] — ASCII空白，像\s

[:upper:] — ASCII大写字母

[:word:] — ASCII字母和_，像\w

[:xdigit:] — ASCII十六进制数字

例如，在#px"[[:alpha:]_]"匹配一个字母或下划线。

```racket
> (regexp-match #px"[[:alpha:]_]" "--x--")

'("x")
> (regexp-match #px"[[:alpha:]_]" "--_--")

'("_")
> (regexp-match #px"[[:alpha:]_]" "--:--")

#f
```

POSIX类符号只适用于在括号表达式内。例如，[:alpha:]，当不在括号表达式内时，
不会被当做字母类读取。确切地说，它是（从以前的原则）包含字符:、a、l、p、h的字符类。

```racket
> (regexp-match #px"[:alpha:]" "--a--")

'("a")
> (regexp-match #px"[:alpha:]" "--x--")

#f
```

- 字符和字符类

通常，在正则表达式中的字符匹配相同文本字符串中的字符。
有时使用正则表达式元序列引用单个字符是有必要的或方便的。例如，元序列\.匹配句点字符。

元字符.匹配任意字符：

```racket
> (regexp-match #rx"p.t" "pet")

'("pet")
```

上面的模式也匹配pat、pit、pot、put和p8t，但不匹配peat或pfffft。

字符类（character class）匹配来自于一组字符中的任何一个字符。一个典型的格式，
这是括号字符类（bracketed character class）[...]，
它匹配任何一个来自包含在括号内的非空序列的字符。
因此，#rx"p[aeiou]t"匹配pat、pet、pit、pot、put，别的都不匹配。

在括号内，一个-介于两个字符之间指定字符之间的Unicode范围。
例如，#rx"ta[b-dgn-p]"匹配tab、tac、ad、tag、tan、tao和tap。

在左括号后一个初始的^将通过剩下的内容反转指定的集合；也就是说，
它指定识别在括号内字符集以外的字符集。例如，#rx"do[^g]"匹配所有以do开始但不是dog的三字符序列。

注意括号内的元字符^，它在括号里边的意义与在外边的意义截然不同。
大多数其它的元字符（.、*、+、？等等）当在括号内的时候不再是元字符，
即使你一直不予承认以求得内心平静。一个-是一个元字符，
仅当它在括号内并且当它既不是括号之间的第一个字符也不是最后一个字符时。

括号内的字符类不能包含其它括号字符类（虽然它们包含字符类的某些其它类型，见下）。
因此，在一个括号内的字符类里的一个[不必是一个元字符；它可以代表自身。
比如，#rx"[a[b]"匹配a、[和b。

此外，由于空括号字符类是不允许的，一个]在开左括号后立即出现也不比是一个元字符。
比如，#rx"[]ab]"匹配]、a和b。

- 量词

量词（quantifier）*、+和?分别匹配：前面的子模式的零个或多个，一个或多个以及零个或一个实例。

```racket
> (regexp-match-positions #rx"c[ad]*r" "cadaddadddr")

'((0 . 11))
> (regexp-match-positions #rx"c[ad]*r" "cr")

'((0 . 2))
> (regexp-match-positions #rx"c[ad]+r" "cadaddadddr")

'((0 . 11))
> (regexp-match-positions #rx"c[ad]+r" "cr")

#f
> (regexp-match-positions #rx"c[ad]?r" "cadaddadddr")

#f
> (regexp-match-positions #rx"c[ad]?r" "cr")

'((0 . 2))
> (regexp-match-positions #rx"c[ad]?r" "car")

'((0 . 3))
```

在#px语法里，你可以使用括号指定比*、+、?更精细的调整量：

1、量词{m}精确匹配前面子模式的m实例；m必须是一个非负整数。

2、量词{m,n}匹配至少m并至多n个实例。m和n是非负整数，m小于或等于n。
你可以省略一个或两个都省略，在这种情况下m默认为0，n到无穷大。

很明显，+和?是{1,}和{0,1}的缩写，*是{，}的缩写，和{0,}一样。

```racket
> (regexp-match #px"[aeiou]{3}" "vacuous")

'("uou")
> (regexp-match #px"[aeiou]{3}" "evolve")

#f
> (regexp-match #px"[aeiou]{2,3}" "evolve")

#f
> (regexp-match #px"[aeiou]{2,3}" "zeugma")

'("eu")
```

迄今为止所描述的量词都是贪婪的：它们匹配最大的实例数，还会导致对整个模式的总体匹配。

```racket
> (regexp-match #rx"<.*>" "<tag1> <tag2> <tag3>")

'("<tag1> <tag2> <tag3>")

为了使这些量词为非贪婪，给它们追加?。非贪婪量词匹配满足需要的最小实例数，以确保整体匹配。

> (regexp-match #rx"<.*?>" "<tag1> <tag2> <tag3>")

'("<tag1>")
```

非贪婪量词分别为：`*?、+?、??、{m}?、{m，n}?`。注意匹配字符?的这两种使用。

- 聚类

聚类（Cluster）——文内的括号(...)——确定封闭模式作为一个单一的实体。
它使匹配去捕获匹配项，或字符串的一部分匹配子模式，除了整体匹配之外：

```racket
> (regexp-match #rx"([a-z]+) ([0-9]+), ([0-9]+)" "jan 1, 1970")

'("jan 1, 1970" "jan" "1" "1970")
```

聚类也导致以下量词对待整个封闭的模式作为一个实体：

```racket
> (regexp-match #rx"(pu )*" "pu pu platter")

'("pu pu " "pu ")
```

返回的匹配项数量总是等于指定的正则表达式子模式的数量，
即使一个特定的子模式恰好匹配多个子字符串或根本没有子串。

```racket
> (regexp-match #rx"([a-z ]+;)*" "lather; rinse; repeat;")

'("lather; rinse; repeat;" " repeat;")
```

在这里，*量化子模式匹配的三次，但这是返回的最后一个匹配项。

对一个量化的模式来说不匹配也是可能的，甚至是对整个模式匹配。
在这种情况下，失败的匹配项通过#f体现。

```racket
> (define date-re
    ; match ‘month year' or ‘month day, year';
    ; subpattern matches day, if present
    #rx"([a-z]+) +([0-9]+,)? *([0-9]+)")
> (regexp-match date-re "jan 1, 1970")

'("jan 1, 1970" "jan" "1," "1970")
> (regexp-match date-re "jan 1970")

'("jan 1970" "jan" #f "1970")
```

- 后向引用

子匹配可用于插入字符串参数的regexp-replace和regexp-replace*程序。
插入的字符串可以使用\n为后向引用（backreference）返回第n个匹配项，这是子字符串，
它匹配第n个子模式。一个\0引用整个匹配，它也可以指定为\&。

```racket
> (regexp-replace #rx"_(.+?)_"
    "the _nina_, the _pinta_, and the _santa maria_"
    "*\\1*")

"the *nina*, the _pinta_, and the _santa maria_"
> (regexp-replace* #rx"_(.+?)_"
    "the _nina_, the _pinta_, and the _santa maria_"
    "*\\1*")

"the *nina*, the *pinta*, and the *santa maria*"
> (regexp-replace #px"(\\S+) (\\S+) (\\S+)"
    "eat to live"
    "\\3 \\2 \\1")

"live to eat"
```

使用\\在插入字符串指定转义符。同时，\$代表空字符串，
并且对从紧随其后的数字分离后向引用\n是有用的。

反向引用也可以用在#px模式以返回模式中的一个已经匹配的子模式。\n代表第n个子匹配的精确重复。
注意这个\0、在插入字符串是有用的，在regexp模式内没有道理，
因为整个正则表达式不匹配而无法回到它。

```racket
> (regexp-match #px"([a-z]+) and \\1"
                "billions and billions")

'("billions and billions" "billions")
```

注意，后向引用不是简单地重复以前的子模式。而这是一个特别的被子模式所匹配的子串的重复 。

在上面的例子中，后向引用只能匹配billions。它不会匹配millions，
即使子模式追溯到——([a-z]+)——这样做会没有问题：

```racket
> (regexp-match #px"([a-z]+) and \\1"
                "billions and millions")

#f
```

下面的示例标记数字字符串中所有立即重复的模式：

```racket
> (regexp-replace* #px"(\\d+)\\1"
    "123340983242432420980980234"
    "{\\1,\\1}")

"12{3,3}40983{24,24}3242{098,098}0234"
```

下面的示例修正了两个单词：

```racket
> (regexp-replace* #px"\\b(\\S+) \\1\\b"
    (string-append "now is the the time for all good men to "
                   "to come to the aid of of the party")
    "\\1")

"now is the time for all good men to come to the aid of the party"
```

- 非捕捉簇

```racket
它通常需要指定一个簇（通常为量化）但不触发子匹配项的信息捕捉。
这种簇称为非捕捉（non-capturing）。要创建非簇，请使用(?:以代替(作为簇开放器。

在下面的例子中，一个非簇消除了“目录”部分的一个给定的UNIX路径名，并获取簇标识。

> (regexp-match #rx"^(?:[a-z]*/)*([a-z]+)$"
                "/usr/local/bin/racket")

'("/usr/local/bin/racket" "racket")
```

- 回廊

一个非捕捉簇?和:之间的位置称为回廊（cloister）。你可以把修饰符放在这儿，
有可能会使簇子模式被特别处理。这个修饰符i使子模式匹配时不区分大小写：

```racket
> (regexp-match #rx"(?i:hearth)" "HeartH")

'("HeartH")
```

修饰符m使子模式在多行模式匹配，在.的位置不匹配换行符，^仅在一个新行后可以匹配，
而$仅在一个新行前可以匹配。

```racket
> (regexp-match #rx"." "\na\n")

'("\n")
> (regexp-match #rx"(?m:.)" "\na\n")

'("a")
> (regexp-match #rx"^A plan$" "A man\nA plan\nA canal")

#f
> (regexp-match #rx"(?m:^A plan$)" "A man\nA plan\nA canal")

'("A plan")
```

你可以在回廊里放置多个修饰符：

```racket
> (regexp-match #rx"(?mi:^A Plan$)" "a man\na plan\na canal")

'("a plan")
```

在修饰符前的一个减号反转它的意思。因此，你可以在子类中使用-i以翻转案例不由封闭簇造导致。

```racket
> (regexp-match #rx"(?i:the (?-i:TeX)book)"
                "The TeXbook")

'("The TeXbook")
```

上述正表达式将允许任何针对the和book的外壳，但它坚持认为Tex有不同的包装。

- 替代

你可以通过用|分隔它们来指定列表的替代子模式。
在最近的封闭簇里|分隔子模式（或在整个模式字符串里，假如没有封闭括号）。

```racket
> (regexp-match #rx"f(ee|i|o|um)" "a small, final fee")

'("fi" "i")
> (regexp-replace* #rx"([yi])s(e[sdr]?|ing|ation)"
                   (string-append
                    "analyse an energising organisation"
                    " pulsing with noisy organisms")
                   "\\1z\\2")

"analyze an energizing organization pulsing with noisy organisms"
```

不过注意，如果你想使用簇仅仅是指定替代子模式列表，却不想指定匹配项，那么使用(?:代替(。

```racket
> (regexp-match #rx"f(?:ee|i|o|um)" "fun for all")

'("fo")
```

注意替代的一个重要事情是，最左匹配替代不管长短。
因此，如果一个替代是后一个替代的前缀，后者可能没有机会匹配。

```racket
> (regexp-match #rx"call|call-with-current-continuation"
                "call-with-current-continuation")

'("call")
```

为了让较长的替代在匹配中有一个镜头，把它放在较短的一个之前：

```racket
> (regexp-match #rx"call-with-current-continuation|call"
                "call-with-current-continuation")

'("call-with-current-continuation")
```

在任何情况下，对于整个正则表达式的整体匹配总是倾向于整体的不匹配。
在下面这里，较长的替代仍然更好，因为它的较短的前缀不能产生整体匹配。

```racket
> (regexp-match
   #rx"(?:call|call-with-current-continuation) constrained"
   "call-with-current-continuation constrained")

'("call-with-current-continuation constrained")
```

- 回溯

我们已经明白贪婪的量词匹配的次数最多，但压倒一切的优先级才是整体匹配的成功。考虑以下内容

```racket
> (regexp-match #rx"a*a" "aaaa")

'("aaaa")
```

这个正则表达式包括两个子正则表达式：a*，其次是a。
子正则表达式a*不允许匹配文本字符串aaaa内的所有的四个a，即使*是一个贪婪量词也一样。
它可能仅匹配前面的三个，剩下最后一个给第二子正则表达式。这确保了完整的正则表达式匹配成功。

正则表达式匹配器通过一个称为回溯（backtracking）的过程实现来这个。
匹配器暂时允许贪婪量词匹配所有四个a，但当整体匹配处于岌岌可危的状态变得清晰时，
它回溯（backtracks）到一个不那么贪婪的三个a的匹配。如果这也失败了，与以下调用一样

```racket
> (regexp-match #rx"a*aa" "aaaa")

'("aaaa")
```

匹配器回溯甚至更进一步。只有当所有可能的回溯尝试都没有成功时，整体失败才被承认。

回溯并不局限于贪婪量词。非贪婪量词匹配尽可能少的情况下，为了达到整体匹配，
逐步回溯会有越来越多的实例。这里替代的回溯也更有向右替代的倾向，
当局部成功的向左替代一旦失败则会产生一个整体的匹配。

有时禁用回溯是有效的。例如，我们可能希望作出选择，或者我们知道尝试替代是徒劳的。
一个非回溯正则表达式在(?>...)里是封闭的。

```racket
> (regexp-match #rx"(?>a+)." "aaaa")

#f
```

在这个调用里，子正则表达式?>a+贪婪地匹配所有四个a，并且回溯的机会被拒绝。
因此，整体匹配被拒绝。这个正则表达式的效果因此对一个或多个a的匹配被某些明确无a（non-a）的予以继承.

- 向前查找

用?=正向前查找窥探以提前确保其子模式能够匹配。

```racket
> (regexp-match-positions #rx"grey(?=hound)"
    "i left my grey socks at the greyhound")

'((28 . 32))
```

正则表达式#rx"grey(?=hound)"匹配灰grey，但前提是它后面紧跟着hound。
因此，文本字符串中的第一个grey不匹配。

用?!反向前查找窥探以提前确保其子模式不可能匹配。

```racket
> (regexp-match-positions #rx"grey(?!hound)"
    "the gray greyhound ate the grey socks")

'((27 . 31))
```

正则表达式#rx"grey(?!hound)"匹配grey，但只有hound不跟着它才行。
因此grey仅仅在socks之前才匹配。

- 向后查找

用?<=正向后查找检查其子模式可以立即向文本字符串的当前位置左侧匹配。

```racket
> (regexp-match-positions #rx"(?<=grey)hound"
    "the hound in the picture is not a greyhound")

'((38 . 43))
```

正则表达式#rx"(?<=grey)hound"匹配hound，但前提是它是先于grey的。

用?<!负向后查找检查它的子模式不可能立即匹配左侧。

```racket
> (regexp-match-positions #rx"(?<!grey)hound"
    "the greyhound in the picture is not a hound")

'((38 . 43))
```

正则表达式#rx"(?<!grey)hound"匹配hound，但前提是它不是先于grey的。

向前查找和向后查找在它们不混淆时可以是实用的。
