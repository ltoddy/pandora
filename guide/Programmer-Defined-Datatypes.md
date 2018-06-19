- 自定义的数据类型

新的数据类型通常用struct表来创造，这是本章的主题。基于类的对象系统，遵循类和对象（Classes and Objects），提供了用于创建新的数据类型的另一种机制，但即使是类和对象也是结构类型的实现方式。

- 简单的结构类型：struct

一个最接近的，struct的语法是

```racket
(struct struct-id (field-id ...))
```

例如：

```racket
(struct posn (x y))
```

struct表将struct-id和从struct-id和field-id构建的数值标识符绑定在一起：

1、struct-id：一个构造函数，它将一些参数作为field-id的数值，并返回结构类型的一个实例。

例如：

```racket
> (posn 1 2)
#<posn>
```

2、struct-id?：一个判断函数，它获取单个参数，如果它是结构类型的实例返回#t，否则返回#f。

例如：

```racket
> (posn? 3)
#f
> (posn? (posn 1 2))
#t
```

3、struct-id-field-id：每个field-id，访问器从结构类型的一个实例中解析相应的字段值。

例如：

```racket
> (posn-x (posn 1 2))
1
> (posn-y (posn 1 2))
2
```

4、struct:struct-id：一个结构类型描述符，这是一个值，它体现结构类型作为最好的价值（与#:super和《更多的结构选项》（More Structure Type Options）一起作为后续讨论）。

一个struct表不限制在结构类型的实例中可以出现的字段的值类型。例如，(posn "apple" #f)过程产生一个posn实例，即使"apple"和#f对posn的实例的显性使用是无效的配套。执行字段值的约束，比如要求它们是数字，通常是合约的工作，如后面讨论的《合约（Contracts）》那样。

- 复制和更新

struct-copy复制一个结构并可选地更新克隆中的指定字段。这个过程有时称为功能性更新（functional update），因为结果是一个具有更新字段值的结构。但原来的结构没有被修改。

```racket
(struct-copy struct-id struct-expr [field-id expr] ...)
```

出现在struct-copy后面的struct-id必须是由struct绑定的结构类型名称（即这个名称不能作为一个表达式直接被使用）。struct-expr必须产生结构类型的一个实例。结果是一个新实例，就像旧的结构类型一样，除这个被每个field-id标明的字段得到相应的expr的值之外。

例如：

```racket
> (define p1 (posn 1 2))
> (define p2 (struct-copy posn p1 [x 3]))
> (list (posn-x p2) (posn-y p2))
'(3 2)

> (list (posn-x p1) (posn-x p2))
'(1 3)
```

- 结构子类

struct的扩展形式可以用来定义结构子类型（structure subtype），它是一种扩展现有结构类型的结构类型：

```racket
(struct struct-id super-id (field-id ...))
```

这个super-id必须是由struct绑定的结构类型名称（即名称不能被作为表达式直接使用）。

例如：

```
(struct posn (x y))
(struct 3d-posn posn (z))
```

一个结构子类型继承其超类型的字段，并且子类型构造器接受这个值作为子类型字段在超类型字段的值之后。一个结构子类型的实例可以被用作这个超类型的断言和访问器。

例如：

```racket
> (define p (3d-posn 1 2 3))
> p
#<3d-posn>

> (posn? p)
#t

> (3d-posn-z p)
3
; a 3d-posn has an x field, but there is no 3d-posn-x selector:

> (3d-posn-x p)
3d-posn-x: undefined;
 cannot reference undefined identifier
; use the supertype's posn-x selector to access the x field:

> (posn-x p)
1
```

- 不透明结构类型与透明结构类型对比

具有以下结构类型定义：

```racket
(struct posn (x y))
```

结构类型的实例以不显示字段值的任何信息的方式打印。也就是说，默认的结构类型是不透明的（opaque）。如果结构类型的访问器和修改器对一个模块保持私有，再没有其它的模块可以依赖这个类型实例的表示。

让结构型透明（transparent），在字段序列后面使用#:transparent关键字：

例如：

```racket
(struct posn (x y)
        #:transparent)

> (posn 1 2)
(posn 1 2)
```

一个透明结构类型的实例像调用构造函数一样打印，因此它显示了结构字段值。透明结构类型也允许反射操作，比如struct?和struct-info，在其实例中使用（参见《反射和动态求值》）（Reflection and Dynamic Evaluation）。

默认情况下，结构类型是不透明的，因为不透明的结构实例提供了更多的封装保证。也就是说，一个库可以使用不透明的结构来封装数据，而库中的客户机除了在库中被允许之外，也不能操纵结构中的数据。

- 结构的比较

一个通用的equal?比较自动出现在透明的结构类型的字段上，但是equal?默认仅针对不透明结构类型的实例标识：

```
(struct glass (width height) #:transparent)

> (equal? (glass 1 2) (glass 1 2))
#t

(struct lead (width height))

> (define slab (lead 1 2))
> (equal? slab slab)
#t

> (equal? slab (lead 1 2))
#f
```

通过equal?支持实例比较而不需要使结构型透明，你可以使用#:methods关键字、gen:equal+hash并执行三个方法来实现：

```racket
(struct lead (width height)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; 比较a和b
     (and (equal?-recur (lead-width a) (lead-width b))
          (equal?-recur (lead-height a) (lead-height b))))
   (define (hash-proc a hash-recur)
     ;计算a的第一个hash代码
     (+ (hash-recur (lead-width a))
        (* 3 (hash-recur (lead-height a)))))
   (define (hash2-proc a hash2-recur)
     ;计算a的第二个hash代码
     (+ (hash2-recur (lead-width a))
             (hash2-recur (lead-height a))))])

> (equal? (lead 1 2) (lead 1 2))
#t
```

列表中的第一个函数实现对两个lead的equal?测试；函数的第三个参数是用来代替equal?实现递归的相等测试，以便这个数据循环可以被正确处理。其它两个函数计算用于哈希表的一级和二级哈希代码：

```racket
> (define h (make-hash))
> (hash-set! h (lead 1 2) 3)
> (hash-ref h (lead 1 2))
3

> (hash-ref h (lead 2 1))
hash-ref: no value found for key
  key: #<lead>
```

这第一个函数提供gen:equal+hash，不需要递归比较结构的字段。例如，表示一个集合的结构类型可以通过检查集合的成员是相同的来执行相等操作，独立于内部表示的的元素顺序来实现相等。只要注意哈希函数对任何两个假定相等的结构类型都会产生相同的值。

- 结构类型的生成性

每次对一个struct表求值时，它就生成一个与所有现有结构类型不同的结构类型，即使某些其他结构类型具有相同的名称和字段。

这种生成性对执行抽象和执行程序是有用的，就像口译员，但小心放置struct表被多次求值的位置。

例如：

```racket
(define (add-bigger-fish lst)
  (struct fish (size) #:transparent) ;每次都生成新的
  (cond
   [(null? lst) (list (fish 1))]
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))

> (add-bigger-fish null)
(list (fish 1))

> (add-bigger-fish (add-bigger-fish null))
fish-size: contract violation;
 given value instantiates a different structure type with
the same name
  expected: fish?
  given: (fish 1)

(struct fish (size) #:transparent)
(define (add-bigger-fish lst)
  (cond
   [(null? lst) (list (fish 1))]
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))

> (add-bigger-fish (add-bigger-fish null))
(list (fish 2) (fish 1))
```

- 预制结构类型

虽然transparent结构类型以显示内容的方式打印，但结构的打印形式不能用于表达式中以获得结构，不像数字、字符串、符号或列表的打印形式。

预制（prefab）（“被预先制造”）结构类型是内置的类型，是已知的Racket打印机和表达式阅读器。有无限多这样的类型存在，他们索引是通过名字、字段计数、超类型以及其它细节。一个预制结构的打印形式类似于一个矢量，但它以#s开始而不是以#开始，而且打印表的第一个元素是预制结构类型的名称。

下面的示例显示具有一个字段的sprout预置结构类型的实例。第一个实例具有字段值'bean，第二个实例具有字段值'alfalfa：

```racket
> '#s(sprout bean)
'#s(sprout bean)

> '#s(sprout alfalfa)
'#s(sprout alfalfa)
```

像数字和字符串一样，预置结构是“自引用”，所以上面的引号是可选的：

```racket
> #s(sprout bean)
'#s(sprout bean)
```

当你随struct使用#:prefab关键字，而不是生成一个新的结构类型，你获得与现有的预制结构类型的绑定操作：

```racket
> (define lunch '#s(sprout bean))
> (struct sprout (kind) #:prefab)
> (sprout? lunch)
#t

> (sprout-kind lunch)
'bean

> (sprout 'garlic)
'#s(sprout garlic)
```

上面的字段名称kind对查找预置结构类型无关紧要，仅名称sprout和字段的数量是紧要的。同时，具有三个字段的预制结构类型sprout是一种不同于单个字段的结构类型：

```racket
> (sprout? #s(sprout bean #f 17))
#f

> (struct sprout (kind yummy? count) #:prefab) ;重定义
> (sprout? #s(sprout bean #f 17))
#t

> (sprout? lunch)
#f
```

预制结构类型可以有另一种预制结构类型作为它的超类型，它具有可变的字段，并可以有自动字段。这些维度中的任何变化都对应于不同的预置结构类型，结构类型的名称的打印形式编码所有相关的细节。

```racket
> (struct building (rooms [location #:mutable]) #:prefab)
> (struct house building ([occupied #:auto]) #:prefab
    #:auto-value 'no)
> (house 5 'factory)

'#s((house (1 no) building 2 #(1)) 5 factory no)
```

每个预制结构类型都是透明的，但甚至比透明类型更抽象，因为可以创建实例而不必访问特定的结构类型声明或现有示例。总体而言，结构类型的不同选项提供了更抽象到更方便的各种可能性：

1、不透明的（Opaque）（默认）：没有访问结构类型声明，就不能检查或创造实例。正如下一节所讨论的，构造函数和属性可以附加到结构类型上，以进一步保护或专门化其实例的行为。

2、透明的（Transparent）：任何人都可以检查或创建一个没有访问结构类型声明的实例，这意味着值打印机可以显示实例的内容。然而，所有实例创建都通过一个构造函数守护程序，这样可以控制实例的内容，并且实例的行为可以通过属性进行专门化。由于结构类型是由其定义生成的，所以实例不能简单地通过结构类型的名称来生成，因此不能由表达式读取器自动生成。

3、预制（Prefab）：任何人都可以在任何时候检查或创建实例，而不必事先访问结构类型声明或实例。因此，表达式读取器可以直接生成实例。实例不能具有构造函数守护程序或属性。

由于表达式读取器可以生成预制实例，所以在方便序列化比抽象更重要时它们是有用的。然而，不透明和透明的结构也可以被序列化，如果他们被serializable-struct定义，其描述见《数据类型和序列化》（Datatypes and Serialization.）。

- 更多的结构选项

struct的完整语法支持许多选项，无论是在结构类型级别，还是在单个字段的级别上：

(struct struct-id maybe-super (field ...)
        struct-option ...)
 
maybe-super	 	=	 	
 	 	|	 	super-id
 	 	 	 	 
field	 	=	 	field-id
 	 	|	 	[field-id field-option ...]

struct-option总是以关键字开头：

#:mutable——

会导致结构的所有字段是可变的，并介绍了每个field-id的一个set-struct-id-field-id!设置方式，在结构类型的实例中设置对应字段的值。

例如：

> (struct dot (x y) #:mutable)
(define d (dot 1 2))
 
> (dot-x d)
1

> (set-dot-x! d 10)
> (dot-x d)
10

#:mutable选项也可以被用来作为一个field-option，在这种情况下，它使个别字段可变。

例如：

> (struct person (name [age #:mutable]))
(define friend (person "Barney" 5))
 
> (set-person-age! friend 6)
> (set-person-name! friend "Mary")

set-person-name!: undefined;
 cannot reference undefined identifier

#:transparent——

控制对结构实例的反射访问，如前面一节所讨论的《不透明结构和透明结构类型》那样。

#:inspector inspector-expr——

概括#:transparent以支持更多的控制访问或反射操作。

#:prefab——

访问内置结构类型，如前一节所讨论的《预置结构类型》那样。

#:auto-value auto-expr——

指定了一个被用于所有结构类型的自动字段的值，这里一个自动字段被#:auto字段选项表明。这个构造函数不接受给自动字段的参数。自动字段无疑是可变的（通过反射操作），但设置函数仅在#:mutable也被指定的时候被绑定。

例如：

> (struct posn (x y [z #:auto])
               #:transparent
               #:auto-value 0)
> (posn 1 2)

(posn 1 2 0)

#:guard guard-expr——

指定在创建结构类型的实例时调用的构造函数保护过程。在结构类型中，保护程序获取与非自动字段相同的参数，再加上一个实例化类型的名称（如果子类型被实例化，在这种情况下最好使用子类型的名称报告错误）。保护过程应该返回与给定值相同的值，减去名称参数。如果某个参数不可接受，或者可以转换一个参数，则保护过程可以引发异常。

例如：

```racket
> (struct thing (name)
          #:transparent
          #:guard (lambda (name type-name)
                    (cond
                      [(string? name) name]
                      [(symbol? name) (symbol->string name)]
                      [else (error type-name
                                   "bad name: ~e"
                                   name)])))
> (thing "apple")
(thing "apple")

> (thing 'apple)
(thing "apple")

> (thing 1/2)
thing: bad name: 1/2
```

即使创建子类型实例，也会调用保护过程。在这种情况下，只有构造函数接受的字段被提供给保护过程（但是子类型的保护过程同时获得子类型添加的原始字段和现有字段）。

例如：

```racket
> (struct person thing (age)
          #:transparent
          #:guard (lambda (name age type-name)
                    (if (negative? age)
                        (error type-name "bad age: ~e" age)
                        (values name age))))
> (person "John" 10)
(person "John" 10)

> (person "Mary" -1)
person: bad age: -1

> (person 10 10)
person: bad name: 10
```

#:methods interface-expr [body ...]——

关联与泛型接口对应的结构类型的方法定义。例如，执行gen:dict方法允许一个结构类型实例用作字典。执行gen:custom-write方法允许定制如何显示结构类型的实例。

例如：

```racket
> (struct cake (candles)
          #:methods gen:custom-write
          [(define (write-proc cake port mode)
             (define n (cake-candles cake))
             (show "   ~a   ~n" n #\. port)
             (show " .-~a-. ~n" n #\| port)
             (show " | ~a | ~n" n #\space port)
             (show "---~a---~n" n #\- port))
           (define (show fmt n ch port)
             (fprintf port fmt (make-string n ch)))])
> (display (cake 5))

   .....   
 .-|||||-. 
 |       | 
-----------
```

#:property prop-expr val-expr——

将属性和值与结构类型相关联。例如，prop:procedure属性允许一个结构实例作为函数使用；属性值决定当使用结构作为函数时如何执行。

例如：

```racket
> (struct greeter (name)
          #:property prop:procedure
                     (lambda (self other)
                       (string-append
                        "Hi " other
                        ", I'm " (greeter-name self))))
(define joe-greet (greeter "Joe"))
 
> (greeter-name joe-greet)
"Joe"

> (joe-greet "Mary")
"Hi Mary, I'm Joe"

> (joe-greet "John")
"Hi John, I'm Joe"
```

#:super super-expr——

一种替代提供super-id与struct-id紧邻。代替这个结构类型的名字（它是一个表达式），super-expr应该产生一种结构类型的描述符的值。对#:super更高级形式是结构类型的描述符是值，所以他们可以通过程序。

例如：

```racket
(define (raven-constructor super-type)
  (struct raven ()
          #:super super-type
          #:transparent
          #:property prop:procedure (lambda (self)
                                      'nevermore))
  raven)
 
> (let ([r ((raven-constructor struct:posn) 1 2)])
    (list r (r)))

(list (raven 1 2) 'nevermore)
> (let ([r ((raven-constructor struct:thing) "apple")])
    (list r (r)))

(list (raven "apple") 'nevermore)
```
