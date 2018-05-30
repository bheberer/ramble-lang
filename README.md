# Ramble

Umm.

Ramble is a toy [esoteric](https://en.wikipedia.org/wiki/Esoteric_programming_language)
progamming language designed to read like everyday conversation. It's unrealistic to
expect us to be so quick in dialogue all the time, so why not reflect that in our
code, you know?

### Features

Some highlights of Ramble include:
- All **um**s are ignored
- Single line comments start with **uh**
- All (sequential) statements end with **yeah**
- Control flow statements end with **you know?**

Ramble is object-oriented, imperative, weakly typed. With that in mind:
- Everything is an object
- Ends of branches act as control statements
- Ramble supports inheritance
- Ramble supports ad-hoc polymorphism


### Examples

Here is an example Hello World program:

```
$ cat hello.um
umm
Uhhh ignore this

um "Hello world!\n".say()

UMMMMmm
uhh yeah

$ ./ramble hello.um
$ ./ramblevm a.out
Hello world!
```

And here is a polymorphic example:

```
$ cat add.um
class C subclasses Object
uhh ok so
    begin
	umm
	def add(a, b)
	    a.+(b)
	you know?
you know? umm

uhh I think
x = new C yeah
x.add("first ","second\n").say() yeah
ummmm x.add(3,5).say() yeah
"\n".say()

$ ./ramble cat.um
$ ./ramblevm a.out
first second
8
```

### Usage

Ramble code is compiled into ramblevm code with **ramble**, which is
then interpreted with **ramblevm**.
