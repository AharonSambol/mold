# Mold
### As easy as Python and as fast as Rust! It's also as slow as Python and as hard as Rust...
<br>
OK, this might need some explaining.

It's a hobby language Iv'e made which explores an interesting concept I haven't seen elsewhere.

Most languages are either interpreted or compiled (or JIT), each coming with their own pros and cons. 
Logically if we were to have a language which is both then that'll have all the pros and none of the cons.

'Mold' explores this idea. I <del>blatantly stole</del> *was inspires* by Python and Rust, to create a 
hybrid language which looks <del>identical</del> similar to Python but can also take advantage of Rust-like 
features inorder to attain rust like performance.

This language can either be interpreted, in which case it's identical to Python (missing a few features ATM) or compiled... Well, sort of... 
in order to compile there are a few things you must do, for example, you would need to add type hints to everything
(some things are automatically inferred by the compiler, and in the future hopefully even more things will be inferred).

"So it's not a dynamic language... BORIINNNGGG!!" 

Hold on! It still kinda is, please keep reading.

But first here's an example:<br>
This is how I would write a lame Fibonacci function in Python
```python:
def fib(x):
    if x < 2:
        return x
    return fib(x - 1) + fib(x - 2)

def main():
    print(fib(35))
```
Coincidentally this works in Mold as well, and if we run it we will get the 35th fibonacci number (9227465).
As you probably know, this is pretty slow. On my laptop it takes about *4 seconds*. However, if we add types, like so:
```python:
def fib(x: int) -> int:
    if x < 2:
        return x
    return fib(x - 1) + fib(x - 2)

def main():
    print(fib(35))
```
Now we can compile it!<br>
And it runs in only **30 milliseconds**! That's a **~130x** speedup!

## Use Cases
So what is this good for? 

<del>Nothing</del>

* Writing Rust is known to be a pain because you need to fight with the 
compiler in order to run your code. So in Mold when you want to add a function/functionality you can write it in Python first
and make sure that it works first and only then add type hints (and the other things we'll get to soon) in order to 
get the Rust-like performance
* Learning Rust is pretty hard. Learning Mold might be easier because you don't need to learn all the new syntax,
you can focus on learning about the borrow checker first (and once you get it you can move on to learn Rust)
* Migrating from Python to Rust might be easier if you can do it part by part and still be able to run your code at any point
* IDK I just thought it would be a fun project...

## Similarities and Differences between Python and Mold
Assuming you know Python learning Mold shouldn't be too hard, there are a few differences though. 
<br>Let's however start with some similarities:

### Making Variables
```python:
x = 1
y = "tabs > 4 spaces"
z = 3.5
lst = [1, 2, 3, 4]
st = {1, 3, 6}
dc = {"1": 1, "2", 2}
```
### Math
```python:
(1 + 2) * 6 // 2
```
### Control Flow
```python:
if x > 2 or y != 5:
    for i in [1, 2, 3]:
        while i < x:
            i += 1
elif y == 6:
    pass
else:
    print("elif > else if")
```
### Functions
```pytohn:
def func(x: int, y: bool) -> int:
    if y:
        return x * 2
    return x
```
Note that we need to specify the types in the function signature, if we want to compile it.

## Now some differences

Mold isn't exactly dynamic in the same way as Python when declaring a variable you need to say which types it can be.
For example:
```python:
x: int | str = 1
x = "wow"
x = False # this wont work because x is only int or str
```
If you want to change it though you can override the variable with another variable with the same name.
<sub>(I'm not sure about this feature... I might remove it in future versions)</sub>
```python:
x: str = "4"
x: int = int(x) # this overrides with a new variable 
x := False # this also overrides it when the compiler can infer the type
```
Note however that if you override it in a separate scope when you exit the scope it'll return back to the original variables, e.g.
```python
x = 2 # x is int
if True:
    x := "str" # x is now overriden to a string
# now we exit the scope (the if statement) so x is back to being an int
```
# TODO this only works when compiling

# TODO pointers, match, and borrow checker

