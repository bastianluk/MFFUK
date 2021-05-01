# Process Memory Layout

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday October 29 at 12:00 CET (noon).


## Heap

Storing data on heap typically incurs space overhead (more memory is consumed than what the application requests).
The reasons for this overhead are many, starting with the need to store heap allocator metadata or the need to
align allocated blocks, and also including dynamic factors such as fragmentation.

**Q1** Assume a heap implementation that uses 8-byte headers and 16-byte data alignment with each block.
Further assume that we are allocating linked list elements that consist of a single pointer (8 bytes)
and a single integer variable (4 bytes). How much memory, in bytes, will be consumed (including
overheads) after allocating 1000 linked list elements (assume no other allocations take place
and blocks are allocated next to each other) ?

**A1** 1 000 * (8 header + 8 align + 8 pointer + 4 int + 4 align) = 32 000 B in memory used

**Q2** In the scenario from the previous question, assume two neighboring elements from the middle of the list
are freed, and the blocks are coalesced. What is the size of the largest data allocation that can be satisfied
from thus created free block, in bytes ?

**A2** 64 B are cleared, 8 + 8 = 16 B are needed for the heap header >>> 48 B of data space can be used.


## Garbage Collection

In the following questions, code fragments are used to test your understanding of garbage collection algorithms.
The code fragments are necessarily short and a smart compiler could easily optimize them away, because no
externally visible action is performed. In your answers, assume no such optimization takes place,
consider that the code executes as written.

Reference counting is a technique that keeps track of what objects should be garbage collected by keeping track
of the number of references pointing to each object. When the reference count reaches zero, the object can
no longer be reached through any reference and can be garbage collected.

**Q3** Assume an imaginary programming language, with syntax inspired by C# and Java, but without the pesky
visibility attributes. Object types are constructed using `new`, passed by reference, reference counted,
and destroyed immediately when their reference count reaches zero. Look at the following code fragment:

```java
class Node {
    Node left;
    Node right;
}

class Main {
    static Node allocate () {
        Node root = new Node (); // Node instance ROOT
        Node left_child = new Node (); // Node instance LEFT
        Node right_child = new Node (); // Node instance RIGHT
        root.left = left_child;
        root.right = right_child;
        return (root);
    }
    static void main () {
        Node root = allocate ();
        root.left = null;
        // Location ONE
        root = null;
        // Location TWO
    }
}
```

When `main` runs, what are the reference counts of ROOT, LEFT and RIGHT at locations ONE and TWO ?

**A3** loc ONE - ROOT = 1, LEFT = 0, RIGHT = 1; loc TWO - ROOT = 0, LEFT = 0, RIGHT = 1;

Reference tracing is a technique that identifies garbage by transitively following references from
garbage collection roots. Any object reachable from a root is considered live, all other objects
are considered dead and garbage collected.

**Q4** In the imaginary programming language from the previous question, we have this program:

```java
class Node {
    Node left;
    Node right;
}

class Main {
    static Node root;
    static void allocate () {
        root = new Node ();
        Node left_child = new Node ();
        Node right_child = new Node ();
        // Location X
        root.left = left_child;
        root.right = right_child;
    }
    static void main () {
        int i;
        Node node;
        allocate ();
        node = root;
    }
}
```

Assume tracing garbage collection is triggered at location X.
List all roots that the garbage collector would consider.

**A4** static is referencing what came from `root = new Node ()`; local vars are referencing their respective objects from `Node left_child = new Node ();`. All the references are at 1 >> no object (from this piece of code) for GC to collect.


## Summary

**Q5** Imagine a program that allocates a megabyte integer array (1) on stack, (2) on non managed heap with
free lists and possible fragmentation, and (3) on managed heap with compaction and no fragmentation.
Order the allocations (1) to (3) in the order of speed, from the fastest to the slowest.

Would your answer change if the allocation also had to initialize the array to all zeros ?

**A5** The order is as declared so (1), (2) and (3) - stack just needs to move the pointer, the drawback of possible fragmentation gives (2) the speed edge over the management of (3).
