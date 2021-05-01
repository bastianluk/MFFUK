# Team assignments

This repository will be used for **all team assignments** in the course.

Apart from assignment `a02`, which is already [present](README-a02.md), the
subsequent assignments will be provided as updates to an upstream repository
which we will announce on the course mailing list and also on the
[course website](https://d3s.mff.cuni.cz/teaching/nswi004).
You are required to merge the updates into this repository.

Please see the ["Keeping your fork up-to-date"](https://d3s.mff.cuni.cz/teaching/nswi004/qa/#keeping-your-fork-up-to-date)
section in the *Q & A* part of the course website for a quick guide. The only
difference is that you will be using an upstream repository for the team
assignments.

## Environment

Please note that **all team assignments require the MSIM simulator and the MIPS
cross-compiler tool-chain**. At this point (after assignment `a01`) everybody
should have access to a working development environment for MIPS on MSIM -- either
on a remote host (*Rotunda lab* machines or `lab.d3s.mff.cuni.cz:22004`), or on your
own computer (see the [download and installation instructions](https://d3s.mff.cuni.cz/teaching/nswi004)
on how to install locally).

## Assignments

The actual assignments are defined in separate files. You will find the first
team assignment `README-a02.md` file; subsequent assignments will follow the
same naming scheme. The source code which provides the context for the 
assignment is organized in the same way as in assignment `a01` (the last
individual assignment), so you should be already familiar with the layout.

## Tests

Just like in the previous (individual) assignments, each team assignment will
come with a suite of tests to help you stay on the right track. The tests
making up each suite are listed in `suite_a*_{base,extended}.txt` files, and
you can execute all tests simply by typing:

```shell
tools/tester.py suite suite_a*.txt
```

However, for incremental development and debugging, we suggest that you
configure your kernel build to execute a single test when you start MSIM.
This will (together with incremental build) speed up the turnaround between
code modification and test execution.

For example, to execute the `basic` test from the `heap` test group when
you start MSIM, you can configure your project as follows:

```shell
./configure.py --debug --kernel-test=heap/basic && make clean
```

Then, whenever you modify (and save) your code, you can quickly rebuild
the kernel and run the pre-configured test as follows:

```shell
make && msim
```


## CI setup

The `.gitlab-ci.yml` file contains CI configuration for your repository. When
working on an assignment, make sure that you always execute (and successfully
pass) also the test suites from the previous assignments, otherwise you might
find it difficult to proceed with the assignment at hand. When you submit an 
assignment for grading, there should be no regressions and all tests should
be green.

## Turn-in

To turn-in your assignment, [**tag the commit**](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
(on your `master` branch) that represents the state of the repository that you
want evaluated and graded. The tag name must correspond to the assignment
identifier, i.e., `a02`, `a03`, and so forth. If there is no tag, we will
evaluate and grade the contents of the repository up to and including the
last commit on `master` before the deadline.
