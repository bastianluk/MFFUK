# File Systems

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday January 7 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the last of three self study modules that will look at devices and I/O.
The goal of this module is to sketch some common file system layouts.


## File System Interface

You should already be familiar with the basic file system interface (functions such as `open`, `read` and `write`, `lseek` and so on for file access, and similar for directory access).
If not, you can see _Arpacci-Dusseau Section 39 Files and Directories_ for a refresher.

The two major abstractions provided by the file system interface are files (streams of bytes with metadata) and directories (collections of named files).
There are variations on this theme (for example NTFS can have files with more than one stream), but mostly the two abstractions have turned out
to be remarkably stable across years and systems.

**Q1** Apart from the name and size, name at least one other example of metadata associated with a file.

**A1** ...

**Q2** If we use the basic file system interface to read C bytes from position P of file F into array A, we have to do the following
(see `man` for details on the functions used, but the details should not be essential here, also error checking omitted):

```c
int handle = open (F, 0, O_RDONLY);
off_t position = lseek (handle, P, SEEK_SET);
size_t size = read (handle, A, C);
int status = close (handle);
```

One could imagine having one simple function that does this:

```c
int status = better_file_read (F, P, A, C);
```

Why do you think the designers of the file system interface keep the four functions from the example above separate, rather than having one better file read function ?

**A2** ...


## File System Internals

The abstractions provided by a typical file system are files and directories.
The abstractions provided by a typical disk are sectors of constant size
that can be read and written in arbitrary order.
A file system is code that implements the former using the latter.

Please read _Arpacci-Dusseau Section 40 File System Implementation_ to familiarize yourself with basic outlines of the UNIX file system internals.
(What is described in the chapter is a decent approximation of the `ext2`, `ext3` and `ext4` file systems often used in Linux,
even though many Linux distributions are gradually migrating towards more modern file systems such as `zfs` or `btrfs`.)

The book chapter does not really talk about another widely used file system, `FAT` from Microsoft, because the authors consider it not elegant enough.
Unfortunately, it is still widely used, so it might be worth knowing about. Please take a look at the reference in the book,
http://www.win.tue.nl/~aeb/linux/fs/fat/fat.html, to see the basic layout of the file system.
Ignore the boot sector, the idea of the file allocation table
and the basic directory layout are enough.

**Q3** True or false: every file in the UNIX file system has its own `inode` structure ?

**A3** ...

**Q4** True or false: every directory in the UNIX file system has its own `inode` structure ?

**A4** ...

**Q5** Imagine you are looking up a file specified `/by/its/full/path/name`
(looking up in this context means locating first byte of data on the disk).
What is the theoretical complexity of this lookup on the FAT file system ?
Use the number of disk accesses needed as the metric,
specify whatever additional parameters you need.

**A5** ...

**Q0** The last chance to ask an arbitrary question in a quiz ! Do not waste it :-)

**A0** Is there a clear cut "best" file system that should be used everywhere? Suppose not but what would be the one closest to it (with least tradeoffs in different usecases)?
