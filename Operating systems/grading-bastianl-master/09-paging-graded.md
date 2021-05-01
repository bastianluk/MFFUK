# Paging

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday December 3 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This time around all questions concern address translation with paging.


**Q1** If we observe virtual address 0x12345678 translated to physical address 0x9ABCD678, what is the upper bound on the page size involved ?
Give your answer as a maximum page size in decimal.

**A1** Page size is 4kB (4096 bytes)


**Q2** Assume we have an Intel processor with a two level paging table.
The structure of the table is shown in the Intel processor manual Volume 3A Section 4.5
on Figure 4-2 with the title "Linear-Address Translation to a 4-KByte Page using 32-Bit Paging".

You see that a process only has a single second level page table allocated.
How big at most is the range of virtual addresses that the process is using ?
Give your answer as the number of consecutive virtual addresses in hexadecimal.

**A2** 1000 (it is second level, just one page > the max number of consecutive addresses equals to the actual page size but in hexa so 4kB ~ 4096b ~ 1000 in hexadecimal)


**Q3** Assume we have an Intel processor with a four level paging table.
The structure of the table is shown in the Intel processor manual Volume 3A Section 4.5
on Figure 4-8 with the title "Linear-Address Translation to a 4-KByte Page using 4-Level Paging".

We translate a virtual address and the translation uses
the first entry of the first level table (PML4E),
the second entry of the second level table (PDPTE),
the third entry of the third level table (PDE), and
the fourth entry of the fourth level table (PTE),
and points to the fifth byte of the page.
What is the address, in hexadecimal ?

**A3** 0x40403020 (entries are counted from 0===>>>0 is the first, PML4E - 9 bites - 000000000, PDPTE - 9 bites - 000000001, PDE - 9 bites - 000000010, PTE - 9 bites - 000000011, page - 12 bites - 000000100000 ===>>> 000000000000000001000000010000000011000000100000 in binary ~ 40403020 hexadecimal)


**Q4** Imagine you were inspecting the page tables of several processes
and you have noticed that some of the entries are exactly the same
among the processes (the same numerical values in all fields).
What does that tell you about the address spaces ?

**A4** It means that the processes are working with the same physical pages, but might have different offsets (so they dont overwrite each others memory/data).


**Q5** You see a dump of a translation structure entry that says:

```
virtual: 0x1234
physical: 0x5678
rights: 0xABCD
flags: 0xEF
```

You know it is either a page table entry or a TLB entry.
Which is it and why ?

**A5** Page table would have the "next address" to look at and possibly other things like rights, yes, but TLB has the both the virtual "tag" and physical page number so the "cached translation" can take place >>> I think this is a TLB entry. 
