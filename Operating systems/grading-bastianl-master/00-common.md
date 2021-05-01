# Common Course Information

This course will show you how the operating systems work,
and how that impacts the way application software executes.
Besides the pure joy of knowledge, this should help you use
computers more efficiently.

This year, the organization is complicated by the COVID pandemic.
Faced with the choice of either lecturing as usual, or changing
the course towards more efficient and less traditional form,
we chose the latter - rather than insulting your intelligence
by reciting lecture content in tedious oneway online meetings,
we have decided to structure the course around self study and coding.
Online meetings should focus on interactive discussions as much as possible.

Of course, this is new for us as it is new for you, but please bear with us.
We sincerely believe that you will enjoy our course more than you would
a regular lecture (there should be enough of those :-), and we will
do our best to iron out any wrinkles along the way.


## General Course Organization

The course will consist of two connected streams - self study and project implementation.
Self study instructions will come in the form of weekly batches of self study material, which
should gradually provide the background knowledge necessary for the project implementation.
The project implementation is where you will learn by doing - initially alone, later in
teams of three, you will implement your own operating system.

### Self Study

The instructions for self study will be distributed through this repository every week, we aim for Fridays.
Each self study instruction package will be in a separate file, such as `01-architecture-before.md`.
Read the file and follow the instructions as you see fit - we fully expect you to skip those parts
that you already know, obviously there might also be parts where you will need to look for
additional background material on your own.
See also the [materials](#materials) section below.

We will primarily refer to the book by Arpaci-Dusseau et al. called Operating Systems: Three Easy Pieces.
This book is [available online](http://www.ostep.org) for free.
Reasonable initiative is expected in looking up additional material,
but the whole self study for one week should not take you more than
one hour - if you feel that is not enough to understand the content,
please bring your questions to the online meeting for discussion.

The online discussion meetings are scheduled for Fridays at 9:00,
the meeting link is https://cesnet.zoom.us/j/91795412536.

To help you assess whether your knowledge is sufficient, the self study instructions include questions.
Please answer the questions by editing the corresponding file and committing it back into your repository.
This should be done by Thursday noon, that is, the day preceding the online meeting,
to provide us with time to summarize your answers and prepare the discussion points.

For example, when the file says:

```
**Q1** What is the meaning of life, universe and everything ?

**A1** ...
```

You should edit it like this:

```
**Q1** What is the meaning of life, universe and everything ?

**A1** 42
```

The correctness of your answers to the self study questions posted before
the online meeting will not impact the grading.

After the online meeting, another set of questions will be posted to the repository
in a separate file, such as `01-architecture-graded.md`. You are expected to answer
these questions in a similar manner, again by Thursday noon. Importantly,
this time the correctness of your answers will contribute to your grade.
See the [grading](#grading) section below for details.

### Project Implementation

Independent on your self study activities, but synchronized so that the content
required for implementation is studied beforehand, you will work on a project
that will eventually culminate in a miniature but working operating system.

The project assignments will be posted in separate repositories with
announcement sent to the course mailing list. Initially, you will work
individually, but in two weeks you should form a team of three students
total, the teams will work on the assignments together throughout most of the semester.

Each assignment will have a deadline, and you will need to commit a working solution
into the corresponding repository by that deadline. Tests will be provided
to filter out obviously failing solutions, additional manual inspection
will be done to grade your work.

Online labs will be dedicated to consultations, the meeting links are:

- Wednesday 14:00 https://cesnet.zoom.us/j/91679907362
- Friday 10:40 https://cesnet.zoom.us/j/98355685738

Participation in online labs is not mandatory, neither is adhering to the assigned student group.


## Grading

The course grading is based on points from three sources:

- Knowledge Points
- Project Activity Points
- Project Implementation Points

All points are normalized on a scale of 0 to 100 percent.
To pass the course, you will need a minimum of 50 percent from each source,
the overall grade will be determined from the average of the three sources.
The `good` grade will be awarded for 50 percent or more,
the `very good` grade will be awarded for 70 percent or more,
the `excellent` grade will be awarded for 85 percent or more.

### Knowledge Points

Knowledge points will be assigned for answering the self study questions.
To receive knowledge points on a particular week, you need to submit
answers to both the before-meeting and after-meeting questions.

Answers to the before-meeting questions are used to prepare the meeting and are not graded for correctness.
Answers to the after-meeting questions are graded for correctness, the scale is linear,
that is, 0 percent for all answers wrong, 100 percent for all answers right.

Answers submitted late will not be considered.

### Project Activity Points

To distinguish the individual activity in the team project,
we will be using the repository commit logs from your team project.
For each day you commit on, you will receive one project activity point.
For each 100 lines of code you add, you will receive one project activity point.
The overall project activity score will be computed as the sum of all received points,
capped at 20 points, and converted to percent scale.

### Project Implementation Points

Each project assignment will include basic and extended requirements.
A solution that meets the basic requirements can be awarded at most 8 points.
A solution that also meets the extended requirements can be awarded at most 10 points.
Points will be deducted for issues such as bugs or code clarity.
The overall project implementation score will be computed as
the sum of points received and converted to percent scale.

Solutions submitted late will be penalized by 1 point for every 2 days.

Please note that we try to keep the instructions compact and avoid legalese,
however, we will penalize attempts at gaming the system through formalistic interpretation.


## Materials

Our primary course book is [Arpaci-Dusseau et al.: Operating Systems: Three Easy Pieces (Version 1.00)](http://www.ostep.org).
We will also occassionally refer to the [MIPS R4000 Microprocessor User’s Manual (2nd Edition)](https://d3s.mff.cuni.cz/teaching/nswi004)
and the [Intel 64 and IA-32 Architectures Software Developer's Manual (Version 325462-072)](https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html).

If, for any reason, you need more information, there are other excellent books that can help you study.
Favorites include:

- Anderson et al.: Operating Systems Principles and Practice
- Silberschatz et al.: Operating System Concepts
- Tanenbaum et al.: Modern Operating Systems

None of these books follows the course exactly, but all contain enough material to suffice as a learning material.

A good book on the computer systems background is Patterson et al.: Computer Organization and Design.

For those who do not regularly work with Linux or C, we have also prepared
[a short tutorial](https://d3s.mff.cuni.cz/files/teaching/nswi004/download/nswi004-tools-recap-2017-18.pdf).
