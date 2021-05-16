# Thesis

## Important links

Repositories
 - [ivis-core](https://github.com/smartarch/ivis-core)
   - [fork](https://github.com/bastianluk/ivis-core)
 - [anomaly detection framework](https://bitbucket.org/rakibulmdalam/time-series-anomaly-detection-framework/src/master/)
   - [re-upload](https://github.com/bastianluk/Time-series-Anomaly-Detection-Framework)

## Content ideas

### Theoretical part
 - anomaly detection theory
 - current state of the framework
 - other visualization frameworks

### Practical part
 - changes needed in ADF
 - component structure of what I added
   - the task
   - the graphing component


## Progress

 - [x] add base task with its parameters
   - [ ] refine it with actual checkboxes?
 - [ ] connect the framework
   - [x] add base package
     - [x] add files, make it part of the build
     - [x] add dependencies to the task build
   - [ ] WIP: rebuilt built-in task - how to
   - [ ] change the frameworks internals
     - [x] remove file inputs
     - [x] remove the plotting used for iterative mode
     - [ ] replace the plotting - connected to interactive mode
   - [ ] prepare i/o on both sides
     - [x] output by ivis
       - [ ] improve the output - loop through the database to query all data
     - [x] input to framework
     - [ ] output by framework
     - [ ] save output in ivis
 - [ ] interactive mode!!!
   - [ ] will be done in graph, will edit job to re-produce new output
 - [ ] graphing component
   - [ ] combine existing components to produce what is in the specs
     - [ ] how to already in code in other components
     - [ ] WIP: add base
     - [ ] add elements
     - [ ] add logic