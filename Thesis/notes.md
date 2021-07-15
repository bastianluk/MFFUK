# Thesis

## Important links

Repositories

- [ivis-core](https://github.com/smartarch/ivis-core)
  - [fork](https://github.com/bastianluk/ivis-core)
    - [compare to base](https://github.com/smartarch/ivis-core/compare/master...bastianluk:devel)
- [anomaly detection framework](https://bitbucket.org/rakibulmdalamtime-series-anomaly-detection-framework/src/master/)
  - [re-upload](https://github.com/bastianluk/Time-series-Anomaly-Detection-Framework)
    - I thought this would be needed, turns out it might not be.

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

## Dev notes

- [ ] **interactive mode!!!**
  - [ ] use set metadata to pass between runs
  - [ ] will be done in graph, will edit job to re-produce new output
  - [ ] remove the plotting used for iterative mode in the task
    - [ ] replace the plotting - connected to interactive mode

### Anomalies need to support

- [x] creation (endpoint)
  - of a job
    - [x] add base package
      - [x] add files, make it part of the build
      - [x] add dependencies to the task build
  - connected signal set
    - [x] solve inserts
- [x] removal (endpoint)
  - of the job
  - connected signal set
    - [x] cascades
- [x] list (endpoint)
  - [x] link to the job edit
  - [x] allow edit
    - [x] solve seasonality edit issues
  - [x] link to the signal set
  - [x] link to the anomaly overview (endpoint)
  - [ ] better icons
  - [x] graphing component
    - [x] combine existing components to produce what is in the specs
      - [x] add elements
      - [x] add logic
    - [x] add related tooltips on hover
- job
  - [x] task package
  - [x] output saved to db
    - [x] signal set created
    - [x] save signals/records
    - [x] get full output
  - [ ] input loop for fetch
  - [ ] save using bulk
