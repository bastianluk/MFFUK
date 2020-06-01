# Intro to Software Engineering

<!-- toc -->

- [What is sofware](#What-is-sofware)
  * [Source code](#Source-code)
    + [Status quo is a disaster](#Status-quo-is-a-disaster)
      - [Complexity = comfort zone](#Complexity--comfort-zone)
        * [Measuring complexity - function point analysis](#Measuring-complexity---function-point-analysis)
      - [Inflexibility creates shadow IT](#Inflexibility-creates-shadow-IT)
    + [Hairball architecture](#Hairball-architecture)
    + [Software costs](#Software-costs)
    + [Software engineering](#Software-engineering)
- [Software processes](#Software-processes)
  * [Legacy software](#Legacy-software)
  * [SWING fundamentals](#SWING-fundamentals)
  * [SWING practice](#SWING-practice)
  * [Takeaways](#Takeaways)
  * [Software process model](#Software-process-model)
    + [Plan driven processes](#Plan-driven-processes)
      - [Waterfall model](#Waterfall-model)
        * [Req. analysis and definition](#Req-analysis-and-definition)
        * [System and software design](#System-and-software-design)
        * [Implementation adn unit testing](#Implementation-adn-unit-testing)
        * [Integration and system testing](#Integration-and-system-testing)
        * [Operation and system maintenance](#Operation-and-system-maintenance)
        * [Realization vs Operation and maintenance](#Realization-vs-Operation-and-maintenance)
        * [Problems with waterfall model](#Problems-with-waterfall-model)
        * [Usages of waterfall model](#Usages-of-waterfall-model)
      - [Incremental development model](#Incremental-development-model)
        * [Benefits](#Benefits)
        * [Drawbacks](#Drawbacks)
  * [Software specification](#Software-specification)
  * [Software design and implementation](#Software-design-and-implementation)
  * [Software validation](#Software-validation)
      - [Reuse-oriented software engineering model](#Reuse-oriented-software-engineering-model)
  * [Software evolution](#Software-evolution)
    + [Coping with change](#Coping-with-change)
      - [Software prototyping](#Software-prototyping)
        * [Benefits](#Benefits-1)
        * [Drawbacks](#Drawbacks-1)
      - [Incremental delivery](#Incremental-delivery)
        * [Incremental development X Incremental delivery](#Incremental-development-X-Incremental-delivery)
        * [Benefits](#Benefits-2)
        * [Drawbacks](#Drawbacks-2)
  * [Software assesment](#Software-assesment)
    + [CMMI - Capability Maturity Model Integration for Development](#CMMI---Capability-Maturity-Model-Integration-for-Development)
      - [Capability Levels](#Capability-Levels)
      - [Maturity levels](#Maturity-levels)
- [DevOps (external)](#DevOps-external)
  * [High performing teams](#High-performing-teams)
  * [Practices](#Practices)
    + [People and coordination](#People-and-coordination)
    + [Cross-functional teams](#Cross-functional-teams)
    + [Vertical slices](#Vertical-slices)
      - [Advantages](#Advantages)
    + [Small releases](#Small-releases)
    + [Versioning](#Versioning)
    + [CI - continuous integration](#CI---continuous-integration)
    + [Automated build and release pipeline (CD - continuous delivery)](#Automated-build-and-release-pipeline-CD---continuous-delivery)
    + [Infrastructure as Code](#Infrastructure-as-Code)
    + [Observability](#Observability)
    + [Testing (Beyond unit testing)](#Testing-Beyond-unit-testing)
  * [Takeaways](#Takeaways-1)
- [Agile development (external)](#Agile-development-external)
  * [Loosely translated from CZECH](#Loosely-translated-from-CZECH)
  * [Agile manifest](#Agile-manifest)
  * [Roles](#Roles)
  * [Definitions (Scrum)](#Definitions-Scrum)
  * [Lifecycle of a feature request](#Lifecycle-of-a-feature-request)
    + [Agile processes](#Agile-processes)
- [Requirements engineering](#Requirements-engineering)
  * [Req. elicitation and analysis](#Req-elicitation-and-analysis)
    + [Problem domain](#Problem-domain)
      - [Ubiquitous language](#Ubiquitous-language)
    + [Discovery](#Discovery)
      - [Models](#Models)
  * [Req. validation](#Req-validation)
    + [Req. reviews](#Req-reviews)
  * [Req. management](#Req-management)
    + [Changing requirements](#Changing-requirements)
  * [Requirements](#Requirements)
    + [User requirements](#User-requirements)
    + [System requirements](#System-requirements)
    + [Requirements imprecision](#Requirements-imprecision)
    + [Requirements completeness and consistency](#Requirements-completeness-and-consistency)
    + [Quality requirements](#Quality-requirements)
    + [Domain requirements](#Domain-requirements)
  * [Use-case diagrams](#Use-case-diagrams)
  * [Activity diagrams](#Activity-diagrams)
  * [State machine diagrams](#State-machine-diagrams)
    + [State](#State)
  * [Sequence diagrams](#Sequence-diagrams)
  * [UML class diagrams](#UML-class-diagrams)
    + [Conceptual models](#Conceptual-models)
    + [Design model](#Design-model)
  * [Problem domain](#Problem-domain-1)
    + [Subdomains](#Subdomains)
    + [Knowledge graphs](#Knowledge-graphs)
      - [RDF model](#RDF-model)
      - [Conceptual model as Knowledge graph](#Conceptual-model-as-Knowledge-graph)
- [Software Design](#Software-Design)
  * [Architectural design](#Architectural-design)
    + [Structure](#Structure)
    + [Documentation](#Documentation)
    + [Static architecture](#Static-architecture)
    + [Runtime architecture](#Runtime-architecture)
    + [Reasoning about quality](#Reasoning-about-quality)
    + [Architectural patterns](#Architectural-patterns)
      - [Microservices - domain driven architecture](#Microservices---domain-driven-architecture)
- [Testing](#Testing)
  * [Goals](#Goals)
    + [Kinds](#Kinds)
    + [Verification VS Validation](#Verification-VS-Validation)
      - [Inspection](#Inspection)
        * [Inspection and testing](#Inspection-and-testing)
  * [Testing flow](#Testing-flow)
    + [Stages](#Stages)
      - [Development testing](#Development-testing)
        * [Test-driven development (TDD)](#Test-driven-development-TDD)
        * [Benefits](#Benefits-3)
        * [Drawback](#Drawback)
        * [Test design strategies](#Test-design-strategies)
      - [Release testing](#Release-testing)
      - [User testing](#User-testing)
    + [A/B testing](#AB-testing)
    + [Stages in acceptance testing process](#Stages-in-acceptance-testing-process)
    + [Usability testing](#Usability-testing)
      - [SUS - System Usability Scale](#SUS---System-Usability-Scale)
        * [Scoring](#Scoring)
- [Evolution](#Evolution)
  * [Types](#Types)
  * [Costs](#Costs)
  * [Software re-engineering](#Software-re-engineering)
  * [Preventitive maintenance by refactoring](#Preventitive-maintenance-by-refactoring)
- [Project managment](#Project-managment)
  * [Project success criteria](#Project-success-criteria)
  * [Software management distinctions](#Software-management-distinctions)
  * [Activities](#Activities)
    + [Risk management](#Risk-management)
      - [Process](#Process)
    + [Project planning](#Project-planning)
      - [Stages](#Stages-1)
    + [SW Pricing](#SW-Pricing)
    + [Project scheduling](#Project-scheduling)
      - [Representation](#Representation)
    + [Price estimation methods](#Price-estimation-methods)
      - [COCOMO](#COCOMO)
- [Legal - copyright (external presentation)](#Legal---copyright-external-presentation)
  * [IN CZECH ONLY](#IN-CZECH-ONLY)
  * [Autorskoprávní ochrana](#Autorskoprávní-ochrana)
    + [Právo duševního vlastnictví](#Právo-duševního-vlastnictví)
      - [Autorské právo](#Autorské-právo)
        * [101 o AP](#101-o-AP)
        * [AP v informačních sítích](#AP-v-informačních-sítích)
        * [Veřejné licence](#Veřejné-licence)
      - [Práva průmyslová](#Práva-průmyslová)
      - [Autorské právo a software](#Autorské-právo-a-software)
      - [Licencování](#Licencování)
      - [DRM](#DRM)
      - [Trestně právní ochrana software](#Trestně-právní-ochrana-software)
      - [Smlouva o zhotovení software](#Smlouva-o-zhotovení-software)
      - [Cloud computing](#Cloud-computing)
  * [Patentová ochrana software](#Patentová-ochrana-software)
    + [Klíčové věci](#Klíčové-věci)
    + [Patent wars (v USA)](#Patent-wars-v-USA)
  * [Právní ochrana databází](#Právní-ochrana-databází)

<!-- tocstop -->

## What is sofware

- executable commands in a programming language


- it defines behavior of computer and conveis and manipulates information


- set of instructions (program code) providing desired behavior using different data structures
  - both of them come fronm higher level compiled language (program code ~ what actually runs on the CPU) ((Cpp)source code => object files or libraries => linker => program code VS (Python)source code => interpreter => program code VS (Java)source code => compiler => byte code => interpreter => program code)

### Source code

Must be...
- designed
- programmed
- tested
- delivered
- maintained
... by someone

Real world into 3 parts (problem domain)
1. stakeholders and the features they require - someone with interest in the software - investors, managment, users, dev team as well...
2. constraints - they exist - laws, budget, deadlines - can only be negotiated
3. real-world entities to be represented - any "thing" or concept


that translates to (representation - problem domain abstraction)
- 1 => features specification (use cases / user stories...)
- 2 => constrains specification (use cases / user stories...)
- 3 => conceptual model (conceptual diagram)

1 + 2 => code which manipulates with ...
3 => ... data stractures

what is hard is making to code modifiable
=> application-centric software - everything is done quickly and using own/specific things which blurs the mapping

![8b429be4.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/8b429be4.png)

Data as a second class citizen
- code is the main thing, datastructures come from the code
- problems with migrations
- data is copied, not shared

Inflatable application-centric software (monolith)
- problem with maintainability, responsability
- problem with contracts - expiration, budget/money
- legacy software issues

Integration of overlapping software is complicated - new and new software systems are added and they exchange data "somehow"

#### Status quo is a disaster
- adding more and more to "the web"
![04a48be1.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/04a48be1.png)
![51370bd5.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/51370bd5.png)

more code -> more faults
- all code has faults

more code -> more maintenance
- less code is better

more code -> more difficult to change because of environment changes

more code -> more expenses


##### Complexity = comfort zone
all requests go through small number "high priests" - more experienced senior members that think they know "whats good for the software"

small number of people can absorb the complexity and they become untouchable (bus factor)

silos increase complexity
software ~ informqtion silo
- information manipulated by the software is trapped in the data structures of the silo

40% - 70% of IT budgets are spent on system integration
- integrating or replacing silos

###### Measuring complexity - function point analysis
it looks at what the complexity and number of data structures that flow in/out the system is.

![ebad4ae5.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/ebad4ae5.png)

it uses empirical constants as multipliers, input the numbers into a table and get a "score"
strong focus on data structures (double number of data structs, double complexity)

##### Inflexibility creates shadow IT
software is hard to change
in enterprises usually monolith -> even harder to change
==> extensions come as shadow software
- adding additional attributes to records or combining data from more resources... (excel sheets)
- usually not documented
- results not returned to the source code


#### Hairball architecture
- result of ad-hoc solutioning

#### Software costs

it is more expensive than hardware

maintenance > development cost

soft-ing focuses on both of the costs and their efficiency


#### Software engineering 
- is concerned with theories, methods and tools for professional software development
- takes into account all the aspects of software production - project managment, cost of dev tools...

- focus on being able to produce reliable and trustworthy systems economically and quickly as the demand rises (more and more things rely on software)

---
## Software processes

- process - collection of activities, actions and tasks that are performed when some work product is to be created
- activity - stives to achieve a broad objective
- action - set of tasks that produce major work product
- task - focuses on a small but well-defined objective

Process defines who does - what, when and how - to reach a certain goal.

In SWING - process is NOT a rigid prescription
- adaptable, can be picked and choosen from
- intent always on being efficient with time and money

Cycle:
1. specification - process of finding out what should be developed - the needs, constraints, features etc
2. development - implementing what the customer want
3. validation - check if step 2 reflects step 1
4. evolution - maintenance to reflect the changing market, demands and requirements

Different view (Pressman):
1. communication - 1
2. planning - 1 and 2
3. modeling - 1 and 2
4. construction - 2 and 3
5. deployment - 3 and 4

No universal tool exists - too many different kinds of software

### Legacy software

- old, often outdated software
- usually developed decades ago but were modified to keep up with the requests for features - it becames difficult to replace and further modified
- poor documentation, only few people know how they work
- VERY difficult to replace

### SWING fundamentals
- software should be developed using a managed and understood development process
- availability and performance are important for all types of software
- understanding and managing the software specification and requirements is important
- where appropriate - reusing existing software is better than "reinventing the wheel"

### SWING practice

- understand the problem (communication and analysis)
- plan a solution (solutioning, modeling)
- carry out the plan (coding/development)
- examine the results (testing)

### Takeaways

- software ~ code and data structures that emerge froim feature requirements, constrains and realworld concepts
- software ~ logical thing and product of team work
- each software today is a part of system of systems

- complexity grows with the number of and complexity of data structures

- SWING is about methods of doing software effectively

- software is a result of a collective software process

- software process models recommend how the proceess should look like

### Software process model

~ abstract representation of a process - presents a description of a process from some particular perspective/POV

Software process help with:

- human understanding and communication/coordination
- managing the project
- measuring and improving quality of the products
- having a basis for automated support of process execution

SW Process desriptions
- description of activities included, or specifications, roles...


- Plan driven processes - planned in advance and progress is measured against the plan
- Agile processes - incremental planning and it is easier to change if needed (customer req. change etc)

In practice - most pracctical processes use both

There is no right way to do it

#### Plan driven processes

##### Waterfall model

- separate and distinct phases of specification and development
- also called linear or predictive model

![2e13a14f.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/2e13a14f.png)

###### Req. analysis and definition

- constraints, goals, services are established by consultation with stakeholders
  - introductory study

- after that - defined in detail and structured
  - analysis, serves as system specification

###### System and software design

- allocation of requirements to either HW or SW systems by establishing overall architecture
- it is important to identify and describe the fundamental software system abstractions and their relationships
- may also include interface designing and algorith specification in detail

###### Implementation adn unit testing

- realize the design
- verification that the software units work correctly

###### Integration and system testing

- units are put together and tested as a complete system to ensure requirements are met
- after testing the SW system is delivered

###### Operation and system maintenance

- longest phase
- system is installed and put to practical use
- maintenance ~
  - correct errors
  - improve implementation
  - enhance the system as new req. are discovered

###### Realization vs Operation and maintenance

Cost of realization is much smaller than (10 times) than operation or maintenance costs

###### Problems with waterfall model

- difficult to respond to changing customer requirements

###### Usages of waterfall model

- when requirements are well-understood and changes are limited
- plan driven nature helps coordinate the work in large distributed teams
- the development process is visible

##### Incremental development model

- specification, development and validation are interleaved
- may be plan driven or agile
- also called adaptive model

![1bac984b.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/1bac984b.png)

###### Benefits

- cost to change something because of req. is reduced
- easier to gather feedback
- rapid delivery and deployment of usefull parts is possible

###### Drawbacks

- development process not visible
  - producing documentation is not cost effective in rapid development
- code degradation because of rapid development
  - money has to be spent on refactoring

### Software specification

Consists of
- feasibility study - is it even feasible?
- requirements elicitation and analysis - what is needed?
- requirements specification - how does "what we need" look?
- requirements validation - validation


- if you cannot update it continusly, you should not have it

![032065d8.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/032065d8.png)

### Software design and implementation

- turning the specification into an executable system


- design - structure that realises the specification
- implementation - translate this structure into an executable program

(usually inter-leaved)

![639431c1.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/639431c1.png)

### Software validation

- shows that a system conforms to its specification
- involves
  - checking processes
  - review process
  - system testing

![ef17159d.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/ef17159d.png)

![57cf552c.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/57cf552c.png)

##### Reuse-oriented software engineering model

- the system is assembled from existing components
- may be plan driven or agile

### Software evolution

- reflecting new requirements and changes in existing requirements
- no real line between development and evolution
- software must be inherently flexible and must be able to change as req. change

#### Coping with change

- it is inevitable
- leads to rework


- change avoidance - make a prototype to make requirement more detailed based on a demo and develop the full version based on that
- change tolerance - designed so changes can be made

##### Software prototyping

- can be used in multiple stages
  - pre-initial version to help with specification
  - during development
  - back to back testing on different versions

![b5b8411b.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/b5b8411b.png)

- usually leaves out functionality
- uses prototyping tools or languages

###### Benefits

- helps with specification
  - better articulation by stakeholders
- improved maintainability if you know where it will be changed

###### Drawbacks

- not used in the same way as final system
- users may not understand it is not the final system
- management might be pushing on developers


-> only throwaway prototypes should be used
-> non executable prorotypes - mockups (wireframes) of UI etc

##### Incremental delivery

- development and delivery are broken down into increments
- each increment delivers part of the required functionality
- allows for prioritization 

###### Incremental development X Incremental delivery

- develop the system in increments
X
- deploy increments for use by end-users

// reference to Devops (external)

###### Benefits

- customer value can ve deliver earlier
- early increments may act as prototype
- lower risk of failure
- high priority services receive most testing

###### Drawbacks

- can be difficult to correctly abstract whats common for all increments
- it can be a problem for the organization - they might need the full system (governments etc.)
- can be difficult when a replacement system is being developed - you need all the functionality at once


### Software assesment

- used to evaluate the content of a software process by a standardized set of criteria
- important for some organizations - "you need to be certified to do work for X"

#### CMMI - Capability Maturity Model Integration for Development

- collection of best practices to improve processes
- CMMI livels
  - capability levels - rating of each process of interest
  - maturity levels - rating of the organization

##### Capability Levels
- Level 0 - incomplete process
  - is not performed at all, may not be performed or in only partial
- Level 1 - performed process
  - accomplishes the needed work to produce work products
- Level 2 - managed process
  - is processed (level 1)
  - is planned
  - is monitored, controlled and reviewed
  - is evaluated for adherence to its description
  - is executed in accordance to a policy by skilled people ith adequate resources to produce controlled outputs
  - involves relevant stakeholders
- Level 3 - defined
  - is managed (level 2)
  - is tailored from the organizations set of standard processed
  - has a maintained process description
  - contributes process related experiences to the organizational process assets (knowledge base)

##### Maturity levels
- Level 1 - intial
  - mostly adhoc and chaotic
  - organization doesnt provide a stable environment to support processes
  - success depends on the people
- Level 2 - managed
  - all processes are managed (cap. level 2)
  - projects are performed and maintained accordinf to their documented plans
  - status of work is visible to management
- Level 3 - defined
  - all processes are defined (cap. level 3)
  - set of standard processes is estasblished an improved over time
- Level 4 - quantitatively managed
  - organization establishes quantitative objectives for quality and process performance
  - performance is controlled using statistical or other quantitative techniques
- Level 5 - optimizing
  - continues improvementens based on the measerements from level 4
  - performes evaluarion using data collected from multiple projects
  - gaps are used to drive org. process improvement

---
## DevOps (external)

DevOps is a set of practices that combines software development (Dev) and information-technology operations (Ops) which aims to shorten the systems development life cycle and provide continuous delivery with high software quality.

![ed99bc9f.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/ed99bc9f.png)

For now - no right way of doing it
-> experiments (thousands per year)

### High performing teams

- delivery lead time
  - time between merge and deploy
  - good value - less than one hour
- deployment frequency
  - number of deploys per time
  - good value - on demand
- time to restore
  - when something fails, how fast do you recover
  - good value - less than one hour
- change fail rate
  - how often a change fails
  - good value - 0-15%

**We want**:
- speed
- safety
- scale

enablers:
- automation
- safety nests (tests)
- infrastructure as code
- CI ~ continuous integration
- people and coordination

### Practices
> Agile is about fail quick!

#### People and coordination

Usually:
- FE and BE dev teams
- QA
- Operations

One hands off work to the next
- Pros
  - people can be specialized
  - economies of scale
  - consistent work
- Cons
  - handoffs cause delays
  - lack of ownership
  - lack of information
  - bottlenecks - if one team is understaffed or overwhelmed
  - local optimization is not good

=>
#### Cross-functional teams

- team with different skills and functions with common goal
- team must be self-sufficient

Pitfalls
- not everybody must do everything - access to production
- DevOps is not developers having root or getting rid of Ops team
- heroism - example: maybe one guy does everything and noone knows how to maintain the code
- low bus factor

#### Vertical slices

Do not build database layer, then API/logic layer, then......

Do it per function
- add whats needed to the DBs, then add the logic and UI for the function
- small increments can be tested and monitored
- finish a slice end to end

##### Advantages
- Integrate early
  - discover problems early
  - trigger ops dependencies early
- Deliver ASAP
- Adapt to changes

#### Small releases
- use feature flags (toggles)
- smaller changes can be maintained easily

#### Versioning

USE VCS!!!
- at least one branch releasable

#### CI - continuous integration

- merging and testing code several times a day
- ![5439b030.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/5439b030.png)
- TeamCity, TravisCI, AzureDevOps

#### Automated build and release pipeline (CD - continuous delivery)

- feature freeze, merge, build, release
- faster, safer, doce serves as living documentation
- bus factor is not an issue

- at least have a script, tooling or pipeline as code
- has to have feedback loop (testing, alerting, ...)
- reproducable
  - docker helps

#### Infrastructure as Code

- defining infrastructure through source code -> can be treated like any software
- code can represent environment configurations

- declarative formats - express target state
  - idempotency
  - known configuration at/after deploy
  - living documentation
  - helps repeatability, recovering etc

- scaling is easier

- not only sysadmins know the "how-to"

- dont write environment specific code

- tools
  - puppet, chef, ansible
  - terraform
  - kubernetes (docker)
  - CI/CD tools

#### Observability

- deploys need to be transparent/not hidden - asses if it was successful
- alerting - zapier
- metrics - newrelic
  - request rate
  - failures
  - latency
- centralized logging - insightops
- tracing - flow of requests


#### Testing (Beyond unit testing)

- integration testing
- synthetic testing
  - run test directly in the working environment (on `prod`)
  - Sellenium
- chaos engineering
  - try breaking stuff - see if after one instance if turned off, it works, clients notice etc...

- docker helps
- automation
- helps with designing the system right


### Takeaways

- cooperation and independent teams > silos
- small, vertical releases
- good engineering practices
  - CI
  - CD
  - Infrastructure as Code
  - Observability
  - Testing (Beyond unit testing)

Maybe the whats needed is change in architecture
- loose coupling is what matters
- the boundaries should respect problem domain

Domain driven design mentioned


---
## Agile development (external)

### Loosely translated from CZECH

codename "Mews"

- mainly iterative
- allows to react to changing
  - environment
  - demand
  - bugs
- is good in long running projects

### Agile manifest

- individuals and interaction > processes and tools
- functioning SW > exhaustive documentation
- interaction with the client > negotiation about the contract
- reacting to changes > sticking to a plan

### Roles

- product owner/manager - produkťák
  - picks the direction of a team
  - prepares tasks
  - evaluates business goals
- scrum master ~ techlead
  - overlooks the processes - agile != chaos
  - guards the team from outside influence
  - end goal is for a team to become self-suffucient

### Definitions (Scrum)

- sprint - short cycle of development
- backlog - one-dimentionally sorted list of stories
- story - product request; WHO wants WHAT and here is WHY
- story points - how difficult is it to implement a story - fibbonaci numbers
- velocity - average number of story points produced by a team in one sprint
- scrumboard - board with whats being done in the current sprint

### Lifecycle of a feature request

- initial request
  - always ask why!
  - WHO wants WHAT and here is WHY
  - feature should be presentable (visually)
  - separate to smaller tasks (add DB layer changes, design, BE, then FE - all separate tasks)
    - better ability to predict the story points
  - features don't have to be perfect, but have to be modifiable
    - prototyping is encouraged
- planning
  - time evaluation is never precise - has to be evaluated based on the team, even meetings during the week are a factor
  - compare stories against each other
  - velocity can be evaluated only based on previous data
  - advantages of story points
    - stability of "old" evaluation
    - speed of evaluation
    - independent on the level of developer
    - independent on time
  - grooming
    - process of evaluation
    - place for additional questions
    - ballpark estimation is enough - its better than anything else
  - priotization
    - based on business value
      - potential income
      - potential savings
      - legislation constraints
      - deadlines or other promises to stakeholders
    - "value for money"
    - ![db6eefb3.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/db6eefb3.png)
  - actual sprint planning
    - dont burnout the devs!
- development
  - small functional incerements that works 100%
  - infrastructure - important questions:
    - local environment
      - how long does it take to implement a junior dev
      - can I have local functioning system
      - how long does it take to find a bug in configuration
    - shared environment
      - where do I get testing fata
    - virtualization
      - local
      - cloud based
  - automated tests
    - prevent future bugs
    - did I break already working code?
    - ![bf584165.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/bf584165.png)
  - refactoring
    - change of code without the change of functionality
    - code degrades over time
  - CI/CD
  - important points
    - VCS
    - coding standards
    - code review
      - is important
    - extreme programming
      - pair programming
      - test driven development
- handover
  - feature (demo) environment so someone can test it/look at it
  - communication is key!!! - both dev to dev and dev to PM
- release
  - deployment has to have a plan to rollback
  - log monitoring
  - marketing has to know about new cool features
- evaluation
  - were the business goals fullfilled?
    - use of google analytics to monitor usage of the feature
  - can we expand on this?

#### Agile processes

- further explained in separate presentation


---
## Requirements engineering

process of establishing
 - services
 - constraints

Common:
- req. elicitation and analysis (req. discovery)
- req. validation
- req. management
Usually iterative and interleaved, not discrete

### Req. elicitation and analysis
Problems:
- Stakeholders dont know what they want
- They know but they dont know how to express it
- Conflicting requirements
- They use domain specific language you don't know
- the environment changes

Steps:
- req. discovery (with the stakeholders)
- req. classification and organization - structurization
- prioritisation and (re)negotiation - solve conflicts
- req. specification - document the outcome via diagrams etc.

#### Problem domain

~ subject area in which a problem is solved

- usual cause of failure - not code development itself
- it is needed to learn the language the stakeholders use
##### Ubiquitous language

- language based on the problem domain and the (UML class) diagrams
- it is always evolving as the experts and stakeholders explain more and more terms

- can be used in code to engrave it (naming)

#### Discovery

![102cd98f.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/102cd98f.png)

dev team + stakeholders => draft business use-cases
dev team + domain experts => translate the use-cases to understandable terms for devs which can later present the conceptual model to stakeholders

iterative process (most likely never finished)

techniques:
- interviewing the stakeholders
  - closed based on predermined questions X open ~ open
  - necessary
    - be open minded
    - avoid preconceived ideas
    - you need to be proactive
      - ask powerful questions
      - proposal requiremnts
      - show sketches, prototypes...
  - not good for understanding the whole problem domain
  - interview ==> prepare scenarios, usecases etc ==> talk to stakeholders and domain experts and iterate (try to deepen problem domain understanding with every step)
- powerful questions
  - context free question
    - who is behind the idea, who will use it, where is the need for it
  - get better understanding of the problem
    - how would you characterize good output, what problems will this solution address
  - solidify the answers you got
    - are you the right person to answer this, is it official, are my questions relevant
- sketching
  - use whiteboard and do it infront of the stakeholders
  - informal
  - iteration is easy
- rapid prototyping
  - deeper version of sketching - using mockups (of visual elements)
  - can be hand-drawn
- event storming
  - workshop for all 3 parties - devs, stakeholders, domain experts
  - quickly build an understanding of the problem domain
  - large space for visual modeling
  - using stickers, whiteboards etc. describe a system consisting of:
    - domain events
    - commands
    - components
    - make them into aggregates
  - do it untill no questions are present
  - usually group of 5 people ~  opinions about everything
  - very helpful to establish the base for ubiquitous language

##### Models

only a snapshot - it should change over time

- scenario based - UML use case diagram
- information oriented - UML class diagram
- flow oriented - data flow diagram (can be partialy replaced by UML activity, state, sequence diagrams)
- behavioral - UML activity, state, sequence diagrams

### Req. validation

concern that the requirements define what the customer really want
can be costly to address after delivery

Important:
- validity
- consistency
- completeness
- realism
- verifiability
- comprehensibility
- traceability
- adaptability

Techniques:
- reviews
- prototyping - similar to before
- test-case generation - can help with testability

#### Req. reviews

should be while requirements definition is being formulated
stakeholders, devs, domain experts involved

### Req. management

managing the changing requirements
keeping links between them

#### Changing requirements

Causes:
- business or technical environment changed
  - new hardware, integration with a different system...
- the ones who pay and the end-users have different priorities
  - end-users may come up with different feature requests
- large systems have many end-users with different requirements which might be contradictory >> final req. are a compromise

### Requirements
- something a stakeholder wants from the system
- abstract statement or mathematical expression

#### User requirements
statements in natural language - services that the system should provide
written for managment, customers/clients, their end users...

#### System requirements
structured document - detailed descriptions of system functions, services and operational constraints
defines what should be implemented
written for endusers, engineers, developers

Further categorization:
 - functional req. - what services should be provided by the system, behaviour and integration with user
 - quality reg. - performance, availability, security, modifyability, testability
 - domain req. - constraints given by the domain - non negotiable with the client

#### Requirements imprecision
- ambiguity - diferent view between customer/user (more loose - usually contains some unspoken relations in results etc) and developer (usually more exact) for example

==> needs to be discussed

#### Requirements completeness and consistency

Should be complete and consistent - practically impossible

Waterfall model mentioned

- Complete ~ include descriptions of all facilities required
- Consistent ~ no conflict or contradictions between the descriptions

#### Quality requirements
- if not met - system might not be usable eeven if it has all the functions
- may affect the architecture, not just components


- product req. - the product must behave in a way described
- organizational req. - not around the product, but process of creation
- external req. - legislative req., interoperability req.


![82f2ac36.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/82f2ac36.png)


- goals - general intentions of the user such as ease of use - and others are described => helps developers
- has to be verifiable (speed, size, ease of use, availability, robustness)

#### Domain requirements

- example - train system needs to take into a count different weather conditions when calculating braking
- usually needs to lead to a collaboration between experts in the field and soft. eng.

Domain driven design

- usually implicit requirements because "they are clear" to experts

Sofware requirements document is the official statement of what is required

IS NOT a design document

Should contain WHAT and WHY but NOT HOW
What it should do
Why it should do it

They can be a part of the contract for the developers that make the system >> they need to be as complete as possible

Ways of writing:
- natural language
- structured natural language
- graphical notations
- mathematical specifications

Use words like:
- shall - mandatory
- should - desirable

Structured req.:
- Function/service
- Description
- Inputs
- Sources
- Outputs

- action
- pre-condition
- post-condition

Scenario based specification ~ user-system interaction description
 - usually UML diagrams are used
   - use-case diagrams
   - activity diagrams
 - usage scenario - real-life example of the system can be used
 - starting situation
 - normal flow (including concurrent actions etc)
 - description of what can go wrong
 - description of the state of the system when the scenario finishes

### Use-case diagrams

Contain:
- subject 
  - represents a system under consideration
- actors
   - users and other systems that may interact with a subject
- use-case 
  - specifies a behavior performed by the subject which yields observable results of value for actors
  - extend
    - relationship from extending (can be inserted) to extended use-case
  - include
    - relationship from including to included use-case (included is inserted to including)

### Activity diagrams

activity = behavior specified as a graph

Multiple columns - user and system(s)
Contain:
- action node - lower level steps in the overall activity
- object node - holds data - input to or output from executable nodes
- control nodes - used for sequencing of executable nodes
join waits for other branch, merge just merges

### State machine diagrams

model behaviour of a system as a finite state machine
 - describes possible states of the system
 - transitions triggered with events

usually a complement to activity diagrams
usefull for testing the system

state machine - represents system or its parts
contains:
- vertices - states/pseudostates
- edges - transitions (one or more triggers) between vertices

![c046e652.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/c046e652.png)

#### State
- models a situation in which system exists and doesn't change on its own
- state is stable and can be changed only by an event
- name should imply the invariant condition that is present

Pseudostate (initial, terminate... further more fork, join):

### Sequence diagrams
focuses on message interchange between a number of lifelines

- lifeline - instance of actor or software component with its existance/state of being active in the system
- arrow between lifelines
  - full is a message from/to lifeline
  - dashed is a response to previous full arrow in the other direction
  - can be conditional
  - can be synchronous or async
- combined fragment - eg for alternative message flow - a rectange divided to two and only one thing happens based on the condition
- 
### UML class diagrams

models object from the structural point of view - only "kinds of" objects (like in OOP) and only important ones

uses:
- classes
  - represents a set of objects which can be categorized with the same set of characteristics
  - have:
    - attributes (properties)
      - structural characteristic
      - [+] is public, [-] is private
      - multiplicity [m...n] (instead of list?)
    - operations (methods)
      - behavioral characteristic
      - [+] is public, [-] is private
- specialization - "inheritance" between classes
- relationships - associations
  - multiplicity [m...n] X associated with m..n Ys
  - name of role
  - read from the opposite end
  - navigatability specified >> easy to access all Y for the X
- association class
  - attributes of the association
  - ![89c10073.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/89c10073.png)
  - usually can be rewritten: ![023b0d8c.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/023b0d8c.png)

#### Conceptual models

- solvable by UML diagrams

Trying to model the domain of our problem:
- important entities
- their properties
- relationships between entities

#### Design model

this is where UML classs diagram ~ object in OOP source code, component, service, database row etc...

### Problem domain

using a singel model is problematic
- too complex or too generic

==> using subdomains (divide and conquer)

#### Subdomains

Kinds:
- generic
  - subdomain which exists in many enterprise software systems (reporting, notifications etc.)
  - existing off the shelf software
  - no modeling
- core
  - subdomain which makes the software system special (main idea behind a project)
  - own solution
  - detailed modeling
- supporting 
  - neither generic nor core
  - own solution with low invested resources or off the shelf
  - little or no modeling

![3b654dee.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/3b654dee.png)

May leed to too much independency - difficult communication between the domains

==> inheritance
![a8922613.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/a8922613.png)

![b352a8bb.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/b352a8bb.png)

![b3eb03c4.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/b3eb03c4.png)

#### Knowledge graphs

definition:
is a label oriented graph (V, E) which represents knowledge where:
- V is a set of vertices representing things and literals where each vertex representing a thing has unique global identity
- E is a set of labeled directed edges representing relationships between things or between a thing and a literal

can be interpreted by a machine - usually stored in its structered form in a database

Models:
- RDF graphs
- Property graphs

##### RDF model

based on a triple - (subject, predicate, object) - defines an endge between subject and object with the label predicate

IRI - internationalized resource identifier
![7585b272.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/7585b272.png)
Property IRI
![1ff23460.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/1ff23460.png)
Class IRI
![8d36ac15.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/8d36ac15.png)

RDF Class and property definition

##### Conceptual model as Knowledge graph

1-2-1 mapping

![2b335f14.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/2b335f14.png)
![12485eec.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/12485eec.png)
(continues)

benefits
- we get a machine interpretable representation of our conceptual models based on standards
- formal definition of semantics
- independant of visual representation
- it is a glues connection different artefacts

![c4af0b28.png](attachments/ecc0dcaa-056c-40a0-8dd1-b67e822a9bbf/c4af0b28.png)

other things can be represented as well:
- requirements
- test cases
- code units
- components
- APIs
- ...

---
## Software Design

- turning the specification into an executable system


- design - structure that realises the specification
- implementation - translate this structure into an executable program

![abb4d744.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/abb4d744.png)

### Architectural design

 - understanding how the software system should be organized and structered
 - link between requirements engineering and design
 - creates architectural model describing elements comprising the system and their relationships

- software architecture of a system is a set of structures needed to reason about the system, which comprise software elements, relations among them and properties of both


architecture = abstraction = skeleton
- it ommits cerain information about elements
  - dont want to deal with the complexity all at once

#### Structure

...is architectural if it supports reasoning about the required properties of the system
 - functional requirements
 - quality requirements (availability, modifiability, testability, ...)

#### Documentation

- informal (boxes and lines) or semiformal drawings (UML, Archimate)


#### Static architecture

elements = modules
- collections of artifacts which need to be developed (source code, data structures ...)
- decomposed to submodules
- have responsibility
- doesnt exist at runtime

both required functionalities and required qualities have to be represented

#### Runtime architecture

elements = components
- exist at runtime
- deployed independently
- other components, external systens or users interact with its interface (user interface - UI, machine interface - API...)

![f27a4367.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/f27a4367.png)

![74e010cb.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/74e010cb.png)

Can and probably will force you to change the static architecture
![092603d6.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/092603d6.png)

![a2fe076e.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/a2fe076e.png)

One app server is not scalable (qualities have to be represented):
![cb70b3fd.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/cb70b3fd.png)

#### Reasoning about quality

- leads to various architectural decisions
- they may introduce
  - new components to runtime
  - new modules to static architecture
- many arch. decisions decrease performance

#### Architectural patterns

(peer-to-peer; client-server...)

- express good practice in designing software architectures
- runtime arch. patterns show how to distribute computation among processes
- static arch. patterns show how to organize the code and other artifacts of the whole system or its part

##### Microservices - domain driven architecture

independent service for each function of the system - can be separately deployed, in different languages etc.

- UI - consumption by users
- infrastructure layer - logging, transactions
- application layer - communication so the intent is fullfilled, NO business logic or rules, provide interface to other services if needed
- domain layer - business logic
- persistence layer - data storage, reading etc

Transactional scripts - not bothering with calling all the layers - just persistence layer, transaction and persistence

![ed493578.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/ed493578.png)

"Onion"
![48886b07.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/48886b07.png)

Full example:
![4eb6aaab.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/4eb6aaab.png)

---
## Testing

- it shows that a software does what it is intended to do
- we execute software using real or artifical data and check the results (for errors)
- can only reveal PRESSENCE and NOT ABSENCE of errors
- is one of verification and validation techniques

### Goals

- demonstrate that the SW meets requirements
- discover situations with incorrect behaviour

==>
#### Kinds

- validation testing
- defect testing


#### Verification VS Validation

- verification
  - are we building the product right
  - does it conform to the specification?
  - inspection
    - analysing static system representation to discover problems
  - testing
    - executing system and observing its behaviour

- validation
  - are we building the right product
  - does it do what the user really wants?

##### Inspection

- people examinig the SW representation
- NO EXECUTION of the system
- good for detecting defects hard to reveal with testing
  - quality attrributes...
- can be done on incomplete SW

- design, as well as code and data structures, modifiability

###### Inspection and testing

- not opposing but complementary
- inspection cannot be automated and used for validation (cannot check if the user would want this feature)

### Testing flow

![49647a06.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/49647a06.png)

#### Stages

- development testing
- release testing
- user testing (production testing, hehe)

##### Development testing

~ all testing done by dev team

- unit testing
  - individual components (methods, functions, classes...) in isolation
  - as complete coverage as possible
  - all states, not just happy testing
  - ![0876ae5f.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/0876ae5f.png)
  - test parts (AAA)
    - Arange (setup)
    - Act (call)
    - Assert (assert)
  - casses
    - normal operation
    - problematic input to get expected error
- module testing
  - module ~ composite of interacting units
    - usually accessed by an interface
  - interface testng
    - detect fauults due to interface error
    - shared memory IFace
    - parameter IFace
    - massage passing IFace
    - errors
      - misuse - wrong order
      - misunderstanding - incorrect assumption about functionality
      - timing errors - too late, too early
  - integration testing
    - interaction between units or modules
    - test that
      - output of one is correctly consumed by other etc
  - should be designed by people NOT developing the system being tested
  - focus on common errors
- system testing
  - involves
    - combining/integrating modules (by different teams or off the shelf solutions) and testing them
    - testing stories
    - testing quality attributes
  - requirements based testing
    - examining requirements and developing tests for them
  - testing policies
    - exhaustive system testing is impossible -> policies on "what has to be cover no matter what" may be developed

###### Test-driven development (TDD)

- interleavng testing and code development
- tests before code
- code is developed incrementally
- you dont move until code passes tests

Activities:
- identifying the increament ("a piece of") functionality
- designing automated tests for it
- runing the tests (expected failure on new test)
- implement unctionality and re-run the tests
- once passing, repeat

###### Benefits

- code coverage
- regression testing
  - new change "didn't break" old code
  - expensive with manual testing
  - must pass before new change is commited
- simplified debugging
- system is being documented along the way

###### Drawback

- speed?

###### Test design strategies

- parition testing
  - identify groups of common inputs
  - input and output usually similar
    - can be split to groups
    - test cases should come from all groups
- guideline-based testing
  - use guidelines to choose test cases
  - there are often known problematic inputs
    - pick those to test
    - based on size (small and big), known fact that something generates a lot of error


##### Release testing

- form of system testing
- testing a particular release
- used to convice user the system si good enough for use
- "black-box" testing process where tests are only derived from specification

##### User testing

- users or customers provide input on testing
- cannot be replicated - you need real user
  - alpha - dev team + testers
  - beta - public users
  - acceptance - customers decide if it is acceptable for release

#### A/B testing

- similar to back to back testing
- usually in beta testing
- two variants (A and B) - one shown at random to users - and statistically determine based on reactions which one is better

#### Stages in acceptance testing process

- define criteria
  - before contract is signed, is part of contract
- plan acceptance testing
  - resources - time, budget; establish schedule
- derive the tests, run them, negotiate results
- accept or reject system

#### Usability testing

- evaluate how easy (or difficult) it is to use the system
- may involve writing user documentation
- system needs to recover from user errors

##### SUS - System Usability Scale

- 10 item questionnaire with fice response options
  - strongly agree ... strogly disagree
  - ![2c99d86b.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/2c99d86b.png)

###### Scoring

- odd items
  - `response - 1`
- even items
  - `5 - response`
-> `Qi in {0..4} for item i`

`Score = (Sum Qi) * 2.5` in `<0..100>`

SUS Score above `68` is above average
- `68` found empirically from 500 studies on SW systems

---
## Evolution

- about coping with change - implementing new functionality, improve availability
- majority of budged is used for this

![78680eb7.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/78680eb7.png)

initial => adding requests => fewer and fewer request lead only to servicing => phase out - "it just exists"

Program evolution dynamics
 - study of processes of system change by Lehman and Belady
 - identifies several "laws" which applu to large software systems
 - laws (empirically proven)
   - continuing change - program must change based on the environment and needs
   - increasing complexity - complexity only growing or constant
   - large program evolution - rate of development (size, speed, number of errors) is approximately invariant
   - organization stability - rate of development independent of the resources devoted
   - conservation of familiarity - the change is approximately same (no drastic changes)
   - continuing growth - has to keep growing or dissatisfaction
   - declining quality - unless modified, only declining

- software maintenance - modifying a program after it has been put into use
- doesn't normally involve major changes to the system's architecture

### Types

- repair software faults
  - corrective evolution (faults noticed by users ~ failures)
  - preventive evolution
- adapt software to a different environment
  - adaptive evolution
- add to or modify system's functionality
  - perfective evolution

![5c46bbca.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/5c46bbca.png)

### Costs

- affected by both technical and non-technical factors
- only increasing

Factors:
- team stability - people move on
- poor development practice - no motivation to keep working on it
- staff skills - maintained by less skilled people
- program age and structure - might be obsolete

![04cab914.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/04cab914.png)

Predicting changes
- requires understanding of the relationship between system and its environment
- tighly coupled systems require changes whenever environment changes
  - also depends on
    - number and complexity of system IFaces
    - number of inherently volotile system requirements
    - the business processses where the system is used

### Software re-engineering

- re-contruction or re-writinf part or all of legacy system without changing its functionality
- applicable if frequent maintenance is needed - might be better to overhaul it

### Preventitive maintenance by refactoring

- refactoring - imnproving software without adding functionality (works the same but is faster, better etc)
- includes:
  - modifying a programe to improve structure
  - reducing complexity
  - making it easier to understand
- slowing down degradation through change

![de89b4a5.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/de89b4a5.png)

---
## Project managment

- ensuring the software is delivered on time, in budget and according to specification

### Project success criteria
- deliver the software to the customer at the agreed time
- keep overall costs within budget
- deliver software that meets the customer's expectations
- maintain a happy and well-functioning development team

usually some of those go against one another
=> project manager needs to prioritize 

### Software management distinctions
- software is intangible
- many project are one-off's - no reference point
- processes are company specific - again no reference points

-> PM has to be very agile

### Activities
- planning - estimating and scheduling
- reporting - to customers and managers
- risk management - monitoring
- people management
- proposal writing - proposal to win a project etc - describing objectives

#### Risk management

- identifying risks and drawing up plans to minimise their effect
- risk ~ a propability that an adversity comes up
  - project risk - affect resources and schedule
  - product risk - quality of what is being delivered
  - business risk - affecting organization that is developing the SW - even low number of users

![f4207da2.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/f4207da2.png)

##### Process

- identification
  - technology risks
  - people risks
  - ogr. risks
  - req. risks
  - estimation risks
  - ![fa434794.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/fa434794.png)
- analysis
  -  asses effect/seriousness or propability of the risks found on a scale
  -  ![3f49f27a.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/3f49f27a.png)
- planning - minimise change to come up or its effect
  - avoidance strats
    - minimise the chance it happens
  - minimalization strats
    - minimise the effect it will have
  - contingency plans
    - once it happens, plan to contain the problem
- monitoring
  - asses the risk individually and regularly
  - use indicators for each risk

#### Project planning

- breaking down the work to parts and assigning them to people
- anticipating problems that might arise
- 
-> projet plan is created at the start of the project (roadmap)

##### Stages

- proposal stage - bidding to win the contract, estimates (time, budget)
- start-up phase - who works on what; change the plan according to priorities; resources are allocated accordingly

#### SW Pricing 

- hardware, software, training, risks, people costs...
- factors
  - market opportunity
  - cost estimate uncertainty
  - contractual terms
  - requirements volotality
  - financial healt

#### Project scheduling

- deciding how work will be organized as separate tasks and when and how it will be executed

- factors
  - time
  - effort
  - (numer of, skill of) people
  - resources

Tasks should be parallelized as much as possible

- milestones - points in the schedule
- deliverables - delivered work products

##### Representation

- graphical
  - (road maps, calendars)
  - bar charts are used
- tasks shouldn't be too small
- dependencies should be visible

#### Price estimation methods

- plan based - on the base of the projecct deliveries, milestones, tasks
- analysis based - on the base of requirements analysis (usecases, concepts)
- empitical - COCOMO etc.

- the smaller the parts are the better the estimation can be

##### COCOMO

- needs the number of lines of code
  - requires senior engineers to take part
- it is well documented and independent model
- takes into account different approaches to development (reuse etc)
![86de442a.png](attachments/8ef379df-417f-4400-97f4-3da2e145ef1d/86de442a.png)

- works well on large projects

- maintenance is 10-20% per year of the total dev cost

---
## Legal - copyright (external presentation)

### IN CZECH ONLY


### Autorskoprávní ochrana

#### Právo duševního vlastnictví

- právní ochrana věcí, které vznikají duševní činností

##### Autorské právo

- není potřeba registrovat

- literární a jiná umělecká díla (i hudební improvizace)
- vědecká díla
- počítačové programy
- (databáze)

- prameny
  - Bernská úmluva - skoro všechny státy na světě - téměř nemožné změnit

###### 101 o AP

autorské právo = kontrola, efektivní proti všem, naprostá možnost se rozhodnout, co se s dílem stane - zveřejnit, nezveřejnit, jak, komu...

- dílo
  - je jedinečným výsledkem tvůrčí činnosti autora a je vyjádřeno v jakékoli objhektivně vnímatelné podobě včetně podoby elektronické, trvale nebo dočasně, bez ohledu na jeho rozsah, úček nebo význam
    - výjimka na jedinečnost - programy, fotografie, databáze - stačí originalita (fotka stejné věci například)

- dílo není (!!!)
  - námět/myšlenka (pouze vyjádření myšlenky)
  - denní zpráva nebo údaj sám o sobě
  - myšlenka
  - postup
  - principy, metody - patent related
  - objev - patent related
  - vědecká teorie
  - matematický vzorec

- dílo a jeho hmotný nosič
  - problém - virtualizace - šíření bez ztráty hodnoty (půjčení knihy)

Autor
- osobnostní práva (EU, ne tolik USA)
  - být uváděn jako autor
  - právo dílo zveřejnit
  - právo na nedotknutelnost díla
    - udělení souhlasu ke změně/k zásahu
    - autorský dohled
- majetková práva
  - rozmnožování díla
  - rozšiřování díla/rozmoženiny
  - pronájem díla/rozmoženiny
  - půjčování díla/rozmoženiny
  - vystavování díla/rozmoženiny
  - sdělování díla veřejnosti (šíření po internetu)

Práv se nedá vzdát ani je převést

Dá se licencovat
- specifický smluvní typ umožňuje užití díla podle podmínek v ní

Výjimky z autorského práva
- osobní užití (netýká se SW)

Zákonné licence
- citační, novinářská, úřední užití, parodie (memes), svoboda panoramatu (architektonicka dila - fotka mesta na socialnich siti)

Třístupňový test
- podmínka k užití výjimky
- musí jít o výjimku v zákoně
- dílo musí být užité běžným způsobem
- nesmí být nepřiměřeně zasaženo do práv autora (first copy is okay, sharing is not)


###### AP v informačních sítích

- digitalizace a distribuovaná komunikace -> "potenciální ubiquita"
- demokratizace tvorby a distribuce
- snadná možnost kopírování (= užití díla) -> rozpad tradičních distribučních struktur - prostředník vynechán
  - snaha o udržení pomocí
    - žalob
    - osvětových kampaní
    - lobbingu
    - DRM - digital rights managment
  - důsledky
    - doba ochrany - 70 let po smrti (filmy 125 let)
    - rozsah - nová práva
    - DRM occured
    - ochrana databází

###### Veřejné licence

změna smíšlení - vynucování otevřenosti a svobody oproti výhradnosti

Open content - 5R:
- Retain
- Reuse
- Revise
- Remix
- Redistribute

Creative commons
Open ... 

- umožnují sdílení pod určitými podmínkami
- vždy uvést zdroj
- neodvolatelné
- automatické ukončení při porušení


##### Práva průmyslová

- úřad průmyslového vlastnictví

- patenty
- užitné vzory < patent
- průmyslové vzory - ochrana designu - lahev od CocaCola
- ochranné známky - trademark - logos
- topografie polovodičových výrobků
- biotechnologické vynálezy

##### Autorské právo a software

terminologie
- software - něco čitelné pro výpočetní zařízení
- programové vybnvení počítače - veškerý software na disku
- počítačový program
  - autorskoprávně chráněný (!!!)


- chráněné jako literární dílo
  - jak strojový tak zdrojový kód (ne funkcionalita, algoritmy...)


- není požadavek na jedinečnost, musí být původní


- autor
  - hednotlivec
  - spoluatoři
  - kolektivní dílo
  - zaměstnanecké dílo
    - AP patří zaměstnavateli


- užití software
  - velice úzké
  - umožnění oprávněnému nabyvateli způsobem, ke kterému je určen, včetně opravy chyb
  - můžu si pořídit backup
  - můžu ho zkoumat a analyzovat, jak funguje
  - dekompilace
    - pouze za účelem interoperability
    - nelze k jiným úČelům - například konkurenční využití


##### Licencování

AP k dílu není možné převést

Podstata licence
- obecné svolení k užití
- úplatné/bezúplatné využití díla
- licence = svolení k užití
- rozdíl v chápání v Evropě a v Anglo-Americkém systému

Svobodné licence - FOSS - Free Open Source Software
- GNU GPL - copyleft
- FREE ~ svobodné a ne zdarma

EULA - End-user License Agreement
- clickwrap - tím, že clicknu a odsouhlasím
- browsewarp - tím, že užívám už souhlasím
- shrink wrap - koupě příležitosti k clickwrap

Vyčerpání práv
- doktrína prvního prodeje
  - prvním prodejem vlastnického práva v hmotné podobě je právo autora vyčerpáno - antikvariáty 
  -> neplatí pro digitální obsah (!!!) a pro software to je jinak
  - mám licenci si přečíst e-knihu, ne ji potom prodat

Pouze přeprodat jako celý balíček

##### DRM

technologický prostředek ochrany autorského práva
zakázáno obcházet

##### Trestně právní ochrana software

I v Budapešťské úmluvě o kyberkriminalitě
- každý stát musí mít jako trestný čin
  - neopravneny pristup k PC
  - poruseni AP
  - sireni child p.

##### Smlouva o zhotovení software

neexistuje - je pouze - Smlouva o dílo
nutné části
- specifikace - "jde to za zadavatelem"
- termín
- jak bude předáno
- cena
- práva k dílu
další
- analýza
- vývoj
- implmentace


##### Cloud computing

- SAS
- Platform as a service (Azure)
- Infrustructure as a service

- právnicky
  - SLA - service level agreement
    - garantovaný uptime etc
    - dlouha dokumentace

### Patentová ochrana software

> 101 vynechano

#### Klíčové věci

- software obecně není patentovatelný
  - pokud jde o nějaké širší technické řešení
    - software klíčový k funkci nějakého vynálezu
    - například
      - vylepšení výkonu HW
      - virtualizace obchodní metody, pokud je tomu tak popré a byly překonány technické obtíže
    - ABS patent

#### Patent wars (v USA)

- slide to unlock

Apple vs Samsung
- užitné vzory
  - tap to zoom
  - multitouch
  - bounce back

vzniklo to přehlcením patentových úŘadů, které patentovali všechno -> patent trolls



### Právní ochrana databází

> vynechano
 
Evropská specialita

Databáze = soubor nezávislých děl, údajů nebo jiných prvků, systematický nebo metodicky uspořádaných a individuálně přístupných elektronickými nebo jinými prostředky, bez ohledu na formu jejich vyjádření

DATA jako taková nejsou chráněná

Druhy chráněných databází
- jako autorské dílo
  - originální - struktura DB je původní (řazení dat apod)
  - kreativní - souborné dílo (výjimka)
- zvláštní práva pořizovatele databáze - obsah
  - nepřímá ochrana obsahu
  - vznik - ochrana investice do pořízení, ověření a předvedení obsahu

---