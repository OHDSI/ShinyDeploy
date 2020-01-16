<center>
<img src="https://www.ohdsi.org/wp-content/uploads/2015/02/h243-ohdsi-logo-with-text.png"
     alt="OHDSI Gold Standard Phenotype Library"
     height = 100/>
<hr>
<h1> Gold Standard Phenotype Library </h1></center>
<hr>

## Objective
The purpose of OHDSI's Gold Standard Phenotype Library is to enable members of the OHDSI community to find, evaluate, and utilize community-validated cohort definitions for research and other activities. The phenotypes which reside in this library are "Gold Standard" cohort definitions, which means that the entries are:

- **Designed** with best practices
- **Evaluated** with best practices
- **Documented** with best practices

## Library Analogy
Throughout the brainstorming process, we’ve frequently benefited by drawing analogies between this phenotype library and a brick-and-mortar library.

### Book = Phenotype
A library is filled with books. Our “books” are phenotypes, which we have defined as the following:

> **Phenotype** - as it pertains to observational research, an observable set of characteristics in health data about an organism. A phenotype’s purpose is the desired intent to identify members in a health dataset with the observed set of characteristics of interest. The observable set of characteristics can include conditions, procedures, exposures, devices, observations, etc.

### Chapter = Cohort Definition
Within each book is a collection of chapters. Our “chapters” are cohort definitions (phenotype algorithms) that are intended to approximate the phenotype of the book in which they are contained. We have adopted the following definition of what is meant of a cohort definition:

> **Cohort Definition** - A coded set of instructions for best approximating the desired intent of identifying members of a phenotype. Defines a set of members in health data who satisfy one or more criteria for a duration of time. Each phenotype could have one or more phenotype algorithms (e.g. T2DM broad, T2DM narrow). The instructions could be rule-based (heuristic) or computable (probabilistic). Heuristic based phenotype algorithms consist of rules and one or more concepts sets. Probabilistic phenotypes are implemented using a predictive model.

In this sense, each chapter is a distinct cohort definition. The existence of multiple chapters within the same book implies that there could be many variations of the same definition. This is because there is not a one-size-fits-all cohort definition for every phenotype. For example, there may be a need for a sensitive and specific definition of the same phenotype.

### Cohort = Applied Cohort Definition
When a cohort definition is selected and executed on a database, a cohort results. Due to differences in data, two people running an identical definition on separate databases will naturally end up with different cohorts even though they followed the same instruction set.

### Librarian = Admin
Libraries are run by librarians. In our context, we view librarians as volunteer members of the OHDSI community. Librarians have elevated access to the library’s staging area where reorganization and edits can occur, and they have the authority to pull definitions from the staging area into the official record. They are responsible for maintaining the integrity of the library by ensuring that documentation is complete and sufficient and that the library is properly organized. It is worth noting that librarians don’t pass judgements about the books/chapters themselves; for instance, it would be inappropriate for a librarian to prevent a cohort definition from entering the library on the basis of the validation metric values.

## Modes of Submission
There are currently 4 different ways to contribute data to the library, each with its own form:
- **Cohort Definition**: An instruction set defining how to capture a set of individuals from an OMOP CDM database
- **Validation Set**: A set of performance metrics detailing how frequently the cohort definition correctly identified individuals
- **Citation Usage**: An instance of a published resource that used a Gold Standard phenotype
- **Cohort Characterization**: A "Table 1" showing demographics of those who were chosen, as well incidence over time, to help users understand who the cohort definition is selecting and how temporally stable it is.

## Library Architecture
When you submit data to the library, it proceeds to a staging area in a Google Drive service account, where it will be reviewed by the librarians. This system supports peer-review to help double check the veracity of the submissions and to clarify any questions before acceptance of the content into the library.

After review, the librarians will publish the content to the [Gold Standard Phenotype Library GitHub page](https://github.com/OHDSI/PhenotypeLibrary). Since the application reads from this GitHub repository, this creates a circular process that supports the accumulation of evidence.

This process is illustrated with the following diagram:

<center>
<img src="https://forums.ohdsi.org/uploads/default/original/2X/6/69601509f4120ffc833fa4c1af3cef01b4353466.jpeg"
     alt="Gold Standard Phenotype Library Architecture"
     height = 400/>
</center>

### Shiny applications
This application exists as one of two applications. For those who are interested only in read-only activities such as viewing, inspecting, and downloading cohort definitions, please use the **viewer application**. For those who are interested in submitting cohort definition, validation, citation, and cohort characterization data to the library, please use this **submission application**.

- The viewer application (Coming soon...):
https://data.ohdsi.org/PhenotypeLibraryView/

- This submission application:
https://data.ohdsi.org/PhenotypeLibrarySubmit/

## Glossary
**Phenotype** - A phenotype, as it pertains to observational research using health data, is a pattern of observable characteristics for a set of people for a duration of time. These characteristics can include conditions, procedures, drug exposures, devices, observations, visits, cost information, etc.

**Phenotype Algorithm = Cohort Definition** - A phenotype algorithm is a coded set of instructions with the desired intent of identifying members of a phenotype in health data. Each phenotype could have one or more phenotype algorithms (e.g. T2DM broad, T2DM narrow). The instructions could be heuristic (rule-based) or probabilistic. A heuristic based phenotype algorithm consists of rules and one or more concepts sets. A probabilistic phenotype algorithm is implemented using a probabilistic model.

**Cohort** - A cohort instance or phenotype instance is a set of patients for a duration of time which result from the execution of phenotype algorithm instructions against health data.

**Concept Set** - A list of codes used to find records in the Common Data Model.

**Gold Standard Phenotype Algorithm** -  A "Gold Standard" phenotype algorithm is one that is designed, evaluated, and documented with best practices. The notion of “best practice” refers to the idea that the phenotype algorithm was held to specific standards of design and evaluation and meets all of the requirements OHDSI deems necessary in order to be included into the Gold Standard Phenotype Library (these requirements are under development).

**Gold Standard Phenotype Library** - A library of publicly available gold standard phenotype algorithms meant to enable members of the OHDSI community to find, evaluate, and utilize community-validated cohort definitions for research and other activities.
