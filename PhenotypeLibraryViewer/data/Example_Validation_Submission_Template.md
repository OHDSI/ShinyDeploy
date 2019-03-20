---
title: "Phenotype Validation Example Template"
author: "Author: Example Author"
date: "Date: February 26, 2019"
bibliography: example_bibliography_validation.bib
output:
  html_fragment # Must be a fragment in order to be properly embedded into the Shiny app
---

## Phenotype Title
Rheumatoid Arthritis

## Author(s) and Affiliations
Author 1, Author 2, etc.
Validated at Example University
xyz@email.com

## Date Validated:
February 26, 2019

[//]: # (Below, please include the validation process you used -- if automated, please describe the settings chosen; if manual, please list the questions you used in your chart review.)

## Validation Procedure Description:
We validated this phenotype via chart review using a panel of 3 expert rheumatologists, with majority consensus used to break ties. We used the following questions in our review:

- Has there been a report of joint pain in the previous 90 days before the index date?
- Has there been any prior history of rheumatoid arthritis in the past two years?
- Etc.
- Etc.

## Metrics
- Total: 400
- True Positives: 180
- True Negatives: 170
- False Positives: 30
- False Negatives: 20

## Other comments
e.g. We expected the phenotype to provide more female cases than male cases since rheumatoid arthritis, per [@kvien2006epidemiological], but in our random sample, they were approximately equal, which is perhaps concerning to how this phenotype operates.

# References
