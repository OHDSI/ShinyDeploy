# OHDSI Gold Standard Phenotype Library Viewer

# Libraries
library(DT)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(visNetwork)

# Read index file
#open(con <- url("https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/master/Gold%20Standard/Index.rds"))
#gold <- readRDS(con)
#close(con)
gold <- readRDS(file.path("data","Index.rds"))

# Unpack into the phenotype and validation datasets
phe <- gold$Phenotype
val <- gold$Validation
