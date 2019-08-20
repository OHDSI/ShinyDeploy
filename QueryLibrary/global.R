library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(SqlRender)
library(DatabaseConnector)
library(DT)
source("widgets.R")
source("helpers.R")
source("markdownParse.R")

queryFolder <- "./queries"
configFilename <- "settings.Rds"
allow_execute <- FALSE

queriesDf <- loadQueriesTable(queryFolder, "")
mdFiles <- list.files(queryFolder, recursive = TRUE, pattern = "*.md")
mdFiles <- paste(queryFolder, mdFiles, sep = "/")
