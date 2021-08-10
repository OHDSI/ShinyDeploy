# @file server.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(plotly)
library(shinycssloaders)
source("helpers.R")
source("plots.R")

source("modules/summaryTable.R")
source("modules/covariateSummary.R")
source("modules/settings.R")
source("modules/cutoff.R")
source("modules/discrimination.R")
source("modules/calibration.R")
source("modules/netBenefit.R")
source("modules/validation.R")
source("modules/download.R")

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  
  #=============
  # sidebar menu
  #=============
  if(useDatabase == F){
    output$sidebarMenu <- shinydashboard::renderMenu(shinydashboard::sidebarMenu(id ='menu',
                                                                                 addInfo(shinydashboard::menuItem("Description", tabName = "Description", icon = shiny::icon("home")), "DescriptionInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Summary", tabName = "Summary", icon = shiny::icon("table")), "SummaryInfo"),
                                                                                 # addInfo(shinydashboard::menuItem("Performance", tabName = "Performance", icon = shiny::icon("bar-chart")), "PerformanceInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Model", tabName = "Model", icon = shiny::icon("clipboard")), "ModelInfo"),
                                                                                 # addInfo(shinydashboard::menuItem("Settings", tabName = "Settings", icon = shiny::icon("cog")), "SettingsInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Log", tabName = "Log", icon = shiny::icon("list")), "LogInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Data Info", tabName = "DataInfo", icon = shiny::icon("database")), "DataInfoInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info")), "HelpInfo")
    ))
  } else {
    output$sidebarMenu <- shinydashboard::renderMenu(shinydashboard::sidebarMenu(id ='menu',
                                                                                 addInfo(shinydashboard::menuItem("Description", tabName = "Description", icon = shiny::icon("home")), "DescriptionInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Library", tabName = "Summary", icon = shiny::icon("table")), "SummaryInfo"),
                                                                                 # addInfo(shinydashboard::menuItem("Performance", tabName = "Performance", icon = shiny::icon("bar-chart")), "PerformanceInfo"),
                                                                                 # addInfo(shinydashboard::menuItem("Model", tabName = "Model", icon = shiny::icon("clipboard")), "ModelInfo"),
                                                                                 # addInfo(shinydashboard::menuItem("Settings", tabName = "Settings", icon = shiny::icon("cog")), "SettingsInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Upload", tabName = "Upload", icon = shiny::icon("upload")), "SettingsInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Data Info", tabName = "DataInfo", icon = shiny::icon("database")), "DataInfoInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info")), "HelpInfo")
    ))
  }
  
  # use the summary module
  resultRow <- summaryServer('sumTab', summaryTable)
  
  # this loads all the results
  plpResult <- shiny::reactive({getPlpResult(result,
                                             validation,
                                             summaryTable, 
                                             inputType, 
                                             val = F, 
                                             resultRow,
                                             mySchema = mySchema, 
                                             connectionDetails = connectionDetails)})
  
  
  covariateSummaryServer('covariateSummary',
                         plpResult,
                         summaryTable, 
                         resultRow, 
                         mySchema, 
                         con,
                         inputSingleView = input$singleView) 
  
  
  # Download plpresult
  output$plpResult <- shiny::downloadHandler(
    filename = function(){
      "plpResult.rds"
    },
    content = function(file) {
      saveRDS(plpResult(), file)
    }
  )
  
  # add settings module
  setingsServer('settings', 
                plpResult)
  
  # add cutoff module
  cutoffServer('cutoff', 
               plpResult)
  
  # discrimination module 
  discriminationServer('discrimination', 
                       plpResult)
  
  #=======================
  # cal for different evals
  calibrationServer('calibration', 
                    plpResult) 
  
  
  nbServer('netBenefit', plpResult) 
  
  
  #=======================
  # validation table and selection
  #=======================
  validationServer('validation', 
                   result,
                   validation,
                   plpResult = plpResult,
                   inputType = inputType,
                   useDatabase = T,
                   summaryTable = summaryTable,
                   resultRow = resultRow,
                   con = con, 
                   mySchema = mySchema,
                   connectionDetails = connectionDetails) 
  
  
  downloadServer('download')
  #=======================
  # get researcher info
  #=======================
  output$researcherInfo <- shiny::renderTable(plpResult()$researcherInfo)
  
  
  # LOG
  output$log <- shiny::renderText( paste(plpResult()$log, collapse="\n") )
  
  
  # HELPER INFO
  
  observeEvent(input$DescriptionInfo, {
    showInfoBox("Description", "html/Description.html")
  })
  observeEvent(input$SummaryInfo, {
    showInfoBox("Summary", "html/Summary.html")
  })
  observeEvent(input$PerformanceInfo, {
    showInfoBox("Performance", "html/Performance.html")
  })
  observeEvent(input$ModelInfo, {
    showInfoBox("Model", "html/Model.html")
  })
  observeEvent(input$LogInfo, {
    showInfoBox("Log", "html/Log.html")
  })
  observeEvent(input$SettingsInfo, {
    showInfoBox("Settings", "html/Settings.html")
  })
  observeEvent(input$DataInfoInfo, {
    showInfoBox("DataInfo", "html/DataInfo.html")
  })
  observeEvent(input$HelpInfo, {
    showInfoBox("HelpInfo", "html/Help.html")
  })
  
 
})
