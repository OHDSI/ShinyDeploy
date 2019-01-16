# @file Ui.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

ui <- shiny::shinyUI(shiny::fluidPage(theme = "mytheme.css",
    #shiny::titlePanel("Multiple Patient-level Prediction Model Viewer"),
	shiny::titlePanel(
		shiny::fluidRow(
			shiny::column(3, shiny::img(height = 50, width = 50, src = "logo.png")),
			shiny::column(9, "Multiple Patient-level Prediction Model Viewer")
		)
	),
    
    shiny::tabsetPanel(type = "tabs",
                       shiny::tabPanel("Summary Table", 
                                       shiny::fluidRow(
                                         shiny::column(3, 
                                                       shiny::h4('Filters'),
                                                       shiny::selectInput('devDatabase', 'Development Database', c('All',unique(as.character(allPerformance$devDatabase)))),
                                                       shiny::selectInput('valDatabase', 'Validation Database', c('All',unique(as.character(allPerformance$valDatabase)))),
                                                       shiny::selectInput('T', 'Target Cohort', c('All',unique(as.character(allPerformance$cohortName)))),
                                                       shiny::selectInput('O', 'Outcome Cohort', c('All',unique(as.character(allPerformance$outcomeName)))),
                                                       shiny::selectInput('riskWindowStart', 'Time-at-risk start:', c('All',unique(allPerformance$riskWindowStart))),
                                                       shiny::selectInput('riskWindowEnd', 'Time-at-risk end:', c('All',unique(as.character(allPerformance$riskWindowEnd)))),
                                                       shiny::selectInput('modelSettingName', 'Model:', c('All',unique(as.character(allPerformance$modelSettingName))))
                                         ),  
                                         shiny::column(8, style = "background-color:#F3FAFC;",
                                                       
                                                       shiny::div(DT::dataTableOutput('summaryTable'), 
                                                                  style = "font-size:70%"),
                                                       shiny::h3('Model Settings: ', shiny::actionLink("modelhelp", "help")),
                                                       DT::dataTableOutput('modelTable'),
                                                       shiny::h3('Population Settings: ', shiny::actionLink("pophelp", "help")),
                                                       DT::dataTableOutput('populationTable'),
                                                       shiny::h3('Covariate Settings: ', shiny::actionLink("covhelp", "help")),
                                                       DT::dataTableOutput('covariateTable')
                                                       
                                         )
                                         
                                       )),
                       shiny::tabPanel("Performance Plots", 
                                       shiny::h3('Problem:'),
                                       shiny::textOutput('info'),
                                       shiny::wellPanel(
                                         shiny::sliderInput("slider1", 
                                                            shiny::h5("Threshold value slider: "), 
                                                            min = 1, max = 100, value = 50, ticks = F),
                                         shiny::tags$script(shiny::HTML("
                                                                        $(document).ready(function() {setTimeout(function() {
                                                                        supElement = document.getElementById('slider1').parentElement;
                                                                        $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
                                                                        }, 50);})
                                                                        ")),
                                         shiny::tableOutput('performance'),
                                         shiny::tableOutput('twobytwo')),
                                       
                                       shiny::h4('ROC plot:'),
                                       plotly::plotlyOutput('roc'),
                                       shiny::h4('Precision recall plot:'),
                                       plotly::plotlyOutput('pr'),
                                       shiny::h4('F1 score plot:'),
                                       plotly::plotlyOutput('f1'),
                                       shiny::h4('Prediction score distribution:'),
                                       shiny::plotOutput('preddist'),
                                       shiny::h4('Preference score distribution:'),
                                       shiny::plotOutput('prefdist'),
                                       shiny::h4('Box plot:'),
                                       shiny::plotOutput('box'),
                                       shiny::h4('Calibration plot:'),
                                       shiny::plotOutput('cal'),
                                       shiny::h4('Demographic plot:'),
                                       shiny::plotOutput('demo')),
                       shiny::tabPanel("Model Plot", 
                                       plotly::plotlyOutput('covariateSummaryBinary'),
                                       plotly::plotlyOutput('covariateSummaryMeasure')),
                       shiny::tabPanel("Log",
                                       shiny::verbatimTextOutput('log'))
                       #, tabPanel("CovariateSummary", covSet, val-T-pop shiny::plotOutput('covSummary'))
                       
                       
                       
                       
                                       )
  )
  )
#)