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
library(shinydashboard)
# if(!require(shiny.i18n)){install.packages('shiny.i18n')}
# require(shiny.i18n)
addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}


ui <- shinydashboard::dashboardPage(skin = 'black',
                                    
                                    shinydashboard::dashboardHeader(title = "Multiple PLP Viewer", 
                                                                    
                                                                    tags$li(div(img(src = 'logo.png',
                                                                                    title = "OHDSI PLP", height = "40px", width = "40px"),
                                                                                style = "padding-top:0px; padding-bottom:0px;"),
                                                                            class = "dropdown")
                                                                    
                                                                    
                                    ), 
                                    
                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenu(
                                        addInfo(shinydashboard::menuItem("Description", tabName = "Description", icon = shiny::icon("home")), "DescriptionInfo"),
                                        addInfo(shinydashboard::menuItem("Library", tabName = "Library", icon = shiny::icon("table")), "LibraryInfo"),
                                        # addInfo(shinydashboard::menuItem("Performance", tabName = "Performance", icon = shiny::icon("bar-chart")), "PerformanceInfo"),
                                        addInfo(shinydashboard::menuItem("Model", tabName = "Model", icon = shiny::icon("clipboard")), "ModelInfo"),
                                        addInfo(shinydashboard::menuItem("Upload Analysis", tabName = "Upload", icon = shiny::icon("upload")), "UploadInfo"),
                                        addInfo(shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info")), "HelpInfo")
                                      )
                                    ),
                                   
                                    shinydashboard::dashboardBody(
                                      shinydashboard::tabItems(
                                        
                                        # help tab
                                        shinydashboard::tabItem(tabName = "Help",
                                                                shiny::h2("Information"),
                                                                shiny::p("Click on a row to explore the results for that model.  When you wish to explore a different model, then select the new result row and the tabs will be updated."),
                                                                shiny::a("Demo Video", href = 'https://youtu.be/StpV40yl1UE', target='_blank')
                                        ),
                                        
                                        # First tab content
                                        shinydashboard::tabItem(tabName = "Description",
                                                                shiny::includeMarkdown(path = "./www/shinyDescription.md")
                                                                
                                        ),
                                        shinydashboard::tabItem(tabName = "Library",
                                                                
                                                                shiny::fluidRow(
                                                                  shiny::column(2, 
                                                                                shiny::h4('Filters'),
                                                                                shiny::selectInput('modelSettingName', 'Model:', c('All',unique(as.character(summaryTable$Model)))),
                                                                                shiny::selectInput('devDatabase', 'Development Database', c('All',unique(as.character(summaryTable$Dev)))),
                                                                                shiny::selectInput('valDatabase', 'Validation Database', c('All',unique(as.character(summaryTable$Val)))),
                                                                                shiny::selectInput('T', 'Target Cohort', c('All',unique(as.character(summaryTable$`T`)))),
                                                                                shiny::selectInput('O', 'Outcome Cohort', c('All',unique(as.character(summaryTable$`O`)))),
                                                                                shiny::selectInput('riskWindowStart', 'Time-at-risk start:', c('All',unique(as.character(summaryTable$`TAR start`)))),
                                                                                shiny::selectInput('riskWindowEnd', 'Time-at-risk end:', c('All',unique(as.character(summaryTable$`TAR end`)))),
                                                                                ),  
                                                                  shiny::column(10, style = "background-color:#F3FAFC;",
                                                                                
                                                                                # do this inside tabs:
                                                                                shiny::tabsetPanel(
                                                                                  
                                                                                  shiny::tabPanel("Results",
                                                                                                  shiny::div(DT::dataTableOutput('summaryTable'), 
                                                                                                             style = "font-size:70%")),
                                                                                  
                                                                                  # shiny::tabPanel("Model Settings",
                                                                                  shiny::tabPanel("Development Settings",
                                                                                                                  shiny::h3('Model Settings: ', 
                                                                                                            shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/index.html", target="_blank") 
                                                                                                  ),
                                                                                                  DT::dataTableOutput('modelTable'),
                                                                                  
                                                                                  # shiny::tabPanel("Population Settings",
                                                                                                  shiny::h3('Population Settings: ', 
                                                                                                            shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.html", target="_blank") 
                                                                                                  ),
                                                                                                  DT::dataTableOutput('populationTable'),
                                                                                  
                                                                                  # shiny::tabPanel("Covariate Settings",
                                                                                                  shiny::h3('Covariate Settings: ', 
                                                                                                            shiny::a("help", href="http://ohdsi.github.io/FeatureExtraction/reference/createCovariateSettings.html", target="_blank") 
                                                                                                  ),
                                                                                                  DT::dataTableOutput('covariateTable')),
                                                                                # )
                                                                                
                                                                  # )
                                                                  
                                                                # )),
                                        # second tab
                                        #  shinydashboard::tabItem(tabName = "Performance", 
                                                                
                                                                # shiny::fluidRow(
                                                                #   tabBox(
                                                                #     title = "Performance", 
                                                                    # The id lets us use input$tabset1 on the server to find the current tab
                                                                    # id = "tabset1", height = "100%", width='100%',
                                                                    shiny::tabPanel("Summary",
                                                                             
                                                                             # shiny::fluidRow(
                                                                               shiny::column(width = 4,
                                                                                             shinydashboard::box(status = 'info', width = 12,
                                                                                                                 title = "Model Info & Download", solidHeader = TRUE,
                                                                                                                 shiny::h4(shiny::textOutput("authorName")),
                                                                                                                 shiny::h4(shiny::textOutput("authorEmail")),
                                                                                                                 shiny::downloadButton("plpResult", "Plp Object")
                                                                                             ),
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = tagList(shiny::icon("question"),"Prediction Question"), status = "info", solidHeader = TRUE,
                                                                                                                 shiny::textOutput('info')
                                                                                             ),
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = tagList(shiny::icon("gear"), "Input"), 
                                                                                                                 status = "info", solidHeader = TRUE,
                                                                                                                 shiny::splitLayout(
                                                                                                                   cellWidths = c('5%', '90%', '5%'),
                                                                                                                   shiny::h5(' '),
                                                                                                                   shiny::sliderInput("slider1", 
                                                                                                                                      shiny::h4("Threshold value slider: ", strong(shiny::textOutput('threshold'))), 
                                                                                                                                      min = 1, max = 100, value = 50, ticks = F),
                                                                                                                   shiny::h5(' ')
                                                                                                                 ),
                                                                                                                 shiny::splitLayout(
                                                                                                                   cellWidths = c('5%', '90%', '5%'),
                                                                                                                   shiny::h5(strong('0')),
                                                                                                                   shiny::h5(' '),
                                                                                                                   shiny::h5(strong('1'))
                                                                                                                 ),
                                                                                                                 shiny::tags$script(shiny::HTML("
                                                                                                                                                $(document).ready(function() {setTimeout(function() {
                                                                                                                                                supElement = document.getElementById('slider1').parentElement;
                                                                                                                                                $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
                                                                                                                                                }, 50);})
                                                                                                                                                "))
                                                                                             )
                                                                                             
                                                                               ),
                                                                               
                                                                               
                                                                               shiny::column(width = 8,
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = "Dashboard",
                                                                                                                 status = "warning", solidHeader = TRUE,
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxThreshold"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxIncidence"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxPPV"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxSpecificity"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxSensitivity"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxNPV")
                                                                                                                 
                                                                                             ),
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = "Cutoff Performance",
                                                                                                                 status = "warning", solidHeader = TRUE,
                                                                                                                 shiny::tableOutput('twobytwo')
                                                                                                                 #infoBoxOutput("performanceBox"),
                                                                                             )
                                                                               )
                                                                             # )
                                                                             
                                                                             
                                                                    ),
                                        shiny::tabPanel("Discrimination", 
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box( status = 'info',
                                                                                                    title = actionLink("rocHelp", "ROC Plot", icon = icon("info")),
                                                                                                    solidHeader = TRUE,
                                                                                                    shinycssloaders::withSpinner(plotly::plotlyOutput('roc'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("prcHelp", "Precision recall plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(plotly::plotlyOutput('pr')))),
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("f1Help", "F1 Score Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(plotly::plotlyOutput('f1'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("boxHelp","Box Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('box')))),
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("predDistHelp","Prediction Score Distribution", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('preddist'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("prefDistHelp","Preference Score Distribution", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('prefdist'))))
                                                                             
                                                                             
                                                                    ),
                                        shiny::tabPanel("Calibration", 
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("calHelp","Calibration Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('cal'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("demoHelp","Demographic Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('demo')))
                                                                             )
                                                                    ),
                                        shiny::tabPanel("Validation",
                                                                             
                                                                          shiny::div(DT::dataTableOutput('validationTable'), 
                                                                                                     style = "font-size:70%"),

                                                                          shiny::fluidRow(
                                                                            shinydashboard::box(status = 'info',
                                                                                          title = actionLink("rocHelp","Roc Plot", icon = icon("info")),
                                                                                          solidHeader = TRUE,
                                                                                          shinycssloaders::withSpinner(shiny::plotOutput('valRoc'))),
                                                                          shinydashboard::box(status = 'info',
                                                                                          title = actionLink("calHelp","Calibration Plot", icon = icon("info")),
                                                                                          solidHeader = TRUE,
                                                                                          side = "right",
                                                                                          shinycssloaders::withSpinner(shiny::plotOutput('valCal')))
                                                                     ),
                                                                     shiny::div(DT::dataTableOutput('databaseInfo'),
                                                                     style = "font-size:70%")
                                                                    )
                                                                  )))),
                                        
                                        # 3rd tab
                                        shinydashboard::tabItem(tabName = "Model",
                                                                shiny::fluidRow(
                                                                  shinydashboard::box( status = 'info',
                                                                                       title = "Binary", solidHeader = TRUE,
                                                                                       shinycssloaders::withSpinner(plotly::plotlyOutput('covariateSummaryBinary'))),
                                                                  shinydashboard::box(status = 'info',
                                                                                      title = "Measurements", solidHeader = TRUE,
                                                                                      side = "right",
                                                                                      shinycssloaders::withSpinner(plotly::plotlyOutput('covariateSummaryMeasure')))),
                                                                shiny::fluidRow(width=12,
                                                                                shinydashboard::box(status = 'info', width = 12,
                                                                                                    title = "Covariates", solidHeader = TRUE,
                                                                                                    DT::dataTableOutput('modelCovariateInfo'))),
                                                                shiny::fluidRow(width=12,
                                                                                shinydashboard::box(status = 'info', width = 12,
                                                                                                    title = "Model Table", solidHeader = TRUE,
                                                                                                    shiny::downloadButton("downloadData", "Download Model"),
                                                                                                    DT::dataTableOutput('modelView')))
                                        ),

                                        # # 4th tab
                                        shinydashboard::tabItem(tabName = "Log",
                                                                shiny::verbatimTextOutput('log')
                                        ),
                                        # 5th tab
                                        shinydashboard::tabItem(tabName = "Upload",
                                                                div(
                                                                  id = "form",
                                                                  selectizeInput(
                                                                    'researcherName', label = "Full name of study lead", choices = NULL,
                                                                    options = list(create = TRUE)
                                                                  ),
                                                                  selectizeInput(
                                                                    'researcherAffiliation', label = "Affiliation of study lead", choices = NULL,
                                                                    options = list(create = TRUE)
                                                                  ),
                                                              
                                                                  textInput("researcherEmail", "E-mail of Study Lead", ""),
                                                                  #databasesection
                                                                  selectizeInput(
                                                                    'databaseName', label = "Name of database used", choices = NULL,
                                                                    options = list(create = TRUE)
                                                                  ),
                                                                  selectizeInput(
                                                                    'databaseAcronym', label = "Acronym of database used", choices = NULL,
                                                                    options = list(create = TRUE)
                                                                  ),
                                                                  selectizeInput(
                                                                    'databaseDesc', label = "Description of database used", choices = NULL,
                                                                    options = list(create = TRUE)
                                                                  ),
                                                                  selectizeInput(
                                                                    'databaseType', label = "Type of database used", choices = c("Claims", "EHR", "GP"),
                                                                    options = list(create = TRUE)
                                                                  ),
                                                                  selectInput("AnalysisType", "Are you submitting a development or a validation?", c("Development","Validation")),
                                                                  textInput("studyDescription", "Provide a short description of the model. Please include the target and outcome definitions as well as information on the envisioned application of the model. If this is a validation study please include the model id number of the development study.                                                                                                                                                        E.G. This is a model to predict outcome of heart failure within 1 year of initialisation of a second type 2 diabetes drug. The model aims to help in guiding treatment choice at index.", ""),
                                                                  textInput("modelName", "Enter a model name. Otherwise default is <surname>-<year> e.g. williams-2020", ""),
                                                                  # fileInput("plpResult", "Choose plpResult File",
                                                                  #           multiple = FALSE,
                                                                  #           accept = c()),
                                                                  textInput("targetName", "Enter the name of the target cohort used"),
                                                                  # fileInput("targetCohort", "Choose target cohort json File",
                                                                  #           multiple = FALSE,
                                                                  #           accept = c(".json")),
                                                                  textInput("outcomeName", "Enter the name of the outcome cohort used"),
                                                                  # fileInput("outcomeCohort", "Choose outcome cohort json File",
                                                                  #                                          multiple = FALSE,
                                                                  #                                          accept = c(".json")),
                                                                  fileInput("libraryUpload", "Choose libraryUpload zip file",
                                                                            multiple = FALSE,
                                                                            accept = c()),
                                                                  actionButton("submitStudy", "Submit", class = "btn-primary")
                                                                )
                                        )
                                                                
                                        
                                        
                                      )
                                    )
)