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

ui <- shiny::shinyUI(shiny::fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: opx solid #000000;}"))
  ),
  #shiny::singleton(
  #  tags$head(tags$script(src = "message-handler.js"))
  #),
  titlePanel(
    title=div(img(src="logo.png", height = 50, width = 50), 
              "PatientLevelPrediction Explorer"),
    windowTitle = "PatientLevelPrediction Explorer"
    ),
  
  shiny::navbarPage(
    title = "",
    id = 'mainnav',
    footer =  paste0("Data generated add ",runPlp$executionSummary$ExecutionDateTime),
    
    shiny::tabPanel(
      title = "Internal Validation",
      value = "plots",
      
      #shiny::uiOutput("resultSelect"),
      
      shiny::tabsetPanel(
        id = "visTabs",
        shiny::tabPanel(
          title = "Evaluation Summary",
          value =
            "panel_evalSum",
          shiny::h4("Evaluation Summary"),
          DT::dataTableOutput("evalSummary")
        ),
        
        shiny::tabPanel(
          title = "Characterization",
          value =
            "panel_characterization",
          shiny::h4("Characterization"),
          
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 2,
              shiny::actionButton(inputId = 'resetCharacter',
                                  label = 'Unselect All'#,
                                  #width = 'auto'
                                  ),
                                  shiny::radioButtons(
                                    inputId = "covSumCol",
                                    label = "Color:",
                                    choiceNames = c('None', 'Included', 'Analysis'),
                                    choiceValues = c('none', 'binary', 'type'),
                                    selected =
                                      'binary'
                                  ),
                                  shiny::radioButtons(
                                    inputId = "covSumSize",
                                    label = "Size:",
                                    choiceNames = c('None', 'Included', 'Coefficient'),
                                    choiceValues = c('none', 'binary', 'coef'),
                                    selected =
                                      'binary'
                                  )
              ),
              shiny::mainPanel(
                shiny::tabsetPanel(
                  id = "characterisation",
                  shiny::tabPanel(
                    title = "Plot",
                    value = "character_plot",
                    withSpinner(plotly::plotlyOutput("characterization"))
                  ),
                  shiny::tabPanel(
                    title = "Table",
                    value = "character_table",
                    withSpinner(DT::dataTableOutput("characterizationTab"))
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            title = "ROC",
            value = "panel_roc",
            shiny::h4("Test"),
            withSpinner(plotly::plotlyOutput("rocPlotTest")),
            shiny::h4("Train"),
            withSpinner(plotly::plotlyOutput("rocPlotTrain")),
            
            shiny::p(
              "The ROC plot shows the general ability of the model to discriminate between people with the outcome during the time at risk and those without the outcome.  It is a plot of specificity vs 1-sensitivity at every threshold (in general you may only be considered with highly specific mdoels and therefore may only want to focus towards a part of the curve)."
            ),
            shiny::p(
              "The x=y line is added to the plot as this shows the performance of a model that assigns a class at random (i.e., the model can not discrimiante between the people who will and will not develop the outcome during the time at risk)"
            )
            
          ),
          
          shiny::tabPanel(
            title = "Calibration",
            value = "panel_cal",
            shiny::h4("Test"),
            withSpinner(plotly::plotlyOutput("calPlotTest")),
            shiny::h4("Train"),
            withSpinner(plotly::plotlyOutput("calPlotTrain"))
          ),
          shiny::tabPanel(
            title = "Demographics",
            value = "panel_demo",
            shiny::h4("Test"),
            withSpinner(plotly::plotlyOutput("demoPlotTest")),
            shiny::h4("Train"),
            withSpinner(plotly::plotlyOutput("demoPlotTrain"))
          ),
          shiny::tabPanel(
            title = "Preference",
            value = "panel_pref",
            shiny::h4("Test"),
            withSpinner(plotly::plotlyOutput("prefPlotTest")),
            shiny::h4("Train"),
            withSpinner(plotly::plotlyOutput("prefPlotTrain"))
          ),
          shiny::tabPanel(
            title = "Box Plot",
            value = "panel_box",
            shiny::h4("Test"),
            withSpinner(shiny::plotOutput("boxPlotTest")),
            shiny::h4("Train"),
            withSpinner(shiny::plotOutput("boxPlotTrain"))
          ),
          #========================================================
          #  view settings
          #========================================================
          shiny::tabPanel(
            "Settings",
            
            shiny::tabsetPanel(
              id = "settingsTabs",
              shiny::tabPanel(
                title = "Settingse",
                value = "panel_options",
                #shiny::h4(shiny::textOutput("modelName")),
                shiny::h4("Modelling Settings"),
                DT::dataTableOutput("modelDetails"),
                shiny::h4("Population Settings"),
                DT::dataTableOutput("popDetails"),
                shiny::h4("Variable Settings"),
                DT::dataTableOutput("varDetails")
              ),
              
              shiny::tabPanel(
                title = "Attrition",
                value = "panel_attrition",
                shiny::h4("Attrition"),
                DT::dataTableOutput("attrition")
              )
            )
            
          ) # end of settings)
          
        )
      ),
      
      shiny::tabPanel(
        title = "External Validation",
        value = "external",
        shiny::tabsetPanel(
          id = "valTabs",
          shiny::tabPanel(
            title = "Evaluation Summary",
            value =
              "panel_evalSum2",
            shiny::h4("Evaluation Summary"),
            DT::dataTableOutput("evalSummaryVal")
          ),
          shiny::tabPanel(
            title = "Characterization",
            value =
              "panel_characterization2",
            shiny::h4("Characterization"),
            DT::dataTableOutput("characterizationTabVal")
          ),
          shiny::tabPanel(
            title = "ROC",
            value = "panel_roc2",
            #shiny::h4("Internal validation"),
            #plotly::plotlyOutput("rocPlotTest"),
            shiny::h4("External Validation"),
            plotly::plotlyOutput("rocPlotVal")
            
          ),
          
          shiny::tabPanel(
            title = "Calibration",
            value = "panel_cal2",
            #shiny::h4("Internal validation"),
            #plotly::plotlyOutput("calPlotTest"),
            shiny::h4("External validation"),
            plotly::plotlyOutput("calPlotVal")
          )
          
          
        )
      ),
      hr()# add select validation location with plots...
      
      
      
    )
  
    
  )
  )
#)