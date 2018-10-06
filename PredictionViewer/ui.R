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
    footer =  paste0("Data generated: ",runPlp$executionSummary$ExecutionDateTime),
    shiny::tabPanel("About",
             HTML("</BR><P>This is a demo of the result viewer from the <a href='https://github.com/OHDSI/PatientLevelPrediction'>Patient-Level Prediction R package</a></P>"),
             div(img(src="about.png", height = 150, width = 450)),
             HTML("<b>The prediction problem we address:</b></BR></BR>
                   Among a population at risk, we aim to predict which patients at a defined moment in time (t = 0) will experience some outcome during a time-at-risk.</BR>
                   </BR>Prediction is done using only information about the patients in an observation window prior to that moment in time."),
              HTML("</BR></BR><P style='color:#FF0000'>The app is currently under development. Do not use.</P>")
    ),
    
    shiny::tabPanel(
      title = "Internal Validation",
      value = "plots",
      
      #shiny::uiOutput("resultSelect"),
      shiny::tabsetPanel(
        id = "visTabs",
        shiny::tabPanel(
          title = "Evaluation Summary",
          value = "panel_evalSum",
          fluidRow(
            column(
              HTML("</BR>"),
              div(strong("Table 1."),"Performance of the model on the train and test set and information about the study population."),
              HTML("</BR>"),
              DT::dataTableOutput("evalSummary"),width=6
            )
          )
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
                    withSpinner(plotly::plotlyOutput("characterization")),
                    HTML("</BR>"),
                    div(strong("Figure 1."),"The variable scatter plot shows the mean covariate value for the people with the outcome against the mean covariate value for the people without the outcome. 
                        The meaning of the size and color of the dots depends on the settings on the left of the figure."
                    )
                  ),
                  shiny::tabPanel(
                    title = "Table",
                    value = "character_table",
                    HTML("</BR>"),
                    div(strong("Table 2."),"This table shows for each covariate the mean value in persons with and without the outcome and their mean difference."),
                    HTML("</BR>"),
                    withSpinner(DT::dataTableOutput("characterizationTab"))
                  )
                )
              )
            )
          ),
        
          shiny::tabPanel(
            title = "ROC",
            value = "panel_roc",
            fluidRow(
              column(shiny::h4("Train"),
                     withSpinner(plotly::plotlyOutput("rocPlotTrain")),width=6),
              column(shiny::h4("Test"),
                    withSpinner(plotly::plotlyOutput("rocPlotTest")),width=6)
             ),
            HTML("</BR>"),
            div(strong("Figure 2."),"The Receiver Operating Characteristics (ROC) curve shows the ability of the model to discriminate between people with and without the outcome during the time at risk. 
                It is a plot of sensitivity vs 1-specificity at every probability threshold. The higher the area under the ROC plot the higher the discriminative performance of the model.
                The diagonal refers to a model assigning a class at random (area under de ROC = 0.5)."
                )
          ),
          
          shiny::tabPanel(
            title = "Calibration",
            value = "panel_cal",
            fluidRow(
              column(shiny::h4("Train"),
                     withSpinner(plotly::plotlyOutput("calPlotTrain")),width=6),
              column(
                shiny::h4("Test"),
                withSpinner(plotly::plotlyOutput("calPlotTest")),width=6),
              HTML("</BR>"),
              div(strong("Figure 3."),"The calibration plot shows how close the predicted risk is to the observed risk. 
                  The diagonal dashed line thus indicates a perfectly calibrated model. 
                  The ten (or fewer) dots represent the mean predicted values for each quantile plotted against the observed fraction of people in that quantile who had the outcome (observed fraction). 
                  The straight black line is the linear regression using these 10 plotted quantile mean predicted vs observed fraction points."
                 )
            )
          ),
          
          shiny::tabPanel(
            title = "Demographics",
            value = "panel_demo",
            fluidRow(
              column(shiny::h4("Train"),
                     withSpinner(plotly::plotlyOutput("demoPlotTrain")),width=6
              ),
              column(shiny::h4("Test"),
                     withSpinner(plotly::plotlyOutput("demoPlotTest")),width=6
              ),
              HTML("</BR>"),
              div(strong("Figure 4."),"This demogrophics plot shows for females and males the expected and observed risk in different age groups and the confidence areas."
              )
            )
          ),

          shiny::tabPanel(
            title = "Preference",
            value = "panel_pref",
            fluidRow(
              column(shiny::h4("Train"),
                     withSpinner(plotly::plotlyOutput("prefPlotTrain")),width=6
              ),
              column(shiny::h4("Test"),
                     withSpinner(plotly::plotlyOutput("prefPlotTest")),width=6
              ),
              HTML("</BR>"),
              div(strong("Figure 5."),"The preference distribution plots are the preference score distributions corresponding to i) people in the test set with the outcome (red) and ii) people in the test set without the outcome (blue)."
              )
            )
           ),
        
          shiny::tabPanel(
            title = "Box Plot",
            value = "panel_box",
            fluidRow(
              column(shiny::h4("Train"),
                     withSpinner(shiny::plotOutput("boxPlotTrain")),width=6
              ),
              column(shiny::h4("Test"),
                     withSpinner(shiny::plotOutput("boxPlotTest")),width=6
              ),
              HTML("</BR>"),
              div(strong("Figure 6."),"The prediction distribution boxplots are box plots for the predicted risks of the people in the test set with the outcome (class 1: blue) and without the outcome (class 0: red)."
              )
            )
          ),
        
        
          #========================================================
          #  view settings
          #========================================================
          shiny::tabPanel(
            "Settings",
            
            shiny::tabsetPanel(
              id = "settingsTabs",
              shiny::tabPanel(
                title = "Settings",
                value = "panel_options",
                fluidRow(
                  column(
                    HTML("</BR>"),
                    div(strong("Table 3."),"Settings for model development."),
                    HTML("</BR>"),
                    DT::dataTableOutput("modelDetails"),
                    HTML("</BR>"),
                    div(strong("Table 4."),"Covariate settings."),
                    HTML("</BR>"),
                    DT::dataTableOutput("varDetails"),
                    width = 6),
                  column(
                        HTML("</BR>"),
                        div(strong("Table 5."),"Population definition settings."),
                        HTML("</BR>"),
                        DT::dataTableOutput("popDetails"), width = 6)
                )
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