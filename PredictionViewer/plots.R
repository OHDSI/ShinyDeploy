# @file Plot.R
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



#' Plot the side-by-side boxplots of prediction distribution, by class#'
#' @details
#' Create a plot showing the side-by-side boxplots of prediction distribution, by class
#' #'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictionDistribution <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$predictionDistribution$Eval==type
  x<- evaluation$predictionDistribution[ind,]
  
  #(x=Class, y=predictedProbabllity sequence:  min->P05->P25->Median->P75->P95->max)
  
  plot <-   ggplot2::ggplot(x, ggplot2::aes(x=as.factor(class),
                                            ymin=MinPredictedProbability,
                                            lower=P25PredictedProbability,
                                            middle=MedianPredictedProbability,
                                            upper=P75PredictedProbability, 
                                            ymax=MaxPredictedProbability, 
                                            color=as.factor(class))) + 
    ggplot2::coord_flip() +
    ggplot2::geom_boxplot(stat="identity")  +
    ggplot2::scale_x_discrete("Class") + 
    ggplot2::scale_y_continuous("Predicted Probability") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = x$P05PredictedProbability[x$class==0], 
                                       xend = 1.1, yend = x$P05PredictedProbability[x$class==0]), color='red') +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = x$P95PredictedProbability[x$class==0], 
                                       xend = 1.1, yend = x$P95PredictedProbability[x$class==0]), color='red') +
    ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = x$P05PredictedProbability[x$class==1], 
                                       xend = 2.1, yend = x$P05PredictedProbability[x$class==1])) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = x$P95PredictedProbability[x$class==1], 
                                       xend = 2.1, yend = x$P95PredictedProbability[x$class==1]))
  
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}

plotCovSummary <- function(reactVars, input) {
  if (is.null(reactVars$plpResult))
    return(NULL)
  warning("Enter plotCovSummary")

  dataVal <- reactVars$plpResult$covariateSummary
  # remove large values...
  dataVal$CovariateCountWithOutcome[is.na(dataVal$CovariateMeanWithOutcome)] <-
    0
  dataVal$CovariateMeanWithOutcome[dataVal$CovariateMeanWithOutcome > 1] <-
    1
  dataVal$CovariateMeanWithNoOutcome[dataVal$CovariateMeanWithNoOutcome >
                                       1] <- 1
  dataVal$covariateValue[is.na(dataVal$covariateValue)] <- 0
  
  dataVal <- subset(dataVal,CovariateMeanWithOutcome>0.01)
  dataVal <- subset(dataVal,CovariateMeanWithNoOutcome>0.001)
  
  warning("After remove large values")
  #get the size
  #====================================
  if (input$covSumSize == 'binary') {
    dataVal$size <- rep(4, length(dataVal$covariateValue))
    dataVal$size[dataVal$covariateValue != 0] <- 8
  } else if (input$covSumSize == 'none') {
    dataVal$size <- rep(6, length(dataVal$covariateValue))
  } else if (input$covSumSize == 'coef') {
    dataVal$size <- abs(dataVal$covariateValue)
    dataVal$size[is.na(dataVal$size)] <- 0
    dataVal$size <- 10 * dataVal$size / max(dataVal$size)
  }
  
  warning("After get size")
  #get the included
  #====================================
  inc <- dataVal$covariateValue != 0
  nonInc <- dataVal$covariateValue == 0
  incAnnotations <- F
  # if (length(input$characterizationTab_rows_selected) > 0) {
  #   inc <- input$characterizationTab_rows_selected
  #   incAnnotations <- T
  #   writeLines(paste0(inc, collapse = '-'))
  # }
  warning("After get included")
  
  #get the color
  #=====================================
  if (input$covSumCol == 'binary') {
    dataVal$color <- rep('blue', length(dataVal$covariateName))
    dataVal$color[nonInc] <- 'red'
  } else if (input$covSumCol == 'type') {
    dataVal$color <- as.factor(dataVal$analysisId)
    #rep('purple', length(dataVal$covariateName)) # need to do this...
  } else if (input$covSumCol == 'none') {
    dataVal$color <- rep('black', length(dataVal$covariateName))
  }
  
  warning("After get color")
  # do annotations
  dataVal$annotation <- sapply(dataVal$covariateName, getName)
  
  if (incAnnotations) {
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc]) %>%
      plotly::add_markers(
        y = dataVal$CovariateMeanWithOutcome[inc],
        marker = list(size = dataVal$size[inc], #sizeBig,
                      color = dataVal$color[inc]),
        text = paste(dataVal$covariateName[inc])
      ) %>%
      plotly::add_annotations(
        x = dataVal$CovariateMeanWithNoOutcome[inc],
        y = dataVal$CovariateMeanWithOutcome[inc],
        text = paste(dataVal$annotation[inc]),
        xref = 'x',
        yref = 'y',
        showarrow = T,
        arrowhead = 4,
        arrowsize = .5,
        ax = 20,
        ay = -40
      ) %>%
      plotly::add_trace(
        x = c(0, 1),
        y = c(0, 1),
        mode = 'lines',
        line = list(dash = "dash"),
        color = I('black'),
        type = 'scatter'
      ) %>%
      layout(
        title = 'Prevalance of baseline predictors in persons with and without outcome',
        xaxis = list(title = "Prevalance in persons without outcome"),
        yaxis = list(title = "Prevalance in persons with outcome"),
        showlegend = FALSE
      )
  } else{
    plot_ly(x = dataVal$CovariateMeanWithNoOutcome[inc]) %>%
      plotly::add_markers(
        y = dataVal$CovariateMeanWithOutcome[inc],
        marker = list(size = dataVal$size[inc], #sizeBig,
                      color = dataVal$color[inc]),
        text = paste(dataVal$covariateName[inc])
      ) %>%
      plotly::add_markers(
        y = dataVal$CovariateMeanWithOutcome[nonInc],
        x = dataVal$CovariateMeanWithNoOutcome[nonInc],
        marker = list(size = dataVal$size[nonInc], #sizeSmall,
                      color = dataVal$color[nonInc]),
        text = paste(dataVal$covariateName[nonInc])
      ) %>%
      plotly::add_trace(
        x = c(0, 1),
        y = c(0, 1),
        mode = 'lines',
        line = list(dash = "dash"),
        color = I('black'),
        type = 'scattergl'
      ) %>%
      layout(
        title = 'Prevalance of baseline predictors in persons with and without outcome',
        xaxis = list(title = "Prevalance in persons without outcome"),
        yaxis = list(title = "Prevalance in persons with outcome"),
        showlegend = FALSE
      )
  }
}
