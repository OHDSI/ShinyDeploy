# @file utils.R
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

getName <- function(x) {
  x <- as.character(x)
  if (length(grep('index month:', x)) > 0) {
    return(x)
  } else if (length(grep('age group:', x)) > 0) {
    return(x)
  } else if (length(grep('Concept set:', x)) > 0) {
    x <- strsplit(paste0(x), split = ':')[[1]][2]
    #x <- gsub(' ','',x)
    return(x)
  } else if (length(grep('index:', x)) > 0) {
    x <- strsplit(paste0(x), split = 'index:')[[1]][2]
    #x <- gsub(' ','',x)
    return(x)
  } else {
    return(x)
  }
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}