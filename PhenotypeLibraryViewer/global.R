# OHDSI Gold Standard Phenotype Library Viewer

# Libraries
library(DT)
library(ggplot2)
library(httr)
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

create_cohort_table <- function(){
  req <- GET("https://api.github.com/repos/OHDSI/PhenotypeLibrary/git/trees/master?recursive=1")
  stop_for_status(req)
  files <- content(req)$tree
  js <- c()
  for (i in 1:length(files)){
    x <- files[[i]]['path']
    if (grepl('\\_data.json$', x) & !grepl("Validation", x, fixed = T) &
        !grepl("Characterization", x, fixed = T) & !grepl("Citation", x, fixed = T)){
      x <- gsub(" ", "%20", x, fixed = TRUE)
      data <- jsonlite::fromJSON(paste0("https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/master/",x))
      js <- c(js,data)
    }
  }
  
  df <- matrix(0, ncol = length(js$Name), nrow = 1)
  df <- data.frame(df)
  names(df) <- js$Name
  for (i in seq(2,length(js),2)){
    x <- js[i]$Value
    for (j in 1:length(x)){
      if(length(x[[j]]) == 0){
        x[[j]] <- NA
      }
    }
    names(x) <- names(df)
    df <- rbind(df,t(x))
  }
  df <- df[-1,]
  rownames(df) <- NULL
  return(df)
}

create_validation_table <- function(){
  req <- GET("https://api.github.com/repos/OHDSI/PhenotypeLibrary/git/trees/master?recursive=1")
  stop_for_status(req)
  files <- content(req)$tree
  js <- c()
  for (i in 1:length(files)){
    x <- files[[i]]['path']
    if (grepl('\\_data.json$', x) & grepl("Validation", x, fixed = T) &
        !grepl("Characterization", x, fixed = T) & !grepl("Citation", x, fixed = T)){
      x <- gsub(" ", "%20", x, fixed = TRUE)
      data <- jsonlite::fromJSON(paste0("https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/master/",x))
      js <- c(js,data)
    }
  }
  
  df <- matrix(0, ncol = length(js$Name), nrow = 1)
  df <- data.frame(df)
  names(df) <- js$Name
  for (i in seq(2,length(js),2)){
    x <- js[i]$Value
    for (j in 1:length(x)){
      if(length(x[[j]]) == 0){
        x[[j]] <- NA
      }
    }
    names(x) <- names(df)
    df <- rbind(df,t(x))
  }
  df <- df[-1,]
  rownames(df) <- NULL
  return(df)
}

create_citation_table <- function(){
  req <- GET("https://api.github.com/repos/OHDSI/PhenotypeLibrary/git/trees/master?recursive=1")
  stop_for_status(req)
  files <- content(req)$tree
  js <- c()
  for (i in 1:length(files)){
    x <- files[[i]]['path']
    if (grepl('\\.txt$', x) & !grepl("Validation", x, fixed = T) &
        !grepl("Characterization", x, fixed = T) & grepl("Citation", x, fixed = T)){
      x <- gsub(" ", "%20", x, fixed = TRUE)
      data <- jsonlite::fromJSON(paste0("https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/master/",x))
      js <- c(js,data)
    }
  }
  
  df <- matrix(0, ncol = length(js$Name), nrow = 1)
  df <- data.frame(df)
  names(df) <- js$Name
  for (i in seq(2,length(js),2)){
    x <- js[i]$Value
    for (j in 1:length(x)){
      if(length(x[[j]]) == 0){
        x[[j]] <- NA
      }
    }
    names(x) <- names(df)
    df <- rbind(df,t(x))
  }
  df <- df[-1,]
  rownames(df) <- NULL
  return(df)
}

create_characterization_table <- function(){
  req <- GET("https://api.github.com/repos/OHDSI/PhenotypeLibrary/git/trees/master?recursive=1")
  stop_for_status(req)
  files <- content(req)$tree
  js <- c()
  for (i in 1:length(files)){
    x <- files[[i]]['path']
    if (grepl('\\_data.json$', x) & !grepl("Validation", x, fixed = T) &
        grepl("Characterization", x, fixed = T) & !grepl("Citation", x, fixed = T)){
      x <- gsub(" ", "%20", x, fixed = TRUE)
      data <- jsonlite::fromJSON(paste0("https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/master/",x))
      js <- c(js,data)
    }
  }
  
  df <- matrix(0, ncol = length(js$Name), nrow = 1)
  df <- data.frame(df)
  names(df) <- js$Name
  for (i in seq(2,length(js),2)){
    x <- js[i]$Value
    for (j in 1:length(x)){
      if(length(x[[j]]) == 0){
        x[[j]] <- NA
      }
    }
    names(x) <- names(df)
    df <- rbind(df,t(x))
  }
  df <- df[-1,]
  rownames(df) <- NULL
  return(df)
}

# Unpack into the phenotype and validation datasets
phe <- create_cohort_table()
val <- create_validation_table()
cit <- create_citation_table()
cha <- create_characterization_table()
#phe <- gold$Phenotype
#val <- gold$Validation

keep <- c("name", "email", "position", "institution", "ohdsi_forum_handle", 
          "orcid", "provenance_book", "provenance_chapter", 
          "provenance_rationale", "coh_therapeutic_areas_book", 
          "coh_tags_book", "coh_tags_chapter")
undo_p <- setdiff(names(phe), test)
undo_v <- setdiff(names(val), test)
undo_ci <- setdiff(names(cit), test)
undo_ch <- setdiff(names(cha), test)

for (i in undo_p){
  phe[,i] <- unlist(phe[,i])
}

for (i in undo_v){
  val[,i] <- unlist(val[,i])
}

for (i in undo_ci){
  cit[,i] <- unlist(cit[,i])
}

for (i in undo_ch){
  cha[,i] <- unlist(cha[,i])
}

books <- c(unlist(phe$coh_existing_books), unlist(phe$coh_book_title))
books <- books[!is.na(books)]
books <- books[books != ""]

p_books <- c()
for (i in 1:nrow(phe)){
  x <- ifelse(is.na(phe[i,"coh_existing_books"]), phe[i,"coh_book_title"], phe[i,"coh_existing_books"])
  p_books <- c(p_books,x)
}
phe$Book <- p_books

p_aa <- c()
for (i in 1:nrow(phe)){
  aa <- c()
  for (j in 1:length(phe[i,]$name$name)){
    aa <- c(aa,paste0(phe[i,]$name$name[j], ", ", phe[i,]$institution$institution[j]))
  }
  p_aa <- c(p_aa,paste(aa, collapse=" & "))
}
phe$Authors_And_Affiliations <- p_aa

p_tv <- c()
for (i in 1:nrow(phe)){
  b <- phe[i,"Book"]
  c <- phe[i,"coh_chapter_title"]
  if (phe[i,"coh_previous_validation"] == "TRUE"){
    p_tv <- c(p_tv, (1 + nrow(subset(val, validation_book_selection == b & validation_chapter_selection == c))))
  } else {
    p_tv <- c(p_tv, nrow(subset(val, validation_book_selection == b & validation_chapter_selection == c)))
  }
}
phe$Times_Validated <- p_tv

p_tci <- c()
for (i in 1:nrow(phe)){
  b <- phe[i,"Book"]
  c <- phe[i,"coh_chapter_title"]
  p_tci <- c(p_tci, nrow(subset(cit, citation_book_selection == b & citation_chapter_selection == c)))
}
phe$Times_Cited <- p_tci

p_tch <- c()
for (i in 1:nrow(phe)){
  b <- phe[i,"Book"]
  c <- phe[i,"coh_chapter_title"]
  p_tch <- c(p_tch, nrow(subset(cha, characterization_book_selection == b & characterization_chapter_selection == c)))
}
phe$Times_Characterized <- p_tch

for (i in 1:nrow(phe)){
  if (is.na(phe[i,"coh_inconclusive"])){
    phe[i,"coh_inconclusive"] <- 0
  }
}

for (i in 1:nrow(phe)){
  if (is.na(phe[i,"coh_true_pos"])){
    phe[i,"coh_true_pos"] <- 0
  }
}

for (i in 1:nrow(phe)){
  if (is.na(phe[i,"coh_true_neg"])){
    phe[i,"coh_true_neg"] <- 0
  }
}

for (i in 1:nrow(phe)){
  if (is.na(phe[i,"coh_false_pos"])){
    phe[i,"coh_false_pos"] <- 0
  }
}

for (i in 1:nrow(phe)){
  if (is.na(phe[i,"coh_false_neg"])){
    phe[i,"coh_false_neg"] <- 0
  }
}

p_sens <- c()
p_spec <- c()
p_ppv <- c()
p_npv <- c()
p_acc <- c()
p_f1 <- c()
for (i in 1:nrow(phe)){
  if(phe[i,"coh_previous_validation"] == "FALSE" | phe[i,"coh_validation_modality"] != "Manual"){
    p_sens <- c(p_sens,0)
    p_spec <- c(p_spec,0)
    p_ppv <- c(p_ppv,0)
    p_npv <- c(p_npv,0)
    p_acc <- c(p_acc,0)
    p_f1 <- c(p_f1,0)
  } else {
    p_sens <- c(p_sens, phe[i,"coh_true_pos"]/(phe[i,"coh_true_pos"] + phe[i,"coh_false_neg"]))
    p_spec <- c(p_spec, phe[i,"coh_true_neg"]/(phe[i,"coh_true_neg"] + phe[i,"coh_false_pos"]))
    p_ppv <- c(p_ppv, phe[i,"coh_true_pos"]/(phe[i,"coh_true_pos"] + phe[i,"coh_false_pos"]))
    p_npv <- c(p_npv, phe[i,"coh_true_neg"]/(phe[i,"coh_true_neg"] + phe[i,"coh_false_neg"]))
    p_acc <- c(p_acc, (phe[i,"coh_true_pos"] + phe[i,"coh_true_neg"])/(phe[i,"coh_false_pos"]+phe[i,"coh_false_neg"])+phe[i,"coh_true_pos"]+phe[i,"coh_true_neg"])
    p_f1 <- c(p_f1, 2*p_sens[i]*p_ppv[i]/(p_sens[i]+p_ppv[i]))
  }
}
phe$coh_sensitivity <- p_sens
phe$coh_specificity <- p_spec
phe$coh_ppv <- p_ppv
phe$coh_npv <- p_npv
phe$coh_accuracy <- p_acc
phe$coh_f1 <- p_f1

phe$Graph_Cluster <- 1
