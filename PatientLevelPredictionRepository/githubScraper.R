# library(httr)

getRepos <- function(user = NULL, password = NULL) {
  url <- "https://api.github.com/orgs/ohdsi-studies/repos"
  result <- list()
  pageNr <- 1
  # writeLines(paste("- Fetching page", pageNr))
  if (is.null(user)) {
    pageGet <- httr::GET(paste0(url, "?page=", pageNr))
  } else {
    pageGet <- httr::GET(paste0(url, "?page=", pageNr), httr::authenticate(user, ))
  }
  page <- httr::content(pageGet)
  if (!is.null(page$message) && page$message == "Git Repository is empty.")
    return(result)
  while (length(page) != 0) {
    result <- append(result, page)
    pageNr <- pageNr + 1
    # writeLines(paste("- Fetching page", pageNr))
    if (is.null(user)) {
      pageGet <- httr::GET(paste0(url, "?page=", pageNr))
    } else {
      pageGet <- httr::GET(paste0(url, "?page=", pageNr), httr::authenticate(user, ))
    }
    page <- httr::content(pageGet)
  }
  return(result)
}

parseRepos <- function(repos) {
  
  ignoreRepos <- c("EmptyStudyRepository", "StudyRepoTemplate")
  
  processRepo <- function(repo) {
    if (repo$name %in% ignoreRepos) {
      return(NULL)
    } else {
      return(data.frame(name = repo$name,
                        createdDate = substr(repo$created_at, 1, 10),
                        lastPushDate = substr(repo$pushed_at, 1, 10)))
    }
  }
  result <- lapply(repos, processRepo)
  result <- do.call(rbind, result)
  return(result)
}

getAllReadmes <- function(repoTable) {
  
  getReadme <- function(repoName) {
    url <- sprintf("https://raw.githubusercontent.com/ohdsi-studies/%s/master/README.md",
                   repoName)
    pageGet <- httr::GET(url)
    page <- httr::content(pageGet)
    return(page)
  }
  
  processRepo <- function(repoName) {
    readme <- getReadme(repoName)
    return(data.frame(name = repoName,
                      readme = readme))
  }  
  
  result <- lapply(repoTable$name, processRepo)
  result <- do.call(rbind, result)
  # Remove rows where README.md not found (wrong case?)
  result <- result[!grepl("404", result$readme), ]
  # Remove rows where README does not adhere to template
  result <- result[grepl("<img src=\"https://img.shields.io/badge/Study%20Status", result$readme), ]
  # Remove rows where template is still empty
  result <- result[!grepl("\\[Study title\\]", result$readme), ]
  return(result)
}


parseReadmes <- function(repoTable) {
  
  parseReadme <- function(readme) {
    
    getElement <- function(header) {
      if (grepl(header, readme)) {
        element <- gsub(sprintf(".*%s([^\n]*)\n.*", header),"\\1",readme)
        element <- trimws(gsub("\\*", "", element))
        if (element == "-") {
          return("")
        } else {
          return(element)
        }
        
      } else {
        return("")
      }
    }

    
    description <- trimws(gsub("\n([^\n]*\n=+\n|#).*", "",  gsub(".*Results explorer:[^\n]*\n *\n", "", readme)))
    status <- gsub("\".*", "", gsub(".*alt=\"Study Status: ", "", readme))
    
    result <- data.frame(title = trimws(gsub("\n==.*", "", readme)),
                         targetName = getElement("Target Name:"),
                         targetSize = getElement("Target Size:"),
                         outcomeName = getElement("Outcome Name:"),
                         outcomeSize = getElement("Outcome Count:"),
                         tar = getElement("Time-at-risk:"),
                         database = getElement("Database:"),
                         modelType = getElement("Model Type:"),
                         covariates = getElement("Covariates:"),
                         auroc = getElement("AUROC:"),
                         auprc = getElement("AUPRC:"),
                         ppv1 = getElement("PPV @ 1% sensitivity:"),
                         ppv10 = getElement("PPV @ 10% sensitivity:"),
                         ppv50 = getElement("PPV @ 50% sensitivity:"),
                         calibrationGradient = getElement("Calibration Gradient:"),
                         calibrationIntercept = getElement("Calibration Intercept:"),
                         targetCriteria = getElement("Atlas T link:"),
                         outcomeCriteria = getElement("Atlas O link:"),
                         vocabulary = getElement("Vocabulary:"),
                         tags = getElement("Tags:"),
                         studyStartDate = getElement("Study start date:"),
                         studyEndDate = getElement("Study end date:"),
                         protocol = getElement("Protocol:"),
                         publications = getElement("Publications:"),
                         resultsExplorer = getElement("Results explorer:"),
                         description = description,
                         stringsAsFactors = FALSE,
                         include = (getElement("Analytics use case\\(s\\):") == 'Patient-Level Prediction' & status == 'Complete')
    )
    return(result)
  }
  extraColumns <- lapply(repoTable$readme, parseReadme)
  extraColumns <- do.call(rbind, extraColumns)
  repoTable <- cbind(repoTable, extraColumns)
  repoTable$readme <- NULL
  return(repoTable)
}

scrapeGithub <- function() {
  writeLines("Scraping GitHub")
  repos <- getRepos()
  repoTable <- parseRepos(repos)
  readmes <- getAllReadmes(repoTable)
  repoTable <- merge(repoTable, readmes)
  repoTable <- parseReadmes(repoTable)
  repoTable$timeStamp <- Sys.time()
  
  # Assign right format to columns:
  x <- sapply(repoTable$studyStartDate, as.Date, tryFormats = c("%B %d, %Y"), optional = TRUE)
  class(x) <- "Date"
  repoTable$studyStartDate <- x
  
  x <- sapply(repoTable$studyEndDate, as.Date, tryFormats = c("%B %d, %Y"), optional = TRUE)
  class(x) <- "Date"
  repoTable$studyEndDate <- x
  
  
  repoTable$lastPushDate <- as.Date(repoTable$lastPushDate)
  
  #repoTable$status <- as.factor(repoTable$status)
  #repoTable$studyType <- as.factor(repoTable$studyType)
  repoTable <- repoTable[repoTable$include==1,]
  repoTable <- repoTable[order(repoTable$lastPushDate, decreasing = TRUE), ]
  return(repoTable)
}


