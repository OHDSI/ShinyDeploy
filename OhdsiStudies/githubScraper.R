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
    getElement <- function(header, structuredElements) {
      index <- grepl(header, structuredElements)
      if (sum(index) == 1) {
        element <- structuredElements[index]
        element <- gsub(header, "", element)
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
    readme <- gsub("\r", "", readme)
    sectionHeaders <- gregexpr("\n[^\n]*\n=+|\n#+", readme)[[1]]
    posResultsExplorer <- gregexpr("Results explorer", readme)[[1]]
    if (all(sectionHeaders < posResultsExplorer)) {
      endOfReadmeHeader <- nchar(readme)
    } else {
      endOfReadmeHeader <- min(sectionHeaders[sectionHeaders > posResultsExplorer])
    }
    readmeHeader <- substr(readme, 1, endOfReadmeHeader)
    
    posEmptyLine <- gregexpr("\n *\n", readmeHeader)[[1]]
    startOfDescription <- min(posEmptyLine[posEmptyLine > posResultsExplorer])
    description <- substr(readmeHeader, startOfDescription, nchar(readmeHeader))
    startOfStucturedElements <- max(posEmptyLine[posEmptyLine < startOfDescription])
    structuredElements <- substr(readmeHeader, startOfStucturedElements, startOfDescription)
    structuredElements <- strsplit(structuredElements, "\n *- ")[[1]]
    status <- gsub("\".*", "", gsub(".*alt=\"Study Status: ", "", readmeHeader))
    result <- data.frame(title = trimws(gsub("\n==.*", "", readmeHeader)),
                         useCases = getElement("Analytics use case\\(s\\):", structuredElements),
                         studyType = getElement("Study type:", structuredElements),
                         tags = getElement("Tags:", structuredElements),
                         lead = getElement("Study lead:", structuredElements),
                         leadTag = getElement("Study lead forums tag:", structuredElements),
                         studyStartDate = getElement("Study start date:", structuredElements),
                         studyEndDate = getElement("Study end date:", structuredElements),
                         protocol = getElement("Protocol:", structuredElements),
                         publications = getElement("Publications:", structuredElements),
                         resultsExplorer = getElement("Results explorer:", structuredElements),
                         status = status,
                         description = description,
                         stringsAsFactors = FALSE)
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
  
  repoTable$status <- as.factor(repoTable$status)
  repoTable$studyType <- as.factor(repoTable$studyType)
  
  repoTable <- repoTable[order(repoTable$lastPushDate, decreasing = TRUE), ]
  return(repoTable)
}


