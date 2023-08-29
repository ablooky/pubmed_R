#code to extract and format pubmed references from ncbi through R.
library(RISmed)
library(easyPubMed)
library(XML)
library(xml2)
#library(qdap)
library(openxlsx)
#library(methods)
library(tidyverse)
library(data.table)
library('plyr')

#Functions

#search some topic on PubMed
#it is recommended to limit max_results to 999
topicSearch <- function(pubmed_id, max_results = 5) {
  
  res_search <- EUtilsSummary(pubmed_id, type = 'esearch', db = 'pubmed')
  pmid_results <- res_search@PMID
  res_records <- EUtilsGet(pmid_results[1:max_results])
  
  #parsing results into a data frame
  authors_list <- res_records@Author
  authors <-
    data.frame('Authors' = NA,
               'First_Author' = NA,
               'Pubmed_id' <- NA)
  if (length(authors_list) >= 1) {
    for (i in 1:length(authors_list)) {
      publication_id <- names(authors_list)[[i]]
      publication <- authors_list[[i]]
      first_author <- paste0(publication['Author', 'LastName'], ',',
                             publication['Author', 'Initials'], ' et al.')
      all_authors <- c()
      for (j in 1:nrow(publication)) {
        all_authors <- c(all_authors,
                         paste0(publication[j, 'LastName'], ',',
                                publication[j, 'Initials']))
      }
      all_authors <- paste(all_authors, collapse = ', ')
      authors[i, ] <- c(all_authors, first_author, publication_id)
    }
  }
  
  #Getting papers metadata
  res <- data.frame(
    pubmed_id = NA,
    doi = NA,
    publication_year = NA,
    first_author = NA,
    authors = NA,
    title = NA,
    volume = NA,
    issue = NA,
    first_page = NA,
    last_page = NA,
    ISSN = NA,
    Publication_type = NA,
    language = NA,
    abstract = NA
  )
  for (i in 1:max_results) {
    temp_row <-
      c(
        res_records@PMID[i],
        res_records@DOI[i],
        res_records@YearPubDate[i],
        first_author = authors$First_Author[i],
        authors[i, 'Authors'],
        res_records@ArticleTitle[i],
        res_records@Volume[i],
        res_records@Issue[i],
        NA,
        NA,
        res_records@ISSN[i],
        res_records@PublicationType[i],
        res_records@Language[i],
        res_records@AbstractText[i]
      )
    res[i, ] <- temp_row
  }
  return(res)
  
}

#search topic within time-frame
#it is recommended to limit max_results to 999
topicSearchByDate <-
  function(pubmed_id,
           max_results,
           year_min = 1995,
           year_end = 2023) {
    print(paste0('Awaiting pubmed_id: ', pubmed_id, '...'))
    
    search <- topicSearch(pubmed_id, max_results)
    
    
    
    filtered_search <- search %>% filter(publication_year <= year_end,
                                         publication_year >= year_min)
    print(paste0(nrow(filtered_search), ' Results!'))
    
    return(filtered_search)
    
  }

#retrieve abstract of an article using pubmed_id
getPubMedAbstract <- function(pubmed_id) {
  #pubmed_id pubmed_id
  res <- get_pubmed_ids(pubmed_id)
  output <- fetch_pubmed_data(res, 0, 1, format = "xml")
  
  #write xml output
  temp_filename <- "output.xml"
  fileConn <- file(temp_filename)
  writeLines(output, fileConn)
  close(fileConn)
  
  #parse xml
  read_ds <- read_xml(temp_filename) %>% as_list()
  abstract_text <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]]
  abstract <- paste(unlist(abstract_text), collapse = ' ')
  
  return(abstract)
}

#retrieve article information using pubmed_id
getPubMedInfo <- function(pubmed_id) {
  #pubmed_id pubmed_id
  res <- get_pubmed_ids(pubmed_id)
  output <- fetch_pubmed_data(res, 0, 1, format = "xml")
  
  #write xml output
  temp_filename <- "output.xml"
  fileConn <- file(temp_filename)
  writeLines(output, fileConn)
  close(fileConn)
  
  #parse xml
  read_ds <- read_xml(temp_filename) %>% as_list()
  abstract_text <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]]
  abstract <- paste(unlist(abstract_text), collapse = ' ')
  journalTitle <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["Title"]][[1]]
  journalIssue <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["Issue"]]
  journalStartPage <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["StartPage"]]
  journalEndPage <- ''
  pub_Date <-
    unlist(read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["PubDate"]])
  pub_Date <- paste(pub_Date, collapse = '-')
  journalVolume <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["Volume"]]
  authors <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["AuthorList"]]
  articleTitle <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["ArticleTitle"]]
  keywords <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["KeywordList"]]
  keywords <- paste(unlist(keywords), collapse = '; ')
  authors_df <- rbindlist(authors, fill = TRUE)
  authors_df <- authors_df[, c("LastName", "ForeName", "Initials")]
  authors_matrix <- matrix(apply(authors_df, 1, paste,
                                 collapse = ' '), ncol = 1)
  authors_formatted <- paste(authors_matrix[, 1], collapse = '|')
  
  info_df <- data.frame(
    'Pubmed_id' = pubmed_id,
    'Article Title' = articleTitle[[1]],
    Authors = authors_formatted,
    Abstract = abstract,
    'Journal Title' = journalTitle[[1]],
    'Journal Volume' = journalVolume[[1]],
    'Journal Issue' = journalIssue[[1]],
    'Start Page' = journalStartPage[[1]],
    'Publication Date' = pub_Date[[1]],
    Keywords = keywords
  )
  return(info_df)
  
}

#Accessing NCBI using  RISmed
getPubMedInfo_via_rismed <- function(pubmed_id) {
  res_search <- EUtilsSummary(pubmed_id, type = 'esearch', db = 'pubmed')
  res_records <- EUtilsGet(res_search)
  
  authors <- Author(res_records)
  authors_df <- authors[[1]][, c("LastName", "ForeName", "Initials")]
  authors_matrix <- matrix(apply(authors_df, 1, paste,
                                 collapse = ' '), ncol = 1)
  authors_formatted <- paste(authors_matrix[, 1], collapse = '; ')
  
  
  res <- data.frame(
    PMID = PMID(res_records),
    'Publication Year' = YearPpublish(res_records),
    Authors =   authors_formatted,
    ISSN =  ISSN(res_records),
    Title = Title(res_records),
    'Article Title' = ArticleTitle(res_records),
    Abstract =  AbstractText(res_records),
    'Publication Language' = Language(res_records),
    'Publication Type' = PublicationType(res_records),
    Issue = Issue(res_records),
    'Journal Title'  =  ISOAbbreviation(res_records),
    'Start Page' =  MedlinePgn(res_records),
    Country = Country(res_records)
  )
  
  
  return(res)
}


#batch downloads of pubmed string search
#download format_type can be 'xml', 'medline', 'abstract'
#This is a slow query

batchTopicSearch <-
  function(pubmed_id,
           batch_size,
           format_type,
           prefix) {
    #Create a folder for  the output
    path <- paste0('output/pubmed_id_', Sys.time(), '/')
    dir.create(path,
               showWarnings = TRUE,
               recursive = FALSE,
               mode = "0777")
    
    
    res <- easyPubMed::batch_pubmed_download(
      pubmed_query_string = queried_string,
      format = format_type,
      batch_size = batch_size,
      dest_file_prefix = paste0(path, prefix),
      encoding = "ASCII"
    )
    #readLines(res[1:30])
    
    return(res)
 
  }

batchPubmedSearch<-function(ids) {
  t <- batch_pubmed_download(
    ids,
    format = 'xml',
    batch_size = length(ids_list),
    dest_dir = 'output',
    dest_file_prefix = 'file_output_'
  )
  f <- list.files('output')
  filename <- str_replace(f[1], '.txt', '.xml')
  file.rename(f[1], filename)
  w <- xmlParse(file = paste0('output/', filename))
  return(w)
}


# Perform searches





#retrieve pmid based on doi
search_by_doi <- function(doi) {
  t <- get_pubmed_ids(doi)
  res <- NA
  output<-NA
  if (length(t) > 0)
    res <- t$IdList$Id[1]
  if (!is.na(res)) {
    output <- getPubMedInfo(res)
  }
  return(output)
}




#test
queried_string <-
  '(pharmacokinetics OR hepatic clearance) AND (rodent OR mice) AND vivo'
search_tox <- topicSearch(queried_string, max_results = 500)
search_tox <- topicSearchByDate(queried_string, 999, 1995, 2023)


#import data (the list of publications)into R.
pubs <- read.csv('input/pubmed.csv', header = T)
doi_id <- '10.1016/j.hrtlng.2019.09.002'
batchDownloads(pubmed_id = queried_string, 
               batch_size = 3000,
               format = 'medline',
               prefix = 'test_')
