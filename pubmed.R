#code to extract and format pubmed references from ncbi through R.
library(plyr)
library(RISmed)
library(easyPubMed)
library(xml2)
library(openxlsx)
library(tidyverse)
library(data.table)


#Functions

#search some topic on PubMed
#it is recommended to limit max_results to 999
topicSearch <- function(pubmed_id, max_results = 5) {
  res_search <-
    EUtilsSummary(pubmed_id, type = 'esearch', db = 'pubmed')
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
      authors[i,] <- c(all_authors, first_author, publication_id)
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
    res[i,] <- temp_row
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
    
    
    
    filtered_search <-
      search %>% filter(publication_year <= year_end,
                        publication_year >= year_min)
    print(paste0(nrow(filtered_search), ' Results!'))
    
    return(filtered_search)
    
  }

# #retrieve abstract of an article using pubmed_id
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
#
# #retrieve article information using pubmed_id
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
  read_ds1 <- read_xml(temp_filename) 
  read_ds<- read_ds1 %>% as_list()
  abstract_text <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]]
  abstract <- paste(unlist(abstract_text), collapse = ' ')
  journal_type<-    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["StartPage"]]
  journalTitle <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["Title"]]

  journalIssue <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["Issue"]]
  journalStartPage <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["StartPage"]]
  journalEndPage <-     read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["EndPage"]]
  MedlinePgn<-    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["MedlinePgn"]]
  pub_Date <-
    unlist(read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["PubDate"]])
  pub_Date4 <- paste(pub_Date, collapse = '-')
  pub_Date2<-read_ds$PubmedArticleSet$PubmedArticle$MedlineCitation$DateCompleted
  pub_Date3<-paste(unlist(pub_Date2), collapse = '-')
  articleDate<-
    unlist(read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
  pub_year<-paste(articleDate[1])
  if(is.null(pub_year) || length(pub_year) == 0) pub_year<-pub_Date2[[1]]
  if(is.null(pub_year) || length(pub_year) == 0) pub_year<-pub_Date[1]
  articleDate<-paste(articleDate, collapse = '-')
  journalVolume <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["Volume"]]
  authors <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["AuthorList"]]
  articleTitle <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["ArticleTitle"]]
  articleTitle<-paste(unlist(articleTitle),collapse = '')
  keywords <-
    read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["KeywordList"]]
  
  doi<-read_ds[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][['ELocationID']]
    doi_nodes_pubmed<-xml_find_all(read_ds1,'.//ArticleId')
    doi_nodes_pubmed_index<-0
    for(l in 1:length(doi_nodes_pubmed)){
      if(xml_attrs(doi_nodes_pubmed[[l]])[["IdType"]] == 'doi'){
        doi_nodes_pubmed_index<-l
      }
    }
  doi1<-xml_text(doi_nodes_pubmed[[doi_nodes_pubmed_index]], "IdType" == 'doi')
    doi_nodes_medline<-xml_find_all(read_ds1,'.//ELocationID')
    doi2<-xml_text(doi_nodes[xml_attr(doi_nodes_medline, 'EIdType') == 'doi'])
  #format output
  keywords <- paste(unlist(keywords), collapse = '; ')
  authors_df <- rbindlist(authors, fill = TRUE)
 #authors_df <- authors_df[, c("LastName", "ForeName", "Initials")]
  authors_df <- authors_df[, c("LastName",  "Initials")]
  authors_matrix <- matrix(apply(authors_df, 1, paste,
                                 collapse = ' '), ncol = 1)
  authors_formatted <- paste(authors_matrix[, 1], collapse = ', ')
  
  apa_citation <- paste0(authors_formatted, '. ',articleTitle,' ')
  if(!is.null(pub_year) && length(pub_year)>0){
    apa_citation<-paste0(apa_citation,  pub_year,'. ')  
  }
  if(!is.null(journalTitle)){
    apa_citation<-paste0(apa_citation,  journalTitle[[1]],' ')  
  }
 
  if(!is.null(journalVolume)){
  apa_citation<-paste0(apa_citation, journalVolume[[1]])  
  }
  if(!is.null(journalIssue)){
    apa_citation<-paste0(apa_citation, '(',journalIssue[[1]],'):')  
  }
  if(!is.na(MedlinePgn[[1]])){
    apa_citation<-paste0(apa_citation, ' ',MedlinePgn[[1]],'. ')  
  }
  apa_citation<-paste0(apa_citation, 'PMID: ', pubmed_id)  
  if(!is.null(doi1) || length(doi2) > 0){
    apa_citation<-paste0(apa_citation, '. DOI: ','https://doi.org/', 
                         doi1,'. ')  
  }

  info_df <- data.frame(col1 = 1) %>%
    mutate(
    'Pubmed_id' = pubmed_id,
    'Article Title' = articleTitle,
    Authors = authors_formatted,
    Abstract = abstract,
    'Journal Title' = journalTitle[[1]],
    'Journal Volume' = journalVolume[[1]],
    'Journal Issue' = journalIssue[[1]],
    'Start Page' = journalStartPage[[1]],
    'End Page' = journalEndPage[[1]],
    'Pagination' = MedlinePgn[[1]],
    'Publication Date' = articleDate,
    'Publication Year' = ifelse(length(pub_year)>0,as.character(pub_year),NA),
    DOI = ifelse(length(doi1) > 0, doi1, doi2),
    Keywords = keywords,
    citation = apa_citation
  
  )
  return(info_df)
  
}

# #Accessing NCBI using  RISmed
getPubMedInfo_via_rismed <- function(pubmed_id) {
  res_search <-
    EUtilsSummary(pubmed_id, type = 'esearch', db = 'pubmed')
  res_records <- EUtilsGet(res_search)
  
  authors <- Author(res_records)
  authors_df <-
    authors[[1]][, c("LastName",  "Initials")]
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

#
# #batch downloads of pubmed string search
# #download format_type can be 'xml', 'medline', 'abstract'
# #This is a slow query
#
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
#
#
# #Batch pubmed ids search
batchPmidSearch <- function(ids) {
  output_df <- data.frame()
  failed_ids <- c()
  for (i in ids) {
    print(i)
    if (!is.na(i)) {
      try(temp_df <- getPubMedInfo(i))
      if (exists('temp_df') && nrow(temp_df)>0) {
        output_df <- bind_rows(output_df, temp_df)
      } else {
        failed_ids <- c(i, failed_ids)
      }
    }
  }
  output_df<-output_df %>% arrange(desc(`Publication Year`))
  openxlsx::write.xlsx(output_df,'output.batchpmidsearch.xlsx')
  return(list(output_df, failed_ids))
}
#
#
# #retrieve pmid based on doi
search_by_doi <- function(doi) {
  t <- get_pubmed_ids(doi)
  res <- NA
  output <- NA
  if (length(t) > 0)
    res <- t$IdList$Id[1]
  if (!is.na(res)) {
    output <- getPubMedInfo(res)
  }
  return(output)
}
#
#
process_file <- function(file_uploaded) {
  filename <- NA
  filepath <- NA
  output <- NA
  article_title <- NA
  pub_year <- NA
  author <- NA
  
  uploaded_df <- read.csv(file_uploaded)
  
  for (r in 1:nrow(uploaded_df)) {
    pubmed_id <- uploaded_df[r, 'pmid']
    doi <- uploaded_df[r, 'doi']
    if (!is.na(pubmed_id)) {
      output <-
        easyPubMed::fetch_pubmed_data(get_pubmed_ids(paste0(pubmed_id, ' [pmid]')), format = 'txt')
      
    }
    else if (!is.na(doi)) {
      output <-
        easyPubMed::fetch_pubmed_data(get_pubmed_ids(paste0(doi, ' [doi]')), format = 'xml')
    }
    else {
      output <- NA
    }
    Sys.sleep(1)
    if (r %% 100 == 0) {
      print('Pausing...')
      Sys.sleep(30)
    }
  }
  #write xml output
  write_xml(output,
            file = "output/output.xml",
            options = c("format", "no_declaration"))
  
  temp_filename <- "output/output.xml"
  fileConn <- file(temp_filename)
  writeLines(output, fileConn)
  close(fileConn)
  
  #parse xml
  result <- xmlParse(file = 'output/output.xml')
  result <- read_xml('output/output.xml')
  read_ds <- read_xml(f) %>% as_list()
  output <-
    paste0(pubmed_id, '_', pub_year, '_', author, '_', article_title)
  if (nchar(output) > 240)
    output <- substr(output, 1, 240)
  filename <- paste0(output, '.pdf')
  filepath <- paste0(author, '/', pub_year, '/')
  return(list(filepath, filename))
}


#Test
test_functions <- function() {
  ##topic search
  queried_string <-
    '(pharmacokinetics OR hepatic clearance) AND (rodent OR mice) AND vivo'
  search_tox <- topicSearch(queried_string, max_results = 500)
  search_tox <- topicSearchByDate(queried_string, 999, 1995, 2023)
  pubmed_id<-37443298
  ##batch search
  pubmed_ids <- c(37443298)
  pubs <- read.csv('input/arnot_pubs.csv', header = T)
  results <- batchPmidSearch(pubs$PMID)
  results <- batchPmidSearch(pubmed_ids)
  sucess_df<-results[[1]]
  failed_ids <- results[[2]]
  
  batchTopicSearch(
    pubmed_id = queried_string,
    batch_size = 3000,
    format = 'medline',
    prefix = 'test_'
  )
  
  ##search based on doi
  doi_id <- '10.1016/j.hrtlng.2019.09.002'
  doi_search <- search_by_doi(doi_id)
  
  
  
  
}
