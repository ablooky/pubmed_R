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
topicSearch <- function(query = queried_string,
                        max_results = 5) {
  print(paste0('Topic search: ', queried_string, '...'))
  res_search <- EUtilsSummary(query, type = 'esearch', db = 'pubmed')
  pmid_results <- res_search@PMID
  res_records <- EUtilsGet(pmid_results[1:max_results])
  return(res_records)
  
}

#search topic within time-frame
#it is recommended to limit max_results to 999
topicSearchByDate <-
  function(query,
           max_results = 1000,
           year_min = 1995,
           year_max = year(Sys.Date())) {
    print(paste0('Topic search: ', queried_string, '...'))
    print(paste0('Filtered search: ', year_min, '-', year_max, '...'))
    search <- topicSearch(query, max_results)
    #format results here:
    formatted_search<-format_results(search,'topic')
    filtered_search <-
      formatted_search %>%
      mutate(`YearPubmed` = as.integer(`YearPubmed`)) %>%
      filter(`YearPubmed` <= year_max & `YearPubmed` >= year_min)
    print(paste0(nrow(filtered_search), ' Results!'))
    
    return(filtered_search)
    
  }

# Search by pubmed_id
# Retrieve article information using pubmed_id
getPubMedInfo <- function(pubmed_id) {
  #pubmed_id pubmed_id
  res <- get_pubmed_ids(pubmed_id)
  output <- fetch_pubmed_data(res, 0, 1, format = "xml")
  return(output)
  
}
# Retrieve abstract of an article using pubmed_id
getPubMedAbstract <- function(pubmed_id) {
  #pubmed_id
  res <- get_pubmed_ids(pubmed_id)
  output <- fetch_pubmed_data(res, 0, 1, format = "xml")
  return(output)
}
# Accessing NCBI using  RISmed
getPubMedInfo_via_rismed <- function(pubmed_id) {
  res_search <- EUtilsSummary(pubmed_id, type = 'esearch', db = 'pubmed')
  res_records <- EUtilsGet(res_search)
  return(res)
}

# Search by doi
# Retrieve pmid based on doi
search_by_doi <- function(doi) {
  results <- get_pubmed_ids(doi)
  res <- NA
  output <- NA
  if (length(results) > 0)
    res <- results$IdList$Id[1]
  if (!is.na(res)) {
    output <- getPubMedInfo(res)
  }
  return(output)
}
#Search by list of pubmed ids

# Batch downloads of pubmed string search
# Download format_type can be 'xml', 'medline', 'abstract', 'text'
# This is a slow query
# pubmed_query_string: String (character-vector of length 1):
# this is the string used for querying PubMed (the standard PubMed Query synthax applies).
# dest_dir: file path required to  save results
# String (character-vector of length 1)
# dest_file_prefix: String (character-vector of length 1):
# this string is used as prefix for the files that are written locally.
# format: String (character-vector of length 1):
# Acceptable values are: c("medline","uilist","abstract","asn.1", "xml").
# When format != "xml", data will be saved as text files (txt).
# batch_size: Integer (1 < batch_size < 5000)
# maximum number of records to be saved in a single xml or txt file.
batchTopicSearch <-
  function(queried_string,
           batch_size = 5000,
           format_type = 'xml',
           prefix = 'record_') {
    #Create a folder for  the output
    path <- paste0('output/', Sys.time(), '/')
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
    return(res)
    
  }
#
#
# Batch pubmed ids search
batchPmidSearch <- function(ids) {
  output_df <- data.frame()
  failed_ids <- c()
  for (i in ids) {
    print(i)
    if (!is.na(i)) {
      try(temp_df <- getPubMedInfo(i))
      if (exists('temp_df') && nrow(temp_df) > 0) {
        output_df <- bind_rows(output_df, temp_df)
      } else {
        failed_ids <- c(i, failed_ids)
      }
    }
  }
  output_df <- output_df %>% arrange(desc(`Publication Year`))
  openxlsx::write.xlsx(output_df, 'output/batchpmidsearch.xlsx', overwrite = T)
  return(list(output_df, failed_ids))
}
#
#

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
  read_doc <- read_xml(f) %>% as_list()
  output <-
    paste0(pubmed_id, '_', pub_year, '_', author, '_', article_title)
  if (nchar(output) > 240)
    output <- substr(output, 1, 240)
  filename <- paste0(output, '.pdf')
  filepath <- paste0(author, '/', pub_year, '/')
  return(list(filepath, filename))
}

# Format results
# output_type can be 'xml', 'text', 'abstract'
format_results <- function(res, output_type = 'xml') {
  # parsing results into a data frame
  formatted_df<-data.frame()
  if (output_type == 'topic') {
    #topicSearch output a S4 Object
    doc_num<-length(res@PMID)
 
    #  "Author"  
    selected_names <- c(
      "PMID",
      "YearArticleDate",
      "MonthArticleDate",
      "DayArticleDate",
      "YearPubmed",
      "MonthPubmed" ,
      "DayPubmed",
      "ISSN" ,
      "Title" ,
      "ArticleTitle",
      "AbstractText",
      "Language"  ,
      "PublicationType"  ,
      "PublicationStatus" ,
      "ArticleId" ,
      "DOI" ,
      "Volume" ,
      "Issue",
      "MedlinePgn" ,
      "Country"
      #"Keywords"
    )
    
    temp_df <- lapply(selected_names, function(nm) slot(res, nm))
    temp_df<-as.data.frame(setNames(temp_df, selected_names))
    
    #parsing Authors
    
    authors_list <- res@Author
    authors <-
      data.frame('Authors' = NA,
                 'First_Author' = NA,
                 'Pubmed_id' <- NA)
    if (length(authors_list) >= 1) {
      for (i in 1:length(authors_list)) {
        publication_id <- names(res@Author)[[i]]
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
        authors[i,] <- c(all_authors, first_author, publication_id)
      }
    }
    names(authors)<-c("Authors",  'First_Author',"PMID")
    merged_df<-bind_cols(temp_df,authors)
    merged_df<-merged_df[,1:(ncol(merged_df)-1)]
    names(merged_df)[1]<-'PMID'
    formatted_df<-merged_df
  }
  if (output_type == 'xml') {
    
    #write xml output
    temp_filename <- "output/output.xml"
    fileConn <- file(temp_filename)
    writeLines(output, fileConn)
    close(fileConn)
    
    #parse xml
    doc <- read_xml(temp_filename) 
    
    #number of results
    #doc_num<-length(xml_find_all(doc, "./PubmedArticleSet"))
    doc_num<-length(xml_find_all(doc, "./PubmedArticle"))
    
    
    for(i in 1:doc_num){
      read_doc<- doc[[i]] %>% as_list()
      
      abstract_text <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]]
      abstract <- paste(unlist(abstract_text), collapse = ' ')
      
      journal_type <-read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]]$Journal
      
      journalTitle <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["Title"]]
      
      journalIssue <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["Issue"]]
      
      journalStartPage <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["StartPage"]]
      journalEndPage <-     read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["EndPage"]]
      MedlinePgn <-    read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Pagination"]][["MedlinePgn"]]
      
      pub_Date <-
        unlist(read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["PubDate"]])
      pub_Date4 <- paste(pub_Date, collapse = '-')
      pub_Date2 <- read_doc$PubmedArticleSet$PubmedArticle$MedlineCitation$DateCompleted
      pub_Date3 <- paste(unlist(pub_Date2), collapse = '-')
      articleDate <-
        unlist(read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
      pub_year <- paste(articleDate[1])
      if (is.null(pub_year) ||
          length(pub_year) == 0)
        pub_year <- pub_Date2[[1]]
      if (is.null(pub_year) ||
          length(pub_year) == 0)
        pub_year <- pub_Date[1]
      articleDate <- paste(articleDate, collapse = '-')
      
      journalVolume <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["Volume"]]
      
      authors <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["AuthorList"]]
      
      articleTitle <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["ArticleTitle"]]
      articleTitle <- paste(unlist(articleTitle), collapse = '')
      
      keywords <-
        read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["KeywordList"]]
      
      doi <- read_doc[["PubmedArticleSet"]][["PubmedArticle"]][["MedlineCitation"]][["Article"]][['ELocationID']]
      doi_nodes_pubmed <- xml_find_all(doc, './/ArticleId')
      doi_nodes_pubmed_index <- 0
      for (l in 1:length(doi_nodes_pubmed)) {
        if (xml_attrs(doi_nodes_pubmed[[l]])[["IdType"]] == 'doi') {
          doi_nodes_pubmed_index <- l
        }
      }
      doi1 <- xml_text(doi_nodes_pubmed[[doi_nodes_pubmed_index]], "IdType" == 'doi')
      doi_nodes_medline <- xml_find_all(doc, './/ELocationID')
      doi2 <- xml_text(doi_nodes_medline[xml_attr(doi_nodes_medline, 'EIdType') == 'doi'])
      
      
      #format output
      keywords <- paste(unlist(keywords), collapse = '; ')
      
      authors_df <- rbindlist(authors, fill = TRUE)
      authors_df <- authors_df[, c("LastName", "Initials")]
      authors_matrix <- matrix(apply(authors_df, 1, paste, collapse = ' '), ncol = 1)
      authors_formatted <- paste(authors_matrix[, 1], collapse = ', ')
      
      apa_citation <- paste0(authors_formatted, '. ')
      
      if (!is.null(pub_year) && length(pub_year) > 0) {
        apa_citation <- paste0(apa_citation, pub_year, '. ')
      }
      apa_citation <- paste0(apa_citation, articleTitle, ' ')
      
      if (!is.null(journalTitle)) {
        apa_citation <- paste0(apa_citation, stringr::str_to_title(journalTitle[[1]]), ' ')
      }
      
      if (!is.null(journalVolume)) {
        apa_citation <- paste0(apa_citation, journalVolume[[1]])
      }
      if (!is.null(journalIssue)) {
        apa_citation <- paste0(apa_citation, '(', journalIssue[[1]], '):')
      }
      if (!is.na(MedlinePgn[[1]])) {
        apa_citation <- paste0(apa_citation, ' ', MedlinePgn[[1]], '. ')
      }
      apa_citation <- paste0(apa_citation, 'PMID: ', pubmed_id)
      if (!is.null(doi1) || length(doi2) > 0) {
        apa_citation <- paste0(apa_citation, '. DOI')
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
          'Publication Year' = ifelse(length(pub_year) > 0, as.character(pub_year), NA),
          DOI = ifelse(length(doi1) > 0, doi1, doi2),
          Keywords = keywords,
          citation = apa_citation,
          citation2 = ifelse(
            length(doi1) > 0,
            paste0('https://doi.org/', doi1, '. '),
            paste0('https://doi.org/', doi2, '. ')
          )
          
        )
      
    }
    
  }
  if (output_type == 'abstract') {
    
  }
  if (output_type == 'text') {
    
  }
  if (output_type == 'rismed') {
    authors <- Author(res_records)
    authors_df <- authors[[1]][, c("LastName", "ForeName", "Initials")]
    authors_df <-
      authors[[1]][, c("LastName", "Initials")]
    authors_matrix <- matrix(apply(authors_df, 1, paste, collapse = ' '), ncol = 1)
    authors_formatted <- paste(authors_matrix[, 1], collapse = '; ')
    
    res <- data.frame(
      PMID = PMID(res_records),
      'Publication Year' = YearPpublish(res_records),
      'Start Page' =  MedlinePgn(res_records),
      Country = Country(res_records)
    )
    
  }
  return(formatted_df)
}


#format authors
get_authors <- function(results) {
  authors_list <- res_records@Author
  authors <-
    data.frame('Authors' = NA,
               'First_Author' = NA,
               'Pubmed_id' <- NA)
  if (length(authors_list) >= 1) {
    for (i in 1:length(authors_list)) {
      publication_id <- names(authors_list)[[i]]
      publication <- authors_list[[i]]
      first_author <- paste0(publication['Author', 'LastName'], ',', publication['Author', 'Initials'], ' et al.')
      all_authors <- c()
      for (j in 1:nrow(publication)) {
        all_authors <- c(all_authors, paste0(publication[j, 'LastName'], ',', publication[j, 'Initials']))
      }
      all_authors <- paste(all_authors, collapse = ', ')
      authors[i, ] <- c(all_authors, first_author, publication_id)
    }
  }
  
}

#Test
test_functions <- function() {
  ##topic search
  queried_string <-
    '(pharmacokinetics OR hepatic clearance OR toxicokinetics) AND (rodent OR mice) AND vivo'
  results <- topicSearch(queried_string, max_results = 5)
  results <- topicSearchByDate(queried_string, 20, 1995, 2023)
  
  res<-format_results(results,'topic')
  #query by pubmed id
  pubmed_id <- 37443298
  
  #query by doi
  doi <- c("10.1038/s41370-023-00582-6")
  results <- search_by_doi(doi)
  
  ##batch search
  pubmed_ids <- c(37443298, 34992839)
  results <- batchPmidSearch(pubmed_ids)
  sucess_df <- results[[1]]
  failed_ids <- results[[2]]
  #upload document
  pubs <- read.csv('input/arnot_pubs.csv', header = T)
  results <- batchPmidSearch(pubs$PMID)
  sucess_df <- results[[1]]
  failed_ids <- results[[2]]
  
  
  batchTopicSearch(
    pubmed_id = queried_string,
    batch_size = 3000,
    format = 'medline',
    prefix = 'test_'
  )
  
}
S4_to_dataframe <- function(s4obj) {
  nms <- slotNames(s4obj)
  
  lst <- lapply(nms, function(nm) slot(s4obj, nm))
  as.data.frame(setNames(lst, nms))
}
