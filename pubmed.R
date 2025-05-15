#code to extract and format pubmed references from ncbi through R.
library(plyr)
library(RISmed)

library(xml2)
library(openxlsx)

library(data.table)
# Load required libraries
library(xml2)  # For XML parsing
library(easyPubMed)
library(tidyverse)


#Functions

#' Search Pubmed
#'
#' @param query string 
#' @param search_type specfiies the kind of pubmed search performed. Can be 'pubmed_id' or 'author'.
#' @return an xml object 
getPubMedInfo <- function(query = NA,
                          search_type = 'pubmed_id') {
  
  # retmax maximum number of records retrieved per search
  retmax <- 1
  
  #if query field is populated, set retmax
  if (!is.na(query)) {
    #for a pmid search
    if (search_type == 'pubmed_id') {
      retmax <- 1
    }
    #for an author search
    if (search_type == 'author') {
      retmax <- 999
    }
  }
  
  #perform pubmed search
  res <- get_pubmed_ids(query)
  output <- fetch_pubmed_data(res, 0, retmax, format = "xml")
  
  return(output)
  
}

#format authors

get_authors_details_xml <- function(xml_data) {
  pmid <- xml_find_first(xml_data, "//PMID") %>% xml_text()
  
  # Extract author information
  authors <- xml_find_all(xml_data, "//Author")
  last_names <- xml_find_first(authors, ".//LastName") %>% xml_text()
  first_names <- xml_find_first(authors, ".//ForeName") %>% xml_text()
  initials <- xml_find_first(authors, ".//Initials") %>% xml_text()
  
  #String format
  author_names <- sapply(authors, function(author) {
    paste(last_names, first_names)
  })
  
  # Extract author details into a data frame
  authors_df <- data.frame(
    PubMed_Id = NA,
    'First_Author' = NA,
    'Authors' = NA
  )
  first_author <- paste0(last_names[1], ",", initials[1], ' et al.')
  
  all_authors <- c()
  if (length(authors) >= 1) {
    for (i in 1:length(authors)) {
      #author_string<-paste0(last_names[i], ",", first_names[i],'.')
      initialss <- paste(unlist(strsplit(initials[i], '?')), collapse =
                           '. ')
      author_string <- paste0(last_names[i], ",", initialss, '.')
      all_authors <- c(all_authors, author_string)
    }
    all_authors <- paste(all_authors, collapse = ', ')
  }
  authors_df[1, ] <- c(pmid, first_author, all_authors)
  
  return(list('dataframe' = authors_df, 'string' = author_names))
}

# Format results
# output_type can be 'text', 'abstract'
fn_format_results_xml <- function(results) {
  
  # data frame used to store parsed data
  formatted_output <- data.frame()
  
  #write xml output into a file
  temp_filename <- "output/output.xml"
  fileConn <- file(temp_filename)
  writeLines(results, fileConn)
  close(fileConn)
  
  # Read the XML file
  xml_data <- read_xml(temp_filename)
  
  #number of results
  doc_num <- length(xml_find_all(xml_data, "./PubmedArticle"))
  print(doc_num)
  #iterate and parse through every record (result)
  for (i in 1:10) {
    print(i)
    xml_data_single <- xml_child(xml_data, n = i)
    
    # Basic extraction of key elements
    pmid <- xml_find_first(xml_data_single, "//PMID") %>% xml_text()
    title <- xml_find_first(xml_data_single, "//ArticleTitle") %>% xml_text()
    journal <- xml_find_first(xml_data_single, "//Journal/Title") %>% xml_text()
    pub_year <- xml_find_first(xml_data_single, "//PubDate/Year") %>% xml_text()
    pub_month <- xml_find_first(xml_data_single, "//PubDate/Month") %>% xml_text()
    pub_day <- xml_find_first(xml_data_single, "//PubMedPubDate/Day") %>% xml_text()
    
    authors <- get_authors_details_xml(xml_data_single)
    author_names <- paste(authors[['string']], collapse = ', ')
    
    # Extract MeSH terms
    mesh_headings <- xml_find_all(xml_data_single, "//MeshHeading")
    mesh_terms <- sapply(mesh_headings, function(heading) {
      descriptor <- xml_find_first(heading, ".//DescriptorName") %>% xml_text()
      descriptor
    })
    mesh_terms <- paste(sort(mesh_terms), collapse = '; ')
    
    #abstract
    abstract <- xml_find_first(xml_data_single, "//AbstractText") %>% xml_text()
    
    
    doi <- NA
    doc <- xml_children(xml_data_single)[[1]]
    ns <- xml_ns(xml_data_single)
    article_ids <- xml_find_all(xml_data_single, '////ArticleId')
    article_ids_nodes <- xml_attr(article_ids, 'IdType')
    if ('doi' %in% article_ids_nodes)
      doi <- xml_text(article_ids, "IdType" == 'doi')
    
    # Create a comprehensive data frame
    one_row <- data.frame(
      pmid = pmid,
      doi = doi,
      title = title,
      journal = journal,
      publication_date = paste0(pub_year, '-', pub_month, '-', pub_day),
      authors = author_names,
      abstract = abstract,
      mesh_terms = mesh_terms
    )
    formatted_output <- bind_rows(formatted_output, one_row)
  }
  if (doc_num == 1) {
    print('here')
    formatted_output <- data.frame(t(formatted_output))
  }
  
  
  
  return(formatted_output)
}

t<-getPubMedInfo('Arnot, JA', 'author')
w<-fn_format_results_xml(t)
