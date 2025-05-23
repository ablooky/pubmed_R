#code to extract and format pubmed references from ncbi through R.
# library(plyr)
# library(RISmed)
# library(data.table)
# Load required libraries
library(xml2)  # For XML parsing
library(easyPubMed)
library(tidyverse)
library(openxlsx)

#Functions

#' Search Pubmed
#'
#' @param query string 
#' @param search_type specfiies the kind of pubmed search performed. Can be 'pubmed_id' or 'author'.
#' @return an xml object 
#' @example fn_getPubMedInfo('3568465', 'pubmed_id')
#' @example fn_getPubMedInfo('Arnot','author')
fn_getPubMedInfo <- function(query = NA,
                          search_type = 'pubmed_id') {
  
  # retmax maximum number of records retrieved per search
  retmax <- NA
  
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
    #for an author search
    if (search_type == 'topic') {
      retmax <- length(query)
    }
  }
  
  #perform pubmed search
  res <- get_pubmed_ids(query)
  output <- fetch_pubmed_data(res, 0, retmax = retmax, format = "xml")
  
  return(output)
  
}

#' Format pubmed search results
#'
#' @param results in an xml object
#' @return a data frame of results
#' @example fn_format_results_xml(fn_getPubMedInfo('Arnot','author'))
fn_format_results_xml <- function(results) {
  
  # data frame used to store parsed data
  formatted_output <- data.frame()
  doc_num<-0
  
  #write xml output into a file
  temp_filename <- "output/output.xml"
  fileConn <- file(temp_filename)
  writeLines(results, fileConn)
  close(fileConn)
  
  # Read the XML file
  xml_data <- read_xml(temp_filename)
  articles<-xml_find_all( xml_data, "//PubmedArticle")
  
  #number of articles
  doc_num <- length(xml_find_all(xml_data, "//PubmedArticle"))
  print(doc_num)
  #iterate and parse through every record (result)
  
  for (i in seq_along(articles)) {

    print(i)
    article<-articles[i]
    pmid <- xml_find_first(article, ".//PMID") %>% xml_text()
    print(paste0("Processing article ", i, ": PMID ", pmid))
    
    # Basic extraction of key elements
    
    title <- xml_find_first(article, ".//ArticleTitle") %>% xml_text()
    journal <- xml_find_first(article, ".//Journal/Title") %>% xml_text()
    pub_year <- xml_find_first(article, ".//PubDate/Year") %>% xml_text()
    pub_month <- xml_find_first(article, ".//PubDate/Month") %>% xml_text()
    pub_day <- xml_find_first(article, ".//PubMedPubDate/Day") %>% xml_text()
    
    
    # Extract author information
    authors <- xml_find_all(article, ".//Author")

    authors_df<-data.frame()
    for(n in seq_along(authors)){
      author<-authors[n]
      last_names <- xml_find_first(author, ".//LastName") %>% xml_text()
      first_names <- xml_find_first(author, ".//ForeName") %>% xml_text()
      initials <- xml_find_first(author, ".//Initials") %>% xml_text()
      authors_df<-bind_rows(authors_df, data.frame(last_names, first_names, initials))
    }
    
    ## Collapse authors
    all_authors <- c()
    if (nrow(authors_df) >= 1) {
      for (author in 1:nrow(authors_df)) {
        initials_string <- paste(unlist(strsplit(authors_df[author,'initials'], '?')), collapse = '. ')
        author_string <- paste0(authors_df[author,'last_names'], ",", initials_string, '.')
        all_authors <- c(all_authors, author_string)
      }
      all_authors <- paste(all_authors, collapse = ', ')
    }
   
    # Extract MeSH terms
    mesh_headings <- xml_find_all(article, ".//MeshHeading")
    mesh_terms<-NA
    if(length(mesh_headings)>0){
      mesh_terms <- sapply(mesh_headings, function(heading) {
        descriptor <- xml_find_first(heading, ".//DescriptorName") %>% xml_text()
        descriptor
      })
      mesh_terms <- paste(sort(mesh_terms), collapse = '; ')
    }
    
    #abstract
    abstract <- xml_find_first(article, ".//AbstractText") %>% xml_text()
    
    #dois
    doi_node <- xml_find_first(article, ".//ArticleId[@IdType='doi']")
    doi <- if (!is.null(doi_node)) xml_text(doi_node) else NA
    
    # Output
    one_row <- data.frame(
      pmid = pmid,
      doi = doi,
      title = title,
      journal = journal,
      publication_date = paste0(pub_year, '-', pub_month, '-', pub_day),
      authors = all_authors,
      abstract = abstract,
      mesh_terms = mesh_terms
    )
    formatted_output <- bind_rows(formatted_output, one_row)
  }
  
  if (doc_num == 1) {
    #print('here')
    formatted_output <- data.frame(t(formatted_output))
  }

  
  
  return(formatted_output)
}

#search some topic on PubMed
#it is recommended to limit max_results to 999
fn_topicSearch <- function(query = query,
                        max_results = 100) {
  print(paste0('Topic search: ', query, '...'))
  res_search <- EUtilsSummary(query, type = 'esearch', db = 'pubmed')
  pmid_results <- res_search@PMID
  res_records <- EUtilsGet(pmid_results[1:max_results])
  
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
    )
    
    temp_df <- lapply(selected_names, function(nm) slot(res_records, nm))
    temp_df<-as.data.frame(setNames(temp_df, selected_names))
 
    #parsing Authors
    
    authors_list <- res_records@Author
    authors <-
      data.frame('Authors' = NA,
                 'First_Author' = NA,
                 'Pubmed_id' <- NA)
    if (length(authors_list) >= 1) {
      for (i in 1:length(authors_list)) {
        print(i)
        publication_id <- names(res_records@Author)[[i]]
        publication <- authors_list[[i]]
       if(length(publication) == 1 && is.na(publication[1])){
          authors[i,] <- c(NA, NA, publication_id)
        }
        #(is.na(publication) == F && nrow(publication) > 0){
        else { 
         first_author <- paste0(publication['Author', 'LastName'], ',',
                                 publication['Author', 'Initials'], ' et al.')
          all_authors <- c()
          for (j in 1:nrow(publication)) {
            all_authors <- c(all_authors,
                             paste0(publication[j, 'LastName'], ',',
                                    publication[j, 'Initials']))
          }
          all_authors <- paste(all_authors, collapse = ', ')
          #authors[i, ] <- c(all_authors, first_author, publication_id)
          authors[i,] <- c(all_authors, first_author, publication_id)
        }
       
      
      }
    }
    names(authors)<-c("Authors",  'First_Author',"PMID")
    merged_df<-inner_join(temp_df,authors, by = 'PMID')

  formatted_df<-merged_df %>% 
    mutate(
           title = ifelse(is.na(Title), ArticleTitle, Title),
           journal = NA, 
           publication_date = paste0(YearArticleDate, '-', MonthArticleDate, 
                                     '-', DayArticleDate)
           ) %>%
    rename(pmid = PMID, 
           authors = Authors,
           abstract = AbstractText,
           doi = DOI) %>%
    select(-c(YearArticleDate, MonthArticleDate, DayArticleDate,
              ISSN, Title, ArticleTitle, PublicationStatus ,
              ArticleId, YearPubmed, MonthPubmed, DayPubmed
              )) %>%
    select(c(pmid, doi, First_Author, title, 
             journal,publication_date,Volume, Issue,
             Language, PublicationType,
             authors, abstract))
  return(formatted_df)
  
}
query<-'in vitro biotransformation half-life rat pesticides'
max_results<-100
t<-fn_topicSearch(query, max_results)
#t<-fn_getPubMedInfo('Arnot, JA', 'author')
t<-fn_getPubMedInfo('10611141', 'pubmed_id')
w<-fn_format_results_xml(t)
w