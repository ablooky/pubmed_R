#code to extract and format pubmed references from ncbi through R.
library(RISmed)
library(easyPubMed)
library(XML)
library(xml2)
library(qdap)
library(openxlsx)
#library(methods)
library(tidyverse)
library(impactr)



#Functions

#search some topic on PubMed
topicSearch<-function(query,max_results=5){
  res_search <- EUtilsSummary(query, type='esearch', db='pubmed')
  pmid_results<-res_search@PMID
  res_records<- EUtilsGet(pmid_results[1:max_results])
  
  #parsing results into a data frame
  authors_list<-res_records@Author
  authors<-data.frame('Authors'=NA, 'First_Author'=NA, 'Pubmed_id'<-NA)
  if(length(authors_list) >=1 ){
    for(i in 1:length(authors_list)){
      publication_id<-names(authors_list)[[i]]
      publication<-authors_list[[i]]
      first_author<-paste0(publication['Author','LastName'],',',
                           publication['Author','Initials'], ' et al.' )
      all_authors<-c()
      for(j in 1:nrow(publication)){
        all_authors<-c(all_authors,
                       paste0(publication[j,'LastName'],',',
                              publication[j,'Initials']))
      }
      all_authors<-paste(all_authors, collapse = ', ')
      authors[i,]<-c(all_authors,first_author, publication_id)
    }
  }
  
  #Getting papers metadata
  res<-data.frame(pubmed_id = NA, doi = NA,publication_year = NA,
                  first_author= NA,authors = NA,
                  title = NA, volume = NA,issue = NA,first_page = NA,last_page = NA,
                  ISSN = NA,Publication_type=NA,language = NA,abstract=NA)
  for(i in 1:max_results){
    temp_row<-c(res_records@PMID[i],res_records@DOI[i],res_records@YearPubDate[i],
                first_author=authors$First_Author[i],
                authors[i,'Authors'],res_records@ArticleTitle[i],
                res_records@Volume[i],
                res_records@Issue[i],NA, NA,
                res_records@ISSN[i], res_records@PublicationType[i], res_records@Language[i],
                res_records@AbstractText[i])
    res[i,]<-temp_row
  }
  return(res)
  
}

#search topic within time-frame
topicSearchByDate<-function(query, max_results, year_min = 1995, year_end = 2023) {
  
  print(paste0('Awaiting Query: ', query, '...' ))
  
  search <- topicSearch(query,max_results)
  
  
  
  filtered_search<-search %>% filter(publication_year <= year_end,
                                     publication_year >= year_min)
  print(paste0(nrow(filtered_search), ' Results!'))
  
  return(filtered_search)
  
}

#test
queried_string<-'(pharmacokinetics OR hepatic clearance) AND (rodent OR mice) AND vivo'
search_tox<-topicSearch(queried_string,max_results = 500)
search_tox<-topicSearchByDate(queried_string, 999,1995,2023)
