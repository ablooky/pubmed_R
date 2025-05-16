#Libraries
library(shiny)
library(DT)

#Importing helper functions
source(file = 'pubmed.R', local = T)



ui <- fluidPage(titlePanel("PubMed Searches"),#####-----
                mainPanel(tabsetPanel(
                  id = 'options',
                  tabPanel(
                    title = 'Pubmed ID Search',
                    numericInput(
                      inputId = 'pubmed_id',
                      label = 'PubMed ID',
                      value = 10611141
                    ),
                  
                    actionButton(inputId = 'submit_search_bttn',
                                 label = 'Retrieve Publication'),
                    hr(),
                    uiOutput('pmid_query'),
                    uiOutput('pmid_results')
                  ),
                  tabPanel(
                    title = 'Author Search',
                    textInput(
                      inputId = 'author_search',
                      label = 'Author Name',
                      value = 'Arnot, JA'
                    ),
                   
                    actionButton(inputId = 'author_search_bttn',
                                 label = 'Retrieve Publications'),
                    hr(),
                    uiOutput('author_query'),
                    uiOutput('author_results')
                  ),
                  
                  tabPanel(
                    title = 'Topic Search',
                    textInput('keyword', 'Enter keywords',
                              'in vitro toxicokinetics'),
                    numericInput('numresults', 'Maximum number of results (999)',
                                 10, 1, 999),
                    actionButton('topic_search_bttn', 'Search Topic'),
                    hr(),
                    #uiOutput('keyword_string'),
                    uiOutput('keyword_results')
                  )
                  #selected = 'Topic Search'
                )))


server <- function(input, output, session) {
  
  output$pmid_query <- renderUI(shiny::tags$h2(''))
  
  #Pmid search
  observeEvent(input$submit_search_bttn, {
    req(input$pubmed_id)
    output$pmid_query <-
      renderUI(shiny::tags$h2(paste0(
        'Searching PubMed ID: ', input$pubmed_id
      )))
    
    output$pmid_results <- renderUI({
      message(input$pubmed_id)
      #results <- data.frame()
      res <- getPubMedInfo(input$pubmed_id, 'pubmed_id')
      results <- format_results_xml(res)
      #print(results)
      DT::renderDT(results, colnames = F
                   #options=list(pageLength=1)
                   )
    })
  })
  
  # Topic search
  observeEvent(input$topic_search_bttn, {
    req(input$keyword, input$numresults)
    output$keyword_string <- renderUI(renderPrint(input$keyword))
    output$keyword_results <- renderUI({
      results <- topicSearch(input$keyword, input$numresults)
      results_without_abstract <- results %>%
        select(-c(abstract))
      DT::renderDT({
        datatable(results_without_abstract,
                  options = list(dom = 'ftp')) %>%
          formatStyle(columns = c(1), width = '400px')
      })
      
    })
  })
  
  # Author Search
  observeEvent(input$author_search_bttn,{
     req(input$author_search)
    output$author_query <-
      renderUI(shiny::tags$h2(paste0(
        'Searching Author: ', input$author_search
      )))
    
    output$author_results <- renderUI({
      results <- data.frame()
      res <- getPubMedInfo(input$author_search, 'author')
      results <- format_results_xml(res)
      DT::renderDT(t(results), colnames = F)
    })
   })
   
 }

# Run the application
shinyApp(ui = ui, server = server)