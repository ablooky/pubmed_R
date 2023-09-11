#Libraries
library(shiny)
library(DT)

#Importing helper functions
source(file = 'pubmed.R', local = T)

ui <- fluidPage(
  titlePanel("PubMed Searches"),
  mainPanel(
    tabsetPanel(
      id = 'options',
      tabPanel(
        title = 'Pubmed Search',
        numericInput(
          inputId = 'pubmed_id',
          label = 'PubMed ID',
          value = 10611141
        ),
        radioButtons(
          inputId = 'search_type',
          label = 'Choose Search Type',
          choices = c("Entire record" = 'entire',
                      'Abstract only' = 'abstract'),
          inline = T
        ),
        actionButton(inputId = 'submit_search_bttn',
                     label = 'Retrieve Publication'),
        hr(),
        uiOutput('pmid_query'),
        uiOutput('pmid_results')
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
    )
  ))


server <- function(input, output, session) {
  print('here')
  output$pmid_query <- renderUI(shiny::tags$h2(''))
  
  #Pmid search
  observeEvent(input$submit_search_bttn, {
    req(input$pubmed_id, input$search_type)
      output$pmid_query <-
        renderUI(shiny::tags$h2(paste0(
          'Searching PubMed ID ', input$pubmed_id
        )))
    
    output$pmid_results <- renderUI({
      results <- data.frame()
      if (input$search_type == 'abstract') {
        results <- data.frame(Abstract = getPubMedAbstract(input$pubmed_id))
      } else {
        results <- getPubMedInfo(input$pubmed_id) 
      }
      renderTable(t(results), rownames = T, colnames = F)
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
                  options = list(
                    dom = 'ftp'
        )) %>% 
          formatStyle(columns = c(1),width = '400px')
      }
      )
      
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)