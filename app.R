library(shiny)
source(file = 'pubmed.R', local = T)



ui <- fluidPage(
  
  # Application title
  titlePanel("PubMed Search"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = 'pubmed_id', label = 'PubMed ID', value = 10611141),
      radioButtons(inputId = 'search_type', label = 'Choose Search Type', 
                   choices = c("Entire record"='entire', 
                               'Abstract only' ='abstract'), 
                   inline = T),
      actionButton(inputId ='submit_search', 
                   label = 'Retrieve Publication'),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput('header'),
      uiOutput('new_ui')
     
    )
  )
)


server <- function(input, output, session) {
  print('here')
  print('HERE')
  output$header<-renderUI(shiny::tags$h2('Hello'))
  
  observeEvent(input$submit_search, {
    req(input$pubmed_id, input$search_type)
    #session$sendCustomMessage(type = 'testmessage',message = 'Thank you for clicking')
    if(input$submit_search > 0) output$header<-renderUI(shiny::tags$h2(paste0('Search PubMed ID', input$pubmed_id)))
    
    print(input$pubmed_id)
    
   #output$pub_info_summary<-renderTable(topicSearch('in vitro toxicokinetics',1))
    output$new_ui<-renderUI({
      results<-data.frame()
      if(input$search_type == 'abstract'){
        results<-getPubMedAbstract(input$pubmed_id)
        
        #output$pub_info_summary<-renderPrint(results)
        
      } else {
        results<-getPubMedInfo(input$pubmed_id)
       #output$pub_info_summary<-renderTable(results)
      }
      renderTable(results)
      # conditionalPanel(condition = "input.search_type == 'abstract' ",
      #                 shiny::verbatimTextOutput('pub_info_summary')
      # )
      # conditionalPanel(condition = "input.search_type == 'entire' ",
      #                 shiny::tableOutput('pub_info_summary')
      # )
    })
     
  })

}

# Run the application 
shinyApp(ui = ui, server = server)