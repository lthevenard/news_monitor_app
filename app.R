library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(bslib)
library(emo)

ref <- readRDS("ref.rds")
news <- readRDS("news.rds")

first_date <- ref$first_date_available
last_update <- ref$last_update
ref_descriptions <- describe_ref(first_date, last_update)
min_date <- dmy(first_date)
max_date <- dmy(last_update)


ui <- fluidPage(
    theme = bs_theme(bootswatch = "journal", primary = "#183d7a", secondary = "#3f8ccb", info = "#50327c"),
    titlePanel(NULL, windowTitle = paste(emo::ji("newspaper"), "RegNotícias")),
    h1(style = "line-height:150%; color:#183d7a;", "RegNotícias - Monitor de notícias sobre regulação"),
    hr(), br(),
    
    sidebarLayout(
        sidebarPanel(
          h3(style = "line-height:150%; color:#051d45;", "Filtrar notícias"),
          selectInput("source", "Fonte:", choices = sources, selected = sources[1]),
          selectInput("filter", "Opção:", choices = options, selected = options[1]),
          dateInput("cut_date", "Visualizar notícias a partir de:", value = min_date, min = min_date, max = max_date),
          br(),
          
          h3(style = "line-height:150%; color:#051d45;", "Ler notícia"), 
          textInput("id_selected", "Informe o ID da notícia que deseja ler", value = news$agencias$id[[nrow(news$agencias)]]),
          actionButton("go", "Ler notícia selecionada"),
          
          br(), hr(), br(), 
          HTML(ref_descriptions$first_date),
          HTML(ref_descriptions$last_update),
          br(),
          HTML("<p>Lucas Thevenard   |   <a href = 'https://github.com/lthevenard/news_monitor_app'>Github</a></p>"),
          
        ),

        mainPanel(
          tabsetPanel(id = "inTabset",
            tabPanel(title = paste(emo::ji("newspaper"), "Explorar notícias"), 
                     value = "explorar",
                     br(), HTML(inform_table),   
                     h2(style = "line-height:150%; color:#051d45;", "Notícias"), hr(), 
                     dataTableOutput("news_table"), br(), br()),
            tabPanel(title = paste(emo::ji("face_with_monocle"), "Ler notícia selecionada"), 
                     value = "ler",
                     br(), HTML(inform_text), br(), hr(), 
                     br(), htmlOutput("news_text"), br(), br(), 
                     actionButton("back", "Voltar para a tabela de notícias"), br(), br(), br())
          )
        )
    )
)


server <- function(input, output, session) {
  observeEvent({input$source}, {
    temp_options <- restrict_options_from_source(input$source)
    updateSelectInput(session, "filter", choices = temp_options, selected = temp_options[1])
  })
  
  generate_table <- reactive({
    news %>% 
      table_selection(input$source, input$filter, input$cut_date)
  })
  
  output$news_table <- renderDataTable(
    generate_table() %>% 
      column_styles() %>% 
      datatable(selection = 'single', rownames = FALSE, escape = FALSE, options = list(language = DT_text_translations))
  )
  
  observeEvent({input$news_table_rows_selected}, {
    index <- input$news_table_rows_selected[[1]]
    id_updated <- generate_table()$ID[index]
    updateTextInput(session, "id_selected", value = id_updated)
  })
  
  observeEvent({input$go}, {
    updateTabsetPanel(session, "inTabset", selected = "ler")
  })
  
  observeEvent({input$back}, {
    updateTabsetPanel(session, "inTabset", selected = "explorar")
  })
  
  get_news_texts <- eventReactive({input$go}, {
    id_selected <- input$id_selected %>% as.numeric()
    news_row <- bind_rows(news) %>% filter(id == id_selected)
    return(list(
      article = news_row$article[[1]], 
      title = news_row$title[[1]], 
      url = news_row$url[[1]],
      agencia = news_row$agencia[[1]],
      date = news_row$date[[1]]
    ))
  })
  
  output$news_text <- renderUI({
    texts <- get_news_texts()
    prepare_text(
      texts$article, 
      texts$title, 
      texts$url,
      texts$agencia,
      texts$date
    ) %>% HTML()
  }) 
}


shinyApp(ui = ui, server = server)
