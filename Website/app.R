
library(R.utils)
library(shiny)
library(data.table)
library(DT)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)


####### load drug - genetic data #######

#Read in data 

Scores<-fread('Data/RareGPS_top.csv')
Scores[, c('Indication', 'Max phase', 'Indicated mechanism', 'Druggable gene','Predicted mechanism')] <- lapply(
             Scores[, c('Indication', 'Max phase', 'Indicated mechanism', 'Druggable gene','Predicted mechanism')], factor)

Evi<-fread('Data/RareGPS_top_evidence.csv')
Evi[, c('Indication', 'Max phase')] <- lapply(
  Evi[, c('Indication', 'Max phase')], factor)

####### run app #######

## home page
about_page <- tabPanel(
  title = "Home",
  setBackgroundColor(color = "#FFFFFF"),
  do.call(tagList, replicate(6, br(), simplify = FALSE)),
  fluidRow(
    column(width = 3),
    column(
      width = 6,
      div(
        style = "text-align:center;padding:4px; border-color:black;background-color: white;",
        h1("RareGPS: a genetic priority score for uncommon and rare diseases", align = 'center'),
        br(),
        div(
          style = "text-align:center;",
          actionButton("view_scores", "View top RareGPS predictions", class = "btn btn-info", style = "background-color: #66a1ff; color: white; margin: 5px; padding: 10px; width: 250px;"),
          br()
        ),
        div(
          style = "text-align:center;",
          actionButton("view_evidence", "View top RareGPS evidence", class = "btn btn-info", style = "background-color: #66a1ff; color: white; margin: 5px; padding: 10px; width: 250px;"),
          br()
        ),
        div(
          style = "text-align:center;",
          actionButton("view_code", "Generate custom predictions", class = "btn btn-info", style = "background-color: #66a1ff; color: white; margin: 5px; padding: 10px; width: 250px;", onclick="window.open('https://github.com/robchiral/RareGPS', '_blank')"),
          br()
        ),
        div(
          style = "text-align:center;",
          actionButton("view_data", "Download all RareGPS scores", class = "btn btn-info", style = "background-color: #66a1ff; color: white; margin: 5px; padding: 10px; width: 250px;", onclick="window.open('https://doi.org/', '_blank')"),
          br()
        ),
        br(),
        align = 'center'
      )
    )
  ),
  do.call(tagList, replicate(3, br(), simplify = FALSE)),
  h4('A genetics-guided, machine learning-assisted tool informing target selection for 19,356 genes and 161 phecodes', align = 'center'),
  h4('Contact: ron.do [at] mssm.edu', align = 'center'),
  h4('Last update: September 15, 2024', align = 'center'),
  do.call(tagList, replicate(4, br(), simplify = FALSE)),
  fluidRow(
    column(5),  # Adjusted to align logos in the center
    column(
      1,
      align = "center",
      tags$a(
        href = 'https://labs.icahn.mssm.edu/dolab/people/',
        tags$img(src = "sinai.png", height = '70', width = '70')
      )
    ),
    column(
      1,
      align = "center",
      tags$a(
        href = 'https://github.com/rondolab/',
        tags$img(src = "github.png", height = '70', width = '70')
      )
    ),
    column(5)  # Adjusted to align logos in the center
  ),
  do.call(tagList, replicate(3, br(), simplify = FALSE))
)

theme_flatly <- shinytheme("flatly")
 
Gene_page <- tabPanel(
    title='Scores',value="Scores",
    theme=theme_flatly,
    sidebarLayout(
        sidebarPanel(
            tags$h3("Filter results"),
            sliderInput('scoreselection', 'RareGPS cutoff', min=0, max=0.573, value=0.0516, step=0.01),
            selectInput('geneselection', 'Gene', c("All",sort(unique(as.character(Scores$Gene))))),
            selectInput('phecodeselection', 'Phecode', c("All",sort(unique(as.character(Scores$Phecode))))),
            selectInput('indicated', 'Indication', c("All",sort(unique(as.character(Scores$Indication))))),
            tags$h4("Druggability"),
            selectInput('druggable', 'Known druggable gene', c("All",sort(unique(as.character(Scores$`Druggable gene`))))),
            sliderInput('drugnomeai', 'DrugnomeAI cutoff', min=0, max=1, value=0, step=0.01),
            actionButton("clear_filters", "Clear filters", class = "btn btn-warning"),
            tags$h3("Export results"),
            downloadButton("download_filtered", "Download filtered results"),
        ),
        mainPanel(
          dataTableOutput("scores")
        ))
)

Evidence_page <- tabPanel(
  title='Evidence',value="Evidence",
  theme=theme_flatly,
  sidebarLayout(
    sidebarPanel(
      tags$h3("Filter results"),
      sliderInput('evi_scoreselection', 'RareGPS cutoff', min=0, max=0.573, value=0.0516, step=0.01),
      selectInput('evi_geneselection', 'Gene', c("All",sort(unique(as.character(Evi$Gene))))),
      selectInput('evi_phecodeselection', 'Phecode', c("All",sort(unique(as.character(Evi$Phecode))))),
      selectInput('evi_indicated', 'Indication', c("All",sort(unique(as.character(Evi$Indication))))),
      tags$h4("Sources of evidence"),
      sliderInput('evi_num_sources', 'Number of evidence sources', min=0, max=6, value=0, step=1),
      checkboxInput('evi_clinical', 'Clinical genetics', value = FALSE),
      checkboxInput('evi_gex', 'Gene expression', value = FALSE),
      checkboxInput('evi_ga', 'Genetic associations', value = FALSE),
      checkboxInput('evi_mantis', 'Mantis-ML', value = FALSE),
      checkboxInput('evi_mm', 'Mouse models', value = FALSE),
      checkboxInput('evi_tm', 'Text mining', value = FALSE),
      actionButton("evi_clear_filters", "Clear filters", class = "btn btn-warning"),
      tags$h3("Export results"),
      downloadButton("evi_download_filtered", "Download filtered results"),
    ),
    mainPanel(
      dataTableOutput("evidence")
    ))
)

ui <- fluidPage(
    navbarPage(
        id='tabset',
        title = 
            div(
                div(
                    img(src="logo.png",
                        height = "35px",width = "35px",style = "position: relative; margin:-10px 0px; display:left-align;")),''),
        position = 'fixed-top',
        windowTitle="RareGPS",
        header=tags$style(type="text/css", "body {padding-top: 70px;},
  
  "),
        
        theme=shinytheme('cosmo'),
        about_page,
        Gene_page,
        Evidence_page
    ))


server <- function(input, output, session) {

    observeEvent(input$view_scores, {
      updateTabsetPanel(session, "tabset", selected = "Scores")
    })
  
    observeEvent(input$view_evidence, {
      updateTabsetPanel(session, "tabset", selected = "Evidence")
    })
    
    observeEvent(input$clear_filters, {
      updateSliderInput(session, "scoreselection", value = 0.0516)
      updateSliderInput(session, "drugnomeai", value = 0)
      updateSelectInput(session, "geneselection", selected = "All")
      updateSelectInput(session, "phecodeselection", selected = "All")
      updateSelectInput(session, "indicated", selected = "All")
      updateSelectInput(session, "druggable", selected = "All")
    })
    
    observeEvent(input$evi_clear_filters, {
      updateSliderInput(session, "evi_scoreselection", value = 0.0516)
      updateSelectInput(session, "evi_geneselection", selected = "All")
      updateSelectInput(session, "evi_phecodeselection", selected = "All")
      updateSelectInput(session, "evi_indicated", selected = "All")
      updateSliderInput(session, "evi_num_sources", value = 0)
      updateCheckboxInput(session, "evi_clinical", value = FALSE)
      updateCheckboxInput(session, "evi_gex", value = FALSE)
      updateCheckboxInput(session, "evi_ga", value = FALSE)
      updateCheckboxInput(session, "evi_mantis", value = FALSE)
      updateCheckboxInput(session, "evi_mm", value = FALSE)
      updateCheckboxInput(session, "evi_tm", value = FALSE)
    })
  
    output$scores <- renderDataTable(datatable({
      data <- Scores
      
      data <- data[data$`RareGPS` >= input$scoreselection,]
      data <- data[data$`DrugnomeAI` >= input$drugnomeai,]
      if (input$geneselection != "All") {
        data <- data[data$Gene == input$geneselection,]
      }
      if (input$phecodeselection != "All") {
        data <- data[data$Phecode == input$phecodeselection,]
      }
      if (input$indicated != "All") {
        data <- data[data$Indication == input$indicated,]
      }
      if (input$druggable != "All") {
        data <- data[data$`Druggable gene` == input$druggable,]
      }
      
      data
      }, caption = 'Only top RareGPS predictions are shown.', filter='top', options = list(pageLength = 10, scrollX='400px', autoWidth = TRUE)))
    
    output$download_filtered <- downloadHandler(
      filename = function() { paste("filtered_data", Sys.Date(), ".csv", sep = "") },
      content = function(file) {
        data <- Scores
        
        data <- data[data$`RareGPS` >= input$scoreselection,]
        data <- data[data$`DrugnomeAI` >= input$drugnomeai,]
        if (input$geneselection != "All") {
          data <- data[data$Gene == input$geneselection,]
        }
        if (input$phecodeselection != "All") {
          data <- data[data$Phecode == input$phecodeselection,]
        }
        if (input$indicated != "All") {
          data <- data[data$Indication == input$indicated,]
        }
        if (input$druggable != "All") {
          data <- data[data$`Druggable gene` == input$druggable,]
        }

        fwrite(data, file)
      }
    )
    
    output$evidence <- renderDataTable(datatable({
      data <- Evi
      
      data <- data[data$`RareGPS` >= input$evi_scoreselection,]
      data <- data[data$`Sources` >= input$evi_num_sources,]
      if (input$evi_geneselection != "All") {
        data <- data[data$Gene == input$evi_geneselection,]
      }
      if (input$evi_phecodeselection != "All") {
        data <- data[data$Phecode == input$evi_phecodeselection,]
      }
      if (input$evi_indicated != "All") {
        data <- data[data$Indication == input$evi_indicated,]
      }
      if (input$evi_clinical) {
        data <- data[data$`OTP clinical genetics` > 0 | data$`HGMD` > 0,]
      }
      if (input$evi_gex) {
        data <- data[data$`Gene expression` > 0,]
      }
      if (input$evi_ga) {
        data <- data[
          data$`Common variants` > 1.301 | 
            data$`Rare variants` > 1.301 | 
            data$`Rare variants (gene-level)` > 1.301 | 
            data$`Ultrarare variants (gene-level)` > 1.301, 
        ]
      }
      if (input$evi_mantis) {
        data <- data[data$`Mantis-ML` > 0,]
      }    
      if (input$evi_mm) {
        data <- data[data$`Mouse models` > 0,]
      }
      if (input$evi_tm) {
        data <- data[data$`Text mining` > 0,]
      }
      
      data
    }, caption = 'Only top RareGPS predictions are shown.', filter='top', options = list(pageLength = 10, scrollX='400px', autoWidth = TRUE)))
    
    output$evi_download_filtered <- downloadHandler(
      filename = function() { paste("filtered_data", Sys.Date(), ".csv", sep = "") },
      content = function(file) {
        data <- Scores
        
        data <- data[data$`RareGPS` >= input$evi_scoreselection,]
        data <- data[data$`Sources` >= input$evi_num_sources,]
        if (input$evi_geneselection != "All") {
          data <- data[data$Gene == input$evi_geneselection,]
        }
        if (input$evi_phecodeselection != "All") {
          data <- data[data$Phecode == input$evi_phecodeselection,]
        }
        if (input$evi_indicated != "All") {
          data <- data[data$Indication == input$evi_indicated,]
        }
        if (input$evi_clinical) {
          data <- data[data$`OTP clinical genetics` > 0 | data$`HGMD` > 0,]
        }
        if (input$evi_gex) {
          data <- data[data$`Gene expression` > 0,]
        }
        if (input$evi_ga) {
          data <- data[
            data$`Common variants` > 1.301 | 
              data$`Rare variants` > 1.301 | 
              data$`Rare variants (gene-level)` > 1.301 | 
              data$`Ultrarare variants (gene-level)` > 1.301, 
          ]
        }
        if (input$evi_mantis) {
          data <- data[data$`Mantis-ML` > 0,]
        }    
        if (input$evi_mm) {
          data <- data[data$`Mouse models` > 0,]
        }
        if (input$evi_tm) {
          data <- data[data$`Text mining` > 0,]
        }
        
        fwrite(data, file)
      }
    )
}

shinyApp(ui, server)
