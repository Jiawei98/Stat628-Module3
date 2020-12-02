#Jiawei Huang: shinyapp developer
library(shiny)
library(RColorBrewer)
library(ggplot2)
library(Matrix)
library(wordcloud2)

ui <- fluidPage(
  # head
  headerPanel(h1("Rating my Burger shops")),
  tags$head(tags$style(
    HTML(
      "
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 {
      font-family: 'Lobster', cursive;
      font-weight: 500;
      line-height: 1.5;
      }
      "
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "padding-right: 12px;background:rgba(107,174,214, 0.2);font-weight: Bold;",
      h4(HTML("<b>Pleasea input the resaurant info.</b>")),br(),
      fluidRow(
        column(
          width = 8,
          style = list("padding-right: 7px;"),
          textInput("name", label = "Restaurant Name", value = "Meatheads")
        ),
        column(
          width = 4,
          style = list("padding-left: 7px;"),
          selectInput("state", "State",c('IL','OH',"PA","WI"), selected='IL')
          )
        ),
      textInput("address", label = "Address", value = "1305 S Neil St"),
      
      actionButton("go",
                   style = "padding:10px; font-size:100%;background:rgba(107,174,214, 0.4);font-weight: Bold;",
                   "Get the Reviews!"),
      h5(HTML("<span style=\"color:#D53E4F\"><i>*Note: this rating system is for burger shop only.</i></span>"))
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Rating Infromation",
          htmlOutput(outputId = "error"),
          htmlOutput(outputId = "ave_info"),
          htmlOutput(outputId = "ave_per"),
          htmlOutput(outputId = "plot_text"),
          plotOutput(outputId = "SummaryPlot")
          ),
        
        tabPanel(
          "Review Cloud",
          htmlOutput(outputId = "cloudtext"),
          wordcloud2Output(outputId = "cloud")
        ),
        
        tabPanel(
          "Helpful Decisions",
          tabsetPanel(
            tabPanel(
              "From Review",
              htmlOutput(outputId = "size"),
              htmlOutput(outputId = "service"),
              htmlOutput(outputId = "food"),
              htmlOutput(outputId = "environment"),
              htmlOutput(outputId = "race")
            ),
            tabPanel(
              "From Attributes",
              htmlOutput(outputId = "att_text"),
              htmlOutput(outputId = "RestaurantsPriceRange2"),
              htmlOutput(outputId = "OutdoorSeating"),
              htmlOutput(outputId = "HasTV"),
              htmlOutput(outputId = "Alcohol"),
              htmlOutput(outputId = "BikeParking")
            )
          )
        ),
      
        tabPanel(
          "Contact info",
          p(HTML('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">
                 For any questions concerning this app, please contact the developer</b></span></font><br>')),
          p(HTML('<font size="5"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">
                 <b>Jiawei Huang<br> Email: <i>jhuang455@wisc.edu</i></b></span></font><br>'))
        )
      )
    )
  )
)

