######################
#  VENNY ACAPACÁ APP #
######################

# Authors: Joaquín Tamargo Azpilicueta (joatamazp@alum.us.es)
# February 2021

## install.packages("shinyjs")

## Load libraries

library(shiny)
library(shinyjs)
library(VennDiagram)

ui <- fluidPage(
  
  # Application title
  h1("Venn acapacá"),
  h5("A", a(href = 'https://bioinfogp.cnb.csic.es/tools/venny/', 'Venny'), "adaptation developed with Andalusian accent by", a(href = 'https://twitter.com/tamargojoaquin', "@tamargojoaquin 🐦")),
  tags$br(),
  
  
  ## Introduction to the App
  fluidRow(
    column(4, tags$img(src="logo.png",alt="VennAcapaca_logo", width=300,height=160, align="middle")),
      
    column(7, h3("About Venn Diagrams"),
    p("A Venn diagram, also called primary diagram, set diagram or logic diagram, is a diagram that shows all possible logical relations
    between a finite collection of different sets. In other words, a Venn diagram might come in handy while trying to determine the intersection of two (or more) given datasets."),
    tags$br(),
    
    h3("About the app"),
    p("Venn acapacá is an adaptation of", a(href = 'https://bioinfogp.cnb.csic.es/tools/venny/', 'Venny'), "Venny app.
    The aim of Venn acapacá was initially to perform an independent lookup through", a(href="https://shiny.rstudio.com/","Shiny"),
      "tools at Bioinformatics and Genomic Analysis subject at Universidad de Sevilla (Seville, Spain) Biochemistry Degree. 
      Apart from the educational objectives, Venn acapacá was designed to be an user-friendly web-based tool to visualize Venn diagrams.
      This tool has been tested with a diferentially expressed genes list obtained from RNA-seq analysis, but may be useful with practically
      any two lists you need to examine."),
    tags$br())
  ),
  
  tags$br(),
  h3("Usage"),
  p("Just introduce your datasets pasting up to two lists, with one element per row. You may change some visualization settings using the options available."),
  tags$br(),
  
  ## shinyjs is required for clear buttons
  shinyjs::useShinyjs(),
  
  sidebarLayout(
    
    ## sidebar panel
    sidebarPanel(
      
      ## input datasets panel
      wellPanel(
        h4("Input datasets and info"),
        
        ##title
        column(width = 6,textInput(inputId = "title_group1", label = "Dataset 1 title", value = NULL)),
        column(width = 6,textInput(inputId = "title_group2", label = "Dataset 2 title", value = NULL)),
        
        ## lists
        column(width = 6,textAreaInput(inputId = "group1", label = "Dataset 1", value = 0, resize="vertical")),
        column(width = 6,textAreaInput(inputId = "group2", label = "Dataset 2", value = 0, resize="vertical")),
        
        ## clear buttons
        column(width = 6,actionLink(inputId = "clear1", label = "Clear")),
        column(width = 6,actionLink(inputId = "clear2", label = "Clear")),
        
        ## percentage or absolute frequences to be displayed checkboxes
        checkboxGroupInput(inputId = "which_venn_to_show", label = "Venn diagrams to be shown:", choices = c("Absolute", "Frequency"),
                           selected = c("Absolute", "Frequency"), inline = F ),
      ),
      
      ## customization panel
      wellPanel(
        h4("⚙️ Preferences & customization"),
        
        ## filling color
        selectInput(inputId = "color_choice",label = "Color", choices = c("Black & White", "Colorful"),selected = "Black & White", multiple = F),
        
        ## line width sliding bar
        sliderInput(inputId = "line_width", label = "Line thickness", min = 1, max = 50, value = 3, step = 1, round = T, ticks = T),
        
        ## font size sliding bar
        sliderInput(inputId = "font_cex", label = "Font size", min = 0, max = 10, value = 1.5, step = 0.1, round = T, ticks = T),
        
        ## font type selection based on Venny 2.0 choices
        selectInput(inputId = "font_type",label = "Font family", choices = c("Serif", "Sans Serif", "Monospace"),selected = "Serif", multiple = F)
      )
    ),
    
    ## main panel (where venn diagrams are displayed)
    mainPanel(
      
        ## display absolute and relative venn diagrams
        plotOutput(outputId = "absolute_venn", width = "500px", height = "500px"),
        plotOutput(outputId = "relative_venn", width = "500px", height = "500px")
      )
  ),
  
  ## disclaimer
  tags$br(),
  h4("Disclaimer"),
  p("Thanks for using Venny acapacá! This tool is free to use for all. I made every attempt to ensure the 
         accuracy and reliability of the results provided through this app. However, the information is provided as 
         is without responsibility or liability of any kind. We do not claim ownership and/or copyright on the input data 
         nor the generated image(s) using our serivce.")
)


## server function for Venn acapaca
server <- function(input, output) {
  
  ## reactive responding to changes in lists and creating variables for each one
  list1 <- reactive({strsplit(x = input$group1, split = "\n")[[1]]})
  list2 <- reactive({strsplit(x = input$group2, split = "\n")[[1]]})
  
  ## computing intersection list from previous lists
  intersection_list <- reactive({intersect(x = list1(), y = list2())})

  ## clear buttons response erasing input boxes content
  observeEvent(eventExpr = input$clear1, {shinyjs::reset("group1")})
  observeEvent(eventExpr = input$clear2, {shinyjs::reset("group2")})
  
  ## ABSOLUTE FREQUENCIES VENN DIAGRAM
  
  output$absolute_venn <- renderPlot({
    
    ## modifying parameters depending on colour choice  
    if (input$color_choice == "Colorful"){
      colour=c("cyan3", "orange")
      line_colour="white"
    }
    
    if (input$color_choice == "Black & White"){
      colour="white"
      line_colour="black"
    }
    
    ## modifying font type depending on choice
    if (input$font_type == "Sans Serif"){
      font_face <- "Arial"   
    }
    
    if (input$font_type == "Serif"){
      font_face <- "Times New Roman"
    }
    
    if (input$font_type == "Monospace"){
      font_face <-  "Courier New"   
    }
    
    ## absolute frequencies venn diagram
    
    if ("Absolute"%in%input$which_venn_to_show){
      
      grid.newpage()
      draw.pairwise.venn(area1 = length(list1()), area2 = length(list2()),
                         cross.area = length(intersection_list()), category = c(input$title_group1, input$title_group2), 
                           scaled = TRUE, col=line_colour, fill=colour, alpha=c(0.9,0.75),
                         cat.ce = input$font_cex, cat.pos=c(-30,30), cat.dist=0.04, cex=input$font_cex, 
                         lwd=input$line_width, fontfamily = font_face, cat.fontface="bold", cat.fontfamily = font_face)
    }
  })
  
  ## RELATIVE FREQUENCIES VENN DIAGRAM (same structure as the one above)
  output$relative_venn <- renderPlot({
    
    if (input$color_choice == "Colorful"){
      colour=c("cyan3", "orange")
      line_colour="white"
    }
    
    if (input$color_choice == "Black & White"){
      colour="white"
      line_colour="black"
    }

    if (input$font_type == "Sans Serif"){
      font_face <- "Arial"   
    }
    
    if (input$font_type == "Serif"){
      font_face <- "Times New Roman"
    }
    
    if (input$font_type == "Monospace"){
      font_face <-  "Courier New"   
    }
        
    if ("Frequency"%in%input$which_venn_to_show){

      grid.newpage()
      draw.pairwise.venn(area1 = length(list1()), area2 = length(list2()),
                         cross.area = length(intersection_list()), category = c(input$title_group1, input$title_group2), 
                         scaled = TRUE, col=line_colour, fill=colour, alpha=c(0.9,0.75),
                         cat.ce = input$font_cex, cat.pos=c(-30,30), cat.dist=0.04, cex=input$font_cex, 
                         lwd=input$line_width, fontfamily = font_face, cat.fontface="bold", cat.fontfamily = "Arial", print.mode = "percent")
    }
  })
}

## run the app
shinyApp(ui = ui, server = server)

