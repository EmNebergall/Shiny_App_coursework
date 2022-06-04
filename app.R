# Error: $ operator is invalid for atomic vectors
# This error appears when I try to render plot_df as a table so that I can see if it at least contains data. :(

# Biggest change to make: must add code for EDA to determine which parts of selected or loaded data should be retained for ordination
# add code to handle environmental variables

library(tidyverse)
library(vegan)
library(ape)
library(ggplot2)
library(DT)
library(bslib)
source("ordi_app_functions.R")

# vegan package built in datasets and their accompanying environmental variables
datas <- c("dune", "BCI", "mite")

env_data <- c("dune.env", "BCI.env", "mite.env")

# dataframe to help with matching datasets to environmental data
data_pairs <- data.frame(datas, env_data)

# methods for ordination
ords <- c("PCoA", "NMDS")

# dist/dissim measures to choose from
dists <- c("Euclidean",
           "Bray Curtis",
           "Jaccard")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "slate"),
  # Application title
  titlePanel("Ordination Example"),
  
  # select data, ord method, and distance/dissimilarity measure
  fluidRow(
    column(4, selectInput("the_data", "Select Data", choices = datas)),
    column(
      4,
      selectInput("ords", "Select Ordination Method", choices = ords)
    ),
    column(
      4,
      selectInput("dists", "Select Distance/Dissimilarity Measure", choices = dists)
    )
  ),
  
  tabsetPanel(
    tabPanel(
      "Data",
      DTOutput("selected_data_2"),
      DTOutput("selected_data_1")
    ),
    tabPanel("Scree Plot", plotOutput("screeplot"),
             fluidRow(column(
               4,
               radioButtons(
                 inputId = "axis_1",
                 label = "Select the component to display on the horizontal axis",
                 choices = seq(1, 4),
                 selected = 1,
                 inline = FALSE
               ),
               radioButtons(
                 inputId = "axis_2",
                 label = "Select the component to display on the vertical axis",
                 choices = seq(1, 4),
                 selected = 2,
                 inline = FALSE
               )
             ))),
    tabPanel(
      "Ordination Plot",
      
      DTOutput("plot_data"),

      plotOutput("ordi_plot"),
      # Start out with the choices from the dune dataset and update using observe and update functions on the server side
      # must add checkboxes to select PCO axes to retain for the plot
      fluidRow(column(
        4,
        radioButtons(
          inputId = "env_shape",
          label = "Select a discrete variable",
          choices = c("A1", "Moisture", "Management", "Use", "Manure"),
          selected = c("Management"),
          inline = FALSE
        ),
        radioButtons(
          inputId = "env_color",
          label = "Select a discrete or continuous variable",
          choices = c("A1", "Moisture", "Management", "Use", "Manure"),
          selected = c("Moisture"),
          inline = FALSE
        )
      ))
    )
  ),
  
  
  # checkboxes to pick axes from pcoa or nmds to show on the plot
  #   checkboxGroupInput(inputId = "env_vars",
  #                      label = "Axes to plot: Select 2",
  #                      choices = ## up to 4 or 5 axes/components to choose from?),
  #                      selected = c(1,2),
  #                      inline = FALSE)
  # ),
  
  # to add: buttons to move through the steps one at a time....can I have them appear sequentially after each preceding step is completed?
  # to add: text and a display about the chosen data: how does the user know what methods to choose if they don't have information about the data)
  
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # output is a list: ind_obs is matrix of data to ordinate and env_vars is environmental data
  selected_data <- reactive({
    select_user_data(input$the_data)
  })
  
  
  selected_dis_measure <- reactive({
    input$dists
  })
  
  selected_ord_method <- reactive({
    input$ords
  })
  
  # get the names of the environmental data for the selected dataset
  observeEvent(input$the_data,
               {
                 choices = names(selected_data()$env_vars)
                 
                 updateRadioButtons(
                   inputId = "env_shape",
                   label = "Shape: Select a discrete variable ",
                   choices = choices,
                   selected = c(choices[1]),
                   inline = FALSE
                 )
                 
                 updateRadioButtons(
                   inputId = "env_color",
                   label = "Color: Select a discrete or continuous variable",
                   choices = choices,
                   selected = c(choices[2]),
                   inline = FALSE
                 )
                 
               }, ignoreNULL = FALSE)
  
  
  
  
  # reactive to hold the distance matrix generated using the method selected by the user on the data selected by the user
  ready_data <-
    reactive({
      measure_distance(selected_dis_measure(), selected_data()$ind_obs)
    })
  
  # display data table as first tab
  output$selected_data_1 <- renderDT({
    selected_data()$ind_obs
  })
  output$selected_data_2 <- renderDT({
    selected_data()$env_vars
  })
  
  
  # perform the ordination, results in the_ord object
  the_ord <- reactive({
    my_ord <- ordinate(selected_ord_method(), ready_data())
    
    vectors <- my_ord$vectors
    values <- my_ord$values
    
    return(list(vectors = vectors, values = values))
  })
  
  axis_1_choice <- reactive({
    choice <- as.integer(input$axis_1)
    
    please <- as_data_frame(the_ord()$vectors[, choice])
    
    return(please)
    
  })
  
  
  axis_2_choice <- reactive({
    choice <- as.integer(input$axis_2)
    
    please <- as_data_frame(the_ord()$vectors[, choice])
    
    return(please)
    
  })
  
  env_shape_choice <- reactive({
    x <- input$env_shape
    
    awwyeah <-  selected_data()$env_vars %>% select(x)
    
    return(awwyeah)
    
  })
  
  
  env_color_choice <- reactive({
    x <- input$env_color
    
    awwyeah <-  selected_data()$env_vars %>% select(x)
    
    return(awwyeah)
    
  })
  
  output$screeplot <-
    renderPlot(plot(the_ord()$values[, 3], type = "b"))
  
  # dataframe to feed to ggplot
  # the vectors selected here need to be updated by checkboxinput from the ui
  plot_df <- reactive({
    data.frame(axis_1_choice(),
               axis_2_choice(),
               env_color_choice(),
               env_shape_choice())
  })
  
  output$plot_data <- renderDT(plot_df())
  
  

  
 output$ordi_plot <- renderPlot({

    # draw the ordination
    ggplot(
      data = plot_df(),
      mapping = aes(
        x = plot_df()[1],
        y = plot_df()[2],
        color = plot_df()[3],
        shape = plot_df()[4]
      )
    ) +
      geom_point(size = 4) +
      xlab("Axis 1") +
      ylab("Axis 2")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
