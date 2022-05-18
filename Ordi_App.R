#

# Biggest change to make: must add code for EDA to determine which parts of selected or loaded data should be retained for ordination
# add code to handle environmental variables

library(shiny)
library(vegan)
library(ape)

datas <- c("dune", "BCI", "mite")

env_data <- c("dune.env", "BCI.env", "mite.env")

data_pairs <- data.frame(datas, env_data)

ords <- c("PCoA", "NMDS")

dists <- c("Euclidean", 
           "Bray Curtis",
           "Jaccard")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ordination Example"),
    
    fluidRow(column(4, selectInput("the_data", "Select Data", choices = data_pairs[ ,1])),
             column(4, selectInput("ords", "Select Ordination Method", choices = ords)),
             column(4, selectInput("dists", "Select Distance/Dissimilarity Measure", choices = dists))),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "env_vars",
                        label = "Environmental Variables: Select 2 to Display on Plot",
                        choices = c("A1","Moisture","Management","Use","Manure"),
                        selected = c("Moisture", "Management"),
                        inline = FALSE)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ord_plot")
        ))
    )


ordinate <- function(ord_method, dist_mat) {
    if (ord_method == "PCoA") {
        ordination_output <- ape::pcoa(selected_data)
    } else {
        if (ord_method == "NMDS") {
            ordination_output <- metaMDS(
                selected_data,
                distance = NULL,
                autotransform = FALSE,
                k = 2
            )
            
        }
    }
    
    return(ordination_output)
}

measure_distance <- function(dis_method, selected_data) {
    if (dis_method == "Euclidean") {
        scale(selected_data)
    } else {
        if (dis_method == "Bray Curtis") {
            vegdist(selected_data)
        } else {
            if (dis_method == "Jaccard") {
                vegdist(selected_data, method = "jaccard")
            }
        }
    }
}



# Define server logic required to draw a histogram
server <- function(input, output) {
  selected_data <- reactive({
    
    #data to pass to the ordination
    ind_obs <- get(data(input$the_data), "package:vegan")
    
    #environmental data from the data pairs object
    env_data <- data_pairs %>% filter(env_data == input$the_data) %>% select(env_data)
    
  })
  
  selected_dis_measure <- reactive({
    input$dists
  })
  
  selected_ord_method <- reactive({
    input$ords
  })
  
  output$env_var_choices <- names(selected_data()$env_data)
  
  ready_data <- measure_distance(selected_dis_measure(), dune)
  
  
  the_ord <- ordination(selected_ord_method(), ready_data())
  
  plot_df <- reactive({
    data.frame(
      axis_1 <- the_ord$vectors[, 1],
      axis_2 <- the_ord$vectors[, 2],
      # these will be changed to get the variable selected by the checkbox input env_vars
      env_color <- selected_data()$env_data[ ,1],
      env_shape <- selected_data()$env_data[ ,2]
    )
    
  })
  
  
  output$ord_plot <- renderPlot({
    # draw the ordination
    ggplot2(
      data == plot_df(),
      mapping = aes(
        x = axis_1,
        y = axis_2,
        color = env_color,
        shape = env_shape
      )
    ) +
      geom_point(size = 4) +
      xlab("Axis 1") +
      ylab("Axis 2")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
