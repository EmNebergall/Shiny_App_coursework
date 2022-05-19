#

# Biggest change to make: must add code for EDA to determine which parts of selected or loaded data should be retained for ordination
# add code to handle environmental variables

library(shiny)
library(vegan)
library(ape)
library(tidyverse)
library(ggplot2)

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
    
    fluidRow(column(4, selectInput("the_data", "Select Data", choices = datas)),
             column(4, selectInput("ords", "Select Ordination Method", choices = ords)),
             column(4, selectInput("dists", "Select Distance/Dissimilarity Measure", choices = dists))),
    
    # Sidebar with a slider input for number of bins 
    # Start out with the choices from the dune dataset and update using observe and update functions on the server side
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
            #plotOutput("ord_plot")
            DT::dataTableOutput("test"),
            #textOutput("test")
        ))
)


ordinate <- function(ord_method, dist_mat) {
    if (ord_method == "PCoA") {
        ordination_output <- ape::pcoa(dist_mat)
    } else {
        if (ord_method == "NMDS") {
            ordination_output <- metaMDS(
                dist_mat,
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
        dist_mat <- scale(selected_data)
    } else {
        if (dis_method == "Bray Curtis") {
            dist_mat <- vegdist(selected_data)
        } else {
            if (dis_method == "Jaccard") {
                dist_mat <- vegdist(selected_data, method = "jaccard")
            }
        }
    }
    return(dist_mat = dist_mat)
}

select_user_data <- function(user_input) {
    if(user_input == "dune") {
        ind_obs <- get(data(dune))
        env_vars <- get(data(dune.env))
    } else {
        if(user_input == "BCI") {
            ind_obs <- get(data(BCI))
            env_vars <- get(data(BCI.env))
        } else {
            if(user_input == "mite") {
                ind_obs <- get(data(mite))
                env_vars <- get(data(mite.env))
            }
        }
    }
    
    return(list(ind_obs = ind_obs, env_vars = env_vars))
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output is a list: ind_obs is matrix of data to ordinate and env_vars is environmental data
    selected_data <- reactive({select_user_data(input$the_data)})
    
    #output$test <- DT::renderDataTable(selected_data()[[2]])
    
    selected_dis_measure <- reactive({
        input$dists
    })
    
    selected_ord_method <- reactive({input$ords})
    
        # get the names of the environmental data for the selected dataset
    env_var_choices <- reactive({
        names(selected_data()$env_vars)
    })
    
    # reactive to hold the distance matrix generated using the method selected by the user on the data selected by the user
    ready_data <- reactive({measure_distance(selected_dis_measure(), selected_data()$ind_obs)})
    
    # perform the ordination, results in the_ord object
    the_ord <- reactive({ordinate(selected_ord_method(), ready_data()$dist_mat)})
    
    # dataframe to feed to ggplot
    plot_df <- reactive({
      data.frame(
        axis_1 = the_ord()$vectors[, 1],
        axis_2 = the_ord()$vectors[, 2],
        env_color = selected_data()$env_vars[2],
        env_shape = selected_data()$env_vars[3]
      )
    })
    
    
    # test print table
    output$test <- DT::renderDataTable(plot_df[1])
    
    
    output$ordi_plot <- renderPlot({
        # draw the ordination
        ggplot(
            data == plot_df,
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

