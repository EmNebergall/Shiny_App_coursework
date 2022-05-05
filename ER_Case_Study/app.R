library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count"))),
    column(2, selectInput("rows", "Display Rows", c(1:15)))
  ),
  
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    actionButton(inputId = "back", label = "Previous Story"),
    actionButton(inputId = "forward", label = "Next Story"),
    column(10, textOutput("narrative"))
  ),
  
  # started adding code to display the title from products of the product code 
  # with the next highest count of injuries for the same body part as the most frequent injury from the selected code
  # got stuck, will continue on thursday
  
  fluidRow(textOutput("other_cause"))
  
)

#<< count_top
# to make the input for n flexible remove the hard coded value from the argument
count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}



#>>

server <- function(input, output, session) {
  
  # reactive to drill down to just the data for the product code selected by the user
  selected_code <- reactive(injuries %>% filter(prod_code == input$code))
  # make a new reactive to hold the user input from the drop down menu of displayed row options
  # when i first ran this code, the output showed an extra row. subracting 1 fixed this but i do not know why it happened!
  # I would maybe expect this behavior if R was zero indexed but it is not. Does something change with the indexing in shiny?
  n <- reactive(as.integer(input$rows) - 1)
  
  #<< tables
  output$diag <- renderTable(count_top(selected_code(), diag, n()), width = "100%")
  output$body_part <- renderTable(count_top(selected_code(), body_part, n()), width = "100%")
  output$location <- renderTable(count_top(selected_code(), location, n()), width = "100%")
  #>>
  
  
  summary <- reactive({
    selected_code() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  ## Forward and back arrows to increment through data
  
  # create a value for the first story so that it is not zero
  which_story <- reactiveVal(1)
  
  # variable to hold the maximum number of stories in the code selected by the user
  # selected_code() gets the data for the selected prod code, we want the number of rows of that data
  n_stories <- reactive(selected_code() %>% nrow())
  
  # reset back to the first story if the user picks a different product code
  observeEvent(input$code, {
    which_story(1)
  })
  
  observeEvent(input$forward, {
    which_story((which_story() %% n_stories()) + 1)
  })

  #
  observeEvent(input$back, {
    which_story(((which_story() - 2) %% n_stories()) + 1)
  })
  
  
  # display narrative: get the narrative for the selected code at the index defined by which_story
  output$narrative <- renderText(selected_code()$narrative[which_story()])
  
  # display the title of the most common cause of injuries similar to the most frequent injury caused by the selected code
  selected_code_title <- reactive(products %>% filter(prod_code == input$code) %>% select(title))
  
  
  # need to make this into a function similar to count_top, ran out of time
  next_code_title <- reactive(injuries %>%
                                group_by(prod_code, body_part) %>%
                                summarise(count = as.integer(sum(weight))) %>%
                                # don't pick the current code if the selected code is the top cause of all injuries to the
                                # top body part affected by the selected code
                                filter(prod_code != selected_code(),
                                      # pick the top body part injured in the selected_code
                                       body_part == count_top(selected_code(), body_part, 1)) %>%
                                left_join(products) %>%
                                select(title))
  
  output$other_cause <-
    renderText(
      paste(
        "The most frequent cause of injuries similar to",
        selected_code_title(),
        "is",
        next_code_title()
      )
    )
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  
}

shinyApp(ui, server)