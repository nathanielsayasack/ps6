library(tidyverse)
library(shiny)
college <- read_delim("college.csv")


ui <- fluidPage(
  titlePanel("The Burden of College Tuition by Nathaniel Sayasack"),
  tabsetPanel(
    tabPanel("Overview",
             br(),
             HTML("<h3>General Information</h3>",
                  "The skyrocketing cost of college tuition has become a major obstacle for students seeking higher education. With tuition fees continuing to rise at a much faster pace than inflation, many students are forced to take on enormous debt burdens, which can take decades to pay off. This can limit their ability to pursue other goals such as buying a home, starting a business, or saving for retirement."), 
             ),
    
    tabPanel("Plot",
             titlePanel("Average Tuition by Type of College for Each State"),
             sidebarPanel(
               fluidRow(
                 column(10,
                        radioButtons(inputId = "plot_type" , label = "Select the type of college", choices = c("Public Schools In-State", "Public Schools Out-of-State", "Private Schools"))
                 ),
               )
             ),
             mainPanel(
               plotOutput("myplot")
             )
    ),
    
    tabPanel("Table",
             titlePanel("Average Tuition by Type of College for Each State"),
             sidebarPanel(
               fluidRow(
                 column(10,
                        radioButtons(inputId = "plot_type" , label = "Select the type of college", choices = c("Public Schools In-State", "Public Schools Out-of-State", "Private Schools"))
                 ),
               )
             ),
             mainPanel(
               dataTableOutput("mytable")
             )
    )
    )
  )

server <- function(input, output) {
  output$myplot <- renderPlot({
    if (input$plot_type == "Public In-State") {
      ggplot(college %>% filter(Type == "Public In-State"), aes(reorder(State, Value))) +
        geom_bar(aes(weight = Value), fill = "red", width = 0.75) +
        coord_flip() +
        ggtitle("The average cost of going to an in-state public college in each state") +
        xlab("State") +
        ylab("Average tuition per year in USD") +
        theme_bw(base_size = 10)
    } else if (input$plot_type == "Public Out-of-State") {
      ggplot(college %>% filter(Type == "Public Out-of-State"), aes(reorder(State, Value))) +
        geom_bar(aes(weight = Value), fill = "black", width = 0.75) +
        coord_flip() +
        ggtitle("The average cost of going to an out-of-state public college in each state") +
        xlab("State") +
        ylab("Average tuition per year in USD") +
        theme_bw(base_size = 10)
    } else if (input$plot_type == "Private") {
      ggplot(college %>% filter(Type == "Private"), aes(reorder(State, Value))) +
        geom_bar(aes(weight = Value), fill = "pink", width = 0.75) +
        coord_flip() +
        ggtitle("The average cost of going to private college in each state") +
        xlab("State") +
        ylab("Average tuition per year in USD") +
        theme_bw(base_size = 12)
    }
  })
  output$mytable <- renderDataTable({
    college %>% 
      sample_n(100)
  })
}

shinyApp(ui = ui, server = server)



