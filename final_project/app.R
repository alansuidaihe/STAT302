library(shiny)
library(shinyjs)
library(tidyverse)
library(ggthemes)

# Read and Preprocess Data

coursera <- read_csv("data/coursera.csv") %>% 
  janitor::clean_names() %>% 
  mutate(course_rating = as.numeric(course_rating)) %>% 
  drop_na(course_rating) %>% 
  filter(difficulty_level != "Not Calibrated")
coursera[coursera$difficulty_level == "Conversant", ]$difficulty_level <- "Intermediate"
names(coursera)[names(coursera) == "difficulty_level"] <- "Difficulty Level"

tennis <- read_csv("data/tennis.csv") %>% 
  janitor::clean_names() %>% 
  filter(best_of == 3) %>% 
  drop_na(minutes) %>% 
  filter(minutes > 0 & minutes <= 240) %>% 
  filter(surface != "Carpet")
names(tennis)[names(tennis) == "surface"] <- "Surface"
names(tennis)[names(tennis) == "year"] <- "Year"

# Define a function we will later use

getMax <- function(dataset, method, var = NULL) {
  if(dataset == "Coursera") {
    ds <- data.frame()
    ds <- coursera
  } else if(dataset == "Tennis") {
    ds <- data.frame()
    ds <- tennis
  }
  if(method %in% c("Simple Sampling", "Systematic Sampling")) {
    return(nrow(ds))
  } else if(method == "Cluster Sampling") {
    temp <- ds %>% 
      select(as.symbol(var)) %>% 
      unique() %>% 
      nrow()
    return(temp)
  } else if(method == "Stratified Sampling") {
    temp <- ds %>% 
      group_by(eval(as.symbol(var))) %>% 
      count(sort = T) %>% 
      pull() %>% 
      tail(1)
    return(temp)
  }
}

# ui

ui <- fluidPage(
  titlePanel("Data Sampling Methods"),
  sidebarLayout(
    sidebarPanel(
      strong("We have two datasets for sampling here, 
             Coursera dataset and Tennis dataset."),
      h3("Coursera Dataset"),
      p("This dataset contains the ", 
        strong("Rating"), 
      " for all 
        available courses on the Coursera website in 2021. 
      We want to estimate the mean and standard deviation of this variable."),
      br(),
      p("(a) Do simple random sampling. Choose an appropiate sample size ", strong("n.")),
      p("(b) Do systematic sampling. Choose an appropiate sampling interval ", strong("k.")),
      p("(c) Consider here we have another varible", strong("Difficulty Level."), "It has three
         levels, Beginner, Intermediate and Advanced. Using this variable, do you think it is more appropriate 
        to do cluster sampling, or stratified sampling? Why?"),
      h3("Tennis Dataset"),
      p("This dataset contains the",
        strong("Match Length"),
        "for all ATP (The Association of Tennis Professionals) Men’s Singles match from 2011 to 2020. 
        We want to estimate the mean and standard deviation of this variable."),
      br(),
      p("(a) Do simple random sampling. Choose an appropiate sample size ", strong("n.")),
      p("(b) We have two other variables here, ", strong("Surface"), "and", strong("Year."),
        "Surface has three levels, Hard, Clay and Grass, and Year is from 2011 to 2020.", 
        "We know that, ",
        strong("the match length on different surfaces may vary a lot."),
        "And also, ",
        strong("almost every tennis tournament is held on a yearly basis."),
        "Given the information above, which variable do you think is approriate for cluster sampling? 
        And which variable is approriate for stratified sampling? Why?"),
      p("(c) The matches are recorded in a ", strong("chronological"), 
      " order in this dataset. Given this information, 
        is it still approriate to do systematic sampling here?")
    ),
    mainPanel(
      useShinyjs(),
      selectInput("dataset", "Choose a dataset", c("Coursera", "Tennis")),
      selectInput("method", "Choose a sampling method", 
                  c("Simple Sampling", "Systematic Sampling", "Cluster Sampling", "Stratified Sampling")),
      selectInput("var", "Choose a variable", NULL),
      sliderInput("n", "Choose the sample size", min = 1, max= 2, value = 1, step = 1, ticks = F),
      numericInput("k", "Choose the sampling interval", min = 1, max= 2, value = 1, step = 1),
      sliderInput("nc", "Choose the number of clusters", min = 1, max= 2, value = 1, step = 1, ticks = F),
      sliderInput("ns", "Choose the sample size for each stratum", min = 1, max= 2, value = 1, step = 1, ticks = F),
      actionButton("button", "Resample"),
      tableOutput("sample_summary"),
      tableOutput("population_summary"),
      plotOutput("dist_plot")
    )
  )
)

# server

server <- function(input, output, session) {
  observeEvent(input$dataset, {
    choices <- case_when(
      input$dataset == "Coursera" ~ c("Difficulty Level"),
      input$dataset == "Tennis" ~ c("Surface", "Year")
    )
    updateSelectInput(inputId = "var", choices = choices, selected = choices[1])
  })
  observeEvent(input$method, {
    if(input$method %in% c("Simple Sampling", "Systematic Sampling")) {
      hide("var")
    } else{
      show("var")
    }
    if(input$method == "Simple Sampling") {
      show("n")
      hide("k")
      hide("nc")
      hide("ns")
    } else if(input$method == "Systematic Sampling") {
      hide("n")
      show("k")
      hide("nc")
      hide("ns")
    } else if(input$method == "Cluster Sampling") {
      hide("n")
      hide("k")
      show("nc")
      hide("ns")
    } else if(input$method == "Stratified Sampling") {
      hide("n")
      hide("k")
      hide("nc")
      show("ns")
    }
    observeEvent({
      input$dataset
      input$method
      input$var
      }, {
      if(input$method == "Simple Sampling") {
        updateSliderInput(inputId = "n", 
                          max = getMax(input$dataset, input$method))
      } else if(input$method == "Systematic Sampling") {
        updateNumericInput(inputId = "k", 
                           max = getMax(input$dataset, input$method))
      } else if(input$method == "Cluster Sampling") {
        if((input$dataset == "Coursera" & input$var == "Difficulty Level") |
           (input$dataset == "Tennis" & input$var == "Surface") |
           (input$dataset == "Tennis" & input$var == "Year")) {
          updateNumericInput(inputId = "nc", 
                             max = getMax(input$dataset, input$method, input$var))
        }
      } else if(input$method == "Stratified Sampling") {
        if((input$dataset == "Coursera" & input$var == "Difficulty Level") |
           (input$dataset == "Tennis" & input$var == "Surface") |
           (input$dataset == "Tennis" & input$var == "Year")) {
        updateSliderInput(inputId = "ns", 
                          max = getMax(input$dataset, input$method, input$var))
        }
      }
    })
  })
  sampling <- reactive({
    input$button
    req( (!(input$dataset == "Coursera" & input$var == "Surface")) &
         (!(input$dataset == "Coursera" & input$var == "Year")) &
         (!(input$dataset == "Tennis" & input$var == "Difficulty Level")))
    if(input$dataset == "Coursera") {
      dataset <- coursera
    } else if(input$dataset == "Tennis") {
      dataset <- tennis
    }
    if(input$method == "Simple Sampling") {
      req(input$n >= 1 & input$n <= nrow(dataset) & is.integer(input$n))
      return(dataset %>% 
        sample_n(input$n))
    } else if(input$method == "Systematic Sampling") {
      req(input$k >= 1 & input$k <= nrow(dataset) & is.integer(input$k))
      ini <- sample(1:input$k, 1)
      vec <- seq(ini, nrow(dataset), by = input$k)
      res <- dataset[vec, ]
      return(res)
    } else if(input$method == "Cluster Sampling") {
      req(input$nc >= 1 & input$nc <= getMax(input$dataset, input$method, input$var) & is.integer(input$nc))
      if(input$var == "Difficulty Level") { 
        clusters <- sample(unique(coursera$`Difficulty Level`), size= input$nc, replace = F)
        res <- coursera[coursera$`Difficulty Level` %in% clusters, ]
        return(res)
      } else if(input$var == "Surface") {
          clusters <- sample(unique(tennis$Surface), size= input$nc, replace = F)
          res <- tennis[tennis$Surface %in% clusters, ]
          return(res)
      } else if(input$var == "Year") {
          clusters <- sample(unique(tennis$Year), size= input$nc, replace = F)
          res <- tennis[tennis$Year %in% clusters, ]
          return(res)
      }
    } else if(input$method == "Stratified Sampling") {
      req(input$ns >= 1 & input$ns <= getMax(input$dataset, input$method, input$var) & is.integer(input$ns))
      if(input$var == "Difficulty Level") { 
        res <- coursera %>% 
          group_by(`Difficulty Level`) %>% 
          sample_n(input$ns) %>% 
          ungroup()
        return(res)
      } else if(input$var == "Surface") {
        res <- tennis %>% 
          group_by(Surface) %>% 
          sample_n(input$ns) %>% 
          ungroup()
        return(res)
      } else if(input$var == "Year") {
        res <- tennis %>% 
          group_by(Year) %>% 
          sample_n(input$ns) %>% 
          ungroup()
        return(res)
      }
    }
  })
  output$sample_summary <- renderTable({
    if(input$dataset == "Coursera") {
      dataset <- coursera
      var_main <- "course_rating"
    } else if(input$dataset == "Tennis") {
      dataset <- tennis        
      var_main <- "minutes"
    }
    sampling() %>% 
      summarise(`Sample Size` = n(),
                `Sample Mean` = mean(eval(as.symbol(var_main))),
                `Sample Standard Deviation` = sd(eval(as.symbol(var_main))))
  })
  output$population_summary <- renderTable({
    if(input$dataset == "Coursera") {
      dataset <- coursera
      var_main <- "course_rating"
    } else if(input$dataset == "Tennis") {
      dataset <- tennis        
      var_main <- "minutes"
    }
    dataset %>% 
      summarise(`Population Size` = n(),
                `Population Mean` = mean(eval(as.symbol(var_main))),
                `Population Standard Deviation` = sd(eval(as.symbol(var_main))))
  })
  output$dist_plot <- renderPlot({
    if(input$dataset == "Coursera") {
      dataset <- coursera
      var_main <- "course_rating"
      lab <- "Course Rating"
    } else if(input$dataset == "Tennis") {
      dataset <- tennis        
      var_main <- "minutes"
      lab <- "Match Length (minutes)"
    }
    ggplot(dataset, aes_string(var_main)) +
      geom_histogram(bins = 40) +
      geom_histogram(data = sampling(), fill = "red", bins = 40) +
      labs(
        title = "Histogram: Sampled Data (Red) in the Population",
        x = lab,
        y = "Count",
        caption = "Stat 302 Final Project: Daihe Sui"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 14),
        plot.caption = element_text(size = 12)
      )
  })
}

shinyApp(ui = ui, server = server)