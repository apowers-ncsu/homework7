library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  #title
  h1(
    "Correlation Exploration"
    ),
  
  #layout
  sidebarLayout(
    
    #sidebar
    sidebarPanel(
      
      #header for corr variable selects
      h2(
        "Select Variables to Find Correlation:"
        ),
      
      #####selections for x and y variables
      #x corr
      selectInput(
        inputId = "corr_x",
        label = "x Variable",
        choices = numeric_vars,
        #selected = names(numeric_vars)[1],
        multiple = FALSE
      ),
      #y corr
      selectInput(
        inputId = "corr_y",
        label = "y Variable",
        choices = numeric_vars,
        #selected = names(numeric_vars)[1],
        multiple = FALSE
      ),
      
      #header for subset radio buttons
      h2(
        "Choose a subset of the data:"
        ),
      
      #####radio buttons
      #hhl_corr
      radioButtons( 
        inputId = "hhl_corr", 
        label = "Household Language", 
        choices = c(
          "All",
          "English only",
          "Spanish",
          "Other"
          )
        ), 
      #fs_corr
      radioButtons( 
        inputId = "fs_corr", 
        label = "SNAP Recipient", 
        choices = c(
          "All",
          "Yes",
          "No"
          )
        ),       
      #schl_corr
      radioButtons( 
        inputId = "schl_corr", 
        label = "Education attainment", 
        choices = c(
          "All",
          "High School not Completed",
          "High School or GED",
          "College Degree"
          )
        ),
      
      #header for selector
      h2(
        "Select a Sample Size"
        ),
      
      ######slider for sample size
      sliderInput(
        "corr_n",
        "", 
        min = 20, 
        max = 500,
        value = 20
        ),
 
      #action here     
      actionButton(
        "corr_sample",
        "Get a Sample!"
        )
      
    ),
    
    #####mainpanel
    mainPanel(
      
      #scatterplot
      plotOutput(
        "plot"
      ),

      #conditional inputs for updates and guessing
      conditionalPanel(
        "input.corr_sample",
        h2(
          "Guess the correlation!"
          ),
        column(
          6, 
          numericInput(
            "corr_guess",
            "",
            value = 0,
            min = -1, 
            max = 1
            )
          ),
        column(
          6, 
          actionButton(
            "corr_submit",
            "Check Your Guess!"
            )
          )
        )
      )
    )
  )

#read in samples
my_sample <- readRDS("my_sample_temp.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ################################################
  ##Correlation tab
  #Create a reactiveValues() object called sample_corr
  #this object should hve two elements, corr_data and corr_truth
  #both should be set to null to start with!
  sample_corr <- reactiveValues(
    corr_data = NULL,
    corr_truth = NULL
  )

  #update input boxes so they can't choose the same variable
  observeEvent(
    c(input$corr_x, input$corr_y),
    {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      
      if (corr_x == corr_y) {
        choices <- choices[-which(choices == corr_x)]
        updateSelectizeInput(
          session,
          "corr_y",
          choices = choices)#we'll cover this kind of thing shortly!
        }
      }
    )
  
  #Use an observeEvent() to look for the action button (corr_sample)
  #Modify the code below (this will need to go in the observeEvent) to
  #subset the data appropriately
  observeEvent(
    input$corr_sample,
    {
      if(input$hhl_corr == "All") {
        hhl_sub <- HHLvals
      } else if(input$hhl_corr == "English Only") {
        hhl_sub <- HHLvals["1"]
      } else if(input$hhl_corr == "Spanish") {
        hhl_sub <- HHLvals["2"]
      } else {
        hhl_sub <- HHLvals[c("0", "3", "4", "5")]
      }
      
      if(input$fs_corr == "All") {
        fs_sub <- FSvals
      } else if(input$fs_corr == "Yes") {
        fs_sub <- FSvals["1"]
      } else {
        fs_sub <- FSvals["2"]
      }
      
      if(input$schl_corr == "All") {
        schl_sub <- SCHLvals
      } else if(input$schl_corr == "High School not Completed") {
        schl_sub <- SCHLvals[as.character(0:15)]
      } else if(input$schl_corr == "High School or GED") {
        schl_sub <- SCHLvals[as.character(16:19)]
      } else {
        schl_sub <- SCHLvals[as.character(20:24)]
      }
      
      corr_vars <- c(input$corr_x, input$corr_y)
      
      subsetted_data <- my_sample |>
        filter(#cat vars first
          HHLfac %in% hhl_sub,
          FSfac %in% fs_sub,
          SCHLfac %in% schl_sub
          ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
        {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .} 
      
      index <- sample(
        1:nrow(subsetted_data), 
        size = input$corr_n, 
        replace = TRUE, 
        prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP)
        )
      
      #Update the sample_corr reactive value object
      #the corr_data argument should be updated to be the subsetted_data[index,]
      #the corr_truth argument should be updated to be the correlation between 
      #the two variables selected: 
      #cor(sample_corr$corr_data |> select(corr_vars))[1,2]
      sample_corr$corr_data <- subsetted_data[index,]
      sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1,2]
    
      #Create a renderPlot() object to output a scatter plot
      #Use the code below to validate that data exists,
      #(you'll need to install the shinyalert package if you don't have it)
      #and then create the appropriate scatter plot
      validate(
        need(!is.null(sample_corr$corr_data), 
             "Please select your variables, subset, and click the 'Get a Sample!' button."
             )
      ) #this is a useful function to add as a placeholder until data is generated!
      
      output$plot <- renderPlot( {
        ggplot(
          sample_corr$corr_data,
          aes_string(
            x = isolate(input$corr_x),
            y = isolate(input$corr_y)
            )
          ) + 
          geom_point()
        } )
    
      #Use this code for the correlation guessing game!
      observeEvent(
        input$corr_submit, 
        {
          close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
          if(close) {
            shinyalert(
              title = "Nicely done!",
              paste0("The sample correlation is ", 
                     round(sample_corr$corr_truth, 4), 
                     "."),
              type = "success"
              )
          } else {
            if(input$corr_guess > sample_corr$corr_truth) {
              shinyalert(title = "Try again!",
                         "Try guessing a lower value."
                         )
              } else {
              shinyalert(title = "Try again!",
                         "Try guessing a higher value."
                        )        
              }
            }
          }
      )
    }
  )
}
 
# Run the application 
shinyApp(ui = ui, server = server)
