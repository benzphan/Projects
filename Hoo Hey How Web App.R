# Importing ---------------------------------------------------------------


library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(knitr)
# Functions and code ------------------------------------------------------
my.bank <- function(my.pictures,
                    my.bet,
                    n.games,
                    bankroll=100,
                    max.profit=200)
{
  counter <- 0
  ## The amount that you have invested is the money you bet times the number of pictures you've placed it on.
  ## For example, placing $2 on 2 pictures means you have invested $4
  amount.invested <- my.bet * length(my.pictures)
  ## While you still have money and you haven't hit your bankroll goal and you aren't tired
  while (bankroll >= amount.invested & 
         bankroll < max.profit & counter < n.games)
  {
    ## place bets
    bankroll <- bankroll - amount.invested
    ## The dice are shaken
    the.dice <- sample(x = 6,
                       size = 3,
                       replace = TRUE)
    
    ## This is the money you get back for placing your money on the right picture
    original.amount <- sum(my.pictures %in% the.dice)
    ## This is the money that the banker pays out to you
    payment <- sum(the.dice %in% my.pictures)
    
    winnings <- (original.amount + payment) * my.bet
    bankroll <- bankroll + winnings
    counter <- counter + 1
  }
  bankroll
  
}
## This function just simulates the same game multiple times
my.bank.simulated <- function(n.sims=100,
                              bankroll = 100,
                              my.pictures,
                              my.bet,
                              n.games,
                              max.profit = 200)
{
  my.results <- numeric(n.sims)
  for (i in 1:n.sims)
  {
    my.results[i] = my.bank(
      bankroll = bankroll,
      my.pictures = my.pictures,
      my.bet = my.bet,
      n.games = n.games,
      max.profit = max.profit
    )
  }
  ## Return the profit that you've made
  my.results - bankroll
}





# UI ----------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",
                  title = "R Project: Hoo Hey How Simulator",
                  tabPanel("Simulator",
                           sidebarPanel(
                             tags$h3("Input:"),
                             checkboxGroupInput(inputId = 'PictureSelection',
                                                label = 'Which pictures do you want to choose?', 
                                                choiceNames = list('fish', 'shrimp', 'crab', 'rooster', 'gourd','tiger'), 
                                                choiceValues = c(1,2,3,4,5,6)),
                             sliderInput(
                               inputId = 'Bet',
                               label = 'How many dollars do you want to bet on each picture?',
                               min = 1,
                               max = 20,
                               value = 1
                             ), 
                             
                             sliderInput(inputId = 'Games', 
                                         label = 'How many games until you stop or get tired of playing?',
                                         min = 1, max = 100, value = 50),
                             
                             numericInput(inputId = 'Bankroll', label = 'How much money do you start with?', 
                                          min = 1, max = 1000, value = 100, step = 1),
                             
                             numericInput(inputId = 'MaxProfit', 
                                       label = 'What is the max amount of money you want to have at the end of the night?', 
                                       value = 200, min = 1, max=5000,step = 1),
                             
                             actionButton(inputId = "submitbutton",
                                          label = "Submit", 
                                          class = "btn btn-primary")
                           ), # sidebarPanel
                           mainPanel(
                             tags$label(h3('Status/Output')), # Status/Output Text Box
                             verbatimTextOutput('contents'),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Plot", plotOutput("plotdata")),
                                         tabPanel("Summary", verbatimTextOutput("summary"))
                                         )
                             
                             
                           ) # mainPanel
                           
                  ), #Simulator, tabPanel
                  tabPanel("About", 
                           titlePanel("About"), 
                           div(includeMarkdown("about.md"), 
                               align="justify")
                  ) #tabPanel(), About
                  ,
                  tabPanel("Code", 
                           titlePanel("My Code"), 
                           div(includeMarkdown("Code-HooHeyHow.md"), 
                               align="justify")
                  ),
                  tabPanel("Contact", 
                           titlePanel("Benz Phan"), 
                           div(includeMarkdown("Contact-HooHeyHow.md"), 
                               align="justify")
                  )
                  
                ) # navbarPage
) # fluidPage


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  #Input data
  datasetInput <- reactive({
    my.data <-data.frame(my.bank.simulated(bankroll = input$Bankroll, n.sims = 10000,
                                           my.pictures = input$PictureSelection, 
                                           my.bet = input$Bet, 
                                           n.games = input$Games, 
                                           max.profit = input$MaxProfit))
    colnames(my.data) <- 'Profits'
    my.data
    }) #Input data 
  
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete. 10000 simulations ran.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Simulation results
  output$plotdata <- renderPlot({
    if (input$submitbutton>0) { 
      isolate(ggplot(data = datasetInput(), mapping = aes(datasetInput()[,"Profits"])) +
                geom_histogram(bins = 30, fill="#56B4E9") +
                labs(title = 'Frequency of Profits', subtitle = 'For 10000 Nights', 
                     x = 'Profit', y = 'Frequency',
                    caption = sprintf('The mean profit is %1.2f', mean(datasetInput()[,"Profits"]))) +
                theme_fivethirtyeight() +
                theme(axis.title = element_text()) +
                geom_vline(xintercept = mean(datasetInput()[,"Profits"]))) 
    } 
  })
  
  # Summary of results
  output$summary <- renderText({
    if(input$submitbutton>0){
      isolate(sprintf("You lose about %1.2f dollars a night.\nThe most that you lost in the 10000 nights was %1.2f.\nThe most that you gained in the 10000 nights was %1.2f", 
                      mean(datasetInput()[,"Profits"]),
                      min(datasetInput()[,"Profits"]),
                      max(datasetInput()[,"Profits"])))
    }
  })
} # server


# Shiny Object ------------------------------------------------------------
shinyApp(ui = ui, server = server)
