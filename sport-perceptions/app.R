#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)

# Create variables for ggplot

sports <- read_rds("raw-data/sports.RDS")
organ <- read_rds("raw-data/organ.RDS")


ui <- navbarPage(
    "Perceptions of Sports and Injuries",
    theme = shinytheme("darkly"),

    #### ABOUT

    tabPanel("About",

             # title and subtitle

             h2("How are different sports percieved in terms of injuries?", align = "center"),
             h4(em("How often are sports and injuries discussed together? How might this impact perception of risks?"), align = "center"),
             br(),
             div(),


             br(),

             fluidRow(column(2), column(11,

                                        h4(strong("About this Project")),

                                        #text to introduce project

                                        p("The aim of my project is to ... "),

                                        br(),


             ))),

    #### DATA

    tabPanel("Graphics",
             tabsetPanel(
             tabPanel("Proportions of Tweets",
                      h3("Associations of Tweeting About a Sport and Injuries"),
                      sidebarPanel(
                          helpText("Select to compare between:"),
                          span(),
                          selectInput("plot1", "Different Comparisons:",
                                      choices = list("Sports" = "sports",
                                                     "Organizations" = "organ"),
                                      selected = "Sports")),
                      mainPanel(plotOutput("proportions_plot"))))))


    #### FOOTNOTES


    # Where I got data

    tabPanel("Footnotes",


             h4("References"),
             br(),

             p("I got my data from ... "),
             p("github link ... ")
    )




server <- function(input, output) {

    #### DATA

    output$proportions_plot <- renderPlot({if(input$plot1 == "sports"){
        ggplot(sports, aes(x = sport, y = prop, fill = sport)) + geom_col() +
            geom_errorbar(aes(x = sport, ymin = lower, ymax = upper)) +
            theme_classic() +
            theme(legend.position = "none") +
            labs(title = "Proportion of Sports Tweets Containing Words Related to Injury",
                 subtitle = "Words Searched For: concussion(s), injury(ies), CTE",
                 caption = "From sample of 4,000 random tweets containing reference to the specific sport, scraped on 4/22/20
       Error bars = 95% confidence interval",
                 x = "Sport",
                 y = "Proportion (in %)") +
            scale_x_discrete(labels = c("Basketball", "Football", "Hockey", "Rugby", "Soccer"))
    } else if(input$plot1 == "organ"){ggplot(organ, aes(x = organization, y = prop, fill = organization)) + geom_col() +
            geom_errorbar(aes(x = organization, ymin = lower, ymax = upper)) +
            theme_classic() +
            theme(legend.position = "none") +
            labs(title = "Proportion of Sports Tweet Containing Words Related to Injury",
                 subtitle = "Words Searched For: concussion(s), injury(ies), CTE",
                 caption = "3,200 most recent tweets from a given organization's verified twitter account, scraped on 4/22/20
       Error bars = 95% confidence interval",
                 x = "Organization",
                 y = "Proportion (in %)") +
            scale_x_discrete(labels = c("NBA", "NFL", "NHL", "Super Rugby", "USA Hockey", "USA Rugby", "US Soccer", "World Rugby"))
    }

})
}

# Run the application
shinyApp(ui = ui, server = server)
