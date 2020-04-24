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
library(dplyr)
library(ggplot2)
library(maps)

# Create variables for ggplot

sports <- read_rds("raw-data/sports.RDS")
organ <- read_rds("raw-data/organ.RDS")
merged <- read_rds("raw-data/organ.RDS")
rugby_map <- read_rds("raw-data/rugby_map.RDS")


ui <- navbarPage(
    "Perceptions of Sports and Injuries",
    theme = shinytheme("darkly"),

    #### ABOUT

    tabPanel("About",

             imageOutput("twitter_picture", width = "100%", height = "100%"),
             br(),

             # title and subtitle

             h2("How are different sports percieved in terms of injuries?", align = "center"),
             h4(em("How often are sports and injuries discussed together? How might this impact perception of risks?"), align = "center"),
             br(),
             div(),


             br(),

             fluidRow(column(2), column(11,

                                        h4(strong("About this Project")),

                                        #text to introduce project

                                        p("The aim of this project is to look at how different sports and injuries are
                                          represented and discussed together. Sports injury, and especially head injuries, have
                                          become increasingly of concern as awareness has grown of potential lifelong effects.
                                          The NFl has come under intense scrutiny for their handling of concussions and CTE prevelance among players.
                                          Medical and research data is often inaccessible to the general population, however Twitter is a platform
                                          where opinions and observations can be made public on any issue of concern. How various sports and injuries
                                          are discussed together on Twitter gives insight into not only public awareness, but also how different
                                          organizations choose to address the issue.
                                          "),

                                        h4(strong("How this Project Works")),

                                        #text to introduce project

                                        p("Data was collected from Twitter, both by random search for tweets containing a key word (reference to a sport),
                                          as well as by scraping timelines of various governing sports organizations. Of those tweets, a key word search
                                          for concussion(s), injury(ies), and CTE was used to find the proportion of times those subjects were tweeted
                                          about together. Bootstrap resampling of the data was used to give a 95% confidence interval of these proportions."),

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
                                                     "Organizations" = "organ",
                                                     "Rugby" = "rugby",
                                                     "Football" = "football",
                                                     "NCAA" = "ncaa"),
                                      selected = "Sports")),
                      mainPanel(plotOutput("proportions_plot"))))),


    #### MAPS

    tabPanel("Maps",
             tabsetPanel(
                 tabPanel("Map",
                          h3("Map"),
                          sidebarPanel(
                              helpText("Select to compare between:"),
                              span(),
                              selectInput("plot2", "Different Comparisons:",
                                          choices = list("Rugby" = "rugby_map",
                                                         "Football" = "football_map",
                                                         "Rugby" = "rugby"),
                                          selected = "Rugby")),
                          mainPanel(plotOutput("map_plot"))))),


    #### FOOTNOTES


    # Where I got data

    tabPanel("Footnotes",

             imageOutput("nfl_concussion", width = "100%", height = "100%"),
             br(),

             h3("References"),
             br(),

             p("I scraped Twitter for my data using the rtweet package developed by Michael Kearney, and the necessary
               API keys. I also used data from Google search trends using the gtrendsR package developed by Philippe Massicotte and,
               Dirk Eddelbuettel."),

             br(),

             h3("About Me"),

             p("My name is Cassidy Bargell and I am an undergraduate at Harvard studying Integrative Biology and Government.
               I am generally interested in issues related to education, and love to play rugby. You can contact me by email
               at cassidybargell@college.harvard.edu. The code for this project can be found on my github",
               a(href = "https://github.com/cassidybargell/sports", "here."))))







server <- function(input, output) {

    #### About

    # load twitter picture

    output$twitter_picture <- renderImage({

        list(src = 'raw-data/twitter_picture.jpeg',
             height = 300,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )

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
                 x = "Twitter Verified Accounts",
                 y = "Proportion (in %)") +
            scale_x_discrete(labels = c("NBA", "NCAA", "NCAA Football", "NFL", "NHL", "NCAA Research", "Super Rugby", "USA Hockey", "USA Rugby", "US Soccer", "World Rugby")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot1 == "rugby"){merged %>% filter(sport == "rugby") %>%
            ggplot(aes(x = organization, y = prop, fill = organization)) + geom_col() +
            theme_classic() +
            geom_errorbar(aes(x = organization, ymin = lower, ymax = upper)) +
            labs(title = "Proportion of Tweets From Major Rugby Accounts Containing Words Related to Injury",
                 subtitle = "Words Searched For: concussion(s), injury(ies), CTE",
                 caption = "3,200 most recent tweets from a given organization's verified twitter account, scraped on 4/22/20
       Error bars = 95% confidence interval",
                 x = "Verified Twitter Accounts",
                 y = "Proportion (in %)",
                 fill = "Organizations") +
            theme(legend.position = "none") +
            scale_x_discrete(labels = c("Super Rugby", "USA Rugby", "World Rugby"))
    } else if(input$plot1 == "football"){merged %>% filter(sport == "football") %>%
            ggplot(aes(x = organization, y = prop, fill = organization)) + geom_col() +
            theme_classic() +
            geom_errorbar(aes(x = organization, ymin = lower, ymax = upper)) +
            labs(title = "Proportion of Tweets Relating to Football Containing Words Related to Injury",
                 subtitle = "Words Searched For: concussion(s), injury(ies), CTE",
                 caption = "3,200 most recent tweets from a given organization's verified twitter account, scraped on 4/22/20
       Error bars = 95% confidence interval",
                 x = "Verified Twitter Accounts",
                 y = "Proportion (in %)",
                 fill = "Organizations") +
            theme(legend.position = "none") +
            scale_x_discrete(labels = c("NCAA Football", "NFL"))
    }
        else if(input$plot1 == "ncaa"){merged %>% filter(sport == "all" | sport == "football") %>%
                filter(! organization == "nfl") %>%
                ggplot(aes(x = organization, y = prop, fill = organization)) + geom_col() +
                theme_classic() +
                geom_errorbar(aes(x = organization, ymin = lower, ymax = upper)) +
                labs(title = "Proportion of NCAA Account Tweets Containing Words Related to Injury",
                     subtitle = "Words Searched For: concussion(s), injury(ies), CTE",
                     caption = "3,200 most recent tweets from a given organization's verified twitter account, scraped on 4/23/20
       Error bars = 95% confidence interval",
                     x = "Verified Twitter Accounts",
                     y = "Proportion (in %)",
                     fill = "Organizations") +
                theme(legend.position = "none") +
                scale_x_discrete(labels = c("NCAA", "NCAA Football", "NCAA Research"))
        }
})

    #### FOOTNOTES

    output$nfl_concussion <- renderImage({

        list(src = 'raw-data/nfl_concussion.jpg',
             height = 300,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
}

# Run the application
shinyApp(ui = ui, server = server)
