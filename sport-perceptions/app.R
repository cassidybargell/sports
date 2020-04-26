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
library(DT)
library(maps)

# Create variables for ggplots and data tables

sports <- read_rds("raw-data/sports.RDS")
organ <- read_rds("raw-data/organ.RDS")
merged <- read_rds("raw-data/organ.RDS")
rugby_map <- read_rds("raw-data/rugby_map.RDS")
tweets <- read_rds("raw-data/tweets.RDS")
cleaned_words <- read_rds("raw-data/cleaned_words.RDS")
g_search <- read_rds("raw-data/gtrend_search.RDS")
g_search2 <- read_rds("raw-data/gtrend_search2.RDS")

ui <- navbarPage(
    "Perceptions of Sports and Injuries",
    theme = shinytheme("darkly"),

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
                                          become increasingly of concern as awareness has grown of the potential for lifelong effects.
                                          The NFL has come under intense scrutiny for their handling of concussions and CTE prevelance among players.
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

    #### WORDS

    tabPanel("Tweet Words",
             tabsetPanel(
                 tabPanel("Most Commonly Used Words in Tweets",
                          sidebarPanel(
                              helpText("Select to compare between:"),
                              span(),
                              selectInput("plot3", "Sports:",
                                          choices = list("Football" = "football",
                                                         "Rugby" = "rugby",
                                                         "Hockey" = "hockey",
                                                         "Soccer" = "soccer",
                                                         "Basketball" = "basketball"),
                                          selected = "football")),
                          mainPanel(plotOutput("words_plot"))))),

    #### GOOGLE SEARCH

    tabPanel("Google Search Hits",
             tabsetPanel(
                 tabPanel("Google Search Hits",
                          h3("Google Search Hits Through Time"),
                          sidebarPanel(
                              helpText("Select a Search Term to View"),
                              span(),
                              selectInput("plot2", "Options:",
                                          choices = list("Basketball" = "basketball",
                                                         "Football" = "football",
                                                         "Rugby" = "rugby",
                                                         "Soccer" = "soccer",
                                                         "Hockey" = "hockey",
                                                         "Concussion" = "concussion",
                                                         "CTE" = "cte",
                                                         "TBI" = "tbi"),
                                          selected = "basketball")),
                          mainPanel(plotOutput("google_plot"))),
                 tabPanel("Sport and Concussion",
                          h3("Google Search Hits Through Time"),
                          sidebarPanel(
                              helpText("Select a Search Term to View"),
                              span(),
                              selectInput("plot4", "Options:",
                                          choices = list("Basketball&Concussion" = "'basketball concussion'",
                                                         "Football&Concussion" = "'football concussion'",
                                                         "Rugby&Concussion" = "'rugby concussion'",
                                                         "Soccer&Concussion" = "'soccer concussion'",
                                                         "Hockey&Concussion" = "'hockey concussion'"),
                                          selected = "'basketball concussion'")),
                          mainPanel(plotOutput("google_plot2"))))),


    #### TWEETS

    tabPanel("Explore Tweets",

             fluidPage(
                 titlePanel("Explore the Tweets"),

                 sidebarLayout(
                     sidebarPanel(
                         helpText("Pick a Sport or Account to explore their tweets related to concussions and injuries"),
                         h3("Tweet Search"),
                         selectInput("x", NULL,
                                     choices = list("@NCAA" = "ncaa",
                                                    "@NCAAResearch" = "ncaaresearch",
                                                    "@NCAAFootball" = "ncaafoot",
                                                    "@NFL" = "nfl",
                                                    "@NHL"= "nhl",
                                                    "@NBA" = "nba",
                                                    "@USARugby" = "usarugby",
                                                    "@SuperRugby" = "sr",
                                                    "@USARugby" = "usarugby",
                                                    "@USSoccer" = "ussoccer",
                                                    "@USAHockey" = "usahockey",
                                                    "Football" = "football",
                                                    "Rugby" = "rugby",
                                                    "Hockey" = "hockey",
                                                    "Soccer" = "soccer",
                                                    "Basketball" = "basketball"
                                                    ),
                                     selected = "ncaa")),
                     mainPanel(
                         DTOutput("tweet_table"))))),


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

    #### ABOUT

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
            geom_errorbar(aes(x = sport, ymin = lower, ymax = upper), width = 0.5, size = 0.5) +
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
            geom_errorbar(aes(x = organization, ymin = lower, ymax = upper), width = 0.5, size = 0.5) +
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
            geom_errorbar(aes(x = organization, ymin = lower, ymax = upper), width = 0.5, size = 0.5) +
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
            geom_errorbar(aes(x = organization, ymin = lower, ymax = upper), width = 0.5, size = 0.5) +
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
                geom_errorbar(aes(x = organization, ymin = lower, ymax = upper), width = 0.5, size = 0.5) +
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

    #### TWEETS

    output$tweet_table <- renderDT({

            #input filters by choice

        tweet_choice <- tweets %>%
            filter(choice == input$x) %>%
            select(! choice)

        datatable(tweet_choice,
                  class = 'display',
                  rownames = FALSE,
                  selection = 'single',
                  colnames = c("Screen Name", "Text"),
                  options = list(dom = 'tip'))
    })

    #### WORDS

    output$words_plot <- renderPlot({if(input$plot3 == "football"){
        cleaned_words %>%
            group_by(choice) %>%
            count(word) %>%
            arrange(desc(n)) %>%
            filter(choice == "football") %>%
            filter(! word == "football", ! word == "amp", ! word == "de") %>%
            slice(1:15) %>%
            ggplot(aes(x = word, y = n, fill = word), color = word) + geom_col() +
            theme(legend.position = "none") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Most Commonly Used Words in Football Tweets",
                 subtitle = "Sport Search Term Filtered Out",
                 x = "Word",
                 y  = "Count")
    } else if(input$plot3 == "rugby"){cleaned_words %>%
            group_by(choice) %>%
            count(word) %>%
            arrange(desc(n)) %>%
            filter(choice == "rugby") %>%
            filter(! word == "rugby", ! word == "amp", ! word == "de") %>%
            slice(1:15) %>%
            ggplot(aes(x = word, y = n, fill = word), color = word) + geom_col() +
            theme(legend.position = "none") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Most Commonly Used Words in Rugby Tweets",
                 subtitle = "Sport Search Term Filtered Out",
                 x = "Word",
                 y  = "Count")
    }
        else if(input$plot3 == "hockey"){cleaned_words %>%
                group_by(choice) %>%
                count(word) %>%
                arrange(desc(n)) %>%
                filter(choice == "hockey") %>%
                filter(! word == "hockey", ! word == "amp", ! word == "de") %>%
                slice(1:15) %>%
                ggplot(aes(x = word, y = n, fill = word), color = word) + geom_col() +
                theme(legend.position = "none") +
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(title = "Most Commonly Used Words in Hockey Tweets",
                     subtitle = "Sport Search Term Filtered Out",
                     x = "Word",
                     y  = "Count")
        }
        else if(input$plot3 == "soccer"){cleaned_words %>%
                group_by(choice) %>%
                count(word) %>%
                arrange(desc(n)) %>%
                filter(choice == "soccer") %>%
                filter(! word == "soccer", ! word == "amp", ! word == "de") %>%
                slice(1:15) %>%
                ggplot(aes(x = word, y = n, fill = word), color = word) + geom_col() +
                theme(legend.position = "none") +
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(title = "Most Commonly Used Words in Soccer Tweets",
                     subtitle = "Sport Search Term Filtered Out",
                     x = "Word",
                     y  = "Count")
        }
        else if(input$plot3 == "basketball"){cleaned_words %>%
                group_by(choice) %>%
                count(word) %>%
                arrange(desc(n)) %>%
                filter(choice == "basketball") %>%
                filter(! word == "basketball", ! word == "amp", ! word == "de") %>%
                slice(1:15) %>%
                ggplot(aes(x = word, y = n, fill = word), color = word) + geom_col() +
                theme(legend.position = "none") +
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(title = "Most Commonly Used Words in Basketball Tweets",
                     subtitle = "Sport Search Term Filtered Out",
                     x = "Word",
                     y  = "Count")
        }
    })

    #### GOOGLE SEARCH

    output$google_plot <- renderPlot({if(input$plot2 == "basketball"){
        g_search %>%
            filter(keyword == "basketball") %>%
            ggplot(aes(date, hits, color = keyword)) + geom_path() +
            labs(title = "Google Search Hits Over Time",
                 x = "Date",
                 y = "Hits (Normalized to Scale of 100)",
                 color = "Search Term") +
            theme_classic()
    }
        else if(input$plot2 == "football"){
            g_search %>%
                filter(keyword == "football") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot2 == "rugby"){
            g_search %>%
                filter(keyword == "rugby") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot2 == "soccer"){
            g_search %>%
                filter(keyword == "soccer") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot2 == "hockey"){
            g_search %>%
                filter(keyword == "hockey") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot2 == "concussion"){
            g_search %>%
                filter(keyword == "concussion") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot2 == "cte"){
            g_search %>%
                filter(keyword == "CTE") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot2 == "tbi"){
            g_search %>%
                filter(keyword == "TBI") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
})
    output$google_plot2 <- renderPlot({if(input$plot4 == "'basketball concussion'"){
        g_search2 %>%
            filter(keyword == "'basketball concussion'") %>%
            ggplot(aes(date, hits, color = keyword)) + geom_path() +
            labs(title = "Google Search Hits Over Time",
                 x = "Date",
                 y = "Hits (Normalized to Scale of 100)",
                 color = "Search Term") +
            theme_classic()
    }
        else if(input$plot4 == "'football concussion'"){
            g_search2 %>%
                filter(keyword == "'football concussion'") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot4 == "'rugby concussion'"){
            g_search2 %>%
                filter(keyword == "'rugby concussion'") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot4 == "'soccer concussion'"){
            g_search2 %>%
                filter(keyword == "'soccer concussion'") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
        }
        else if(input$plot4 == "'hockey concussion'"){
            g_search2 %>%
                filter(keyword == "'hockey concussion'") %>%
                ggplot(aes(date, hits, color = keyword)) + geom_path() +
                labs(title = "Google Search Hits Over Time",
                     x = "Date",
                     y = "Hits (Normalized to Scale of 100)",
                     color = "Search Term") +
                theme_classic()
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
