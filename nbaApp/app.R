#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Packages'
library(plotly)
library(tidyverse)
library(httr)
library(rvest)
library(devtools)
library(jsonlite)
library(rtweet)
library(reactable)
library(glue)
library(twitteR)
library(ballr)
library(stringr)
library(shiny)


#function to manipulate sidebar panel
  sidebarPanel2 <- function (..., out = NULL,out1 = NULL, out2 = NULL, out3 = NULL, width = 4) 
  {
    div(class = paste0("col-sm-", width), 
        tags$form(class = "well", ...),
        out,
        out1,
        out2,
        out3
    )
  }
#User Interface in SHINY
  ui <- navbarPage("NBA 2019-2020 Player Radar Plot",
                   tabPanel("Graphic",fluidPage(theme = shinythemes::shinytheme("cerulean")),
                            tags$head(
                              tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                            pageWithSidebar(
                              headerPanel('Apply filters'),
                              sidebarPanel2(width = 4,
                                            selectInput('player', 'Choose a player:', paste(playerdist$player,"-",playerdist$tm)),
                                            submitButton("Update filters"),
                                            out = h3("Player Info"),
                                            out1 = column(3,uiOutput("image",width = 500, height = 500)),
                                            out2 =  column(6,verbatimTextOutput("name")),
                                            out3 =  column(9,verbatimTextOutput("team")),
                                            br(),
                                            
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Player Statistics",fluidRow(
                                              column(6 ,plotlyOutput("plot2", width = 500, height=500)),
                                              column(6,plotlyOutput("plot1",width = 500, height = 500))),
                                              
                                            ),
                                            tabPanel("twitter",reactableOutput("tweets"))
                                            
                                ),
                                
                                tableOutput("table"),
                                
                              ))
                            
                   ),
                   tabPanel("Info",p("We used a data set consisting of game statistics from 511 NBA players from basketball-reference.com. The data set
                                   was obtained from ", a("basketball reference", href="https://www.basketball-reference.com/leagues/NBA_2020_totals.html", target="_blank"),
                                     "website using webscraping. This app is an interactive tool that allows any user to choose a player from the NBA and check their stats."),
                            
                            hr(), 
                            p("The available player positions are:",style = "font-size:25px"),
                            p("PG: Point Guard",style = "font-size:15px;color: blue"),
                            p("SG: Shooting Guard",style = "font-size:15px;color: blue"),
                            p("SF: Small Forward",style = "font-size:15px;color: blue"),
                            p("PF: Power Forward",style = "font-size:15px;color: blue"),
                            p("C: Center",style = "font-size:15px;color: blue"),
                            p("SF-SG: Small Forward and Shooting Guard",style = "font-size:15px;color: blue"),
                            p("C-PF: Center and Power Forward",style = "font-size:15px;color: blue"),
                            p("PF-SF: Power Forward and Small Forward",style = "font-size:15px;color: blue"),
                            hr(), 
                            
                            p("The abbreviations used in the radar chart are:",style = "font-size:25px"),
                            
                            p("PPG: Points Per Game",style = "font-size:15px;color: blue"),
                            p("RPG: Rebounds Per Game",style = "font-size:15px;color: blue"),
                            p("AST: Assist Per Game",style = "font-size:15px;color: blue"),
                            p("MPG: Minutes Played Per Game",style = "font-size:15px;color: blue"),
                            p("TOV: Turnovers Per Game",style = "font-size:15px;color: blue"),
                            p("PF: Personal Fouls Per Game",style = "font-size:15px;color: blue"),
                            p("BLK: Blocks Per Game",style = "font-size:15px;color: blue"),
                            p("STL: Steals Per Game",style = "font-size:15px;color: blue"),
                            hr(),
                            p("Other abbreviation",style = "font-size:25px"),
                            p("TOT: total stats for the season",style = "font-size:15px;color: blue")),
                   
                   
                   tabPanel("Developers",
                            p("John Tran",style = "font-size:25px"),
                            p("Alvin Hui",style ="font-size:25px")
                   )
  )
#SERVER
  
  
  function(input, output, session) {
 #reactive data for the dropdown menu selects from the distinct player set   
    selectedData1 <- reactive({
      playerdata %>%
        distinct(input$player, keep_all =TRUE) %>% 
        filter(playerdata$player != gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
    })
    
    
#player stats for the radar plot
    
    selectedData3 <- reactive({
      playerdata %>%
        select(player,tm,pts,trb,ast,stl,blk,tov,pf,mp) %>% 
        filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
      
    })
#player dataframe for the histogram  
    selectedData4 <- reactive({
      playerdata %>%
        select(player,fgpercent,x3ppercent,ftpercent)  %>% 
        filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
      
      
 #for radar plot else statement     
    })
    selectedData5 <- reactive({
      playerdata %>%
        filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) %>% 
        select(pts,trb,ast,stl,blk,tov,pf,mp) 
      
      
    })
#for the stat table    
    selectedData6  <- reactive({
      playerdata %>%
        select(player,tm,pos,pts,trb,ast,stl,blk,tov,pf,mp,fgpercent,x3ppercent,ftpercent) %>% 
        rename(Player = player,Team =tm,Position = pos, PPG = pts,RPG = trb, APG = ast, STL = stl, Turnovers = tov,BPG = blk, `Personal Fouls` = pf, MPG = mp, `FG%` = fgpercent, `3P%` = x3ppercent, `FT%` = ftpercent) %>% 
        filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
      
    })
    # Combine the selected variables into a new data frame
    output$plot1 <- renderPlotly({
      
      if(nrow(selectedData3())==1){
        plot_ly(
          x = c("Field goal", "3 point", "Free throw"),
          y = c(selectedData4()$fgpercent, selectedData4()$x3ppercent, selectedData4()$ftpercent),
          type = "bar", name = selectedData3()[1]) %>% 
          add_trace(y = c(nbaleagueavg$`FG%`,nbaleagueavg$`3P%`,nbaleagueavg$`FT%`),name ="League Averages") %>%  
          layout(title = paste("Shooting Percentages of ", selectedData3()[1]),
                 yaxis = list(title = "Percent",range = c(0,1)))
        #else statement for players that got traded
      }else{
        plot_ly(
          x = c("Field goal", "3 point", "Free throw"),
          y = c(selectedData4()$fgpercent[1], selectedData4()$x3ppercent[1], selectedData4()$ftpercent[1]),
          type = "bar", name = selectedData3()$tm[1]) %>% 
          add_trace(y = c(selectedData4()$fgpercent[2], selectedData4()$x3ppercent[2], selectedData4()$ftpercent[2]), name = selectedData3()$tm[2])%>% 
          add_trace(y = c(selectedData4()$fgpercent[3], selectedData4()$x3ppercent[3], selectedData4()$ftpercent[3]), name = selectedData3()$tm[3]) %>% 
          add_trace(y = c(nbaleagueavg$`FG%`,nbaleagueavg$`3P%`,nbaleagueavg$`FT%`),name ="League Averages") %>%  
          layout(title = paste("Shooting Percentages of ", selectedData3()$player[1]),
                 yaxis = list(title = "Percent",range = c(0,1)))
      }
      
      
    })
    
    
    #radar plot stats are skewed because players average more points per game than any other stat category besides minutes(can add weights but decided not too)
    output$plot2 <- renderPlotly({
      if (nrow(selectedData3()) ==1){   
        plot_ly(
          type = 'scatterpolar',
          mode = "closest",
          fill = 'toself'
        ) %>%
          add_trace(
            r = c(selectedData3()$pts,selectedData3()$trb,selectedData3()$ast,selectedData3()$stl,selectedData3()$blk,selectedData3()$tov,selectedData3()$pf,selectedData3()$mp),
            theta = c("PPG","RPG","AST","STL","BLK","TOV","PF","MPG"),
            showlegend = TRUE,
            mode = "markers",
            name = selectedData3()[1]
          ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,selectedData3()$mp[1] +2)
              )
            ),
            
            showlegend=TRUE
            
          )
      }else{
        plot_ly(
          type = 'scatterpolar',
          mode = "closest",
          fill = 'toself'
        ) %>%
          add_trace(
            r = as.matrix(selectedData5()[1,]),
            theta = c("PPG","RPG","AST","STL","BLK","TOV","PF","MPG"),
            showlegend = TRUE,
            mode = "markers",
            name = selectedData3()$tm[1]
          ) %>%
          add_trace(
            r = as.matrix(selectedData5()[2,]),
            theta = c("PPG","RPG","AST","STL","BLK","TOV","PF","MPG"),
            showlegend = TRUE,
            mode = "markers",
            name = selectedData3()$tm[2]
          ) %>% 
          add_trace(
            r = as.matrix(selectedData5()[3,]),
            theta = c("PPG","RPG","AST","STL","BLK","TOV","PF","MPG"),
            showlegend = TRUE,
            mode = "markers",
            name = selectedData3()$tm[3]
          ) %>% 
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,selectedData3()$mp[1]+2)
              )
            ),
            
            showlegend=TRUE
            
          ) 
        
        
        
      }
      
    })
    #stat table
    output$table <-renderTable({
      selectedData6()
      
    })
    
    #UI under the sidebar panel (player info) 
    #this is also how we access the the photos and make it reactive based on the player
    
    c_id <- reactive({
      lol %>%
        select(player,PlayerID,Team) %>% 
        filter(lol$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player)))
      
    })
    
    c_url <- reactive({
      paste0("https://s3-us-west-2.amazonaws.com/static.fantasydata.com/headshots/nba/low-res/", c_id()$PlayerID[1], ".png")
    })
    
    output$image <- renderUI({
      tags$img(src = c_url())
    })
    
    output$name <- renderText({
      paste("Name: ",c_id()$player[1])
    })
    
    output$team <- renderText({
      paste("Team :", c_id()$Team[1])
    })
    #accessing twitter api using rtweet package to find the tweets about the player
    twitterdata <-reactive({
      search_tweets(input$player , n = 200, include_rts = FALSE,token = twitter_token) %>%
        select(user_id, status_id, created_at, screen_name, text,
               favorite_count,retweet_count,urls_expanded_url) %>%
        select(created_at, status_id,screen_name, text, favorite_count,
               retweet_count,urls_expanded_url )%>%
        mutate(
          Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>") 
        )%>%
        select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url) 
      
      
    })
    #public tweet table
    output$tweets <- renderReactable({
      reactable(twitterdata(), 
                filterable = TRUE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200),
                columns = list(
                  DateTime = colDef(defaultSortOrder = "asc"),
                  User = colDef(defaultSortOrder = "asc"),
                  Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                  Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                  RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                  URLs = colDef(html = TRUE)
                )
      ) 
    })
    
  }

shinyApp(ui = ui, server = server)

