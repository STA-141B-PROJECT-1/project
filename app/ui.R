library(plotly)
ui <- navbarPage("NBA 2019-2020 Player Radar Plot",
                 tabPanel("Graphic",fluidPage(theme = shinythemes::shinytheme("cerulean")),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('Apply filters'),
                            sidebarPanel(width = 4,
                                         selectInput('player', 'Choose a player:',paste(playerdist$player,"-",playerdist$tm)),
                                         submitButton("Update filters")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("stat graph",fluidRow(
                                            column(6 ,plotlyOutput("plot2", width = 500, height=500)),
                                            column(6,plotlyOutput("plot1",width = 500, height = 500))),
                                     p("Here's is radar plot of the stats of the selected NBA Player",
                                       style = "font-size:25px"))
                                     
                              ),
                              tableOutput("table")
                            )
                          )),
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
                          p("John Tran",style = "font-size:25px")
                 )
)
