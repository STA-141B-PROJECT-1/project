library(plotly)
library("ballr")

playerdata <- NBAPerGameStatistics(season = 2020)
names(playerdata)
a <- playerdata %>% filter("LeBron James" == player) %>% select(pts,trb,ast,stl,blk,tov,pf)

plot_ly(
  type = 'scatterpolar',
  mode = "closest",
  fill = 'toself'
) %>%
  add_trace(
    r = as.matrix(a[1,]),
    theta = c("PPG","RPG","AST","STL","BLK","TOV","PF"),
    showlegend = TRUE,
    mode = "markers",
    name = "whatever"
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,20)
      )
    ),
    
    showlegend=TRUE
    
    
  )
