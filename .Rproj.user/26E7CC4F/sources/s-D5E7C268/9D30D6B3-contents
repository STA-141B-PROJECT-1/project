



function(input, output, session) {
  
  selectedData1 <- reactive({
    playerdata %>%
      filter(playerdata$player != gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
  })
  
  
  
  selectedData2 <- reactive({
    selectedData1() %>%
      select(player,pos,mp,trb,ast,stl,blk,tov,pf,pts) %>%
      filter(selectedData1()$pos %in% input$pos)
  })
  
  selectedData3 <- reactive({
    playerdata %>%
      select(player,pos,mp,trb,ast,stl,blk,tov,pf,pts) %>%
      filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
    
  })
  
  selectedData4 <- reactive({
    rbind(selectedData3(),selectedData2())
    
  })
  
  selectedData5 <- playerdata %>% select(pts,trb,ast,stl,blk,tov,pf,mp)
 
  
  
  # Combine the selected variables into a new data frame
  output$plot1 <- renderPlotly({
    
    
    plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.matrix(selectedData5[1,]),
        theta = c("PPG","RPG","AST","STL","BLK","TOV","PF","MPG"),
        showlegend = TRUE,
        mode = "markers",
        name = "whatever"
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        
        showlegend=TRUE
        
        
      )
    
  })
  
}


