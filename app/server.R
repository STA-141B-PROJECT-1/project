

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
      select(player,tm,pts,trb,ast,stl,blk,tov,pf,mp) %>% 
      filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
    
  })
  
  selectedData4 <- reactive({
    playerdata %>%
      select(player,fgpercent,x3ppercent,ftpercent)  %>% 
      filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
      
    
    
  })
  
  selectedData5 <- reactive({
    selectedData1%>% 
      select(pts,trb,ast,stl,blk,tov,pf,mp)
  })
  
  selectedData6 <- reactive({
    selectedData1() %>% 
      select(fgpercent,x3ppercent,ftpercent)
  })
 
  selectedData7 <- reactive({
    selectedData1()%>% 
      select(player)
  })
  
  # Combine the selected variables into a new data frame
  output$plot1 <- renderPlotly({
 
    
    plot_ly(
      x = c("Field goal", "3 point", "Free throw"),
      y = c(selectedData4()$fgpercent, selectedData4()$x3ppercent, selectedData4()$ftpercent),
      type = "bar") %>% 
      layout(title = paste("Shooting Percentages of ", selectedData3()[1]),
             yaxis = list(title = "Percent"))
    
    
  })
  

    
    output$plot2 <- renderPlotly({
    if (nrow(selectedData3()) == 1)
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
         name = selectedData3()[2]
       ) %>%
       layout(
         polar = list(
           radialaxis = list(
             visible = T,
             range = c(0,40)
           )
         ),
      
         showlegend=TRUE
  
     )
    else  
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
          name = selectedData3()[1,2]
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,40)
            )
          ),
          
          showlegend=TRUE
          
        )
      

  })
  
}


