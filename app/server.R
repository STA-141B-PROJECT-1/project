

function(input, output, session) {
  
  selectedData1 <- reactive({
    playerdata %>%
      distinct(input$player, keep_all =TRUE) %>% 
      filter(playerdata$player != gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
  })
  
  
  
  selectedData2 <- reactive({
    playerdata %>%
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
    playerdata %>%
      filter(playerdata$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) %>% 
      select(pts,trb,ast,stl,blk,tov,pf,mp) 
      
    
  })

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
             range = c(0,40)
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
             range = c(0,40)
           )
         ),
         
         showlegend=TRUE
         
       ) 
     
     
     
   }
  
  })
    
    output$table <-renderTable({
      selectedData6()
      
    })
    }



    

