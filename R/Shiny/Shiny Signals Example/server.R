function(input, output, session) {
  output$rawtable <- renderDataTable({
    prd <- strsplit(input$period, split = "'")[[1]]
    since <- auto_since(master.tb, prd)
    arrange(pivot_longer(
      mutate(
        as_tibble(filter(
          select(master.tb, c(
            'userid', 'regdate', local(input$selectVar)
          )),
          regdate >= since
          &
            userid == local(input$userid)
        )),
        across(input$selectVar, .fns = as.numeric),
        regdate = as_datetime(regdate)
      ),
      -c(userid, regdate)
    ), desc(regdate))
    # return
  },
  options = list(pageLength = 12))
  
  output$packagePlot <- renderPlot({
    prd <- strsplit(input$period, split = "'")[[1]]
    since <- auto_since(master.tb, prd)
    ggplot(arrange(mutate(
      pivot_longer(
        mutate(
          as_tibble(filter(
            select(master.tb, c(
              'userid', 'regdate', local(input$selectVar)
            )),
            regdate >= since
            &
              userid == local(input$userid)
          )),
          across(input$selectVar, .fns = as.numeric),
          regdate = as_datetime(regdate)
        ),
        -c(userid, regdate)
      ),
      value = rollmean(value, input$maWindow, fill = NA)
    ), desc(regdate)), aes(x = regdate, y = value, col = name)) +
      theme_minimal() +
      geom_line(show.legend = F) +
      geom_point(size = .7, show.legend = F) +
      ggtitle(input$selectVar)
  })
}