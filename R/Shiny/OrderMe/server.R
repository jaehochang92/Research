function(input, output) {
  output$table <- renderDataTable({
    vals$proto %>% arrange(desc(`시간`), `빵/샐러드`) %>% datatable(escape = F)
  })
  
  observeEvent(input$delrow,
               {
                 vals$proto <- vals$proto[-which(input$delrow == cur.idv), ]
                 cur.idv <<-
                   cur.idv[-which(cur.idv == input$delrow)]
                 showNotification('주문 삭제 성공!', duration = 2, type = 'error')
               })
  
  observeEvent(input$submit,
               {
                 cur.idv <<- c(pop(idv) %>% as.character(), cur.idv)
                 vals$proto <- vals$proto %>% add_row(
                   `삭제` = actionButton(cur.idv %>% head(1),
                                       icon('eraser'),
                                       onclick = 'Shiny.onInputChange(\"delrow\", this.id)') %>%
                     as.character,
                   `시간` = now2(),
                   `멤버` = input$orderer,
                   `메뉴` = input$etc,
                   `빵/샐러드` = if (input$brdorsal == '빵') {
                     paste(input$bread, input$bread.cm, sep = '_')
                   } else if (input$brdorsal == '샐러드') {
                     input$brdorsal
                   },
                   `치즈` = input$cheese %>% unlist %>% paste0(collapse = ','),
                   `야채` = input$vege %>% unlist %>% paste0(collapse = ','),
                   `소스` = input$sauce %>% unlist %>% paste0(collapse = ','),
                   `음료` = input$bev,
                   `추가` = input$add %>% unlist %>% paste0(collapse = ',')
                 ) %>% arrange(desc(시간), `빵/샐러드`)
                 showNotification(paste0(input$orderer, ': 주문 완료!'), duration = 2)
                 if (input$orderer == '경호') {
                   Sys.sleep(.5)
                   showNotification('경호 선임님 놀러오세요~', duration = 2)
                 }
               })
  
  observeEvent(input$submitr, {
    cur.idv <<- c(pop(idv) %>% as.character(), cur.idv)
    rbrdorsal <- sample(brdorsal, 1)[[1]]
    vals$proto <- vals$proto %>% add_row(
      `삭제` = actionButton(cur.idv %>% head(1),
                          icon('eraser'),
                          onclick = 'Shiny.onInputChange(\"delrow\", this.id)') %>%
        as.character,
      `시간` = now2(),
      `멤버` = input$orderer,
      `메뉴` = input$etc,
      `빵/샐러드` = if (rbrdorsal == '빵') {
        paste(breads %>% rs(1), bread.cms %>% rs(1), sep = '_')
      } else if (rbrdorsal == '샐러드') {
        rbrdorsal
      },
      `치즈` = cheeses %>% rs(3) %>% paste0(collapse = ','),
      `야채` = veges %>% rs(3) %>% paste0(collapse = ','),
      `소스` = sauces %>% rs(3) %>% paste0(collapse = ','),
      `음료` = bevs %>% rs(1),
      `추가` = addons %>% rs(3) %>% paste0(collapse = ',')
    ) %>% arrange(desc(시간), `빵/샐러드`)
    showNotification(paste0(input$orderer, ': U serious..?'), duration = 2)
  })
  
  output$dwnDT <- downloadHandler(
    filename = '주문표.xlsx',
    content = function(file) {
      vals$proto %>% select(-(`삭제`:`시간`)) %>%
        arrange(across(`빵/샐러드`:`음료`)) %>%
        as.data.frame() %>%
        t %>%
        write.xlsx(file,
                   row.names = T,
                   showNA = F)
    }
  )
}