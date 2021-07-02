dashboardPage(
  dashboardHeader(title = "OrderMe.io 0.2.2",
                  titleWidth = 180),
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      menuItem("메뉴 보기", href = 'https://www.subway.co.kr/sandwichList',
               icon = icon("compass")),
      menuItem("Subway", tabName = "order", icon = icon("bread-slice")),
      menuItem("주문표", tabName = "report", icon = icon("table"))
    ),
    disable = F
  ),
  skin = 'green',
  ## Body content
  dashboardBody(tabItems(
    tabItem(
      "order",
      fluidRow(
        tabBox(
          title = h5("주문내용"),
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          height = "400px",
          tabPanel(
            h6('빵/샐러드'),
            radioButtons("빵/샐러드",
                         inputId = "brdorsal",
                         choices = brdorsal),
            conditionalPanel(
              "input.brdorsal == '빵'",
              tabPanel(
                h6('빵'),
                radioButtons(
                  "빵",
                  inputId = "bread",
                  choices = breads,
                  inline = T
                ),
                radioButtons("빵 길이",
                             inputId = "bread.cm",
                             choices = bread.cms)
              )
            )
          ),
          tabPanel(h6('치즈'),
                   checkboxGroupInput('cheese',
                                      "치즈 종류(선다형)",
                                      cheeses)),
          tabPanel(
            h6('야채'),
            checkboxGroupInput('vege',
                               "야채 종류(선다형)",
                               veges,
                               c('양상추', '토마토'),
                               inline = F)
          ),
          tabPanel(
            h6('소스'),
            checkboxGroupInput('sauce',
                               "소스(선다형)",
                               sauces,
                               inline = T)
          ),
          tabPanel(h6('음료'),
                   radioButtons('bev',
                                '음료 선택',
                                bevs)),
          tabPanel(
            h6('추가'),
            checkboxGroupInput('add',
                               "추가(선다형)",
                               addons,
                               inline = F)
          ),
          tabPanel(
            h6("메뉴 및 기타"),
            textAreaInput(
              inputId = "etc",
              label = "메뉴 및 기타사항",
              placeholder = "예) 로티세리 샐러드, 스프라이트",
              height = '200px'
            )
          )
        ),
        tabBox(
          title = h5('개인정보'),
          height = "200px",
          tabPanel(h6("이름"),
                   textInput(
                     '이름을 적으세요', inputId = "orderer", width = 70
                   ))
        )
      ),
      fluidRow(tabBox(
        # Title can include an icon
        title = tagList(icon("gear")),
        width = 12,
        tabPanel(
          h6("주문"),
          actionButton('submit', h6("주문서 제출"), icon('file-export')),
          actionButton('submitr',
                       h6("Random?"),
                       icon('smile-wink'))
        ),
        br(),
        a('랜덤주문 방법: 이름과 "메뉴 및 기타"를 작성하고 Random?(찡긋) 버튼을 누르세요!')
      )),
      br(),
      fluidRow(tabBox(tabPanel(
        '모바일은 여기가 잘릴수도 있어서 여백으로~'
      ), width = 12))
    ),
    tabItem("report", fluidRow(column(
      12,
      dataTableOutput('table')
    )),
    downloadButton("dwnDT", h5("주문표")))
  ))
)