dashboardPage(
  dashboardHeader(title = "Vital Monitor"),
  dashboardSidebar(
    numericInput(
      "maWindow",
      "MA window",
      min = 1,
      max = 100,
      value = 1,
      step = 1
    ),
    textInput('period',
              "Period (H'M'S)",
              placeholder = "1'1'22 -> 1h 1m 22s"),
    selectInput("userid",
                "Select user",
                users$userid,
                selected = users$userid[1]),
    selectInput("selectVar",
                "Variable to plot",
                Vars,
                selected = Vars[1]),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(tabItems(
    tabItem("dashboard",
            fluidRow(
              box(
                width = 12,
                status = "info",
                solidHeader = TRUE,
                title = "Vital Signs",
                plotOutput("packagePlot")
              )
            )),
    tabItem("rawdata",
            {
              fluidRow(column(8,
                              dataTableOutput('rawtable')))
            })
  ))
)