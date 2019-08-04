library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(sidebarUserPanel("Kate"),
                    selectizeInput("selected","Select Victim Race to Explore", choice),
                   sidebarMenu(
                     menuItem("Map", tabName = "map", icon = icon("map")),
                     menuItem("Data", tabName = "data", icon = icon("database"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(box(htmlOutput("map"),
                           title = "Relative Killings"),
                       box(plotlyOutput("plot"),
                           title = "Age Histogram"))
              
              ), #can break into multiple boxes
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"))))
    ))))


#fluid row breaks rows in 12ths