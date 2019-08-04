library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(sidebarUserPanel("Kate"),
                   sidebarMenu(
                     menuItem("Explore by Race", tabName = "race", icon = icon("balance-scale")),
                     menuItem("National Data", tabName = "national", icon = icon("flag")),
                     menuItem("Data", tabName = "data", icon = icon("table"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "race",
     selectizeInput("selected","Select Victim Race to Explore", choice),
          fluidRow(
                box(
                  htmlOutput("map"),
                  title = "Relative Killings"),
                box(
                  plotlyOutput("plot"),
                  title = "Age Histogram"))),
      tabItem(tabName = "national",
              fluidRow(
                box(
                  htmlOutput("national_map"),
                  title = "Relative Chance of Being Killed (By State)"
                ),
                box(
                  plotlyOutput("national_risk"),
                  title = "Relative Chance of Being Killed (By Race)"
                ))),
      tabItem(tabName = "data",
              fluidRow(
                box(
                  DT::dataTableOutput("table")
                  )))
    ))))
                