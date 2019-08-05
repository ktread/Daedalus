library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Police Killings"),
  dashboardSidebar(
                   sidebarMenu(
                     menuItem("National Data", tabName = "national", icon = icon("flag")),
                     menuItem("Explore by Race", tabName = "race", icon = icon("balance-scale")),
                     menuItem("Explore by Details", tabName = "detail", icon = icon("layer-group"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "race",
     selectizeInput("selected","Select Victim Race to Explore", choice),
          fluidRow(
              box(
                width = 12,
                leafletOutput("city_deaths_race"),
                title = HTML("Number of Deaths by Race")
                ),
                box(
                  htmlOutput("map"),
                  title = "Relative Killings"),
                box(
                  plotlyOutput("plot"),
                  title = "Age Histogram"))),
      tabItem(tabName = "national",
              fluidRow(
                box(
                  width = 12,
                  leafletOutput("city_deaths"),
                  title = "Number of Deaths"
                )),
              fluidRow(
                box(
                  htmlOutput("national_map"),
                  title = "Relative Likelihood of Being Killed (By State)"
                ),
                box(
                  plotlyOutput("national_risk"),
                  title = "Relative Likelihood of Being Killed (By Race)"
                )),
              fluidRow(
                box(
                  plotlyOutput("male_nation"),
                  title = "Number of Men Killed"
                ),
                box(
                  plotlyOutput("female_nation"),
                  title = "Number of Women Killed"
                )),
              fluidRow(
                box(
                  width = 12,
                  htmlOutput("cal"),
                  title = "Number of Deaths by Day"
                ))
              ),
      tabItem(tabName = "detail",
              fluidRow(
                box(
                  varSelectInput("variable", "Variable:", police_detail),
                  #selectizeInput("views","Select Details to Explore", views),
                  width = 12,
                  plotlyOutput("view_by"),
                  title = paste("Number of Deaths")
                  )))
    ))))
          

    