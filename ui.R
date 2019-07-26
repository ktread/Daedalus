library(shiny)

fluidPage( 
  titlePanel("Bees"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId= 'State',
                       label= 'State',
                       choices= unique(bees$State)
                       ),
      selectizeInput(
        inputId="County",
          label="County",
          choices= unique(bees$County)
      )
          ),
    mainPanel(
      plotOutput('value'),
      plotOutput('count')
          )
  )
)





