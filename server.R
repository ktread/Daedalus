library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


shinyServer(function(input, output){ 
  output$map <- renderGvis({
    gvisGeoChart(state_race_input <- state_race_input[ state_race_input$Victim_race == input$selected,], locationvar='State.x',
                 colorvar= "Relative-Percent",
                 hovervar = "State_name",
                 options=list(region="US", 
                              displayMode="regions", 
                              resolution="provinces",
                              color = 'red',
                              colorAxis = "{minValue: -1,maxValue: 5,  colors: ['#00FF00','#FF0000']}"))
    })
  output$table <- DT::renderDataTable({
    datatable(police, rownames=FALSE) %>% 
      formatStyle(colnames(police), background="skyblue", fontWeight='bold')
    # Highlight selected column using formatStyle
  })
  
  output$plot <- renderPlotly({police %>% 
      filter(Victim_race == input$selected) %>% 
      plot_ly(x=~Age, 
              type = "histogram",
              name = "",
              hovertemplate = paste(
                "Age Group: %{x}<br>",
                "Number killed: %{y}<br>"
              ),
              marker = (list(color = "#c00000",  opacity = .75
              )
              
              )) %>%
      layout(yaxis=list(type='linear'),legend = ~Age) 
  })
  
  output$hist <- renderPlotly({police %>% 
      filter(Victim_race == input$selected) %>% 
      plot_ly(x=~Age, 
              type = "histogram",
              name = "",
              hovertemplate = paste(
                "Age Group: %{x}<br>",
                "Number killed: %{y}<br>"
              ),
              marker = (list(color = "#c00000",  opacity = .75
              )
              
              )) %>%
      layout(yaxis=list(type='linear'),legend = ~Age) 
  })
  
  output$national_map <- renderGvis({
  gvisGeoChart(state_relative_population_deaths, locationvar='State', 
               colorvar='relative_percent',
               options=list(region="US", 
                            displayMode="regions", 
                            resolution="provinces",
                            colorAxis = "{colors: ['#00FF00', '#FF0000']}"))
 

})
  
  output$national_risk <- renderPlotly({
    plot_ly(national_race_data, x = ~Victim_race, y = ~relative_percent, type = 'bar', 
            text = (~relative_percent), textposition = 'auto',
            marker = list(color = c('rgba(204,204,204,1)', '#fd0000',
                                    'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                    '#fd0000'),
                          opacity = .75
                          
            )
    )  %>%
      layout(title = "Percent More or Less Likely to be Killed",
             barmode = "group",
             xaxis = list(title = ""),
             yaxis = list(title = "")) 
    
  })
  
})









