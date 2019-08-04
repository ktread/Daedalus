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
    plot_ly(national_race_agg, x = ~Victim_race, y = ~relative_percent, type = 'bar', 
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
  
  output$city_deaths <- renderLeaflet({
  leaflet(city) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(-96, 37.8, 4) %>%
    addCircleMarkers(lat= ~latt,lng= ~long,
                     color = "#fd0000",
                     stroke = FALSE,
                     fillOpacity = 0.5,
                     radius = ~deaths/2,
                     #popup = ~label,
                     popupOptions = labelOptions(noHide = T,
                                                 textsize = "15px"))  %>%
    addProviderTiles(providers$CartoDB.Positron)
  
})
  
  output$male_nation <- renderPlotly({
  plot_ly(date_gender_summary) %>%
    add_bars(
      x = ~Year,
      y = ~Male,
      marker = list(
        color = "#fd0000",
        opacity = .85
      ),
      name = 'Men Killed',
      text = ~Male,
      textposition = "auto"
    ) %>% 
    layout(title = 'Men Killed in the US (2015-2019)',
           xaxis = list(title = "Year"),
           yaxis = list(title = "Number of Deaths"),
           legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
                         bordercolor = 'rgba(255, 255, 255, 0)'),
           barmode = 'group', 
           bargap = 0.09
    )

})

  output$female_nation <- renderPlotly({ 
  plot_ly(date_gender_summary) %>%
    add_bars(
      x = ~Year,
      y = ~Female,
      marker = list(
        color = "#fd0000",
        opacity = .85
      ),
      text = ~Female,
      textposition = "auto"
    ) %>% 
    layout(title = 'Women Killed in the US (2015-2019)',
           xaxis = list(title = "Year"),
           yaxis = list(title = "Number of Deaths"),
           legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
                         bordercolor = 'rgba(255, 255, 255, 0)'),
           barmode = 'group', 
           bargap = 0.09
    ) 

})

  output$city_deaths_race <- renderLeaflet({
    city_race %>% 
    filter(Victim_race == input$selected) %>% 
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(-96, 37.8, 4) %>%
      addCircleMarkers(lat= ~latt,lng= ~long,
                       color = "#fd0000",
                       stroke = FALSE,
                       fillOpacity = 0.5,
                       radius = ~deaths/2,
                       #popup = ~label,
                       popupOptions = labelOptions(noHide = T,
                                                   textsize = "15px"))  %>%
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
output$cal <- renderGvis({
  gvisCalendar(by_date, 
               datevar="Date", 
               numvar="deaths",
               options=list(
                 height=920,
                 width= 1700,
                 calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#fd0000', bold: true},
                               cellSize: 19,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
  )
  
})

output$how_armed <- renderPlotly({ 
plot_ly(how_armed) %>%
  add_bars(
    x = ~Armed,
    y = ~deaths,
    marker = list(
      color = "#fd0000",
      opacity = .85
    ),
    name = 'Top 10 Weapons Held by Victims',
    text = ~deaths,
    textposition = "auto"
  ) %>% 
  layout(title = 'Top 10 Weapons Held by Victims',
         xaxis = list(title = "Weapon"),
         yaxis = list(title = "Number of Deaths"),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
                       bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', 
         bargap = 0.01
  )
  
})


})

