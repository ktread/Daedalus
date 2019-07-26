library(shiny)
library(ggplot2)
library(dplyr)

function(input, output, session) {
    observe({
        Countyvec = unique(bees[State == input$State, County])
    updateSelectizeInput(
        session = session,
        inputId = "County",
        choices = Countyvec,
        selected = Countyvec[1])
    })
    

    BeesValue = reactive(
        bees %>% 
            filter(State == input$State & County == input$County) %>% 
            group_by(Year) %>% 
            summarize(v = Value, n = n())
    )
    
    output$value = renderPlot(
            BeesValue() %>% 
                ggplot(aes(x=Year, y=v)) +
                geom_col(fill = 'gray') +
                ggtitle('Value by Year')
        
    )
    
    output$count = renderPlot(
          BeesValue() %>% 
            ggplot(aes(x=Year, y=n)) +
            geom_col(fill = 'gray') +
            ggtitle('Value by Year')
        
    )

    }
