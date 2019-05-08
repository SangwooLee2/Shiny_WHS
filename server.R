# unesco

library(DT)
library(shiny)
library(googleVis)

shinyServer(
  function(input, output){

  # No_per_region
  output$No_per_region = renderPlotly(No_per_region)
  
  # No_per_state
  output$No_per_state = renderPlotly(No_per_state)  
  
  # No_per_year_world
  output$No_per_year_world = renderPlotly(No_per_year_world)    
  
  # No_per_year_region
  output$No_per_year_region = renderPlotly(No_per_year_region)      
  
  # whole table
  # output$whsites = DT::renderDataTable(whsites)
  output$whsites = DT::renderDataTable(
    {
    datatable( whsites %>% select(., -rev_bis, -justification_en, -secondary_dates, -danger, -date_end, -danger_list, -category_short, -udnp_code,-transboundary) )
    }, 
    options = list(autoWidth = T)
  )    
  
  
  ###
  output$dynamic_widget = renderUI({
    selectizeInput(inputId='cat_table1', label = "category(cultural, natural, mixed)", choices = cat_choices() )
  })

  
  ##
  cat_choices = reactive({
    sort(unique(filter(whsites,states_name_en == input$stateID)$category))
  })

  
  ##
  output$whsites_state_cat = DT::renderDataTable({datatable(
    whsites %>% filter(states_name_en == input$stateID, category == input$cat_table1)
    %>% select(., -rev_bis, -secondary_dates, -danger, -date_end, -danger_list, -category_short, -udnp_code,-transboundary),
    options = list(searching = FALSE))
  })  
  
  
  ##
  output$BarplotPerYearPerNation <- renderPlotly({
    whsites_s3 = whsites[order(whsites$date_inscribed),] %>% filter(., states_name_en ==input$stateID) %>% group_by(., category, date_inscribed) %>% summarise(., total=n())
    # whsites_s3 = whsites[order(whsites$date_inscribed),] %>% filter(., states_name_en =='Republic of Korea') %>% group_by(., category, date_inscribed) %>% summarise(., total=n())
    BarplotPerYearPerNation = plot_ly(data = whsites_s3, 
                                     x = reorder(whsites_s3$date_inscribed, whsites_s3$category),
                                     y = whsites_s3$total,
                                     type = "bar",
                                     color = whsites_s3$category) %>%
    
    
    layout(title = "No of sites listed per year",
           margin = m, 
           # xaxis = list(title = "countries", margin=m),
           # xaxis = list(title = "", margin=m),
           xaxis = list(title = "year"), 
           # yaxis = list(title = "No of sites listed", margin=m)
           yaxis = list(title = "No of sites listed")
           # autosize = T
           # autosize = F, width = 500, height = 500
    )
    }
  ) # end of renderPlotly

  
  
  
  ##
  output$world_map <- renderPlotly({
    styles <- schema()$layout$layoutAttributes$mapbox$style$values
    
    style_buttons <- lapply(styles, function(s) {
      list(label = s, method = "relayout", args = list("mapbox.style", s))
    }) # end of style_buttons
    
    layout(
      plot_mapbox(data=whsites) %>%
        
        add_markers(
          x = ~longitude, 
          y = ~latitude,
          # color = ~iso_code,
          color = ~( as.numeric(iso_code) %% 10 ),
          colors = "Accent",
          text = ~paste(states_name_en, name_en, category, sep='::'),
          hoverinfo = "text",   
          size =8
        ),  # end of add_markers()
      
      mapbox = list(style = "dark"),
      updatemenus = list(
        list(y = 0.8, buttons = style_buttons)
      )
    ) 
  })
  
  
} # function(input, output){
) # shinyServdr

