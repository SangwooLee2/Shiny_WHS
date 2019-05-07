# unesco

library(DT)
library(shiny)
library(googleVis)

shinyServer(
  function(input, output){
  # show map using googleVis
  output$map <- renderGvis({
    gvisGeoChart(state_stat, "state.name", input$selected,
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width="auto", height="auto"))
  }) # output$map
  
  # show histogram using googleVis
  output$hist <- renderGvis({
    gvisHistogram(No_per_state[,input$selected, drop=FALSE])
  })
  
  # show data using DataTable
  output$table <- DT::renderDataTable({
    datatable(state_stat, rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
  # show statistics using infoBox
  output$maxBox <- renderInfoBox({
    max_value <- max(state_stat[,input$selected])
    max_state <- 
      state_stat$state.name[state_stat[,input$selected] == max_value]
    infoBox(max_state, max_value, icon = icon("hand-o-up"))
  })
  output$minBox <- renderInfoBox({
    min_value <- min(state_stat[,input$selected])
    min_state <- 
      state_stat$state.name[state_stat[,input$selected] == min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected),
            mean(state_stat[,input$selected]), 
            icon = icon("calculator"), fill = TRUE) # infoBox()
    ) # renderInfoBox
  
  # No_per_region
  output$No_per_region = renderPlotly(No_per_region)
  # No_per_state
  output$No_per_state = renderPlotly(No_per_state)  
  
  # whole table
  # output$whsites = DT::renderDataTable(whsites)
  output$whsites = DT::renderDataTable(
    {
    datatable( whsites %>% select(., -rev_bis, -justification_en, -secondary_dates, -danger, -date_end, -danger_list, -category_short, -udnp_code,-transboundary) )
    }, 
    options = list(autoWidth = T)
  )    
  
  
  ##############################################################################
  output$dynamic_widget = renderUI({
    selectizeInput(inputId='cat_table1', label = "category(cultural, natural, mixed)", choices = cat_choices() )
  })
  ##############################################################################

  
  ##############################################################################
  cat_choices = reactive({
    sort(unique(filter(whsites,states_name_en == input$stateID)$category))
  })
  ##############################################################################  
  
  ##############################################################################
  output$whsites_state_cat = DT::renderDataTable({datatable(
    whsites %>% filter(states_name_en == input$stateID, category == input$cat_table1)
    %>% select(., -rev_bis, -secondary_dates, -danger, -date_end, -danger_list, -category_short, -udnp_code,-transboundary),
    options = list(searching = FALSE))
  })  
  ##############################################################################
  
  output$world_map <- renderPlotly({
    styles <- schema()$layout$layoutAttributes$mapbox$style$values
    
    style_buttons <- lapply(styles, function(s) {
      list(label = s, method = "relayout", args = list("mapbox.style", s))
    })
    
    layout(
      p = plot_mapbox(data=whsites) %>%
        
        add_markers(
          x = ~longitude, 
          y = ~latitude,
          # color = ~iso_code,
          color = ~( as.numeric(iso_code) %% 10 ),
          colors = "Accent",
          text = ~paste(states_name_en, name_en, category, sep='::'),
          hoverinfo = "text",   
          size =8
        ),    # end of add_markers 
      
      mapbox = list(style = "dark"),
      updatemenus = list(
        list(y = 0.8, buttons = style_buttons) # inner list
      ) # outer list
    ) # layout
  })
  
  
} # function(input, output){
) # shinyServdr

