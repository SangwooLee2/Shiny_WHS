# Unesco

library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(RSQLite)
library(shinyLP)

##
# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice <- colnames(state_stat)[-1]


##
whsites = read.csv(file = "./whc-sites-2018-2.csv")
whsites = select(whsites, -name_fr, -short_description_fr, -justification_fr,
                 -C1,-C2,-C3,-C4,-C5,-C6,-N7,-N8,-N9,-N10, -states_name_fr, 
                 -region_fr)
# choice <- colnames(whsites)


if (FALSE){
  g=ggplot(whsites_10more, aes(x=reorder(states_name_en, NoPerState), y=NoPerState)) 
  g+geom_bar(aes(fill=states_name_en),stat = "identity") + coord_flip()
  
  ##
  g2=ggplot(whsites_reg , aes(x=reorder(region_en, NoPerregion), y=NoPerregion)) 
  g2+geom_bar(aes(fill=region_en),stat = "identity") + coord_flip()
}


fluidRow(DT::dataTableOutput("Noperstate_table"))

# Noperstate_table = datatable(Noperstate, class = "display")
# Noperstate_table

###
whsites_10more = whsites %>% group_by(., states_name_en) %>% 
  summarise( NoPerState = n()) %>% filter(., NoPerState>=10)

whsites_state = whsites %>% group_by(., states_name_en) %>% 
  summarise( NoPerState = n()) 

whsites_state_code = whsites %>% group_by(., states_name_en, iso_code) %>% 
  summarise( NoPerState = n()) 

m <- list(
  l = 50,
  r = 50,
  b = 150,
  t = 100,
  pad = 4
)

No_per_state = plot_ly(data = whsites_10more,
                       x = reorder(whsites_10more$states_name_en, whsites_10more$NoPerState),
                       y = whsites_10more$NoPerState,
                       type = "bar",
                       color = whsites_10more$states_name_en,
                       showlegend = FALSE) %>%
  layout(title = "Countries with ten or more sites listed",
         margin = m, 
         # xaxis = list(title = "countries", margin=m),
         # xaxis = list(title = "", margin=m),
         xaxis = list(title = "countries", tickangle = 45), 
         # yaxis = list(title = "No of sites listed", margin=m)
         yaxis = list(title = "No of sites listed")
         # autosize = T
         # autosize = F, width = 500, height = 500
         )


# choice2 <- colnames(whsites_10more)

####
whsites_reg = whsites %>% group_by(., region_en) %>% 
  summarise( NoPerregion = n()) 

No_per_region = plot_ly(data = whsites_reg,
                       x = reorder(whsites_reg$region_en, whsites_reg$NoPerregion),
                       y = whsites_reg$NoPerregion,
                       type = "bar",
                       color = whsites_reg$region_en,
                       showlegend = FALSE) %>%
  layout(title = "No of sites listed per region",
         margin = m, 
         # xaxis = list(title = "", margin=m),
         # xaxis = list(title = "No of sites listed"),
         xaxis = list(title = "regions",tickangle = 45),
         # yaxis = list(title = "regions", margin=m)
         yaxis = list(title = "No of sites listed")
         # autosize = T
         # autosize = T
         )
# choice3=colnames(whsites_reg)

##
# choice = c(choice, choice2, choice3)

state_vec = sort(unique(whsites$states_name_en))
region_vec = sort(unique(whsites$region_en))
cat_vec = sort(unique(whsites$category))

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibHN3MDBrb3IiLCJhIjoiY2p2YTM5Z3BjMTgycTRkbnQzbnJoZzkwdSJ9.WHVBDFGeyl5W3UHod6oUCg' )