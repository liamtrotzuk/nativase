################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

map_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

map_main <- function(input,output,session,REACT_DF_Existing_Arranged_Final,REACT_DF_Plants_Adjusted,REACT_DF_Zip_Code_State) {
  
  DF_Existing_Arrange_Raw <- REACT_DF_Existing_Arranged_Final() %>% 
    tibble::rowid_to_column("ID")
  
  DF_Main_Adj <- REACT_DF_Plants_Adjusted() %>% 
    select(Common_N,State_Native_Symbol,TE_Indicator_Final,`Threatened in my State RANK`)
  
  DF_Existing_Arrange_A <- DF_Existing_Arrange_Raw %>% 
    select(ID,Common_N) %>% 
    distinct() %>% 
    left_join(DF_Main_Adj,
              by = c('Common_N')) %>% 
    select(ID,Common_N,State_Native_Symbol,TE_Indicator_Final,`Threatened in my State RANK`) %>% 
    mutate(TE_Indicator_Final = ifelse(TE_Indicator_Final == "No Concern","Native",TE_Indicator_Final)) %>% 
    distinct()
  
  DF_GEO_Tigris_Data <- rmapshaper::ms_simplify(tigris::states(cb = T))
  
  DF_Existing_Arrange_B <- geo_join(DF_GEO_Tigris_Data,DF_Existing_Arrange_A %>% filter(ID == 1),"STUSPS","State_Native_Symbol")
  
  DF_Existing_Arrange_C <- DF_Existing_Arrange_A %>% filter(ID != 1)
  
  ID_Distinct <- as.list(unique(DF_Existing_Arrange_C$ID))
  
  for (x in ID_Distinct) {
    
    DF_Append_A <- DF_Existing_Arrange_C %>% filter(ID == x)
    
    DF_Append_B <- geo_join(DF_GEO_Tigris_Data,DF_Append_A,"STUSPS","State_Native_Symbol")
    
    DF_Existing_Arrange_B %>% rbind(DF_Append_B) -> DF_Existing_Arrange_B
    
  }
  
  DF_Existing_Arrange_D <- DF_Existing_Arrange_B %>% 
    mutate(TE_Indicator_Final = ifelse(is.na(TE_Indicator_Final),"Non-Native",TE_Indicator_Final),
           TE_Indicator_Final_Adj = ifelse((TE_Indicator_Final == "Non-Native" | TE_Indicator_Final == "Native"),TE_Indicator_Final,"Other"))
  
  pal <- colorFactor(palette = c("black", "#EBAF4C", "red"), 
                     levels = c("Non-Native", "Native", "Other"))
  
  plants.df <- split(DF_Existing_Arrange_D,factor(DF_Existing_Arrange_D$Common_N,levels=unique(DF_Existing_Arrange_D$Common_N)))
  
  l <- leaflet() %>% setView(REACT_DF_Zip_Code_State()[[1,7]], REACT_DF_Zip_Code_State()[[1,6]]+10, zoom = 4) %>% 
    addProviderTiles(providers$Stamen.Toner)
  
  names(plants.df) %>%
    purrr::walk(function(df) {
      l <<- l %>%
        addPolygons(data = plants.df[[df]],
                    stroke = FALSE, 
                    fillColor = ~pal(TE_Indicator_Final_Adj), 
                    color = "transparent",
                    highlight = highlightOptions(weight = 5,
                                                 fillOpacity = 0.5,
                                                 color = "white",
                                                 opacity = 1,
                                                 bringToFront = TRUE),
                    label = ~as.character(TE_Indicator_Final),
                    group = df,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        )
    })
  
  l <- l %>%
    addLayersControl(
      baseGroups = names(plants.df),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addMarkers(REACT_DF_Zip_Code_State()[[1,7]],
               REACT_DF_Zip_Code_State()[[1,6]],
               icon = icons(iconUrl = "images/map-marker-3.svg",
                            iconWidth = 30,
                            iconHeight = 30,
                            iconAnchorX = 15,
                            iconAnchorY = 29))
  
  REACTIVE_MAP_Main <- reactive(l)
  
  output$module_ui <- renderUI({
    
    htmlTemplate(
      filename = "www/modules/map/index.html",
      map_for_index_HTML = renderLeaflet(REACTIVE_MAP_Main())
    )
  })

}