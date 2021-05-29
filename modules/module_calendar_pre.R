################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

palette_sparkline = list("#ffffff","#EBAF4C","#95695D","#3E236E")

calendar_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

calendar_pre <- function(input,output,session) {
  
  DF_zip_code_state <- gcs_get_object('initial_zip_table.csv',bucket="adj_outputs") %>% select(-X1)
  DF_Existing_Arranged <- gcs_get_object('initial_calendar.csv',bucket="adj_outputs") %>% select(-X1)
  
  REACT_DF_Existing_Arranged_Final <- reactive( 
    {DF_Existing_Arranged %>%
        mutate(slide = seq(1, n()),
               first = 1 == slide)}
  )
  
# Package UI --------------------------------------------------------------
  
  output$module_ui <- renderUI({
  
    req(REACT_DF_Existing_Arranged_Final())
    
    STR_glyph_yes = "glyphicon glyphicon-ok" 
    STR_glyph_no = "glyphicon glyphicon-remove"
    STR_glyph_snowflake = "far fa-snowflake"
    STR_glyph_cloud_sun_rain = "fas fa-cloud-sun-rain"
    STR_glyph_sun = "fas fa-sun"
    STR_glyph_cloud_sun = "fas fa-cloud-sun"
    
    #do as individual columns in the dataframe? it's fucked
    
    DF_Existing_Arranged_Final_Cal_Specific <- REACT_DF_Existing_Arranged_Final() %>%
      mutate(Flowering_Y_N_GLYPH = ifelse(Flowering == "Yes",STR_glyph_yes,STR_glyph_no),
             Edible_Human_Y_N_GLYPH = ifelse(Edible_Human == "Yes",STR_glyph_yes,STR_glyph_no),
             Edible_Animal_Y_N_GLYPH = ifelse(!is.na(Edible_Animal),STR_glyph_yes,STR_glyph_no),
             Berry_Nut_Y_N_GLYPH = ifelse(Berry_Nut_Seed == "Yes",STR_glyph_yes,STR_glyph_no),
             Fire_Resist_Y_N_GLYPH = ifelse(Fire_Resistance == "Yes",STR_glyph_yes,STR_glyph_no),
             Cold_Strat_Y_N_GLYPH = ifelse(Cold_Strat_Required == "Yes",STR_glyph_yes,STR_glyph_no),
             Growth_Month_GLYPH = case_when(Growth_Month_NUM == 1 ~ STR_glyph_snowflake,
                                            Growth_Month_NUM == 2 ~ STR_glyph_snowflake,
                                            Growth_Month_NUM == 3 ~ STR_glyph_cloud_sun_rain,
                                            Growth_Month_NUM == 4 ~ STR_glyph_cloud_sun_rain,
                                            Growth_Month_NUM == 5 ~ STR_glyph_cloud_sun_rain,
                                            Growth_Month_NUM == 6 ~ STR_glyph_sun,
                                            Growth_Month_NUM == 7 ~ STR_glyph_sun,
                                            Growth_Month_NUM == 8 ~ STR_glyph_sun,
                                            Growth_Month_NUM == 9 ~ STR_glyph_cloud_sun,
                                            Growth_Month_NUM == 10 ~ STR_glyph_cloud_sun,
                                            Growth_Month_NUM == 11 ~ STR_glyph_cloud_sun,
                                            Growth_Month_NUM == 12 ~ STR_glyph_snowflake,
                                            T ~ "whatever"),
             Image_Link = paste0("https://species.wikimedia.org/wiki/",as.character(Genus),"_",as.character(Species)))
    
    LST_calendar_pre <- split(DF_Existing_Arranged_Final_Cal_Specific, seq(nrow(DF_Existing_Arranged_Final_Cal_Specific)))
    
    htmlTemplate(
      filename = "www/modules/calendar/index.html",
      county = DF_zip_code_state[[1,9]],
      environment = " Urban (window box, small garden)",
      captions = calendar_captions_ui(LST_calendar_pre),
      items = calendar_items_ui(LST_calendar_pre)
    )
  })
  
  calendar_captions_ui <- function(LST_calendar_pre) {
    tagList(
      sapply(LST_calendar_pre, function(x) {
        htmlTemplate(
          filename = "www/modules/calendar/caption.html", 
          growth_month = x$Growth_Month_NAME,
          growth_month_glyph = tags$i(class = x$Growth_Month_GLYPH),
          slide = x$slide - 1,
          active_class = ifelse(x$first, 'class="active"', "")
        )
      })
    )
  }
  
  calendar_items_ui <- function(LST_calendar_pre) {
    tagList(
      lapply(LST_calendar_pre, function(x) {
        htmlTemplate(
          filename = "www/modules/calendar/item.html", 
          growth_month = x$Growth_Month_NAME, 
          common_name = x$Common_N,
          scientific_name = x$Scientific_Name,
          te_in_state_sparkline= tags$span(spk_chr(1:x$TE_in_State_Inverse_RANK,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          threatened_in_my_state = x$TE_Indicator_Final,
          avail_to_buy_sparkline = tags$span(spk_chr(1:x$`Available to Buy RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          available_to_buy = x$Commercial_Avail,
          growth_rate_sparkline = tags$span(spk_chr(1:x$`Fast Growing RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          lifespan_sparkline = tags$span(spk_chr(1:x$`Long Lifespan RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          moisture_use_sparkline = tags$span(spk_chr(1:x$`Moisture Use Inverted RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          drought_tolerance_sparkline = tags$span(spk_chr(1:x$`Drought Tolerant RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          frost_tolerance_sparkline = tags$span(spk_chr(1:x$`Frost Tolerant RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          cold_strat = tags$span(class = x$Cold_Strat_Y_N_GLYPH),
          fire_tolerance_sparkline = tags$span(spk_chr(1:x$`Fire Tolerant RANK`,type="bar",chartRangeMax=4,colorMap=palette_sparkline,disableTooltips=TRUE)) %>% spk_add_deps(),
          fire_resistance = tags$span(class = x$Fire_Resist_Y_N_GLYPH),
          edible_for_animals = tags$span(class = x$Edible_Animal_Y_N_GLYPH),
          flowering = tags$span(class = x$Flowering_Y_N_GLYPH),
          edible_for_people = tags$span(class = x$Edible_Human_Y_N_GLYPH),
          berries_nuts_seeds = tags$span(class = x$Berry_Nut_Y_N_GLYPH),
          genus = x$Genus,
          species = x$Species,
          image_link = paste0("images/initial_table/",x$Genus,"_",x$Species,".jpg"),
          #
          active_class = ifelse(x$first, "active", "")
        )
      }),
      tags$script(src = "js/main.js")
    )
  }
}