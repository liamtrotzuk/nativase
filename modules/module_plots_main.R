################################################################################
# module calendar_pre
#
# Author: Liam Trotzuk
# Created: 2020-10-20
################################################################################

# Module UI ---------------------------------------------------------------

plots_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui"))
}

# Module server logic -----------------------------------------------------

plots_main <- function(input,output,session,REACT_DF_Existing_Arranged_Final,REACT_DF_County_Precip_Min_Adjusted,REACT_DF_County_Temp_Min_Adjusted) {
  
  DF_Precip_Plot_Pre_A <- REACT_DF_Existing_Arranged_Final() %>% 
    rename(`Growth Month` = Growth_Month_NUM,
           `Growth Month Name` = Growth_Month_NAME,
           `Common Name` = Common_N,
           `Precipitation (Minimum)` = Precip_Min) %>% 
    mutate(`Precipitation (Minimum)` = `Precipitation (Minimum)`/12)
  
  DF_Precip_Plot_Pre_B <- REACT_DF_County_Precip_Min_Adjusted() %>% 
    select(-State_Num,
           -County_Num,
           -Yearly_Precip) %>% 
    pivot_longer(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec),
                 "Growth Month Name") %>% 
    mutate(`Precipitation (Minimum)` = value,
           `Common Name` = "County Average",
           `Growth Month` = case_when(`Growth Month Name` == "Jan" ~ 1,
                                        `Growth Month Name` == "Feb" ~ 2,
                                        `Growth Month Name` == "Mar" ~ 3,
                                        `Growth Month Name` == "Apr" ~ 4,
                                        `Growth Month Name` == "May" ~ 5,
                                        `Growth Month Name` == "Jun" ~ 6,
                                        `Growth Month Name` == "Jul" ~ 7,
                                        `Growth Month Name` == "Aug" ~ 8,
                                        `Growth Month Name` == "Sep" ~ 9,
                                        `Growth Month Name` == "Oct" ~ 10,
                                        `Growth Month Name` == "Nov" ~ 11,
                                        `Growth Month Name` == "Dec" ~ 12)) %>% 
    select(`Growth Month`,
           `Growth Month Name`,
           `Common Name`,
           `Precipitation (Minimum)`)
  
  PLOT_precip_pre = ggplot(DF_Precip_Plot_Pre_A,aes(x = `Growth Month`, y = `Precipitation (Minimum)`)) + 
    geom_point(aes(text = `Common Name`), shape = 8, size = 3, colour='#3E236E') + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
    scale_y_continuous(label=function(x){return(paste0(x,"\""))}) + 
    geom_line(data = DF_Precip_Plot_Pre_B, aes(text = `Common Name`), colour = '#EBAF4C', size = 2) + 
    theme_bw() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Precipitation")
  
  DF_Temp_Plot_Pre_A <- REACT_DF_Existing_Arranged_Final() %>% 
    rename(`Growth Month` = Growth_Month_NUM,
           `Growth Month Name` = Growth_Month_NAME,
           `Common Name` = Common_N,
           `Temperature, Minimum (°F)` = Temp_Min)
  
  DF_Temp_Plot_Pre_B <- REACT_DF_County_Temp_Min_Adjusted() %>% 
    select(-State_Num,
           -County_Num,
           -Temp_Min) %>% 
    pivot_longer(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec),
                 "Growth Month Name") %>% 
    mutate(`Temperature, Minimum (°F)` = value,
           `Common Name` = "County Average",
           `Growth Month` = case_when(`Growth Month Name` == "Jan" ~ 1,
                                        `Growth Month Name` == "Feb" ~ 2,
                                        `Growth Month Name` == "Mar" ~ 3,
                                        `Growth Month Name` == "Apr" ~ 4,
                                        `Growth Month Name` == "May" ~ 5,
                                        `Growth Month Name` == "Jun" ~ 6,
                                        `Growth Month Name` == "Jul" ~ 7,
                                        `Growth Month Name` == "Aug" ~ 8,
                                        `Growth Month Name` == "Sep" ~ 9,
                                        `Growth Month Name` == "Oct" ~ 10,
                                        `Growth Month Name` == "Nov" ~ 11,
                                        `Growth Month Name` == "Dec" ~ 12)) %>% 
    select(`Growth Month`,
           `Growth Month Name`,
           `Common Name`,
           `Temperature, Minimum (°F)`)
  
  PLOT_tempmin_pre = ggplot(DF_Temp_Plot_Pre_A,aes(x = `Growth Month`, y = `Temperature, Minimum (°F)`)) + 
    geom_point(aes(text = `Common Name`), shape = 9, size = 3, colour='#3E236E') + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
    scale_y_continuous(label=function(x){return(paste0(x,"°"))}) + 
    geom_line(data = DF_Temp_Plot_Pre_B, aes(text = `Common Name`), colour = '#EBAF4C', size = 2) + 
    theme_bw() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Temperature")
  
  PLOT_precip = ggplotly(PLOT_precip_pre,hoverinfo = text) %>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=TRUE),yaxis=list(fixedrange=TRUE))
  
  PLOT_tempmin = ggplotly(PLOT_tempmin_pre,hoverinfo = text) %>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=TRUE),yaxis=list(fixedrange=TRUE))
  
  REACT_PLOT_precip <- reactive(PLOT_precip)
  
  REACT_PLOT_temp <- reactive(PLOT_tempmin)
  
  output$module_ui <- renderUI({
    
    htmlTemplate(
      filename = "www/modules/plots/index.html",
      precip_plot_for_index_HTML = renderPlotly(REACT_PLOT_precip()),
      temp_plot_for_index_HTML = renderPlotly(REACT_PLOT_temp())
    )
  })
  
}