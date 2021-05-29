################################################################################
# UI of the app
#
# Author: Liam Trotzuk
# Created: 2020-10-10
################################################################################

htmlTemplate(
  
  filename = "www/index.html",
  
  ### images ###
  sunflower_logo = tags$img(src = 'images/sunflower-icon.png',
                            class = "img-fluid",
                            id = "test",
                            width = "50",
                            height = "50"),
  usda_logo = tags$img(src = "images/USDA_short.png",
                       class="img-fluid", 
                       id="footer-img", 
                       alt="Responsive image", 
                       width="300", 
                       height="300"),
  ncss_logo = tags$img(src = "images/ncss_stacked_color.png",
                       class="img-fluid",
                       id="footer-img",
                       alt="Responsive image",
                       width="300",
                       height="300"),
  noaa_logo = tags$img(src = "images/noaa.png",
                       class="img-fluid",
                       id="footer-img",
                       alt="Responsive image",
                       width="300",
                       height="300"),
  wikimedia_logo = tags$img(src = "images/wikimedia.png",
                            class="img-fluid",
                            id="footer-img",
                            alt="Responsive image",
                            width="300",
                            height="300"),
  nrcs_logo = tags$img(src = "images/NRCSlogo.png",
                       class="img-fluid",
                       id="footer-img",
                       alt="Responsive image",
                       width="300",
                       height="300"),
  ### end images ###
  
  ### start inputs ###
  zip_code_input = textInput("zip_code_entry",
                             "What's your US zip code?",
                             width = '80px',
                             placeholder = "10025"),
  
  select_growth_environment = radioGroupButtons("select_growth_environment",
                                                "What environment do you live in? Pick 1.",
                                                c("Urban (window box, small garden)",
                                                  "Suburban (front lawn, backyard)",
                                                  "Rural (fields, woodland)"),
                                                size = 'normal',
                                                individual = T,
                                                checkIcon = list(
                                                  yes = icon("ok",
                                                             lib = "glyphicon"))),
  
  select_desired_characteristics = checkboxGroupButtons("select_desired_characteristics",
                                                        "What characteristics would you like your plants to have? Pick up to 3.",
                                                        c("Available to Buy",
                                                          "Threatened in my State",
                                                          "Fast Growing",
                                                          "Long Lifespan",
                                                          "Moisture Efficient",
                                                          "Drought Tolerant",
                                                          "Frost Tolerant",
                                                          "Cold Stratification",
                                                          "Fire Tolerant",
                                                          "Fire Resistant",
                                                          "Edible for Animals",
                                                          "Flowering Plant",
                                                          "Edible for People",
                                                          "Produces Berries/Nuts"),
                                                        selected = "Available to Buy",
                                                        size = 'normal',
                                                        individual = T,
                                                        checkIcon = list(
                                                          yes = icon("ok",
                                                                     lib = "glyphicon"))),
  
  enter_button = actionBttn("main_button",
                            "CREATE",
                            style = "material-flat",
                            color = "royal",
                            size = "lg"),
  
  ### end inputs ###
  
  ### start outputs ###
  
  calendar_output = calendar_ui("calendar"),
  
  map_output = map_ui("map_output"),
  
  plots_output = plots_ui("plots_output")
  
)
