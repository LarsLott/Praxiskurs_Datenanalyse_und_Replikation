#### "Conceptualizing and Measuring Autocratization Episodes" ####
# authors: "Pelke, Lars; Croissant, Aurel"
# date: 2021-01-12
# journal: Swiss Political Science Review
# written under "R version 3.6.0 (2019-12-12)"

#### Preliminaries ####

R.version$version.string

###### Shiny App ####
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(shinydashboard)
library(DT)
library(viridis)


vdem_map_master <- readRDS("data/vdem_map_master10.rds")
names(st_geometry(vdem_map_master)) = NULL

vdem_sub_fig_shiny <- readRDS("data/vdem_sub_fig_shiny10.rds")

vdem_auto_forms <- readRDS("data/vdem_auto_forms10.rds")

country_list_convergence <- readRDS("data/CC_country_list.rds")

country_convergence_map <- readRDS("data/CC_country_map.rds")
names(st_geometry(country_convergence_map)) = NULL

vdem_map_master$year <- as.character(vdem_map_master$year)


#### User Interface ####
ui <- navbarPage("Autocratization", id="nav",
                 
                 tabPanel("Introduction",
                          
                          tags$h2("Introduction"),
                          tags$div(
                            tags$p("This is an interactive appendix, accompanying the study: "),
                            tags$div(
                            tags$base("'Conceptualizing and Measuring Autocratization Episodes'"),
                            tags$i("Swiss Political Science Review"),
                            tags$base("by Lars Pelke and Aurel Croissant (2020)"))),
                          
                          tags$h2("Purpose of this App"), 
                          tags$div(
                            tags$p("In the current debate on autocratization, a comprehensive overview of different autocratization concepts 
                                  and empirical measures is still lacking. Addressing that gap, this research note provides new insights on "),
                            tags$p("how to conceptualize and measure autocratization periods with Freedom House, Polity IV and V-Dem data, and 
                                  how different measures produce different findings regarding the number of autocratization periods."),
                            tags$p("Hence, this Shiny Web Application enables users to assess the different operationalization strategies discussed 
                                  in the study and enables users to map autocraitzation between 1990 and 2019.")
                          ),
                          
                          tags$h2("Context"),
                          tags$div(
                            tags$p("Interacting with the study's data, you can compute various estimates of interest and map autocratization periods")
                            ), 
                          
                          tags$h3("Interactive map"), 
                          tags$li("Assess the different autocratization operationalization by mapping these on the world map"), 
                          tags$li("Compare different autocratization periods between 1990 and 2019"), 
                          
                          tags$h3("Figure 2: Autocratization Periods by Year"), 
                          tags$li("Assess the different autocratization operationalization using a lineplot and the cumulative sums of each operationalization by year"), 
                          tags$li("Compare different autocratization operationalization on a yearly basis between 1900 and 2018"), 
                          
                          tags$h3("Figure 3 and 4: Autocratization forms by different operationalization"), 
                          tags$li("Assess autocratization forms by different operationalization after 1990"), 
                          tags$li("Compare different autocratization operationalization and the number of autocratization forms, e.g. autocratization with no regime types change or with change from Liberal democracy to Electoral Democracy"), 
                          
                          tags$h3("Figure 5: Country Convergence Map"), 
                          tags$li("Assess the convergence between the measures by country"), 
                          tags$li("Compare the convergence measure within the world map and find which country have which convergence measure"), 
                          
                          tags$h3("Table A1: List of Different Backsliders"), 
                          tags$li("Assess the list of different backsliders based on Table A1 in the Supplementary Appendix"), 
                          tags$li("Find different countries and the autocratization measure based on EDI 0.1 CI, LDI 0.1 CI, Polity IV 0.1 and FH 0.1"), 
                          
                          
                          tags$h2("Acknowledgement"),
                          tags$div(
                            tags$p("To create this app, we drew on the insights of various contributors on Stackoverflow.  
                                  These sources are listed in the replication code for this app.")
                          ),
                          tags$h2("Download Dataset"),
                          tags$div (
                            tags$a(href="https://osf.io/76xy8/", "https://osf.io/76xy8/")
                          )
                          
                 ),
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              br(),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("myMap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Map Autocratization"),
                                            selectInput(inputId = "index",
                                                        label = "Select Index:",
                                                        choices = unique(vdem_map_master$Index),
                                                        multiple = FALSE, 
                                                        selected = "EDI 0.1"),
                                            selectInput(inputId = "year",
                                                        label = "Select Year:",
                                                        choices = unique(vdem_map_master$year),
                                                        multiple = FALSE, 
                                                        selected = 1990)
                              )
                          )
                          ),
                 tabPanel("Figure 2: Autocratization Periods by Year",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              br(),
                              align = "center",
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              plotlyOutput(outputId = "p", width = "80%", height = "70%"), 
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 250, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Autocratization measures of time. Differences and Conformities"),
                                            selectizeInput(
                                              inputId = "auto_def", 
                                              label = "Select Autocratization Period Definition", 
                                              choices = unique(vdem_sub_fig_shiny$auto_def), 
                                              selected = "EDI 0.1",
                                              multiple = TRUE
                                            )
                              )
                              
                          )   
                 ),
                 
                 tabPanel("Figure 3 and 4: Autocratization forms",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              br(),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              plotlyOutput(outputId = "auto_forms", width = "80%", height = "70%"), 
                              br(),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto", 
                                            
                                            tags$h3("Autocratization forms by different operationalization after 1990"), 
                                            tags$p("Assess autocratization forms by different operationalization"), 
                                            tags$p("Compare different autocratization operationalization and the number of autocratization forms, e.g. autocratization with no regime types change or with change from Liberal democracy to Electoral Democracy"), 
                                            tags$i("Note: The unit of analysis are country-years. For each autocratization operationalization, each bar illustrates the distribution of the recorded cases by regime types. Sources: Author's own calculations based on V-Dem, Polity IV and Freedom House.")
                                            )
                             
                          )   
                 ),
                 
                 tabPanel("Figure 5: World Map with Congruence by Country",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              br(),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("CountryCongruenceMap", width="100%", height="100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto", 
                                            
                                            tags$h3("World Map with Mean Congruence between Autocratization Measures by Country"), 
                                            tags$i("Note: The unit of analysis are countries. Darker colours indicate less mean congruence between different autocratization measures. Countries filled with grey have no autocratization episode between 1900 and 2019 according to our different autocratization measures: Canada, Sweden, Switzerland, Slovakia, Slovenia, North Macedonia, Saudi Arabia, Myanmar, New Zealand, Swaziland, Republic of Congo. Sources: Author's own calculations based on V-Dem, Polity IV and Freedom House. "),
                                            tags$h3(" Select a threshold"),
                                            tags$p("Plese select a threshold you want to display. The map shows the mean congruence by country for over all thresholds (see Figure 6 in the original paper), for the 0.05 threshold, for the 0.1 threshold, for the 0.15 threshold, and for the 0.2 threshold"),
                                            selectInput(inputId = "thresholds",
                                                        label = "Select threshold:",
                                                        choices = unique(country_convergence_map$thresholds),
                                                        multiple = FALSE, 
                                                        selected = "all"), 
                                            tags$i("Note:Please note that the 'Number of Periods' displayed for each country are the overall number of periods, that were ever detected by any of the autocratization measurements.")
                                            )
                          )
                 ),
                 
                 tabPanel("Table A1: List of Different Backsliders",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              align = "center",
                              
                              br(),
                              
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              DT::dataTableOutput("CongruenceTable", width = "80%"), 
                              style = "overflow-y: scroll;"
                             
                          )
                              
                  ),  
                 
                
                 tabPanel("Sources and Information",
                                    
                                    tags$h2("Overview"),
                                    tags$p("This page contains information on the datasets that were used to create the maps and plots. Also, information on the replication material can be found here. "),

                                    tags$h2("Datasets"), 
                                    tags$li("Coppedge, M. et al. (2020). V-Dem Country-Year Dataset v10."),
                                    tags$li("Freedom House. (2019). Freedom in the World."),
                                    tags$li("Monty G. Marshall, Gurr, T. R., & Jaggers, K. (2019). POLITY IV Annual Time Series, 1800-2018."),
                                    
                                    tags$h2("Code Sources and Stackoverflow"),
                                    tags$li(tags$a(href= "https://github.com/rstudio/shiny-examples","https://github.com/rstudio/shiny-examples")), 
                                    tags$li(tags$a(href= "https://rstudio.github.io/shinydashboard/appearance.html#icons", "https://rstudio.github.io/shinydashboard/appearance.html#icons")),
                                    tags$li(tags$a(href= "https://shiny.rstudio.com/gallery/superzip-example.html", "https://shiny.rstudio.com/gallery/superzip-example.html")),
                                    tags$li(tags$a(href= "https://stackoverflow.com/questions/44118373/r-shiny-chorpleth-map", "https://stackoverflow.com/questions/44118373/r-shiny-chorpleth-map")),
                            
                                    tags$h2("Replication Material"),
                                    tags$p("The replication material for the paper 'Conceptualizing and Measuring Autocratization Episodes'(Lars Pelke and Aurel Croissant, 2020) are available from: "), 
                                    tags$a(href="https://osf.io/76xy8/", "https://osf.io/76xy8/")
                                    
                           ),
                           

conditionalPanel("false", icon("crosshair"))
)


#### Server Background #####

library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(htmltools)
library(shinydashboard)
library(viridis)


vdem_map_master <- readRDS("data/vdem_map_master10.rds")

vdem_sub_fig_shiny <- readRDS("data/vdem_sub_fig_shiny10.rds")

country_list_convergence <- readRDS("data/CC_country_list.rds")

country_convergence_map <- readRDS("data/CC_country_map.rds")

server <- function(input,output, session, ...){
  
  output$myMap <- renderLeaflet({
    temp <- which(vdem_map_master$Index == input$index &
                    vdem_map_master$year == input$year)
    print(nrow(vdem_map_master))
    vdem_map_master <- vdem_map_master[temp,]
    print(nrow(vdem_map_master))
    
    pal <- colorNumeric("darkred",vdem_map_master$auto_period)
    
    label1 <- paste('<strong>', vdem_map_master$name,  ": ",  vdem_map_master$first_year, "- ", 
                    vdem_map_master$last_year, ": ", '</strong>', vdem_map_master$auto_decline_period, sep = ""
    ) %>% lapply(htmltools::HTML)
    
    label2 <- paste("Regime Type before: ", vdem_map_master$lag_v2x_regime, sep = ""
                    ) %>% lapply(htmltools::HTML)
    
    label3 <- paste("Regime Type end: ", vdem_map_master$last_v2x_regime, sep = ""
                    ) %>% lapply(htmltools::HTML)
    
    labels <- paste0('<p>', label1, '<p></p>', label2, '</p><p>', label3, '</p>'
                    )%>% lapply(htmltools::HTML)

    leaflet(data = vdem_map_master) %>% 
      addPolygons(
        fillColor = ~pal(vdem_map_master$auto_period),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "1", 
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),  
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
  })
  
  output$p <- renderPlotly({
    req(vdem_sub_fig_shiny$auto_def)
    if (identical(vdem_sub_fig_shiny$auto_def, "")) return(NULL)
    p <- ggplot(data = filter(vdem_sub_fig_shiny, auto_def %in% input$auto_def)) + 
      geom_line(aes(year, auto_num, group = auto_def, color = auto_def)) +
      theme_bw() +
      labs(title = "", 
           x = " Year", 
           y = " Number of Countries", 
           color = "Autocratization Definitions")
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    ggplotly(p, height = height, width = width)
  })
 
  output$auto_forms <- renderPlotly({
    
    auto_forms <- ggplot(vdem_auto_forms, aes(x = auto_forms, y = number_auto_forms, fill = Index)) +
      geom_col(position = "dodge", color = "black") +
      theme_bw() +
      scale_fill_viridis(discrete = TRUE)+
      labs(x =" Autocratization Forms", 
           y = "Number of Autocratization Periods") +
      coord_flip() +
      theme(legend.position="bottom")
    
    ggplotly(auto_forms, tooltip =  c("Index", "number_auto_forms"))
    
  })
  
  output$CongruenceTable = DT::renderDataTable({
    country_list_convergence
  })
  
  output$CountryCongruenceMap <- renderLeaflet({
    
    temp <- which(country_convergence_map$thresholds == input$thresholds)
    print(nrow(country_convergence_map))
    country_convergence_map <- country_convergence_map[temp,]
    print(nrow(country_convergence_map))
    
    palmap <- colorNumeric(
      palette = "viridis",
      domain = country_convergence_map$country_convergence, 
      na.color = "#808080")
    
    label1 <- paste('<strong>', country_convergence_map$country, '</strong>') %>% lapply(htmltools::HTML)
    
    label2 <- paste("Number of Periods: ", '<strong>', country_convergence_map$count_country_periods,  '</strong>', sep = ""
    ) %>% lapply(htmltools::HTML)
    
    label3 <- paste("Convergence between Measures: ", '<strong>', country_convergence_map$country_convergence, "%", '</strong>', sep = ""
    ) %>% lapply(htmltools::HTML)
    
    labels <- paste0('<p>', label1, '<p></p>', label2, '</p><p>', label3, '</p>'
    )%>% lapply(htmltools::HTML)
    
    leaflet(data = country_convergence_map) %>% 
      addPolygons(
        fillColor = ~palmap(country_convergence_map$country_convergence),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "1", 
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),  
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
  })
  
  
}


shinyApp(ui, server)

