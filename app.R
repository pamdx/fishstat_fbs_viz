library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyfullscreen)
library(shinycssloaders)

source("helpers.R")

ui <- function(request){
  
  navbarPage(title = a(href = "https://www.fao.org/fishery/en/collection/global_fish_consump", target = "_blank", style = "text-decoration:none;color:inherit", div(img(src = "fao-logo-three-lines.svg", id = "logo", height = "35px", style = "border-right: 1px solid grey; padding: 0 0.5rem; position: relative; margin:-15px 0px; display:right-align; "), "FishStat Consumption of Aquatic Products")),
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('species_choice', 'Species group', choices = c('All species', unique(fbs_group$faostat_group_name))),
                          selectInput('year','Year', choices = sort(unique(fbs_total$year), decreasing = TRUE), selected = max(unique(fbs_total$year))),
                          hr(),
                          fullscreen_button("full_screen", label = "Fullscreen On/Off", icon = shiny::icon("expand", lib = "font-awesome"), target = NULL),
                          br(),
                          br(),
                          bookmarkButton(label = "Share this view", icon = shiny::icon("share-alt", lib = "font-awesome")),
                          width = 2
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(
                              "Map", 
                              highchartOutput("countrymap", height = "850px") %>% withSpinner()
                            ),
                            tabPanel(
                              "Chart", 
                              highchartOutput("chart", height = "850px") %>% withSpinner()
                            ),
                            tabPanel(
                              "Table", 
                              DT::dataTableOutput("data_table", height = "850px") %>% withSpinner()
                            ),
                          )
                        )
                      ),
                      
                      hr(),
                      div(
                        class = "footer",
                        includeHTML("./footer.html")
                      ),
                      
             ),
             tabPanel("Instructions",
                      fluidPage(
                        mainPanel(
                          h1("How to use this tool"),
                          p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_fish_consump", "Consumption of Aquatic Products", target="_blank"), "dataset."),
                          p("We hope you enjoy this application. Click ", a(href="https://www.fao.org/fishery/en/fishstat", "here", target="_blank"), "to learn more about FAO's Fisheries and Aquaculture statistics."),
                          p("We encourage users to provide their feedback or ask their questions about this tool at", a(href="mailto:Fish-Statistics-Inquiries@fao.org", "Fish-Statistics-Inquiries@fao.org", target="_blank", .noWS = c('after')), "."),
                          h2("Notes on the data"),
                          p("A Food Balance Sheet for aquatic products presents a comprehensive picture of the pattern of a country's supply and utilization of aquatic products. The total quantity of aquatic products produced in a country added to the total quantity imported and adjusted to any change in stocks minus exports, gives the supply available for the indicated reference period. The result of this equation corresponds to supply, i.e. food available for consumption. This is the reason why FAO data always refer to 'apparent consumption'. In order to obtain comparable statistics in homogeneous units applicable to all countries in the world, all series have been converted into live-weight equivalent."),
                          p("The compilation of FBS, according to FAO current methodology, is a statistical exercise drawing together data from various sectors, for example production and trade, on the basis of information available in a given period – in general calendar year. Aquatic products contained in Food Balance Sheets do not represent individual commodities, but the aggregation of different products and forms derived from aquatic animal species. About 2000 species produced and 1000 items traded are conveyed into 8 main groups of similar biological characteristic, reflecting FAO ISSCAAP Classification. "),
                          p("The derived consumption data are as reliable as the basic production, trade and domestic utilization data on which they are based. Trends in food availability may reflect improved primary data rather than real changes in food intake."),
                          h2("The Data Explorer"),
                          p("Under", em("Data Explorer,"), "you can visualize the world's apparent consumption of aquatic products by the species group and year of your choice."),
                          h3("Side panel"),
                          p("The", em("side panel"), "located on the left of the user interface allows you to select the species group to visualize on the right side of the interface. Statistics of apparent consumption for aquatic products are divided into the following eight broad groups of species:" ),
                          tags$ul(
                            tags$li(em("Freshwater and Diadromous fish", .noWS = c('after')), ": including carps, barbels, tilapias, sturgeons, eels, salmons, trouts, shads, etc."), 
                            tags$li(em("Demersal fish", .noWS = c('after')), ": including flatfishes, cods, hakes, haddocks, redfishes, sharks, etc."), 
                            tags$li(em("Pelagic fish", .noWS = c('after')), ": including anchovies, herrings, sardines, tunas, mackerels, etc. "),
                            tags$li(em("Marine fish, other", .noWS = c('after')), ": including unidentified marine fish."),
                            tags$li(em("Crustaceans", .noWS = c('after')), ": including crabs, lobsters, shrimps, krill, etc."),
                            tags$li(em("Molluscs excl. Cephalopods", .noWS = c('after')), ": including abalones, oysters, mussels, scallops, clams, etc."),
                            tags$li(em("Cephalopods", .noWS = c('after')), ": including squids, cuttlefishes, octopuses, etc."),
                            tags$li(em("Aquatic animals, others", .noWS = c('after')), ": including frogs, turtles, sea-cucumbers, sea-urchins, etc."),
                          ),
                          p("The user can also use the filter in this panel to select the year of the data. Finally, the two buttons on the bottom of the side panels allow the user to display the application in fullscreen and to share the current view with somebody else."), 
                          tags$img(src = "side_panel.png"),
                          h3("Map"),
                          p("The data is first represented on a map under the", em("Map"), "tab. Each bubble represents a country or territory's apparent consumption of aquatic products in kg/capita for the species group and year selected in the side panel. Placing your cursor on individual bubbles will give you more information on a given country or territory's apparent consumption You can zoom in or out of the map using the + and - buttons on the top left of the map. This is particularly useful to better explore data in areas of the world with a high density of countries or territories. Finally, you can export the map as an image by clicking on the three lines on the top right of the map."), 
                          tags$img(src = "map_illustration.png"),
                          h3("Chart"),
                          p("If you want to focus on the countries/territories with the highest apparent consumption for the species group you selected, you can display the top 10 of these countries/territories as a bar chart under the", em("Chart"), "tab. Placing your cursor on individual bars will give you more information on a given country or territory's apparent consumption You can export the visual as an image by clicking on the three lines on the top right of the chart."), 
                          tags$img(src = "chart_illustration.png"),
                          h3("Table"),
                          p("Finally, you can display a table listing the countries that consume the species group of your choice in the", em("Table"), "tab. The data can be exported by clicking on any of the buttons on the top left of the table."),
                          tags$img(src = "table_illustration.png"),
                          h1("Map disclaimer"),
                          p("The boundaries and names shown and the designations used on this map do not imply the expression of any opinion whatsoever on the part of FAO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers and boundaries."),
                          h1("License"),
                          tags$img(src = "cc_by.png"),
                          p(""),
                          p("This work is made available under the Creative Commons Attribution-4.0 International licence (CC BY 4.0 ", a(href = "https://creativecommons.org/licenses/by/4.0/legalcode.en", "https://creativecommons.org/licenses/by/4.0/legalcode.en", target="_blank", .noWS = c('after')), "). By using this database, you agree to be bound by the terms of this license and the ", a(href = "https://www.fao.org/contact-us/terms/db-terms-of-use/en", "FAO Statistical Database Terms of Use", target="_blank", .noWS = c('after')), ".")
                        )
                      )
             ),
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
             )
  )
  
}

server <- function(input, output, session) {
  
  # Map of consumption by country
  
  data_map <- reactive({
    
    {if (input$species_choice == 'All species') fbs_total
      else fbs_group} %>%
      filter(year == input$year) %>%
      filter(faostat_group_name %in% input$species_choice) %>%
      filter(!is.na(supply_capita_kg)) %>%
      filter(supply_capita_kg > 0) %>%# better not to show bubbles when per capita supply = 0
      rename(z = supply_capita_kg) %>%
      mutate(value_formatted = addUnits(z))
    
  })
  
  data_average <- reactive({
    
    {if (input$species_choice == 'All species') fbs_total
      else fbs_group} %>%
      filter(year == input$year) %>%
      filter(faostat_group_name %in% input$species_choice) %>%
      filter(!is.na(supply_capita_kg)) %>%
      summarise_at(c("total_food_supply", "population"), sum, na.rm = TRUE) %>%
      mutate(supply_capita_kg = total_food_supply/population*1000) %>%
      pull() %>%
      addUnits()
    
  })
  
  title <- reactive({
    if (input$species_choice == 'All species') {paste0('Per capita apparent consumption of aquatic products, ', input$year)}
    else if (input$species_choice != 'All species') {paste0('Per capita apparent consumption of ', tolower(input$species_choice), ", ", input$year)}
  })
  
  subtitle = reactive({
    paste('Global average:', data_average(), 'kg/capita.')
  })
  
  source = paste0("Source: FAO ", format(Sys.Date(), "%Y"), ". Consumption of Aquatic Products. In: Fisheries and Aquaculture. Rome. [Cited ", format(Sys.time(), "%A, %B %d %Y"), "]. 
                                https://www.fao.org/fishery/en/collection/global_fish_consump")
  
  output$countrymap <- renderHighchart(
    
    highchart(type = "map") %>%
      hc_add_series(mapData = map, showInLegend = F) %>%
      hc_add_series(data = data_map(), 
                    showInLegend = F,
                    type = "mapbubble", 
                    name = "Country or area",
                    # color = '#377eb8', # choose color based on species group?
                    minSize = "0.1",
                    maxSize = "60",
                    tooltip = list(pointFormat = "Country or area: {point.country_name}<br>Species group: {point.faostat_group_name}<br>Year: {point.year}<br>Apparent consumption: {point.value_formatted}<br> Unit: kg/capita<br>")) %>%
      hc_colorAxis(minColor = "#e4eef7", 
                   maxColor = '#377eb8',
                   showInLegend = FALSE) %>%
      hc_title(text = title()) %>%
      hc_subtitle(text = subtitle()) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_legend(enabled = TRUE, 
                layout = "horizontal", 
                align = "right",
                verticalAlign = "bottom") %>%
      hc_caption(text = source) %>%
      hc_exporting(enabled = TRUE,
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(backgroundColor = "#FFFFFF"),
                     legend = list(bubbleLegend = list(enabled = TRUE, 
                                                       color = '#377eb8', 
                                                       borderColor = '#999999', 
                                                       connectorColor = '#999999'))
                   ),
                   sourceWidth = 1920,
                   sourceHeight = 1080,
                   filename = paste0("(Map) ", title())
      )
  )
  
  # Bar chart of consumption by country
  
  data_chart <- reactive({
    
    {if (input$species_choice == 'All species') fbs_total
      else fbs_group} %>%
      filter(year == input$year) %>%
      filter(faostat_group_name %in% input$species_choice) %>%
      filter(!is.na(supply_capita_kg)) %>%
      rename(z = supply_capita_kg) %>%
      mutate(value_formatted = addUnits(z)) %>%
      arrange(desc(z)) %>%
      head(10)
    
  })
  
  output$chart <- renderHighchart({
    hchart(data_chart(), 
           type = "column", 
           hcaes(x = country_name, y = z), 
           name = "Apparent consumption",
           color = '#377eb8',
           tooltip = list(pointFormat = "Country or area: {point.country_name}<br>Species group: {point.faostat_group_name}<br>Year: {point.year}<br>Apparent consumption: {point.value_formatted}<br> Unit: kg/capita<br>")) %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Apparent consumption (kg/capita)")) %>%
      hc_title(text = title()) %>%
      hc_subtitle(text = paste("Top 10 countries/territories.", subtitle())) %>%
      hc_caption(text = source) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(
                       backgroundColor = "#FFFFFF"
                     )
                   ),
                   # sourceWidth = 1920,
                   # sourceHeight = 1080,
                   filename = paste0("(Chart) ", title())
      )
  })
  
  # Table of apparent consumption by country and species group
  
  data_table <- reactive({
    
    {if (input$species_choice == 'All species') fbs_total
      else fbs_group} %>%
      filter(year == input$year) %>%
      filter(faostat_group_name %in% input$species_choice) %>%
      filter(!is.na(supply_capita_kg)) %>%
      rename(z = supply_capita_kg) %>%
      mutate(value_formatted = addUnits(z)) %>%
      mutate(unit_name = "kg per capita") %>%
      select(country_name, faostat_group_name, element_name, unit_name, year, value_formatted) %>%
      arrange(desc(value_formatted)) %>%
      add_row(country_name = source) # add citation in the last row
    
  })
  
  output$data_table <- DT::renderDataTable(server = FALSE, { # server = FALSE used to make sure the entire dataset is downloaded when using the buttons
    datatable(data_table(),
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = list('copy', list(
                  extend = 'collection',
                  buttons = list(
                    list(extend = 'csv', 
                         filename = paste0("(Table) ", title())),
                    list(extend = 'excel', 
                         filename = paste0("(Table) ", title())),
                    list(extend = 'pdf', 
                         filename = paste0("(Table) ", title()))),
                  text = 'Download'
                )),
                pageLength = 15, 
                lengthMenu = c(10,50,100)
              ),
              rownames = FALSE,
              class = "display",
              caption = title(), 
              colnames = c("Country or area", "Species group", "Element name", "Unit", "Year", "Value"))
  })
  
  
  
  ### BOOKMARKING-RELATED CODE
  
  # Parse query string to identify which elements should be shown (either everything or just one of the interactive visuals)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['show']])) {
      output$show <- renderText({
        query[['show']]
      })
      outputOptions(output, "show", suspendWhenHidden = FALSE)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")