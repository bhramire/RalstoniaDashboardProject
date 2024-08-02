#BRLibraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(gsheet)
library(tidyverse)
library(leaflet)
library(shinyjs)
library(DT)
library(patchwork)
library(cowplot)
library(ggthemes)
library(fresh)
library(googlesheets4)
library(plotly)


# files = c(".secrets/bb8565617b5e9996ff93e6d214ba7218_kaique.alves@ufv.br",
#           ".Rprofile",
#           "app.R")
# rsconnect::deployApp(appFiles = files)
#BRCreating Objects to color the dashboard
mytheme <- create_theme(
  adminlte_color(
    light_blue = "steelblue"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_color = "black",
    dark_bg = "steelblue",
    dark_hover_bg = "black",
    dark_submenu_color = "black"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)
#BRGoogle lock for loading data, remove this
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "kaique.alves@ufv.br"
)
# run sheets auth
gs4_auth(use_oob = TRUE)
#BRdata load, replace with read_csv
# fgsc_data_load = read_sheet('https://docs.google.com/spreadsheets/d/1uYGcq9TSMOzhloJfZjXrIcFp-amrsvmtElvn0jXnCpg/edit#gid=0')
fgsc_data_load = gsheet2tbl('https://docs.google.com/spreadsheets/d/17Edg-RUuzcwGE_m7d1DRPb0UKCVOusdrxY5tEH6poKQ/edit?usp=sharing') 
set.seed(1)
#BRCreating pre-selected options for dashboard interactives
specie_selected = c("F. graminearum","F. meridionale", "F. asiaticum","F. boothii")
genotype_selected = c("15-ADON", "3-ADON", "NIV", "NX-2")
#BRDatacleaning, setNA as "undetermined" or others, i like it
fgsc_data = fgsc_data_load %>% 
  mutate(TRI_genotype2 = TRI_genotype) %>%
  mutate(TRI_genotype2 = case_when(!is.na(TRI_genotype2) ~ TRI_genotype2,
                                   is.na(TRI_genotype2) ~ "Undetermined")) %>% 
  
  mutate(TRI_genotype = case_when( TRI_genotype %in% genotype_selected  ~TRI_genotype,
                                   is.na(TRI_genotype) ~ "Undetermined",
                                   !is.na(TRI_genotype) & !TRI_genotype %in% genotype_selected ~ "Others")) %>%
  mutate(FGSC = case_when(!is.na(FGSC) ~ FGSC,
                          is.na(FGSC) ~ "Undetermined")) %>% 
  #Actually, I don't like this one, others indicates that it's new host
  mutate(FGSC2 = case_when(FGSC %in% specie_selected ~ FGSC,
                           is.na(FGSC) ~ FGSC,
                           !is.na(FGSC) & !FGSC %in% specie_selected ~ "Others"))%>% 
  #BRPrevents geopoints from containing multiple isolations, keep?
  unite("latlong", Latitude, Longitude, sep ="/", remove = F ) %>% 
  group_by(latlong) %>% 
  mutate(n = n(),
         Latitude = case_when(n > 1 ~ rnorm(n, Latitude,0.01),
                              n == 1 ~ Latitude),
         Longitude = case_when(n > 1 ~ rnorm(n, Longitude,0.01),
                               n == 1 ~ Longitude)
  ) %>%
  #BRUnnecessary?
  mutate(Pub1 = case_when(!is.na(Pub1) ~ Pub1,
                          is.na(Pub1) ~ "Unpublished")) %>% 
  mutate(Host = case_when(!is.na(Host) ~ Host,
                          is.na(Host) ~ "unknown")) %>%
  #BRcounts species
  group_by(FGSC) %>% 
  mutate(n_iso_per_specie = n(),
         FGSC3 = paste(FGSC," (",n_iso_per_specie,")", sep = ""),) %>% 
  ungroup() %>% 
  #BR removes created n and latlong columns
  dplyr::select(-n, -latlong) 



#App time, time for App, App o clock
ui = dashboardPage(skin = "blue",
                   
                   dashboardHeader(title = "FGSCdb",
                                   titleWidth = 250),
                   
                   dashboardSidebar(collapsed = F,
                                    width = 250 ,
                                    br(),
                                    #BRMarkdown Notations
                                    div(style="display:inline-block;width:80%;margin-left:18px;text-align: left;",
                                        "A georeferenced database of isolates of the", em("Fusarium graminearum") ," clade identified to species
       and trichothecene chemotypes as reported in the literature.
       Use the filters below to refine search, visualize and download the data"),   
                                    sidebarMenu(id = "sidebarid",
                                                
                                                # menuItem("Explorer", tabName = "map_view", icon = icon("grid-horizontal")),
                                                
                                                # sliderInput(inputId = "year",
                                                #             label = "Select year interval",
                                                #             min = 1979,
                                                #             max = 2021,
                                                #             step = 1,
                                                #             value = c(1979,2021)),
                                                #BRInteractives
                                                pickerInput(inputId = "specie",
                                                            label = "Species",
                                                            choices = unique(fgsc_data$FGSC3),
                                                            options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 3"
                                                            ),
                                                            selected = unique(fgsc_data$FGSC3),
                                                            multiple = T
                                                ),
                                                
                                                pickerInput(inputId = "genotype",
                                                            label = "Select genotype(s)",
                                                            choices = unique(fgsc_data$TRI_genotype2),
                                                            options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 3"
                                                            ),
                                                            selected = unique(fgsc_data$TRI_genotype2),
                                                            multiple = T),
                                                
                                                pickerInput(inputId = "host",
                                                            label = "Host",
                                                            choices = sort(unique(fgsc_data$Host)),
                                                            options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 3"
                                                            ),
                                                            selected = unique(fgsc_data$Host),
                                                            multiple = T),
                                                
                                                pickerInput(inputId = "country",
                                                            label = "Country",
                                                            choices = sort(unique(fgsc_data$Country)),
                                                            options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 3"
                                                            ),
                                                            selected = unique(fgsc_data$Country),
                                                            multiple = T),
                                                
                                                pickerInput(inputId = "article",
                                                            label = "Reference",
                                                            choices = sort(unique(fgsc_data$Pub1)),
                                                            options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 3"
                                                            ),
                                                            selected = unique(fgsc_data$Pub1),
                                                            multiple = T),
                                                
                                                div(style="display:inline-block;width:25%;text-align: center;",
                                                    actionButton(inputId = "search",
                                                                 label = "Filter",
                                                                 icon =icon("filter"))),
                                                div(style="display:inline-block;width:25%;text-align: center;",
                                                    actionButton(inputId = "reset",
                                                                 label = "Select all",
                                                                 icon =icon("retweet"))),
                                                # br(),
                                                div(style="display:inline-block;width:60%;text-align: center;",
                                                    downloadButton("download",
                                                                   "Get data"))
                                    )
                   ),
                   dashboardBody(use_theme(mytheme),
                                 # tabItems(
                                 # tabItem(tabName = "map_view",
                                 shinyjs::useShinyjs(),
                                 div(id = "myapp",
                                     fluidRow(
                                       #BR Display the counts
                                       valueBoxOutput("n_isolados", width = 3),
                                       infoBoxOutput("n_artigos", width = 3),
                                       infoBoxOutput("n_countries", width = 3),
                                       valueBoxOutput("n_species", width = 3),
                                       # infoBoxOutput("n_hosts", width = 2),
                                       #BR Displays multiple visualization options to choose from
                                       tabBox(title = "",
                                              width = 12,
                                              height = "100%",
                                              tabPanel(icon = icon("map"),
                                                       "Map: Species",
                                                       leafletOutput("map_specie",
                                                                     width = "100%",
                                                                     height = 500)
                                              ),
                                              tabPanel(icon = icon("map"),
                                                       "Map: Genotypes",
                                                       leafletOutput("map_myco",
                                                                     width = "100%",
                                                                     height = 500)
                                              ),
                                              tabPanel(icon = icon("table"),
                                                       "Grid View",
                                                       # div(
                                                       DT::dataTableOutput("grid")#)
                                              ),
                                              tabPanel(icon = icon("chart-bar"),
                                                       "Charts",
                                                       plotlyOutput("plot1")
                                                       # plotOutput("plot1",
                                                       # width = "100%",
                                                       # height = 500)#)
                                              ),
                                              tabPanel(icon = icon("chart-line"),
                                                       "Trends",
                                                       plotlyOutput("plot2")
                                                       # plotOutput("plot2",
                                                       # width = "100%",
                                                       # height = 500)
                                              )
                                              
                                       ),
                                       # tabBox(title = "",
                                       #         width = 4,
                                       #         height = 500,
                                       #         tabPanel(icon = icon("chart-bar"),
                                       #                  "Species",
                                       #                  plotOutput("plot1")),
                                       #         tabPanel(icon = icon("chart-line"),
                                       #                  "Temporal",
                                       #                  plotOutput("plot2"))
                                       #    
                                       #  ),
                                       # box(width =2,
                                       #     height = 30,
                                       #     solidHeader = F),
                                       
                                       
                                     ))
                                 
                   )
                   # tabItem()grid 
                   # )
                   
                   # )
                   
)

server = function(input, output, session) {
  
  observeEvent(input$reset, {
    shinyjs::reset("year")
    shinyjs::reset("host")
    shinyjs::reset("specie")
    shinyjs::reset("genotype")
    shinyjs::reset("country")
    shinyjs::reset("article")
    
    
  })
  
  
  
  
  filtered_year_type <- eventReactive(input$search,{
    
    fgsc_data #%>% 
    # filter(Year >= input$year[1],
    # Year <= input$year[2])
  })
  
  filtered_specie_type <- eventReactive(input$search,{
    
    filtered_year_type() %>%
      filter(FGSC3 %in% input$specie)
    
  })
  
  filtered_genotype_type <- eventReactive(input$search,{
    
    filtered_specie_type() %>%
      filter(TRI_genotype2 %in% input$genotype)
    
  })
  
  filtered_host_type <- eventReactive(input$search,{
    
    filtered_genotype_type() %>%
      filter(Host %in% input$host)
    
  })
  
  filtered_country_type <- eventReactive(input$search,{
    
    filtered_host_type() %>%
      filter(Country %in% input$country)
    
  })
  
  filtered_article_type <- eventReactive(input$search,{
    
    filtered_country_type() %>%
      filter(Pub1 %in% input$article)
    
  })
  
  
  
  
  
  
  
  output$map_specie =  renderLeaflet({
    
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    if(nrow(data_leaflet) == 0){
      leaflet(a)%>%
        addTiles()
    }else{
      
      factpal <- colorFactor(palette = "Set2", domain = unique(fgsc_data$FGSC2))
      
      leaflet(data_leaflet,
              width = "100%",
              height = 15) %>%
        setView(-0, 15, zoom = 2) %>%
        addTiles() %>%
        
        addCircleMarkers(
          radius = 3,
          stroke = FALSE,
          lng = ~Longitude,
          lat =~Latitude,
          color = ~factpal(FGSC2),
          fillOpacity = 1,
          label = paste(data_leaflet$FGSC,"- click for details"),
          labelOptions = labelOptions(style = list("font-style" = "italic")),
          popup = paste("Collection code:<b>",
                        data_leaflet$`Collection code`,
                        "</b>", "<br><i>",
                        data_leaflet$FGSC,"</i>(", data_leaflet$TRI_genotype2,
                        ")<br>", "Host:", data_leaflet$Host,"<br>",
                        "Location:", data_leaflet$Country, "<br>",
                        "Year of collection:",
                        data_leaflet$Year, "<br>","Reference:", data_leaflet$Pub1)
        ) %>%
        addLegend("bottomright",
                  pal = factpal,
                  values = ~FGSC2,
                  title = "Species",
                  opacity = 1
        ) %>%
        addLayersControl(
          baseGroups = c("Default", "Aerial", "Terrain"),
          overlayGroups = "FGSC",
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Back to initial view",
          onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addProviderTiles("OpenTopoMap", group = "Terrain") %>%
        addScaleBar("bottomleft") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Default")
      # addProviderTiles(providers$CartoDB.Voyager, group = "Default")
      
      
    }
  })
  
  
  output$map_myco = renderLeaflet({
    
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    
    if(nrow(data_leaflet) == 0){
      mico_map = leaflet(a) %>%
        addTiles()
    }else{
      
      factpal <- colorFactor(palette = "Set2", domain = unique(fgsc_data$TRI_genotype))
      
      leaflet(data_leaflet,
              width = "100%",
              height = 15) %>%
        setView(-0, 15, zoom = 2) %>%
        addTiles() %>%
        
        addCircleMarkers(
          radius = 3,
          lng = ~Longitude,
          lat =~Latitude,
          color = ~factpal(TRI_genotype),
          fillOpacity = 1,
          stroke = FALSE,
          label = paste(data_leaflet$FGSC," - click for details"),
          labelOptions = labelOptions(style = list("font-style" = "italic")),
          popup = paste("Collection code:<b>",
                        data_leaflet$`Collection code`,
                        "</b>", "<br><i>",
                        data_leaflet$FGSC,"</i>(", data_leaflet$TRI_genotype2,
                        ")<br>", "Host:", data_leaflet$Host,"<br>",
                        "Location:", data_leaflet$Country, "<br>",
                        "Year of collection:",
                        data_leaflet$Year, "<br>","Reference:", data_leaflet$Pub1)
        ) %>%
        addLegend("bottomright",
                  pal = factpal,
                  values = ~TRI_genotype,
                  title = "TRI genotype",
                  opacity = 1
        ) %>%
        addLayersControl(
          baseGroups = c("Default", "Aerial", "Terrain"),
          overlayGroups = "FGSC",
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Back to initial view",
          onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addProviderTiles("OpenTopoMap", group = "Terrain") %>%
        addScaleBar("bottomleft") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Default")
      # addProviderTiles(providers$CartoDB.Voyager, group = "Default")
      
      
    }
  })
  
  
  
  
  output$grid = DT::renderDataTable({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    data_leaflet %>% 
      mutate(Article = Pub1) %>% 
      dplyr::select(`Collection.code`, Host, Country, Year, FGSC, TRI_genotype, Article )
    
  },options = list(autoWidth = F,autoHeight = F, scrollX = TRUE))
  
  
  output$plot1 = renderPlotly({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    g_fgsc = data_leaflet %>% 
      group_by(FGSC,TRI_genotype) %>%  
      summarise(count = n()) %>% 
      ggplot(aes(reorder(FGSC,count),count, fill = TRI_genotype))+
      geom_col()+
      # scale_y_log10()+
      theme_minimal_vgrid(font_size = 10)+
      scale_fill_viridis_d(na.value = "grey50")+
      labs(x = "Species",
           fill = "TRI genotype",
           title = "Frequency of isolates by species and TRI genotype")+
      theme(axis.text.y = element_text(face = "italic"),
            panel.background = element_rect(color = "gray"),
            legend.position = "bottom")+
      coord_flip()  
    
    ggplotly(g_fgsc) 
    
    
  })
  
  output$plot2 = renderPlotly({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    g2 = data_leaflet %>% 
      group_by(Year, FGSC2) %>%  
      summarise(count_name_occurr = n()) %>% 
      ungroup() %>% 
      group_by(FGSC2) %>% 
      mutate(count = cumsum(count_name_occurr)) %>% 
      ggplot(aes(Year, count, color= FGSC2))+
      # geom_col(size = .1, color= "white", alpha =0.9)+
      geom_line(size = 1) +
      geom_point(size=2)+
      # scale_y_log10()+
      theme_minimal_vgrid(font_size = 10)+
      scale_color_colorblind()+
      labs(x = "Year",
           y = "Cumulative number of isolates",
           color = "Species",
           title = "Frequency of isolates over time")+
      guides(color=guide_legend(nrow=2, byrow=TRUE))+
      theme(axis.text.y = element_text(face = "italic"),
            panel.background = element_rect(color = "gray"),
            legend.text =  element_text(face = "italic"),
            legend.position = "bottom")
    
    ggplotly(g2)
    
    
  })
  
  output$download <- downloadHandler(
    filename = function(){"FGSCdb_data.csv"}, 
    content = function(fname){
      
      if(input$search == 0){
        data_leaflet = fgsc_data
      }else{
        data_leaflet = filtered_article_type()
      }
      
      
      write.csv(data_leaflet, fname)
    })
  
  
  output$n_isolados = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    n =nrow(data_leaflet)
    if(n>1){sub = "Isolates"}else{sub = "Isolate"}
    infoBox(title = "Collection",
            value = n,
            subtitle = sub,
            color= "blue",
            icon = icon("list")
    )  
    
  })
  
  output$n_artigos = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    n = length(unique(data_leaflet$Pub1)[unique(data_leaflet$Pub1) != "Unpublished"])
    
    if(n>1){sub = "Articles"}else{sub = "Article"}
    infoBox(title = "Literature",
            value = n,
            subtitle = sub,
            color= "olive",
            icon = icon("newspaper")
    )
  })
  
  output$n_countries = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    n = length(unique(data_leaflet$Country))
    if(n>1){sub = "Countries"}else{sub = "Country"}
    infoBox(title = "Distribution",
            value = n,
            subtitle = sub,
            color= "navy",
            icon = icon("globe")
    )
  })
  output$n_species = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    n = length(unique(data_leaflet$FGSC)[unique(data_leaflet$FGSC) != "Undetermined"])
    if(n>1){sub = "Species"}else{sub = "Species"}
    infoBox(title = "Diversity",
            value = n,
            subtitle = sub,
            color= "red",
            icon = icon("chart-pie")
    )
  })
  output$n_hosts = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = fgsc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    n = length(unique(data_leaflet$Host))
    if(n>1){sub = "Hosts"}else{sub = "Host"}
    infoBox(title = "Host range",
            value = n,
            subtitle = sub,
            color= "olive",
            icon = icon("leaf")
    )
  })
  
}

shinyApp(ui, server)