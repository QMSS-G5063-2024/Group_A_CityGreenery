# Load required packages
library(devtools)
library(knitr)
library(leaflet)
library(shiny)
library(raster)
library(RColorBrewer)
library(shinydashboard)
library(pkgnet)
library(praise)
library(rsconnect)  
library(patchwork)
library(ggridges)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(leaflet.extras)
library(tm)
library(topicmodels)
library(broom)
library(wordcloud)
library(plotly)
library(stringdist)
library(textclean)
library(ggthemes)
library(gplots)
library(shinycssloaders)

nyc_trees <- readRDS("Data/NewYork_Final_2022-06-18.rds")

park_data <- read.csv("Data/social-distancing-park-areas-1.csv")
# map_data
nyc_sf <- st_read("Data/CommunityDistricts/geo_export_d81daad1-2b49-44c3-81d4-72436a58def3.shp")
borough_boundaries <- st_read("Data/nybb_23d/nybb.shp")
colors_tools <- c(
  "healthy_green" = "#4CAF50",  
  "urban_gray"    = "#9E9E9E",  
  "water_blue"    = "#2196F3", 
  "soil_brown"    = "#795548",  
  "leafy_green"   = "#8BC34A"   
)
# identify borough for nyc_trees by zipcodes
classify_borough <- function(zipcode) {
  if (zipcode >= 10001 & zipcode <= 10282) {
    return("Manhattan")
  } else if (zipcode >= 10301 & zipcode <= 10314) {
    return("Staten Island")
  } else if (zipcode >= 10451 & zipcode <= 10475) {
    return("Bronx")
  } else if ((zipcode >= 11004 & zipcode <= 11109) | (zipcode >= 11351 & zipcode <= 11697)) {
    return("Queens")
  } else if (zipcode >= 11201 & zipcode <= 11256) {
    return("Brooklyn")
  } else {
    return(NA) # for zip codes that do not match any borough
  }
}

# Ensure the zipcode column is in the integer format 
nyc_trees$zipcode <- as.integer(sub(" .*", "", nyc_trees$zipcode)) 

# Apply the function to create a new 'borough_name' column
nyc_trees <- nyc_trees %>%
  mutate(borough_name = sapply(zipcode, classify_borough))

# nyc_trees year preprocessing
nyc_trees$year <- year(mdy(nyc_trees$most_recent_observation))

year_range <- range(nyc_trees$year, na.rm = TRUE)

# shape files preparation
# Convert the multipolygon column to an sf object
park_sf <- st_as_sf(park_data, wkt = "multipolygon", crs = 4326)
# Convert the tree data into an sf object
trees_sf <- st_as_sf(nyc_trees, coords = c("longitude_coordinate", "latitude_coordinate"), crs = 4326)

borough_boundaries <- st_transform(borough_boundaries, crs = 4326)
borough_boundaries <- st_make_valid(borough_boundaries)
trees_sf_select <- trees_sf %>% 
  sample_n(10000)
trees_sf <- trees_sf %>% 
  sample_n(10000)
custom_colors <- c(
  "healthy_green" = "#4CAF50", 
  "urban_gray"    = "#9E9E9E"   
)

# tree density plot preparation

#Count the number of trees in each borough
tree_counts <- trees_sf %>%
  rename(BoroName = borough_name) %>% 
  group_by(BoroName) %>%
  summarize(tree_count = n())

#Join the tree counts with the borough areas
tree_density <- tree_counts %>%
  left_join(borough_boundaries %>% st_set_geometry(NULL) %>% select(BoroName, Shape_Area), by = "BoroName") %>%
  mutate(density = tree_count / Shape_Area)

tree_density <- tree_density %>%
  st_cast("POINT") %>%
  group_by(BoroName) %>%
  summarize(density = mean(density)) %>%
  st_set_geometry(NULL)

borough_boundaries <- borough_boundaries %>%
  left_join(tree_density, by = "BoroName")


borough_boundaries$density_per_sq_km <- borough_boundaries$density * 1e7


# Density Vs density plot data preprocessing (ui_densityVsdensity)
park_counts <- park_data %>%
  group_by(park_borough) %>%
  summarize(park_count = n(), .groups = 'drop')

park_density <- borough_boundaries %>%
  st_set_geometry(NULL) %>% 
  left_join(park_counts, by = c("BoroName" = "park_borough")) %>%
  mutate(park_density = park_count / Shape_Area) %>%
  select(BoroName, park_density)

density_data <- left_join(tree_density, park_density, by = "BoroName")

density_data <- density_data %>% 
  filter(!is.na(BoroName))

nyc_sf <- st_transform(nyc_sf, crs = 4326)



#### Topic Dispersion #### 
# Function to read PDF files
read_pdf <- function(file_path) {
  text <- pdftools::pdf_text(file_path)
  return(text)
}

# Read and preprocess PDF files
pdf_files <- c("Data/Document4.pdf", 
               "Data/Document1.pdf", 
               "Data/Document2.pdf", 
               "Data/Document3.pdf", 
               "Data/Document5.pdf")

documents <- lapply(pdf_files, read_pdf)

preprocess_text <- function(text) {
  # Convert to lowercase
  text <- tolower(text)
  
  # NYC-Specific substitutions
  text <- gsub("new york", "newyork", text)
  text <- gsub("new york city", "nyc", text)
  text <- gsub("\\bnycs\\b", "nyc", text)
  
  # Tree-Specific substitution
  text <- gsub("\\btrees\\b", "tree", text)
  
  # Remove punctuation (excluding spaces)
  text <- gsub("[[:punct:]&&[^[:space:]]]+", "", text)
  
  # Remove numbers
  text <- gsub("\\b\\d+\\b", "", text)
  
  # Remove whitespace
  text <- stripWhitespace(text)
  
  text <- unlist(strsplit(text, "\\b\\s+\\b"))
  
  # Remove stopwords
  text <- removeWords(text, stopwords("english"))
  
  # Remove specific words and characters
  custom_stopwords <- c("canopy", "urban", "city", "york", "forest", "funding",
                        "can", "and\\n", "training", "programs", "•",
                        "forestry", "skills", "park", "management", "natural",
                        "park,", "forests", "practical", "not", "areas",
                        "parks", "forested", "where", "this", "from", "i", "can",
                        "s", "es", "mn", "bk", "bx", "al", "an", "fy", "et",
                        "t", "qn", "i", "py", "c", "may", "within", "e", "n", "m", "doi",
                        "p", "cr", "also", "will", "re")
  
  text <- removeWords(text, custom_stopwords)
  
  # Remove words containing "www"
  # text <- text[!grepl("www", text)]
  
  # Remove URLs and special characters
  text <- gsub("(http|https)://[a-zA-Z0-9\\./]+", "", text)
  text <- gsub("[^a-zA-Z]", "", text)
  
  text <- text[nchar(text) > 2]
  
  # Remove empty strings
  text <- text[text != ""]
  
  return(text)
}

documents <- lapply(documents, preprocess_text)

# Create a corpus
corpus <- Corpus(VectorSource(documents))

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Fit an LDA model
set.seed(123)
lda_model <- LDA(dtm, k = 10, control=list(seed=123))  

# Extract topic distribution
topic_dist <- posterior(lda_model)$topics
topic_dist_df <- data.frame(topic_dist)
topic_dist_df$document <- 1:nrow(topic_dist_df) 
melted_df <- reshape2::melt(topic_dist_df, id.vars = "document")


#### Air Quality ####
final_airquality <- read.csv("Data/Final_airquality.csv")

# Filter data for PM2.5
pm25 <- final_airquality %>% filter(Name == "Fine particles (PM 2.5)")
pm25 <- pm25[!is.na(pm25$Start_Date), ]
pm25$Start_Date <- mdy(pm25$Start_Date) 

pm25_filtered <- pm25 %>%
  filter(year(Start_Date) %in% c(2010, 2015, 2020)) %>%
  group_by(year = year(Start_Date), Borough) %>%
  summarize(mean_pm25 = mean(Data.Value))

num_colors <- 5

qual_palette <- brewer.pal(n = num_colors, name = "Set3")



ui_native_plot <- fluidPage(
  titlePanel("NYC Trees: Native vs Non-Native Status"),
  sidebarLayout(
    sidebarPanel(
      selectInput("borough_native", "Choose a Borough:", 
                  choices = unique(nyc_trees$borough_name)),
      uiOutput("tree_select"),
      sliderInput("year_slider_native", "Select Year:",  
                  min = year_range[1],
                  max = year_range[2],
                  value = c(year_range[1], year_range[2]),
                  step = 1)
    ),
    mainPanel(
      plotOutput("treePlot")%>% withSpinner()
    )
  )
)
ui_distribution <- fluidPage(
  titlePanel("Distribution Map of NYC Trees by Condition and Category"),
  sidebarLayout(
    sidebarPanel(
      selectInput("borough", "Choose a Borough:",
                  choices = c("All Boroughs", unique(trees_sf_select$borough_name))),
      selectInput("condition", "Select Tree Condition:",
                  choices = c("good", "fair"),
                  selected = "good"),
      uiOutput("common_name_ui"), # Dynamic UI for selecting common name
      sliderInput("year_slider", "Select Year:",
                  min = year_range[1],
                  max = year_range[2],
                  value = c(year_range[1], year_range[2]),
                  step = 1)
    ),
    mainPanel(
      leafletOutput("map")%>% withSpinner()
    )
  )
)
ui_leaflet <- fluidPage(
  titlePanel("NYC Tree Density Map"),
  leafletOutput("densityMap")  %>% withSpinner()
)

ui_parkDist <- fluidPage(
  plotOutput("parkDistPlot") %>% withSpinner() 
)


ui_densityVsdensity <- fluidPage(
  titlePanel("Relationship between Tree and Park Density by Borough"),
  mainPanel(
    plotOutput("densityPlot")%>% withSpinner()
  )
)




ui <- fluidPage(
  
  tags$head(
    # Include Google Fonts
    tags$link(href="https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&family=Oswald:wght@700&display=swap", rel="stylesheet"),
    tags$style(HTML("
              .navbar-default .navbar-nav > li > a {
        color: #FFFFFF !important;  /* White font color for tab labels */
      }
      
      .navbar-default .navbar-nav > .active > a {
        color: #000000 !important;  /* Black font color for active tab labels */
      }
        "))
  ),
  
  # Use a div to apply styles
  #titlePanel(div("Urban Green Space", class = "main-title")),
  
  navbarPage(
    title = tags$div("Urban Green Space", style = "font-size: 30px; color: #FFFFFF;"),
    
    # Home tab
    tabPanel("Home", 
             tabsetPanel(
               tabPanel("Overview",
                        fluidRow(column(
                          width=10,
                          offset=1,
                          h4('Background', style = "color: #00008B; font-size: 35px; font-weight: bold;"),
                          h5("Our team aimed to investigate the impacts of greenery setups in cities around the world. We wanted to know how different cities incorporate the design of green spaces into their urban planning philosophy. Green Spaces play a crucial role in various aspects of a city’s development by offering myriad benefits.",style="font-size:15px;"),
                          h4('Focus on New York City', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                          h5("Our group chose to focus on New York City mainly due to the city’s commitment to environmental sustainability and extraordinary urban planning initiatives.",style="font-size:15px;"),
                          h5("Documents like New York City's Urban Forest, PlaNYC / OneNYC, and New York City Parks Department’s Community Forestry Program all serve as the world’s leading sustainable efforts.",style="font-size:15px;"),
                          tags$ul(
                            tags$li(tags$a(href="https://www.nycgovparks.org/trees", "New York City's Urban Forest"),
                                    tags$ul(tags$li(h5("New York City's Urban Forest is a comprehensive report related to the health and management of the city’s urban forest, detailing tree counts, species diversity, and the benefits provided by the trees.",style="font-size:15px;")))),
                            tags$li(tags$a(href="https://www.nyc.gov/site/sustainability/onenyc/onenyc.page", "OneNYC"),
                                    tags$ul(tags$li(h5("OneNYC is an environmental sustainability plan for the city that outlines strategies for green space. It includes commitments to open space, such as the goal of having all New Yorkers live within a 10-minute walk of a park.",style="font-size:15px;")))),
                            tags$li(tags$a(href="https://dec.ny.gov/nature/forests-trees/urban-and-community-forestry#:~:text=The%20NYS%20Urban%20and%20Community,through%20competitive%20cost%2Dshare%20grants.", "Community Forestry Program"),
                                    tags$ul(tags$li(h5("New York City Parks Department’s Community Forestry Program is a state emergent action plan included in the CLCPA (Climate Leadership and Community Protection Act), ensuring NYC is standing proudly in the forefront of urban sustainability and green space advocacy.",style="font-size:15px;"))))
                          ),
                          h4("Unraveling NYC's Research Potential", style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                          h5("All of the above stresses the competency of NYC as a data analysis centroid for urban greenery study. Another essential aspect of the choice of the city is its data availability. There is a wealth of data available on NYC's green spaces, including tree databases, making it a preferable choice for research alike.",style="font-size:15px;"),
                          h5("Our team could easily reference official city government websites and reports published by the NYC Department of Parks and Recreation or other city agencies.",style="font-size:15px;"),
                          h5("Last, but not least, NYC has a rich history of urban planning that integrates green spaces, dating back to the creation of Central Park in the 19th century!",style="font-size:15px;"),
                          h5("In sum, a plethora of initiatives highlight the city's dedication to environmental sustainability and make NYC a valuable and interesting subject for study on urban green spaces and their impact.",style="font-size:15px;"),
                          br(),
                          tags$img(src = "https://www.nycgovparks.org/facilities/images/great-trees-header.jpg",style = "display: block; margin-left: auto; margin-right: auto; width: 50%;"), 
                          tags$hr(),
                          br(),
                          p("Image: Great Trees Header by NYC Parks. Retrieved from https://www.nycgovparks.org/facilities/images/great-trees-header.jpg",style = "display: block; margin-left: auto; margin-right: auto; width: 50%;")
                        )
                        )),
               tabPanel("About", 
                        fluidRow(column(
                          width=10,
                          offset=1,
                          h4('Team Members', style = "color: #00008B; font-size: 35px; font-weight: bold;"),
                          h5("We are a group of four students who are attending Columbia University, in NYC, and have a passion for Urban Trees.",style="font-size:15px;"),
                          tags$ul(
                            tags$li(tags$a(href="https://www.linkedin.com/in/jcxiong0809?trk=people_directory", "Junchen Xiong")),
                            tags$li(tags$a(href="https://www.linkedin.com/in/xuanying-shannon-mao-3459691b4", "Xuanying Mao")),
                            tags$li(tags$a(href="https://www.linkedin.com/in/jeonseo-lee", "Jeonseo Lee")), 
                            tags$li(tags$a(href="https://github.com/fsarshad", "Faaz Arshad")),
                            br()
                          ),
                          tags$img(src = "https://www.appily.com/sites/default/files/styles/max_1200/public/images/hero/college/190150_hero.jpg?itok=xJZlqHGh",style = "display: block; margin-left: auto; margin-right: auto; width: 50%;"),
                          br(),br()
                        ))
               )
             )
    ),
    
    # Sustainability Benefits tab
    tabPanel("Sustainability Benefits", 
             tabsetPanel(
               
               tabPanel("Botanical Diversity",
                        tags$style(HTML("
                       /* Specify the font family */
                       body {
                       font-family: 'Aptos', sans-serif; /* Change 'Arial' to the desired font name */
      }")),
                        fluidRow(column(
                          width=10,
                          offset=1,
                          h2("Plant Analysis Through NYC Trees", style = "color: #00008B; font-size: 35px; font-weight: bold;"),
                          br(),
                          h4('Dataset', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                          h5('A dataset of 5 million city trees from 63 US cities: species, location, nativity status, health, and more.',style="font-size: 15px;"),  #
                          tags$a(href="https://datadryad.org/stash/dataset/doi:10.5061/dryad.2jm63xsrf", "Dryad"),
                          h5('NYC Parks: Park Distribution Dataset',style="font-size: 15px;"),                                                                        #
                          tags$a(href="https://www.kaggle.com/datasets/thedevastator/nyc-parks-social-distancing-dataset", "NYC Parks"),
                          br(),
                          h4('Description',style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                          h5('As mentioned in the overview section, since we decided to focus our analysis focus on New York City, we would select the data file for New York City only. The NYC data file provides detailed information about individual trees in New York City. Each record in the dataset represents a single tree and includes a selected number of attributes: Tree ID, unique identifiers and precise geological location details (longitude, latitude), common and scientific names, physical characteristics like tree condition (e.g., good, fair), and size metrics like diameter at breast height, the borough in which the tree is located, dates of most recent observations for an individual tree.',style="font-size: 15px;"),
                          br(),
                          h4('Usage ', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                          h5('· Native vs. introduced tree species visualization: interactive Shiny apps, users can filter and visualize the distribution of native and non-native tree species within different boroughs. It contributed to assessing the impact of non-native species on local ecosystems and guiding potential conservation efforts.',style="font-size: 15px;"),
                          h5('· Borough-wise tree distribution: Analysis of tree counts and types, based on the common name of myriad categories of trees, distributed across different boroughs',style="font-size: 15px;"),
                          h5('· Density Maps: Using geographical data to create density maps of trees in relation to borough boundaries offers a visual representation of green space distribution, crucial for urban planning and environmental sustainability.',style="font-size: 15px;"),
                          br(),
                          tabsetPanel(
                            tabPanel("Plant Diversity Visualizations & Insights",
                                     fluidRow(
                                       column(width=12,
                                              ui_native_plot,
                                              h5('· Native vs. introduced tree species visualization:',style="font-size: 15px;font-weight: bold;"),
                                              h5('Additional filters are applied to the shiny application to map and analyze tree health (e.g., condition ratings like "good" or "fair"), providing insights into the environmental stress factors urban trees face and how these might be mitigated.',style="font-size: 15px;"),
                                              h5('The resulting graph shows a surprising discrepancy between the number of introduced tree species to the naturally occurring, native tree species. We observed that introduced trees outnumber their native counterparts in NYC.',style="font-size: 15px;"),
                                              tags$h5('From the staggering result, it is safe to conclude that non-native species are a dominant part of the urban tree population across all boroughs. Our team proposed that the phenomenon is not foreign to metropolitan centers like New York City. As a study from the United States Forest Service pointed out that "non-native species can be chosen for their ability to thrive in urban environments, where conditions are far from those of a tree\'s natural habitat."',
                                                      style="font-size: 15px;",tags$sup('1')),
                                              
                                              tags$h5("As the paper, “Municipal forest benefits and costs in five US cities,” mentioned The overwhelmingly outnumbered introduced plants can potentially inform urban green space planning, indicating a long-term need to increase biodiversity within New York City metropolitan areas",
                                                      style="font-size: 15px;",tags$sup('2')),
                                              
                                              tags$div(
                                                tags$p('References:'),
                                                tags$ol(
                                                  tags$li('Nowak, D. J., & Greenfield, E. J. (2012). Tree and impervious cover change in U.S. cities. Urban Forestry & Urban Greening, 11(1), 21-30.'),
                                                  tags$li('McPherson, E. G., Simpson, J. R., Peper, P. J., Maco, S. E., & Xiao, Q. (2005). Municipal forest benefits and costs in five US cities. Journal of Forestry, 103(8), 411-416.')
                                                ),
                                                style = "font-size: 14px;"
                                              ),
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput("borough", "Choose a Borough:",
                                                              choices = c("All Boroughs", unique(trees_sf_select$borough_name))),
                                                  selectInput("condition", "Select Tree Condition:",
                                                              choices = c("good", "fair"),
                                                              selected = "good"),
                                                  uiOutput("common_name_ui"),
                                                  sliderInput("year_slider", "Select Year:",
                                                              min = year_range[1],
                                                              max = year_range[2],
                                                              value = c(year_range[1], year_range[2]),
                                                              step = 1)
                                                ),
                                                mainPanel(
                                                  leafletOutput("map")
                                                )
                                              ),
                                              h5('· Borough-wise tree distribution',style="font-size: 15px;font-weight: bold;"),
                                              h5("With the dropdown menu to choose a borough, users can compare the concentration and spread of trees among different boroughs. For example, we could observe that trees are more densely populated in certain boroughs and we assumed that factors including urban planning, population density, or availability of parks and green spaces may influence the actual distribution of the trees.",style="font-size: 15px;"),
                                              h5("Overlaying this tree data with other urban datasets (e.g., park distribution, air quality data) could provide more comprehensive insights into the role trees play in the urban environment and how they interact with other factors. Our team would provide some follow-up data analysis using other datasets.",style="font-size: 15px;"),
                                              br()
                                              
                                       )
                                     )
                            ),
                            tabPanel("Density Visualizations",
                                     fluidRow(
                                       column(
                                         width=12,
                                         ui_leaflet,
                                         h5("· Density Maps",style="font-size: 15px;font-weight: bold;"),
                                         h5("Darker shades of green represent areas with higher tree density, while lighter shades represent lower tree density.",style="font-size: 15px;"),
                                         h5("From the visible sections of the map, we can see that certain areas, possibly including parts of Manhattan and the surrounding regions, have higher tree densities as indicated by the darker greens.",style="font-size: 15px;"),
                                         
                                         br()
                                       )
                                       
                                     )
                            ),
                            tabPanel("Park Visualizations",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(width=12,
                                                ui_parkDist
                                         )
                                       ),
                                       fluidRow(
                                         column(width=12,
                                                h5("· Park Distribution",style="font-size: 15px;font-weight: bold;"),
                                                h5("Using geographical data to generate density maps of trees in relation to borough boundaries, providing a visual representation of park distribution across New York City.",style="font-size: 15px;"),
                                                h5("Parks in New York City are distributed balanced across the different boroughs. This widespread distribution plays a critical role in ensuring that green spaces are accessible to residents throughout the city, enhancing urban living and ecological sustainability.",style="font-size: 15px;"),
                                         )
                                       ),
                                       fluidRow(
                                         column(width=12,
                                                ui_densityVsdensity
                                         )
                                       ),
                                       fluidRow(
                                         column(width=12,
                                                h5("· Park density vs. tree density",style="font-size: 15px;font-weight: bold;"),
                                                h5("The plot highlights the relationship between tree and park densities across various boroughs. Manhattan has the highest densities of both trees and parks, showing a close correlation between the two. In contrast, Staten Island exhibits the lowest densities, indicating a minimal relationship. Brooklyn, on the other hand, displays a significantly higher tree density compared to its park density, suggesting a weaker correlation between these factors. Both Queens and the Bronx show normal positive relationships between tree and park densities.",style="font-size: 15px;"),
                                                br()
                                         )
                                       )
                                     )
                                     
                            )
                          )
                        )
                        ))
               ,
               tabPanel("Air Quality Improvements",
                        tags$style(HTML("
                       /* Specify the font family */
                       body {
                       font-family: 'Aptos', sans-serif; /* Change 'Arial' to the desired font name */
      }")),
                        fluidRow(
                          column(
                            width=10,
                            offset=1,
                            h2("Air Quality Study", style = "color: #00008B; font-size: 35px; font-weight: bold;"),
                            br(),
                            fluidRow(
                              column(
                                width=12,
                                offset=0,
                                h4('Dataset', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                                h5('National Ambient Air Quality Standards (NAAQS):',style="font-size: 15px;"),
                                tags$a(href="https://data.cityofnewyork.us/Environment/Air-Quality/c3uy-2p5r/about_data", "NAAQS"),
                                br(),
                                h4('Description',style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                                h5('This dataset has been curated to focus specifically on air quality across New York City - with varying measure concentrations such as, Nitrogen Dioxide (NO2), Ozone (O3), Fine Particles (PM 2.5), Sulfur Dioxide (SO2), and etc. The original dataset includes various air pollutants, but we have subset it to exclusively focus on PM2.5 due to its significant health implications. Additionally, a new column categorizing each data point into one of the five boroughs of NYC—Manhattan, Brooklyn, Queens, The Bronx, and Staten Island—has been added to facilitate borough-specific air quality analysis.',style="font-size: 15px;"),
                                br(),
                                h4('Usage', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                                h5('· Correlation between PM2.5 exposure and health outcomes in different boroughs, contributing to targeted public health interventions.',style="font-size: 15px;"),
                                h5('· Insights on policy making to identify boroughs with critically high levels of PM2.5 and implement policies to reduce pollutant emissions in those areas',style="font-size: 15px;"),
                                br()
                              ),
                              
                              column(
                                width=12,
                                offset=0,
                                h4('Plot Interpretation 1 - Line Plot:', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                                h5('We observe a seasonal pattern across the years though it appears inconsistent. Whats crucial to note is the declining trend in PM2.5 levels across all five boroughs of NYC from 2010 to 2020, possibly influenced by the urban forestry strategy in NYC.',style="font-size: 15px;")
                              ),
                              
                              column(
                                width=12,
                                offset=0,
                                plotOutput("line_plot")
                              ),
                              
                              column(
                                width=12,
                                offset=0,
                                h4('Plot Interpretation 2 - Plotly Bar Chart:', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                                h5('Upon closer examination of each borough, Manhattan consistently exhibits the highest PM2.5 levels, which intuitively makes sense given its densely populated, metropolitan character. Following Manhattan, either the Bronx or Brooklyn typically show the next highest levels.',style="font-size: 15px;")
                              ),
                              
                              column(
                                width=12,
                                offset=0,
                                plotlyOutput("plotly_chart"),
                                br()
                              )
                              
                            )
                          )
                        )
               ))),
    
    tabPanel("Text Analysis", 
             tabsetPanel(
               tabPanel("Text Analysis",
                        tags$style(HTML("
                       /* Specify the font family */
                       body {
                       font-family: 'Aptos', sans-serif; /* Change 'Arial' to the desired font name */
      }"))
               ),
               
               
               fluidRow(
                 column(
                   width = 10, # Adjusted width to move everything to the left
                   offset=1,
                   h2("Text Analysis on NYC Urban Forestry Reports", style = "color: #00008B; font-size: 35px; font-weight: bold;"),
                   br(),
                   
                   
                   # Dataset/ Description/ Methodologies/ Usage/ Wordclouds
                   fluidRow(
                     column(
                       width = 6, # Occupy half of the available width
                       offset = 0, # Adjust as needed
                       h2("Dataset", style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                       tags$a(href = "https://forestforall.nyc/resources/", "Forest For All NYC"),
                       br(),
                       h4('Description', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                       h5('The dataset from Forest For All NYC includes pdf reports and fact sheets that address various aspects of urban forestry in New York City. The reports available cover topics such as urban forestry workforce training, the current state of the urban forest in NYC, funding for forested areas, the role of trees in the NYCHA, and a comprehensive forest management framework. They aim to provide a baseline of information, assess opportunities for improvement, and strategize the expansion and protection of the urban forest.',style="font-size: 15px;"),
                       br(),
                       h4('Methodologies', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                       h5('In our text analysis process, we initially preprocessed the data from the PDFs, customizing functions specific to our project needs such as standardizing words, tokenization, and removing non-essential elements including stop words and URLs, as well as unifying references to New York City (NYC, New York, New York City) under a single term. We adopted a four-pronged approach utilizing Latent Dirichlet Allocation (LDA) for topic modeling to capture the varied subjects within NYCs urban forestry sector:',style="font-size: 15px;"),
                       br(),
                       h5('We adopted 4 different approaches utilizing Latent Dirichlet Allocation (LDA) for topic modeling to capture the varied subjects within NYCs urban forestry sector.',style="font-size: 15px;"),
                       h5('   1) Visualizing the top ten topics across documents to determine the topic distribution',style="font-size: 15px;"),
                       h5('   2) Examining the ten most frequent words per topic to gauge central themes of each topic',style="font-size: 15px;"),
                       h5('   3) Generating a word cloud for a holistic view of prevalent terms across all documents',style="font-size: 15px;"),
                       h5('   4) Calculating Jaccard similarity scores to access document parallels, with self-comparison scored as one',style="font-size: 15px;"),
                       br(),
                       h4('Usage', style = "color: #00008B; font-size: 25px; font-weight: bold;"),
                       h5('Developing urban forestry educational programs by analyzing workforce training needs and topics through the insights gathered from topic modeling.',style="font-size: 15px;"),
                       h5('Informing policy and funding decisions by assessing the current state of urban forestry and identifying key themes and trends that emerge from the word cloud and similarity scores between reports.',style="font-size: 15px;")
                     ),
                     column(
                       width = 6, # Occupy half of the available width
                       offset = 0, # Adjust as needed
                       plotOutput("wordcloud", height = "700px", width = "100%") # Word cloud
                     ),
                     
                     
                     # Top 10 Words
                     column(
                       width = 12, # Adjusted width to move everything to the left
                       offset = 0,
                       h2("Top 10 Words", style = "color: #00008B; font-size: 25px; font-weight: bold;")
                     ),
                     
                     column(
                       width = 12,
                       offset = 0,
                       h5('Embarking on topic modeling, we have selected 10 topics across all documents. To gain a basic understanding of each topic, we examine the top 10 keywords per topic. Some topics feature distinct keywords that set them apart from others; for instance, topic 10 includes notable terms such as “framework,” topic 3 contains words like “workforce” and “job,” while topic 8 is characterized by terms like “funds',style="font-size: 15px;"),
                     ),
                     
                     column(
                       width =12,
                       offset = 0,
                       plotOutput("top_terms_plot", height = "500px", width = "100%"),
                     ),
                     
                     
                     # Topic Modeling
                     column(
                       width = 12, # Adjusted width to move everything to the left
                       offset = 0,
                       h2("Topic Dispersion across Documents", style = "color: #00008B; font-size: 25px; font-weight: bold;")
                     ),
                     
                     column(
                       width = 12,
                       offset = 0,
                       h5('Then we could delve more into how topic disperses across 5 different documents with a better understanding of the topics. We possess a collection of five documents all falling under the umbrella of NYC urban forestry, each addressing nuanced topics such as forest management frameworks, the urban forest workforce, funding, the current state of NYC forestry, and overarching goals. Delving into the prevailing themes of the top 10 topics, identified by their distinctive sets of key words, we observed that Document 5 prominently features Topics 2 and 3. These topics are characterized by keywords related to "Workforce," including terms such as workforce, participants, and conservancy.',style="font-size: 15px;"),
                       h5('It becomes evident that Document 3 is exclusively focused on Topic 10, which prominently features keywords related to “framework,” clearly indicating that document 3 revolves around this framework theme. Similarly, Document 4 exhibits a strong association with Topic 8, suggesting a focus on funding. This is evident from keywords such as budget, funds, expenses, and others found within the document.',style="font-size: 15px;")
                     ),
                     
                     
                     column(
                       width = 12, # Adjusted width to move everything to the left
                       offset = 0,
                       plotOutput("barplot", height = "500px", width = "100%"), # Adjusted width for the plot
                       plotlyOutput("histogram", height = "500px", width = "100%") # Adjusted width for the plot
                     ),
                     
                     
                     
                     # Jaccard 
                     column(
                       width = 12, # Adjusted width to move everything to the left
                       offset = 0,
                       h2("Jaccard similarity Matrix", style = "color: #00008B; font-size: 25px; font-weight: bold;")
                     ),
                     column(
                       width = 12,
                       offset = 0,
                       h5('The self-comparison in the documents yielded a score of 1, indicating perfect similarity. Beyond this, the highest similarity score was noted between documents 4 and 5, which represent the current state of urban forestry and workforce training, respectively.',style="font-size: 15px;")
                     ),
                     column(
                       width =12,
                       offset = 0,
                       imageOutput("jaccard_heatmap_image"),
                       br()
                       
                     )
                     
                   )
                 )
               ))),
    tags$style(type='text/css', ".navbar {background-color: #006400; }")),
  verbatimTextOutput("text_output")
)








server <- function(input, output, session) {
  
  output$tree_select <- renderUI({
    req(input$borough_native)  # Updated input ID
    all_species <- unique(nyc_trees %>%
                            filter(borough_name == input$borough_native) %>%
                            pull(common_name))
    selectInput("common", "Select Tree Species:",
                choices = c("All", all_species),
                selected = "All")
  })
  
  # Generate the plot based on inputs
  output$treePlot <- renderPlot({
    Sys.sleep(2)
    req(input$borough_native, input$year_slider_native, input$common)
    req(length(input$year_slider_native) == 2)
    if (input$common == "All") {
      filtered_data <- nyc_trees %>%
        filter(borough_name == input$borough_native, year >= input$year_slider_native[1], year <= input$year_slider_native[2])
    } else {
      filtered_data <- nyc_trees %>%
        filter(borough_name == input$borough_native, common_name == input$common, year >= input$year_slider_native[1], year <= input$year_slider_native[2])
    }
    
    # Count native and non-native trees
    count_data <- filtered_data %>%
      group_by(native) %>%
      summarise(count = n())
    custom_palette <- colorRampPalette(c("#0D6217", "#3E9A1A", "#BCD631", "#D2F0B0", "#C8E6C9"))(length(unique(count_data$native)))
    
    ggplot(count_data, aes(x = native, y = count, fill = native)) +
      geom_col() +
      scale_fill_manual(values = custom_palette) + 
      labs(title = paste("Distribution of Native and Non-native",
                         ifelse(input$common == "All", "Trees", input$common),
                         "in", input$borough_native, "from", input$year_slider_native[1], "to", input$year_slider_native[2]),
           subtitle = "By Native Status",
           x = NULL,
           y = "Number of Trees") +
      theme_minimal()+
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            legend.position = "none",
            axis.title.x = element_blank())
  })
  
  
  
  
  # ui_distribution
  output$common_name_ui <- renderUI({
    req(input$borough, input$condition)
    valid_names <- trees_sf_select %>%
      filter(if (input$borough != "All Boroughs") borough_name == input$borough else TRUE,
             condition == input$condition) %>%
      pull(common_name) %>%
      unique()
    selectInput("common_name", "Select Common Name:",
                choices = valid_names,
                selected = valid_names[1])
  })
  
  output$map <- renderLeaflet({
    Sys.sleep(2)
    req(input$borough, input$condition, input$common_name, input$year_slider)
    
    borough_data <- trees_sf_select %>%
      filter(if (input$borough != "All Boroughs") borough_name == input$borough else TRUE,
             condition == input$condition,
             common_name == input$common_name,
             year >= input$year_slider[1], year <= input$year_slider[2])
    
    
    selected_borough_boundary <- if (input$borough != "All Boroughs") {
      borough_boundaries %>%
        filter(BoroName == input$borough)
    } else {
      borough_boundaries
    }
    
    colors <- colorFactor(custom_colors, domain = trees_sf_select$native)
    
    map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = selected_borough_boundary, fillColor = "#ffffff00", color = "#000000", weight = 1) %>%
      addCircleMarkers(data = trees_sf_select, color = ~colors(native), fillOpacity = 0.8, radius = 8, clusterOptions = markerClusterOptions()) 
    if (input$borough != "All Boroughs") {
      map <- setView(map, lng = st_coordinates(st_centroid(st_union(selected_borough_boundary)))[1], lat = st_coordinates(st_centroid(st_union(selected_borough_boundary)))[2], zoom = 12)
    }
    
    map
  })
  
  # ui_leaflet
  output$densityMap <- renderLeaflet({
    
    
    colors <- c(
      "#E8F5E9", 
      "#C8E6C9", 
      "#A5D6A7", 
      "#81C784", 
      "#66BB6A",  
      "#4CAF50"   
    )
    
    # Recalculate breaks for the new palette length
    breaks <- seq(min(borough_boundaries$density_per_sq_km, na.rm = TRUE), 
                  max(borough_boundaries$density_per_sq_km, na.rm = TRUE), 
                  length.out = length(colors) + 1)
    
    # Create a color palette function
    pal <- colorBin(palette = colors, domain = borough_boundaries$density_per_sq_km, bins = breaks, na.color = "transparent")
    
    # Create the Leaflet map
    leaflet(borough_boundaries) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(density_per_sq_km),
                  fillOpacity = 0.7,
                  weight = 1,
                  smoothFactor = 0.5,
                  label = ~paste(BoroName, "Density: ", round(density_per_sq_km,3), " trees/sq km"),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~density_per_sq_km,
                title = "Tree Density (trees per sq km)",
                position = "bottomright",
                opacity = 1) %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  
  
  
  
  
  #ui_parkDist
  output$parkDistPlot <- renderPlot({
    ggplot() +
      geom_sf(data = nyc_sf, fill = "#dddddd", color = "#bbbbbb", size = 0.2) +
      geom_sf(data = park_sf, fill = "#008000", color = "#006400", size = 0.2) +
      labs(
        title = "Distribution of Parks in New York City",
        subtitle = "Parks are marked in green over the NYC area",
        caption = "Data source: NYC Open Data"
      ) +
      theme_minimal() +  # Clean theme
      theme(
        text = element_text(family = "Arial", color = "#333333"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      ) +
      coord_sf()  # Use coordinate function suitable for sf objects
  })
  
  
  
  # ui_densityPlot
  output$densityPlot <- renderPlot({
    Sys.sleep(2)
    borough_colors <- c(
      "Bronx"          = "#FFD54F",  
      "Brooklyn"       = "#FF8A65", 
      "Manhattan"      = "#388E3C",  
      "Queens"         = "#4FC3F7",  
      "Staten Island"  = "#FFB74D"   
    )
    
    
    
    # Plot the scatter plot showing the relationship between tree density and park density
    ggplot(density_data, aes(x = park_density, y = density, color = BoroName, size = park_density)) +
      geom_point() +
      scale_size_continuous(guide = FALSE) +
      theme_minimal() +
      labs(x = "Park Density (Parks per square unit area)", y = "Tree Density (Trees per square unit area)", 
           title = "Relationship between Tree Density and Park Density by Borough",
           color = "Borough") +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "right") +
      scale_color_manual(values = borough_colors, na.translate = FALSE)
  })
  
  
  
  
  
  # ui.AirQuality
  
  # Bar plot 
  # output$line_plot <- renderPlot({
  #   # Line graph code
  #   ggplot(pm25, aes(x = Start_Date, y = Data.Value, color = Borough)) +
  #     geom_line() +
  #     labs(title = "PM2.5 Values Across Time for All Boroughs",
  #          x = "Start Date",
  #          y = "Mean PM2.5 Value",
  #          color = "Borough") +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
  
  
  
  output$line_plot <- renderPlot({
    # Calculate overall mean PM2.5 value across all boroughs for each time point
    overall_mean_pm25 <- pm25 %>%
      group_by(Start_Date) %>%
      summarise(mean_pm25 = mean(Data.Value, na.rm = TRUE))
    
    # Line graph code with enhancements
    p <- ggplot(pm25, aes(x = Start_Date, y = Data.Value, color = Borough)) +
      geom_line(size = 1, alpha = 0.8) +
      geom_smooth(data = overall_mean_pm25, aes(x = Start_Date, y = mean_pm25, linetype = "Overall Trend"),  # Specify linetype for legend
                  method = "loess", se = FALSE, color = "black") +  # Specify color for legend
      labs(title = "PM2.5 Values Across Time for All Boroughs",
           x = "Start Date",
           y = "Mean PM2.5 Value",
           color = "") +  # Remove legend title for color aesthetic
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1, size = 12),  # Adjust x-axis label size and boldness
            axis.text.y = element_text(size = 12),  # Adjust y-axis label size
            axis.title.x = element_text(size = 14, face = "bold"),  # Adjust x-axis title size and boldness
            axis.title.y = element_text(size = 14, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +  # Adjust y-axis title size and boldness
      scale_color_manual(values = c(
        "Bronx"          = "#FFD54F",  
        "Brooklyn"       = "#FF8A65", 
        "Manhattan"      = "#388E3C",  
        "Queens"         = "#4FC3F7",  
        "Staten Island"  = "#FFB74D"
      ), name = "Borough") +  # Add legend title for Boroughs
      scale_linetype_manual(values = c("Overall Trend" = "dashed"), name = "Overall Trend") +  # Add legend title for Overall Trend
      guides(color = guide_legend(override.aes = list(
        linetype = "dashed",
        color = c(
          "#FFD54F",  # Bronx
          "#FF8A65",  # Brooklyn
          "#388E3C",  # Manhattan
          "#4FC3F7",  # Queens
          "#FFB74D"   # Staten Island
        ),
        size = 1
      ), title.position = "top")) +  # Adjust legend for Overall Trend
      theme(legend.position = c(0.9, 0.6))  # Adjust legend position
    
    print(p)
  })
  
  
  
  
  
  
  
  
  borough_colors <- c(
    "Bronx"          = "#FFD54F",  
    "Brooklyn"       = "#FF8A65", 
    "Manhattan"      = "#388E3C",  
    "Queens"         = "#4FC3F7",  
    "Staten Island"  = "#FFB74D"
  )
  
  
  output$plotly_chart <- renderPlotly({
    # Plotly code
    plot_ly(data = pm25_filtered, x = ~factor(year), y = ~mean_pm25, type = "bar",
            color = ~Borough, colors = borough_colors,
            text = ~paste("Year: ", factor(year), "<br>",
                          "Borough: ", Borough, "<br>",
                          "Mean Value: ", round(mean_pm25, 2)),
            hoverinfo = "text") %>%
      layout(title = "Mean PM2.5 Values by Borough for Each Year",
             xaxis = list(title = "Year", showgrid = TRUE, gridcolor = "white"),
             yaxis = list(title = "Mean PM2.5 Value", showgrid = TRUE, gridcolor = "white"),
             legend = list(title = NULL),
             paper_bgcolor = "rgba(245,245,245,0.9)",  # Background color with transparency
             plot_bgcolor = "rgba(245,245,245,0.9)")  # Background color with transparency
  })
  
  
  
  
  
  
  # ui_Topic Dispersion
  
  # Successful Top 10 Bar plots 
  custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#ff1493")
  
  # Output for the bar plot
  output$barplot <- renderPlot({
    # Plot bar plot with 10 topics
    ggplot(melted_df, aes(x = factor(document), y = value, fill = factor(variable))) +
      geom_bar(stat = "identity") +
      labs(x = "Document", y = "Topic Probability", fill = "Topic") +
      scale_fill_manual(values = custom_palette) +  
      ggtitle("Topic Probability Across Documents") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.title.position = "plot")
  })
  
  # Output for the histogram using plotly
  output$histogram <- renderPlotly({
    # Plotly - 10 Topics Histogram
    plotly_colors <- custom_palette[1:10]
    plot_ly(data = melted_df, x = ~factor(document), y = ~value, color = ~factor(variable), colors = plotly_colors,
            type = "bar", hoverinfo = "text",
            text = ~paste("Document:", document, "<br>",
                          "Topic:", variable, "<br>",
                          "Probability:", value),
            barmode = "stack") %>%
      layout(title = list(text = "Topic Probability Across Documents", font = list(weight = "bold")),
             xaxis = list(title = "Document"),
             yaxis = list(title = "Topic Probability"),
             showlegend = TRUE,
             plot_bgcolor = "#F5F5F5",  # Grey background
             shapes = list(
               list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 1.5, x1 = 1.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 2.5, x1 = 2.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 3.5, x1 = 3.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 4.5, x1 = 4.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1))
             ),
             shapes = list(
               list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 1.5, x1 = 1.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 2.5, x1 = 2.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 3.5, x1 = 3.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1)),
               list(type = "line", x0 = 4.5, x1 = 4.5, y0 = 0, y1 = 1, 
                    line = list(color = "#BEBEBE", width = 1))
             ))
  })
  
  # ui_the top_terms_plot
  output$top_terms_plot <- renderPlot({
    set.seed(123)
    top_words <- lapply(topics(lda_model), function(topic) {
      top_terms <- terms(lda_model, 10)[topic, ]
      return(top_terms)
    })
    
    for (i in seq_along(top_words)) {
      cat(paste("Topic", i, ":", paste(top_words[[i]], collapse = ", "), "\n"))
    }
    
    beta <- tidytext::tidy(lda_model, matrix = "beta")
    
    get_top_terms <- function(beta, n = 10) {
      top_terms <- beta %>%
        mutate(term = gsub('"', '', term),
               term = gsub(',', '', term)) %>%  # Remove double quotes
        group_by(topic) %>%
        top_n(n, wt = beta) %>%
        arrange(topic, desc(beta))
      return(top_terms)
    }
    
    top_terms <- get_top_terms(beta)
    
    gg <- ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
      geom_bar(stat = "identity") +
      facet_wrap(~topic, scales = "free") +
      labs(x = "Word", y = "Probability", fill = "Topic", title = "Top 10 Words for Each Topic") +
      scale_fill_manual(values = custom_palette) +  # Add custom colors
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")) +
      coord_cartesian(ylim = c(0, max(top_terms$beta) * 1.2))  
    
    gg
  })
  
  # ui.Wordcloud
  
  output$wordcloud <- renderPlot({
    
    # Combine all documents into a single corpus
    corpus <- Corpus(VectorSource(documents))
    
    # Create a document-term matrix
    dtm <- DocumentTermMatrix(corpus)
    
    # Get the frequency of each term across all documents
    term_freq <- colSums(as.matrix(dtm))
    
    # Remove any empty strings and other specific strings you want to exclude
    term_freq <- term_freq[names(term_freq) != ""]
    term_freq <- term_freq[names(term_freq) != "\"\""]  # Remove double empty quotes if present
    
    # Sort terms by frequency and select the top 50
    top_terms <- head(sort(term_freq, decreasing = TRUE), 50)
    
    # Remove both double quotes and commas from the terms
    cleaned_terms <- gsub("[\",]", "", names(top_terms))
    
    custom_palette <- colorRampPalette(c("#8B4513", "#556B2F", "#228B22", "#32CD32", "#006400"))(50)
    
    wordcloud(words = cleaned_terms, freq = top_terms,
              min.freq = 1, max.words = 70, random.order = FALSE,
              colors = custom_palette, scale = c(9, 0.5))
  })
  
  # ui.heatmap
  output$jaccard_heatmap_image <- renderImage({
    # Assuming jaccard_heatmap.jpg is located in the www directory of your Shiny app
    list(src = "jaccard_heatmap.jpg",
         contentType = 'image/jpeg',  # Adjust if the image type is different
         width = '100%',
         height = 'auto',
         alt = "Jaccard Heatmap")
  })
  
  
}



shinyApp(ui = ui, server = server)
