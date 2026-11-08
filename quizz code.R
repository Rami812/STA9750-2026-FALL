#' Acquire Netflix What We Watched Reports
#' 
#' This function will acquire and standardize all data for MP#01
#' from Netflix. Because Netflix hosts the data files behind a CDN, we
#' are accessing the release pages and automatically extracting links to the
#' relevant files (with CDN hash). 
#' 
#' After import, files are stacked vertically and some basic cleaning is
#' performed. 
load_netflix_data <- function(){
  # Note to students: Make sure you have installed the tidyverse and fs packages
  #                   before beginning this assignment. If you aren't sure,
  #                   simply run `library(tidyverse)` at your R Console and, if
  #                   you don't see an error message, `tidyverse` has been 
  #                   installed. (Same for `fs`) If you still need to install
  #                   either package, use the `install.packages` function as 
  #                   demonstrated in class.
  library(tidyverse)
  library(rvest)
  library(readxl)
  library(fs)
  
  data_dir <- path("data", "mp01")
  
  if(!dir_exists(data_dir)){
    dir_create(data_dir, showWarnings=FALSE, recurse=TRUE)
  }
  
  NETFLIX_URLS <- list(
    `2023H1` = "https://about.netflix.com/news/what-we-watched-a-netflix-engagement-report",
    `2023H2` = "https://about.netflix.com/news/what-we-watched-the-second-half-of-2023",
    `2024H1` = "https://about.netflix.com/news/what-we-watched-the-first-half-of-2024",
    `2024H2` = "https://about.netflix.com/news/what-we-watched-the-second-half-of-2024",
    `2025H1` = "https://about.netflix.com/news/what-we-watched-the-first-half-of-2025",
    `2025H2` = "https://about.netflix.com/news/what-we-watched-the-second-half-of-2025",
    `2026H1` = "https://about.netflix.com/news/what-we-watched-the-first-half-of-2026"
  )
  
  download_if_needed <- function(url, local){
    if(!file_exists(local)){
      download.file(url, local, quiet = TRUE, mode = "wb")
    }
    
    return(local)
  }
  
  NETFLIX_URLS |> 
    map(read_html) |> 
    map(html_element, xpath = "//a[contains(@href, '.xlsx')]") |> 
    map_chr(html_attr, "href") |> 
    enframe("period", "url") |> 
    mutate(path = path(data_dir, paste0(period, ".xlsx")), 
           path = map2(url, path, download_if_needed), 
           data = map(path, read_excel, skip=3, na="*"), 
           data = map(data, \(x) x |> mutate(`Release Date` = as.Date(`Release Date`)))) |>
    unnest(data) |> 
    select(-url, -path) |> 
    mutate(year=str_sub(period, 1, 4), 
           half=str_sub(period, start=6), 
           period_start = make_date(year, if_else(half==1, 1, 7), 1), 
           period_end = make_date(year, if_else(half==1, 6, 12), if_else(half==1, 30, 31))) |> 
    filter(!is.na(`Hours Viewed`)) |> 
    mutate(live_event = !is.na(Runtime) & str_detect(Runtime, "\\d+:\\d+\\*"), 
           clean_time = str_remove(Runtime, fixed("*")),
           Runtime = 60 * hour(hm(clean_time)) + minute(hm(clean_time))) |>
    select(-clean_time) |>
    mutate(Runtime = coalesce(Runtime, mean(Runtime, na.rm=TRUE)), 
           .by=Title, 
           Title = stringi::stri_trans_general(Title, "Latin-ASCII"),
           Title = str_remove(Title, " // .*"), 
           Title = str_remove(Title, "[^[:ascii:]].*"), 
           Title = str_trim(Title)) |>
    filter_out(Title == "") |>
    select(year, 
           period, 
           period_start, 
           period_end, 
           Title, 
           -half, 
           Runtime, 
           Views, 
           `Hours Viewed`, 
           `Release Date`,
           `Available Globally?`, 
           live_event)
}

NETFLIX_DATA <- load_netflix_data()

show(NETFLIX_DATA)

NETFLIX_DATA <- NETFLIX_DATA |>
  rename(runtime = Runtime, 
         views  = Views,
         hours_viewed='Hours Viewed',
         release_date='Release Date',
         available_globally='Available Globally?'
  )

show(NETFLIX_DATA)

NETFLIX_DATA|> mutate(
  year = as.integer(year), 
  available_globally = available_globally == "Yes" # Recall that double equals (==) checks for equality
)

glimpse(NETFLIX_DATA)

install.packages("gt")

if(!require("usethis")) install.packages("usethis")
usethis::create_github_token()

ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}
ensure_package(sf)

library(sf)
library(ggplot2)
system.file("shape/nc.shp", package = "sf") |>
  sf::st_read(quiet=TRUE) |>
  ggplot(aes(geometry=geometry, 
             fill=NAME)) + 
  geom_sf() + 
  guides(fill="none")

ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}
ensure_package(gifski)
ensure_package(gganimate)

library(tidyverse)
library(gganimate)

readr::read_csv("https://michael-weylandt.com/STA9750/labs/ssa_babynames.csv.gz") |>
  filter(name %in% c("Mary", "Ellen", "Robert", "Michael")) |>
  summarize(n = sum(number), .by=c("year", "name")) |> 
  mutate(name=factor(name, 
                     levels = c("Robert", "Michael", "Mary", "Ellen"))) |>
  ggplot(aes(x=year, y=n, color=name)) + 
  geom_point() +
  geom_line() + 
  theme_bw() + 
  theme(legend.position='bottom') +
  xlab("Year") + 
  ylab("Number of Registered Births") + 
  scale_color_brewer(type="qual", palette=2, name="First Name")  + 
  transition_reveal(year) 

ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}
ensure_package(shiny)
library(shiny)
shinyApp(
  ui = fluidPage(
    numericInput("n", "n", 10, min=3, max=30),
    plotOutput("plot")
  ),
  server = function(input, output) {
    output$plot <- renderPlot(plot(rnorm(input$n)) )
  }
)

nc <- system.file("shape/nc.shp", package = "sf") |>
  sf::st_read(quiet=TRUE)
nc

library(sf)
subway_lines <- st_read("https://data.ny.gov/resource/s692-irgq.geojson", 
                        quiet=TRUE)
subway_stations <- st_read("https://data.ny.gov/resource/39hk-dx4f.geojson", 
                           quiet=TRUE) |>
  select(-borough) # We're going to use borough as an example later so drop

# Install the package (only needed once)
install.packages("tidycensus")

# Load the package (run each new R session)
library(tidycensus)

library(tidycensus)
nyc_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001", 
  state = "NY",
  county = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
  geometry = TRUE,
  year = 2024) |>
  mutate(borough = case_when(
    str_detect(NAME, "Queens County") ~ "Queens", 
    str_detect(NAME, "Kings County") ~ "Brooklyn", 
    str_detect(NAME, "Bronx County") ~ "Bronx", 
    str_detect(NAME, "Richmond County") ~ "Staten Island", 
    str_detect(NAME, "New York County") ~ "Manhattan",
    .unmatched="error"
  )) |>
  rename(population = estimate) |>
  select(-GEOID, -variable, -moe) |>
  st_transform(crs=4326) # We'll talk about this in class