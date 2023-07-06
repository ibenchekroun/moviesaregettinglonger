library(shiny)
library(tidyverse)
library(pryr)
library(DT)
library(plotly)
library(fuzzyjoin)

data <- readRDS("movies_so_far_processed_jun2023.RDS") %>% mutate(Year = as.numeric(Year)) %>% filter(!is.na(Runtime_numeric))
bins <- seq(1900, 2025, by = 10)
data$Decade <- as.factor((cut(data$Year, bins, labels = FALSE)-1)*10+1900)
data$Year_factor <- as.factor(data$Year)
data$Year_numeric <- data$Year
data$Year <- data$Year_factor
genres <- unlist(unique(c(data$Genre1, data$Genre2, data$Genre3)))
writers <- unlist(unique(c(data$Writers))) %>% unique()
directors <- unlist(unique(c(data$Directors))) %>% unique()

data$Genre1 <- sapply(data$Genre1, paste, collapse=",")
data$Genre2 <- sapply(data$Genre2, paste, collapse=",")
data$Genre3 <- sapply(data$Genre3, paste, collapse=",")
data$Genres <- sapply(data$Genres, paste, collapse=",")

data$Writers <- sapply(data$Writers, paste, collapse=",")
data$Directors <- sapply(data$Directors, paste, collapse=",")

data <- data %>%
  group_by(Year) %>%
  mutate(Yearly_rank = rank(desc(IMDB_raters_count)))

production_companies <- c("Warner Bros", 
                          "Universal Pictures", 
                          "Columbia Pictures", 
                          "Paramount Pictures", 
                          "Twentieth Century Fox", 
                          "New Line Cinema", 
                          "Metro-Goldwyn-Mayer",
                          "Walt Disney Pictures",
                          "StudioCanal",
                          "Canal+",
                          "Miramax",
                          "Netflix",
                          "Dreamworks Pictures",
                          "TriStar Pictures",
                          "Lionsgate",
                          "Searchlight Pictures",
                          "Blumhouse Productions",
                          "Amazon Studios",
                          "A24",
                          "DC Entertainment",
                          "Marvel Studios",
                          "Castle Rock Entertainment",
                          "EuropaCorp",
                          "Dentsu",
                          "Fox 2000 Pictures",
                          "Imagine Entertainment",
                          "Hollywood Pictures",
                          "British Film Institute",
                          "Amblin Entertainment",
                          "Working Title Films",
                          "Focus Features",
                          "Film4",
                          "Dimension Films",
                          "Summit Entertainment",
                          "Golden Harvest Company",
                          "CJ Entertainment",
                          "Arte France Cinéma",
                          "Village Roadshow Pictures",
                          "Relativity Media",
                          "France 3 Cinéma",
                          "France 2 Cinéma",
                          "Gaumont",
                          "TF1 Films Production",
                          "New Regency Productions",
                          "Goldmines Telefilms",
                          "BBC Films",
                          "Touchstone Pictures",
                          "All") %>% sort()

plot_types <- c("Boxplot (shows 25th, 50th and 75th percentiles)", 
  "Scatter plot (shows all movies)",
  "Maximum plot (shows longest movie)",
  "Minimum plot (shows shortest movie)",
  "Mean plot (shows average movie length)",
  "Median plot (shows 50th percentile)")
# UI
ui <- navbarPage(

  title = "'Movies keep getting longer'",
  sidebarPanel(
    # Sidebar content that remains fixed
    # Add your input controls and other elements here
    # fluidRow(
    #   column(width = 6,
    #          numericInput("min_year", "From (YYYY)", min = 1900, max = 2023, value = 2000)
    #   ),
    #   column(width = 6,
    #          numericInput("max_year", "To (YYYY)", min = 1900, max = 2023, value = 2020)
    #   )
    # ),
    # numericInput("ratings_threshold", "Min. # of IMDb Ratings", min = 5000, value = 10000),
    # selectInput("plot_column", "Column to Plot", choices = c("Year", "Decade"), selected = "Year"),
    # fluidRow(
    #   column(width = 7,
    #          selectInput("the_genres", "Genre(s)", choices = c(genres, "All"), selected = "All", multiple = TRUE)),
    #   column(width = 3,
    #          radioButtons("filter_mode", "Filter Mode", choices = c("AND", "OR"), selected = "OR"))
    # ),
    # selectizeInput("the_production", "Production Company(ies)", choices = production_companies, selected = "All", multiple = TRUE)
    # 
  ),
  tabPanel("Plots",
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(
             tabPanel("Filter",
                      fluidRow(
                        column(width = 6,
                               numericInput("min_year", "From (YYYY)", min = 1900, max = 2023, value = 2000)
                        ),
                        column(width = 6,
                               numericInput("max_year", "To (YYYY)", min = 1900, max = 2023, value = 2020)
                        )
                      ),
                      numericInput("ratings_threshold", "Min. # of IMDb Ratings", min = 5000, value = 50000),
                      selectInput("type_of_plot", "Type of Plot", choices = plot_types, selected = plot_types[5]),
                      
                      )
             )),
           mainPanel(
             # Main panel content for plots tab
             plotOutput("main_plot_years"),

           ))
  ),
  tabPanel("by Genre",
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(
                 
                 tabPanel("Genres",
                          div("               ."),
                          fluidRow(
                            column(width = 6,
                                   selectInput("the_genres1", "Plot 1", choices = c(genres, "All"), selected = c("Comedy"), multiple = TRUE)),
                            column(width = 6,
                                   selectInput("the_genres2", "Plot 2", choices = c(genres, "All"), selected = c("Drama"), multiple = TRUE)),
                          ),
                          fluidRow(
                            column(width = 6,
                                   selectInput("the_genres3", "Plot 3", choices = c(genres, "All"), selected = "Action", multiple = TRUE)),
                            column(width = 6,
                                   selectInput("the_genres4", "Plot 4", choices = c(genres, "All"), selected = "Animation", multiple = TRUE)),
                          )),
                 tabPanel("General Filters",
                          fluidRow(
                            column(width = 6,
                                   numericInput("min_year_g", "From (YYYY)", min = 1900, max = 2023, value = 2000)
                            ),
                            column(width = 6,
                                   numericInput("max_year_g", "To (YYYY)", min = 1900, max = 2023, value = 2020)
                            )
                          ),
                          numericInput("ratings_threshold_g", "Min. # of IMDb Ratings", min = 5000, value = 50000),
                          
                          
                 ),
                 tabPanel("The Movies",
                          tags$style("
    .sentence {
      font-size: 18px;
      padding: 5px;
      display: inline-block;
    }
  "),

                          fluidRow(
                            column(width = 10,
                                   tags$span(
                                     "Table below is showing movies from ",
                                     class = "sentence"
                                   )
                            ),
                            column(width = 10,
                                   div(selectInput("plot_number_g", "", choices = c("Plot 1", "Plot 2", "Plot 3", "Plot 4")), style = "font-size: 85%; width: 100%")
                            )
                          ),
                          div(dataTableOutput("genre_table_1"), style = "font-size: 75%; width: 100%")
                 )),
               )               ,
           mainPanel(
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 # style = "font-size: 20px;",
                 selectInput("type_of_plot_g", "Type of Plot", choices = plot_types, selected = plot_types[6])
               )),             
             # Main panel content for plots tab
             fluidRow(
               column(width = 6,
                      plotlyOutput("genre_plot_1"),
                      plotlyOutput("genre_plot_3"),
               ),
               column(width = 6,
                      plotlyOutput("genre_plot_2"),
                      plotlyOutput("genre_plot_4"),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
               )
           )))),
  tabPanel("by Production Company",
           mainPanel(
            
           )
  ),
  tabPanel("Dataset",
           mainPanel(
             # Main panel content for dataset tab
             # dataTableOutput("filtered_table")
           )
  ),
  tabPanel("Linear Regression",
           mainPanel(
             # Main panel content for linear regression tab
             # plotOutput("linear_regression")
           )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    # Filter the dataframe based on the selected year range and count threshold
    filtered <- data[data$Year_numeric >= input$min_year & data$Year_numeric <= input$max_year, ]
    filtered <- filtered[filtered$IMDB_raters_count > input$ratings_threshold, ]
    if(!("All" %in% input$the_genres)){
      if(input$filter_mode == "AND"){
        filtered <- filtered %>% filter(Genres %>% str_detect(paste0("(?=.*", paste(input$the_genres, collapse = ")(?=.*"), ")")))
      }else{
        filtered <- filtered %>% filter(Genres %>% str_detect(paste(input$the_genres, collapse="|")))
      }
    }
    # if(!("All" %in% input$the_writers)){
    #   filtered <- filtered %>% filter(Writers %>% str_detect(paste(input$the_writers, collapse="|")))
    # }
    if(!("All" %in% input$the_directors)){
      filtered <- filtered %>% filter(Directors %>% str_detect(paste(input$the_directors, collapse="|")))
    }
    if(!("All" %in% input$the_production)){
      filtered <- filtered %>% filter(Production_companies %>% str_detect(paste(input$the_production, collapse="|")))
    }
    filtered
  })
  output$filtered_table <- renderDataTable({
    filtered_data() %>% 
      arrange(Year, IMDB_raters_count) %>% 
      select(Title, Year, Runtime_numeric) %>% 
      rename(`Runtime (mins)` = Runtime_numeric)
  })
  
  filtered_data2 <- reactive({
    # Filter the dataframe based on the selected year range and count threshold
    filtered <- data[data$Year_numeric >= input$min_year_g & data$Year_numeric <= input$max_year_g, ]
    filtered <- filtered[filtered$IMDB_raters_count > input$ratings_threshold_g, ]
    filtered
  })
  output$filtered_table <- renderDataTable({
    filtered_data() %>% 
      arrange(Year, IMDB_raters_count) %>% 
      select(Title, Year, Runtime_numeric) %>% 
      rename(`Runtime (mins)` = Runtime_numeric)
  })
  # Render box plot
  output$main_plot_years <- renderPlot({
    if(input$type_of_plot == "Boxplot (shows 25th, 50th and 75th percentiles)"){
      filtered_data() %>% 
        ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) + 
        geom_boxplot() + 
        labs(y = "Runtime (minutes)") + 
        ggtitle("Runtime over the years") + 
        guides(fill = FALSE) + #+
        scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))
      
    } else if(input$type_of_plot == "Scatter plot (shows all movies)"){
      filtered_data() %>% 
        ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) + 
        geom_point() + 
        labs(y = "Runtime (minutes)") + 
        ggtitle("Runtime over the years") + 
        guides(fill = FALSE) + #+
        scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))
    } else if(input$type_of_plot == "Mean plot (shows average movie length)"){
      filtered_data() %>% 
        group_by(Year_numeric) %>%
        summarise(mean_Runtime = mean(Runtime_numeric)) %>%
        ggplot(aes(x = Year_numeric, y = mean_Runtime)) + 
        geom_line() + 
        labs(y = "Runtime (minutes)", x = "Year") + 
        ggtitle("Average Runtime over the years") + 
        guides(fill = FALSE) + #+
        scale_x_continuous(breaks = seq(input$min_year, input$max_year, by = 5))
    }else if(input$type_of_plot == "Median plot (shows 50th percentile)"){
      filtered_data() %>% 
        group_by(Year_numeric) %>%
        summarise(median_Runtime =  median(Runtime_numeric)) %>%
        ggplot(aes(x = Year_numeric, y = median_Runtime)) + 
        geom_line() + 
        labs(y = "Runtime (minutes)", x = "Year") + 
        ggtitle("Median Runtime (50th percentile) over the years") + 
        guides(fill = FALSE) + #+
        scale_x_continuous(breaks = seq(input$min_year, input$max_year, by = 5))
    } else if(input$type_of_plot == "Maximum plot (shows longest movie)"){
      filtered_data() %>% 
        group_by(Year_numeric) %>%
        summarise(max_Runtime =  max(Runtime_numeric)) %>%
        ggplot(aes(x = Year_numeric, y = max_Runtime)) + 
        geom_line() + 
        labs(y = "Runtime (minutes)", x = "Year") + 
        ggtitle("Maximum Runtime over the years") + 
        guides(fill = FALSE) + #+
        scale_x_continuous(breaks = seq(input$min_year, input$max_year, by = 5))
    } else if (input$type_of_plot == "Minimum plot (shows shortest movie)"){
      filtered_data() %>% 
        group_by(Year_numeric) %>%
        summarise(min_Runtime =  min(Runtime_numeric)) %>% ungroup() %>%
        ggplot(aes(x = Year_numeric, y = min_Runtime)) + 
        geom_line() + 
        labs(y = "Runtime (minutes)", x = "Year") + 
        ggtitle("Minimum Runtime over the years") + 
        guides(fill = FALSE) + #+
        scale_x_continuous(breaks = seq(input$min_year, input$max_year, by = 5))
    }
   })
  filter_by_genres <- function(reactive_data, input_genres){
    filtered <- reactive_data()
    if(!("All" %in% input_genres)){
      filtered <- filtered %>% filter(Genres %>% str_detect(paste0("(?=.*", paste(input_genres, collapse = ")(?=.*"), ")")))
    }
    filtered
  }
  get_boxplot_years <- function(the_data, func){
    the_data %>% 
      ggplot(aes(x = Year, y = Runtime_numeric, fill = Year, label = Title)) +
      geom_boxplot() +
      labs(y = "Runtime (minutes)") +
      ggtitle("Runtime over the years") +
      guides(fill = FALSE) + #+
      scale_x_discrete(breaks = seq(input$min_year_g, input$max_year_g, by = 5))
  }
  get_scatterplot_years <- function(the_data, func){
    the_data %>% 
      ggplot(aes(x = Year, y = Runtime_numeric, label = Title)) +
      geom_point() +
      labs(y = "Runtime (minutes)") +
      ggtitle("Runtime over the years") +
      guides(fill = FALSE) + #+
      scale_x_discrete(breaks = seq(input$min_year_g, input$max_year_g, by = 5))
  }
  get_func_from_stri <- function(stri){
    if(stri == "Maximum plot (shows longest movie)"){
      func <- max
    }else if(stri == "Minimum plot (shows shortest movie)"){
      func <- min
    }else if(stri == "Mean plot (shows average movie length)"){
      func <-  mean
    }else if(stri == "Median plot (shows 50th percentile)"){
      func <- median
    } 
    func
  }
  get_string_from_function <- function(func){
    # func = "min"
    # func_as_stri <- as.character(substitute(func))
    # print(func_as_stri)
    if(identical(func, min)){
      stri <- "Minimum"
    }else if(identical(func, max)){
      stri <- "Maximum"
    }else if(identical(func, mean)){
      stri <- "Average"
    }else if(identical(func, median)){
      stri <- "Median (50th percentile)"
    }
    stri
  }
  get_aggplot_years <- function(the_data, func){
    title_string <- get_string_from_function(func)
    # print(title_string)
    agg_data <- the_data %>% 
      arrange(-IMDB_raters_count) %>% 
      group_by(Year_numeric) %>%
      summarise(Runtime_numeric =  round(func(Runtime_numeric)), `Total movies` = n()) %>% ungroup()
    agg_data <- agg_data %>%
      # fuzzy_left_join(
      left_join(
        the_data %>% select(Year_numeric, Runtime_numeric, Title),
                      # by = c("Year_numeric" = "Year_numeric", "Runtime_numeric" = "Runtime_numeric"),
                             # match_fun = function(x, y) abs(x - y)) %>%
      ) %>%
      group_by(Year_numeric)%>%
      summarise(agg_Runtime =  round(func(Runtime_numeric)), Titles = paste0(Title, collapse = ", \n"), `Total releases` = mean(`Total movies`)) %>% ungroup() %>%
      rename(Year = Year_numeric, Runtime = agg_Runtime)
    p <- agg_data %>%
      ggplot(aes(x = Year, y = Runtime, label = Titles, label2 = `Total releases`)) + 
      geom_line() + 
      geom_point(aes(size = `Total releases`)) + 
      labs(y = "Runtime (minutes)", x = "Year") + 
      ggtitle(paste0(title_string, " Runtime over the years")) +
      # lims(y = c(60, 400)) +
      # guides(fill = FALSE) + #+
      scale_x_continuous(breaks = seq(input$min_year_g, input$max_year_g, by = 5)) + 
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
    list(p, agg_data$Runtime)
    
  }

  get_plotfunc_from_type <- function(the_type_of_plot){
    if(the_type_of_plot ==  "Boxplot (shows 25th, 50th and 75th percentiles)"){
      plotfunc <- get_boxplot_years
    }else if(the_type_of_plot == "Scatter plot (shows all movies)"){
      plotfunc <- get_scatterplot_years
    }else{
      plotfunc <- get_aggplot_years
    }
    plotfunc
  }
  
  plotfunc_g <- reactive({
    get_plotfunc_from_type(input$type_of_plot_g)
  })
  
  min_max_runtime_g <- reactive({
    data_g1_r <- filter_by_genres(filtered_data2, input$the_genres1)$Runtime_numeric
    data_g2_r <- filter_by_genres(filtered_data2, input$the_genres2)$Runtime_numeric
    data_g3_r <- filter_by_genres(filtered_data2, input$the_genres3)$Runtime_numeric
    data_g4_r <- filter_by_genres(filtered_data2, input$the_genres4)$Runtime_numeric
    min_runtime <- min(data_g1_r, data_g2_r, data_g3_r, data_g4_r)
    max_runtime <- max(data_g1_r, data_g2_r, data_g3_r, data_g4_r)
    c(min_runtime, max_runtime)
  })
  
  min_max_g_2 <- reactive({
    l1 <- plotfunc_g()(filter_by_genres(filtered_data2, input$the_genres1), get_func_from_stri(input$type_of_plot_g))
    l2 <- plotfunc_g()(filter_by_genres(filtered_data2, input$the_genres2), get_func_from_stri(input$type_of_plot_g))
    l3 <- plotfunc_g()(filter_by_genres(filtered_data2, input$the_genres3), get_func_from_stri(input$type_of_plot_g))
    l4 <- plotfunc_g()(filter_by_genres(filtered_data2, input$the_genres4), get_func_from_stri(input$type_of_plot_g))
  
    p1 <- l1[[1]]
    p2 <- l2[[1]]
    p3 <- l3[[1]]
    p4 <- l4[[1]]
    
    r1 <- l1[[2]]
    r2 <- l2[[2]]
    r3 <- l3[[2]]
    r4 <- l4[[2]]
    
    min_g <- min(r1, r2, r3, r4)
    max_g <- max(r1, r2, r3, r4)
    list(list(p1, p2, p3, p4), c(min_g, max_g))
    
})
  
  output$genre_plot_1 <- renderPlotly({
    the_genres <- input$the_genres1
    p <- min_max_g_2()[[1]][[1]]
    r <- min_max_g_2()[[2]]
    gg <- ggplotly(p + ggtitle(paste0(the_genres, collapse = ", ")) + lims(y = r), tooltip = c("x", "y", "label", "label2", "custom_variable" = ~"Runtime")) 
    layout(gg, showlegend = FALSE)
  })
  


  genres_from_plot_number_g <- reactive({
    if(input$plot_number_g == "Plot 1"){
      the_genres <- input$the_genres1
    }else if(input$plot_number_g == "Plot 2"){
      the_genres <- input$the_genres2
    }else if(input$plot_number_g == "Plot 3"){
      the_genres <- input$the_genres3
    }else if(input$plot_number_g == "Plot 4"){
      the_genres <- input$the_genres4
    }
    the_genres
  })
  output$genre_table_1 <- renderDataTable({
    filtered_data <- filter_by_genres(filtered_data2, genres_from_plot_number_g()) %>%
      select(Title, Year, Runtime_numeric, Genres) %>%
      arrange(Year, Title, Runtime_numeric) %>% 
      rename(`Runtime (mins)` = Runtime_numeric)
    
    datatable(
      filtered_data,
      options = list(
        # dom = 't',
        searching = TRUE,
        paging = TRUE,
        pageLength = 5,
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = TRUE,
        buttons = c('csv', 'excel'),
        initComplete = JS(
          "function(settings, json) {",
          "  var api = this.api();",
          "  api.columns().every(function() {",
          "    var column = this;",
          "    var columnIdx = column.index();",
          "    var header = $(column.header());",
          "    var filterInput = $('<input type=\"text\" class=\"column-filter\">')",
          "      .appendTo($(column.footer()).empty())",
          "      .on('keyup change clear', function() {",
          "        column.search(this.value).draw();",
          "      });",
          "  });",
          "}"
        )
      
        # searchCols = rep(TRUE, ncol(filtered_data))
      ),
      filter = 'bottom',
      rownames = FALSE
    )
  })
  output$genre_plot_2 <- renderPlotly({
    the_genres <- input$the_genres2
    p <- min_max_g_2()[[1]][[2]]
    r <- min_max_g_2()[[2]]
    gg <- ggplotly(p + ggtitle(paste0(the_genres, collapse = ", ")) + lims(y = r), tooltip = c("x", "y", "label", "label2", "custom_variable" = ~"Runtime")) 
    layout(gg, showlegend = FALSE)
  })
  output$genre_plot_3 <- renderPlotly({
    the_genres <- input$the_genres3
    p <- min_max_g_2()[[1]][[3]]
    r <- min_max_g_2()[[2]]
    gg <- ggplotly(p + ggtitle(paste0(the_genres, collapse = ", ")) + lims(y = r), tooltip = c("x", "y", "label", "label2", "custom_variable" = ~"Runtime")) 
    layout(gg, showlegend = FALSE)
  })
  output$genre_plot_4 <- renderPlotly({
    the_genres <- input$the_genres4
    p <- min_max_g_2()[[1]][[4]]
    r <- min_max_g_2()[[2]]
    gg <- ggplotly(p + ggtitle(paste0(the_genres, collapse = ", ")) + lims(y = r), tooltip = c("x", "y", "label", "label2", "custom_variable" = ~"Runtime")) 
    layout(gg, showlegend = FALSE)
  })
  
  # output$genre_plot_2 <- renderPlot({filtered_data_genre2() %>%
  #     ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) +
  #     geom_boxplot() +
  #     labs(y = "Runtime (minutes)") +
  #     ggtitle("Runtime over the years") +
  #     guides(fill = FALSE) + #+
  #     scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))})
  # 
  # output$genre_plot_3 <- renderPlot({filtered_data_genre3() %>%
  #     ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) +
  #     geom_boxplot() +
  #     labs(y = "Runtime (minutes)") +
  #     ggtitle("Runtime over the years") +
  #     guides(fill = FALSE) + #+
  #     scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))})
  # 
  # output$genre_plot_4 <- renderPlot({filtered_data_genre4() %>%
  #     ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) +
  #     geom_boxplot() +
  #     labs(y = "Runtime (minutes)") +
  #     ggtitle("Runtime over the years") +
  #     guides(fill = FALSE) + #+
  #     scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))})

  # output$genre_plot_2 <- filtered_data_genre2() %>% 
  #   ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) + 
  #   geom_boxplot() + 
  #   labs(y = "Runtime (minutes)") + 
  #   ggtitle("Runtime over the years") + 
  #   guides(fill = FALSE) + #+
  #   scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))
  # 
  # output$genre_plot_3 <- filtered_data_genre3() %>% 
  #   ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) + 
  #   geom_boxplot() + 
  #   labs(y = "Runtime (minutes)") + 
  #   ggtitle("Runtime over the years") + 
  #   guides(fill = FALSE) + #+
  #   scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))
  # 
  # output$genre_plot_4 <- filtered_data_genre4() %>% 
  #   ggplot(aes(x = Year, y = Runtime_numeric, fill = Year)) + 
  #   geom_boxplot() + 
  #   labs(y = "Runtime (minutes)") + 
  #   ggtitle("Runtime over the years") + 
  #   guides(fill = FALSE) + #+
  #   scale_x_discrete(breaks = seq(input$min_year, input$max_year, by = 5))
  # 
  
  output$scatter_plot <- renderPlot({
    filtered_data() %>% 
      ggplot(aes_string(x = input$plot_column, y = "Runtime_numeric", fill = input$plot_column)) + 
      geom_point() + 
      guides(fill = FALSE) #+
    # scale_x_discrete(breaks = seq(input$year_range[1], input$year_range[2], by = 10)) 
  })
  
  # Render linear regression plot
  output$linear_regression <- renderPlot({
    # Generate linear regression plot code here
  })
}

# Run the app
shinyApp(ui, server)
