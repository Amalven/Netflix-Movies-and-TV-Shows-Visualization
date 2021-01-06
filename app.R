#loading the libraries
library(plotly)
library(ggplot2)
library(stats)
library(knitr)
library(graphics)
library(httr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maps)
library(forcats)
library(tidyr)
library(highcharter)
library(shiny)
library(leaflet)
library(shinydashboard)
library(packcircles)
library(viridis)
library(ggiraph)
library(shinyWidgets)
library(shiny)
library(shinythemes)
# ----------------------------------------------------------------------------------------------------------
## Pre-processing the data in this section ##

original_df <- read.csv("netflix_titles.csv",na.strings = c("NA", ""), stringsAsFactors=F)
df <- original_df[,c(2,6,7,9,11)]
df <-na.omit(df)
#
y <- strsplit(df$date_added, split = ", ")
df1<- data.frame(type = rep(df$type, sapply(y, length)), date = y %>%sapply( "[", 2 ), country = rep(df$country, sapply(y, length)), listed_in = rep(df$listed_in, sapply(y, length)))
head(df1)

df1$country <- as.character(df1$country)
#
k <- strsplit(df1$country, split = ", ")
df2 <- data.frame(listed_in = rep(df1$listed_in, sapply(k, length)), country = unlist(k),date = rep(df1$date, sapply(k, length)),type = rep(df1$type, sapply(k, length)))
head(df2)
df2$listed_in <- as.character(df2$listed_in)
#
l <- strsplit(df2$listed_in, split = ", ")

df3 <- data.frame(country = rep(df2$country, sapply(l, length)), listed_in = unlist(l),date = rep(df2$date, sapply(l, length)),type = rep(df2$type, sapply(l, length)))

df3$country <-gsub(",","",df3$country,fixed = TRUE)
# ---------------------------------------------------------------
## app.R ##

ui <-fluidPage(
  
  # use a gradient in background
  setBackgroundColor(
    color = "red",
    gradient = "linear",
    direction = "bottom"
  ), 
  dashboardPage(skin = "red",
                dashboardHeader(title = "Netflix"),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",startExpanded = FALSE,
                    menuItem("Netflix Timeline", tabName = "General", icon = icon("history"),startExpanded = FALSE),
                    menuItem("Netflix Popularity Map ", tabName = "map", icon = icon("map"),startExpanded = FALSE),
                    menuItem("Directors & Ratings", tabName = "Contents", icon = icon("list-alt"),startExpanded = FALSE)
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Contents",
                            fluidPage(
                              box( h1("Top Directors in Netflix",style="color:red"),
                                   h2("Have you seen the movies of these five directors in Netflix?"),
                                   sidebarLayout(
                                     sidebarPanel(h4("Bar chart showing top 5 directors in Neflix from all over the world"),
                                                  h4("Hover over the plot to see the name and total number of movies", style="color:brown"),
                                                  h4("Top directors are taken on the basis of directors having the most content in Netflix and sometimes two directors together work ")
                                                  
                                     ),
                                     mainPanel(
                                       highchartOutput("top_director",height = 350)
                                     ),
                                     position = "left"),
                                   h2("Often wondered what are the ratings given to the contents in netflix??"),
                                   sidebarLayout(
                                     sidebarPanel(h4("This tree map shows the different ratings given to the contents in Netflix"),
                                                  h4("Each Tv Shows and Movie on Netflix is assigned a maturity rating to help
                                  members to make informed choice for themselves and their children. Netflix determines maturity ratings
                                  by the frequency and impact of mature content in a Tv show or Movie"),
                                                  h4("It is seen that most of the contents are TV-MA which means it means the contents is for mature audience only")
                                                  
                                     ),
                                     mainPanel(
                                       highchartOutput("rating",height = 450)
                                     )
                                   ),width = 600
                                   
                              )
                              
                              
                            )
                            
                            
                    ),
                    
                    # second tab content
                    tabItem(tabName = "map",
                            
                            fluidPage(theme = shinytheme('flatly'),h1("Choropleth Map showing the Popularity of Netflix in different countries"),
                                      h5("Hover over the countries to see the name and total number of contents",style= "color:brown"),
                                      leafletOutput("map",height = 600,width = 1600),
                                      
                                      h4("If you want to Know more about contents in Netflix"),
                                      absolutePanel(id = "controls", class = "panel panel-default",
                                                    top = 290, left  = 450, width = 250, fixed=TRUE,
                                                    draggable = TRUE, height = "auto",style = "opacity: 0.72",
                                                    span(tags$i(h5("Want to explore the trend of Netflix in your country???")), style="color:red"),
                                                    h3(textOutput("Genre_count"), align = "right"),
                                                    h4(textOutput("Genre"), align = "right"),
                                                    h4(textOutput("movie"), align = "right"),
                                                    h4(textOutput("tvshow"), align = "right"),
                                                    selectInput("country_choice","Select the country", choices =c("all",unique(df3[,1]))),
                                                    plotOutput("plot1", height="130px", width="100%")
                                      ),actionButton("switch_tab2", "Please Click Here",align = "right",style= "color:black")
                                      
                                      
                                      
                                      # fluidPage(
                                      #   box(title = "Popularity of Netflix in Different Countries", leafletOutput("map",height = 700),width = 500,solidHeader = TRUE,status = "primary",collapsible = TRUE)
                                      
                            )
                            
                    ),
                    
                    # general tab
                    tabItem(
                      tabName = "General",
                      fluidPage(
                        box(column(12,align="center",uiOutput(outputId = "image")),width = 600,background = "black"),
                        box(h2("Why is Netflix called as 'Netflix' ?",style="color:red"), 
                            
                            h4("You may or may not have been able to work this out for yourself. But in 
          case you are still stumped, the name Netflix is a combination of 'Net'(common 
          parlace abbrevation of Internet and 'Flix'(a common abbrevation of 'flicks' which 
          in turn is slang for a movie or film."),
                            h4("It is facinating to know the fact that the biggest online movie streaming app was once website based movie rental service. 
             Let's see the trend of Netflix."),
                            
                            h2("Netlix Timeline",style="color:red"),
                            
                            h3("Did you know that Netflix had less than 20 Million Subscribers and less than 100 contents in 2013?"),
                            sidebarLayout(
                              sidebarPanel(h4("This density chart shows how Netflix content skyrocketed from 2014 to 2019."),
                                           h4("Under content, there are two main categories 'Movies' & 'Tv Shows'."),
                                           h4("You can hover over the line to see the amount of Tv Show and Movies at the end of a particular year.",style = "color:brown"),
                                           h4("Netflix had a very low number of Movies and Tv Shows in 2014 and after that the number of Movies added had soared significantly to around 8000 and Tv Shows to around 3500."),
                                           h4("Movies became more popular than Tv Shows from 2014 to 2019."),
                                           
                                           
                                           
                              ),
                              mainPanel(
                                plotlyOutput("density_chart",height = 400)
                              )
                            ),h2("Did you know much Tv Shows and Movies are there in Netflix ?"),
                            sidebarLayout(
                              sidebarPanel(h4("This Donut chart shows the percentage of Tv Shows and Movies in Netflix till 2019."),
                                           h4('Hover over the plot to see the labels and values', style= "color:brown"),
                                           h4("Most of the contents in Netflix are Movies.")
                                           
                              ),
                              mainPanel(
                                plotlyOutput("donut_chart", height = 350)
                              )
                            ),h2("Genre",style="color:red"),h2("Ever wondered what kind of Genres are famous in Netflix?"),
                            sidebarLayout(
                              sidebarPanel(
                                h4("Netflix has more than 76,000 genres of Movies and TvShows"),
                                h4("This Bubble chart shows different Genres where the size shows the count. International Movies and Dramas are the
                                          most famous genre category in Netflix.")
                                
                              ),
                              mainPanel(
                                plotOutput("words",height = 450)
                              )
                            ),h4("Now that we have covered the history of Netflix, to know more about how popular they have become on different countries"),
                            actionButton("switch_tab", "Please Click Here",align = "right",style= "color:black"),
                            width =500
                        )
                      )        
                    )
                    
                    )
                )
  )
)

##Server##
server <- function(input, output,session) {
  
  # for donut chart
  output$donut_chart <- renderPlotly({
    
    #donut chart in plotly
    donut_df <- group_by(df3, type)%>%summarise(count = n())
    head(donut_df)
    fig <- plot_ly(donut_df, labels = ~type, values = ~count)
    
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Movies vs TV shows",  showlegend = T,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  #plot for map
  output$map <- renderLeaflet({
    map_df <- group_by(df3, country)%>% summarise(count = n())
    map_df$country <- as.character(map_df$country)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_joined <- left_join(world, map_df, by = c('name' = 'country'))
    mybins <- c(0,10,20,40,60,80,100,200,300,500,1000,2000,3000,4000,Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=world_joined$count, na.color="transparent", bins=mybins)
    
    mytext <- paste(
      "Country: ", world_joined$name,"<br/>", 
      "Count: ", world_joined$count, "<br/>", sep="") %>% lapply(htmltools::HTML)
    leaflet(world_joined) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons(
        fillColor = ~mypalette(count), stroke=TRUE,
        color="white",
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=world_joined$count, opacity=0.9, title = "Popularity of Netflix", position = "bottomleft" )
    
  })
  
# plot for density chart  
  output$density_chart <- renderPlotly({
    movie <- df3 %>% group_by(date,type)%>%summarise(count = n()) %>% filter(date != "2020") %>% filter(type == "Movie")
    tv <- df3 %>% group_by(date,type)%>%summarise(count = n()) %>% filter(date != "2020") %>% filter(type != "Movie")
    
    fig <- plot_ly(x = ~movie$date, y = ~movie$count, type = 'scatter', mode = 'lines', name = 'Movies', fill = 'tozeroy')
    fig <- fig %>% add_trace(x = ~tv$date, y = ~tv$count, name = 'Tv shows', fill = 'tozeroy')
    fig <- fig %>% layout(xaxis = list(title = 'Year'),
                          yaxis = list(title = 'Number of Contents'))
    
    fig
  })
 #plot for bubble chart
  
  output$words <- renderPlot({
    genres <- df3 %>% group_by(listed_in) %>% summarise(count = n())
    # Generate the layout
    packing <- circleProgressiveLayout(genres$count, sizetype='area')
    packing$radius <- 0.95*packing$radius
    genres <- cbind(genres, packing)
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    ggplot() +
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
      scale_fill_viridis() +
      geom_text(data = genres, aes(x, y, size=count, label = listed_in), color="black") +
      theme_void() +
      theme(legend.position="none")+
      coord_equal()
    
  })
#plot for bar chart inside the map  
  output$plot1 <- renderPlot({
    if (input$country_choice == "all"){
      df3 %>% group_by(country,date) %>% summarise(count = n()) %>%  filter( date != 2020 &date != 2012&date != 2013&date != 2010&date != 2011&date != 2008&date != 2009) %>% ggplot()+ geom_bar(stat = "identity", aes(date,count),alpha=.6,width=.6,fill= "red") +theme_bw()
    } else{
      df3 %>% group_by(country,date) %>% summarise(count = n()) %>% filter(country == input$country_choice)%>%  filter( date != 2020 &date != 2012&date != 2013&date != 2010&date != 2011&date != 2008&date != 2009) %>% ggplot()+ geom_bar(stat = "identity", aes(date,count),alpha=.6,width=.6,fill= "red") +theme_bw()
    }
  })

  # for rendering text  
  output$Genre_count <- renderText({
    if (input$country_choice == "all"){
      paste0(prettyNum(nrow(df3), big.mark=",")," Contents")
    }else{
      c <-df3 %>% group_by(country) %>% summarise(count = n()) %>% filter(country == input$country_choice)
      paste0(prettyNum(c$count, big.mark=",")," Contents")
    }
    
  })
  # for rendering text
  output$Genre <- renderText({
    if (input$country_choice == "all"){
      genre_count<-df3 %>%  group_by(listed_in,date) 
      length <- length(unique(genre_count$listed_in))
      paste0(prettyNum(length, big.mark=",")," Genres")
    }else{
      genre_count<-df3 %>% filter(country == input$country_choice) %>% group_by(listed_in,date) 
      length <- length(unique(genre_count$listed_in))
      paste0(prettyNum(length, big.mark=",")," Genres")
    }
  })
  # for rendering text in absolute panel
  output$movie <- renderText({
    if (input$country_choice == "all"){
      movie_tv_count <- df3  %>% group_by(type)%>% summarise(count = n())
      paste0(prettyNum(movie_tv_count$count[1], big.mark=",")," Movies")
    }else{
      movie_tv_count <- df3 %>% filter(country == input$country_choice) %>% group_by(type)%>% summarise(count = n())
      paste0(prettyNum(movie_tv_count$count[1], big.mark=",")," Movies")
    }
  })
  # for rendering text in absolute panel
  output$tvshow <- renderText({
    if (input$country_choice == "all"){
      movie_tv_count <- df3  %>% group_by(type)%>% summarise(count = n())
      paste0(prettyNum(movie_tv_count$count[2], big.mark=",")," Tv Shows")
    }else{
      movie_tv_count <- df3 %>% filter(country == input$country_choice) %>% group_by(type)%>% summarise(count = n())
      paste0(prettyNum(movie_tv_count$count[2], big.mark=",")," Tv Shows")
    }
  })
  
  # for switch between tabs
  observeEvent(input$switch_tab, {
    updateTabItems(session, "tabs",selected = "map")
  })

  # for switch between tabs  
  observeEvent(input$switch_tab2, {
    updateTabItems(session, "tabs",selected = "Contents")
  })
  
  # to insert picture
  output$image <- renderUI({
    
    
    tags$div(img(src = "pic.png",height="50%", width="60%"))
    
  })
  
  # to plot bar chart using highchart for top directors
  output$top_director <- renderHighchart({
    directors <- group_by(original_df, director)%>%summarise(count = n())
    
    directors <- na.omit(directors)
    directors<-directors[order(-directors$count),][1:5,]
    director_df <- as.data.frame(directors)
    
    hc <- director_df %>% hchart('column', hcaes(x = director, y = count))%>% 
      
      hc_title(text = 'Top 5 Directors') %>% 
      
      hc_colors(c("red", "lightgoldenrodyellow"))
    hc
  })
  
  # plotting treemap for ratings
  output$rating <- renderHighchart({
    
    original_df %>% group_by(rating) %>% count() %>% hchart("treemap", hcaes(x = 'rating', value = 'n', color = 'n'))
  })  
  
}

shinyApp(ui, server)



