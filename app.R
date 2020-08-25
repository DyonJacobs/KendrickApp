#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = 'Kendrick Sentiment',

    # Application title
    # Sidebar with a slider input for number of bins 
    tabPanel('Specific Sentiment Albums',
    sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:inherit;",
            actionButton("update", "Change"),
            hr(),
            selectInput('album', 'Album Name', lyrics_nrc$album_name, selected = "Overly Dedicated"),
            selectInput('album1', 'Album Name', lyrics_nrc$album_name, selected = "Section.80"),
            selectInput('sentiment', 'Sentiment', c("anger","joy","positive","negative","sadness", "anticipation","fear","trust","disgust","surprise"), selected = "anger"),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("                       Word Count Selected Sentiment", align = "center"),
          splitLayout(cellWidths = c("800px", "800px"), plotOutput("plot",height = "600px"), plotOutput("plot1",height = "600px")),
          plotOutput("plot2", height = "600px", width = "800px"),
          h2("                      Percentage of Lyrics of Selected Sentiment", align = "center"),
          splitLayout(cellWidths = c("800px", "800px"), plotOutput("plot3",height = "600px"), plotOutput("plot4",height = "600px")),
          plotOutput('plot5', height = "600px", width = "800px"),
          plotOutput('plotcareersentiment', height = "600px")
          
        )
    )
    ),
    tabPanel('Unique Words Albums',
               # Show a plot of the generated distribution
               mainPanel(
                 fluidRow(
                   splitLayout(cellWidths = c("900px", "900px"), plotOutput("piratePlot",height = "600px"), plotOutput("piratePlot2",height = "600px"))
                 , width = 12)
               )
             
    ),
  tabPanel('Rap Speed',
           # Show a plot of the generated distribution
           mainPanel(
             fluidRow(
            plotOutput("rapSpeedPlot", height = "900px", width = "150%", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
            uiOutput("hover_info")
           )
           
  )),
    tabPanel('All sentiments',
             sidebarLayout(
               
               sidebarPanel(
                 selectInput("chosenAlbum1", "album 1", lyrics_nrc$album_name),
                 selectInput("chosenAlbum2", "album 2", lyrics_nrc$album_name),
                 width = 2
                 ),
               # Show a plot of the generated distribution
               mainPanel(
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("sentiment1"), plotOutput("sentiment2"))
                 )
               )
             )
    ),
    tabPanel('Spotify Stats',
             sidebarLayout(
               
               sidebarPanel(
                 style = "position:fixed;width:inherit;",
                 actionButton("update2", "Change"),
                 hr(),
                 selectInput("album1", "album 1", c("none",lyrics_nrc$album_name)),
                 selectInput("album2", "album 2", c("none",lyrics_nrc$album_name)),
                 selectInput("album3", "album 3", c("none",lyrics_nrc$album_name)),
                 selectInput("album4", "album 4", c("none",lyrics_nrc$album_name)),
                 width = 2
               ),
              mainPanel(
                fluidRow(
                 h2("Energy", align = "center"),
                 plotOutput("SpotifyPlot"),
                 h2("Danceability", align = "center"),
                 plotOutput("SpotifyPlot1"),
                 h2("Acousticness", align = "center"),
                 plotOutput("SpotifyPlot2"),
                 h2("Liveness", align = "center"),
                 plotOutput("SpotifyPlot3"),
                 h2("Tempo", align = "center"),
                 plotOutput("SpotifyPlot4")
                )
              )
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$rapSpeedPlot <- renderPlot({
    ggplot(WS) +
      geom_point(aes(x=duration_seconds, y = WPS, colour = album_name), shape = 20, size = 5) +
      labs( x = "Track Duration", y = "Words Per Second", title = "Rap Speed Scatter Plot",
            subtitle = "In Seconds", colour = "Album Name") +
      theme(axis.title.x = element_text(size = 15),
            title = element_text(size = 20, face = "bold"),
            axis.title.y = element_text(size = 15),
            legend.background = element_rect(colour = "grey", linetype = 3),
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 20, face = "bold")) 
  })
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(WS, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Album Name: </b>", point$album_name, "<br/>",
                    "<b> Track Name: </b>", point$track_name, "<br/>",
                    "<b> Words per Second: </b>", point$WPS, "<br/>")))
    )
  })
  
  
    cols <- reactive({
        input$update
        isolate({
            getColor(input$sentiment)
        })
        
    })
    terms <- reactive({
        input$update
        isolate({
            getSentimentSpecific(input$sentiment, input$album)
            })

        })
    terms1 <- reactive({
      input$update
      isolate({
        getSentimentSpecific(input$sentiment, input$album1)
      })
      
    })
    terms2 <- reactive({
      input$update
      isolate({
        getSentimentPercentage(input$sentiment, input$album)
      })
      
    })
    terms3 <- reactive({
      input$update
      isolate({
        getSentimentPercentage(input$sentiment, input$album1)
      })
      
    })
    terms4 <- reactive({
      input$update
      isolate({
        GetSentimentCareer(input$sentiment)
      })
      
     })    

    #Plot absolute values of sentiment occurence album 1
    output$plot <- renderPlot({
        v <- terms()
        color <- cols()

        barplot(v$count, 
                width = .2, 
                ylim = c(0,max(v$count)), 
                xlim = c(0,4), 
                xlab = "Track Number", 
                main = v$album_name[1], 
                names.arg = v$track_number , 
                col = color)
        
    })
    #Plot absolute values of sentiment occurence album 2
    output$plot1 <- renderPlot({
      v1 <- terms1()
      color <- cols()
      
      barplot(v1$count, 
              width = .2, 
              ylim = c(0,max(v1$count)), 
              xlim = c(0,4), 
              xlab = "Track Number", 
              main = v1$album_name[1], 
              names.arg = v1$track_number , 
              col = color)
      
    })
    #plot percentage of sentiment in album 1
    output$plot3 <- renderPlot({
      v3 <- terms2()
      color <- cols()
      
      barplot(v3$sentiment_percentage, 
              width = .2, 
              ylim = c(0,max(v3$sentiment_percentage)), 
              xlim = c(0,4), 
              xlab = "Track Number", 
              main = v3$album_name[1], 
              names.arg = v3$track_number , 
              col = color)
      
    })
    #plot percentage of sentiment in album 2
    output$plot4 <- renderPlot({
      v4 <- terms3()
      color <- cols()
      
      barplot(v4$sentiment_percentage, 
              width = .2, 
              ylim = c(0,max(v4$sentiment_percentage)), 
              xlim = c(0,4), 
              xlab = "Track Number", 
              main = v4$album_name[1], 
              names.arg = v4$track_number , 
              col = color)
      
    })    
    
    #Comparison occurence sentiment album 1 vs album 2
    output$plot2 <- renderPlot({
  v1 <- terms1()
  v <- terms()
  v2 <- rbind(v1,v)
  ggplot(v2, aes(fill = v2$album_name, y = v2$count, x = v2$track_number)) + 
    labs( x = "Track Number", y = " Occurence Sentiment", title = "Comparison Occurence Sentiment",
          subtitle = "In number of words") +
    geom_bar(position = "fill", stat = "identity", show.legend = NA) +
    theme(axis.title.x = element_text(size = 15),
          title = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 15),
          legend.background = element_rect(colour = "grey", linetype = 3),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20, face = "bold")) +
    labs(fill = "Album Name")

 
})
     
    #Comparison percentage  sentiment album 1 vs album 2
    output$plot5 <- renderPlot({
       v3 <- terms2()
       v4 <- terms3()
       v5 <- rbind(v3,v4)
       v5$track_number <- as.numeric(v5$track_number)
       ggplot(v5, aes(fill = v5$album_name, y = v5$sentiment_percentage, x = v5$track_number))+ 
         labs( x = "Track Number", y = "Relative Occurence Sentiment", title = "Comparison Relative Occurence Sentiment",
               subtitle = "Relative in Percentages") +
         geom_bar(position = "fill", stat = "identity", show.legend = NA) +
         theme(axis.title.x = element_text(size = 15),
               title = element_text(size = 20, face = "bold"),
               axis.title.y = element_text(size = 15),
               legend.background = element_rect(colour = "grey", linetype = 3),
               legend.text = element_text(size = 15), 
               legend.title = element_text(size = 20, face = "bold")) +
         labs(fill = "Album Name")
       
     })    
    
    #Selected Sentiment value average % over all albums 
    output$plotcareersentiment <- renderPlot({
       car <- terms4()
       color <- cols()
       
       ggplot(car) +
         geom_line(aes(x3, var1), group = 1, size = 1.5, colour = "orange") +
         ylim(min(car$var1) - 3 , max(car$var1)+5) +
         labs( x = "Album Name", y = "Percentage Lyrics of Sentiment", title = "Sentiment Values Over Career",
               subtitle = "%") +
         theme(axis.title.x = element_text(size = 15),
               title = element_text(size = 20, face = "bold"),
               axis.title.y = element_text(size = 15))
     })
    
    #Unique Words per album
    output$piratePlot <- renderPlot({
      pirateplot(formula =  word_count ~ x1, #Formula
                 data = word_summary, #Data frame
                 xlab = 'Album', ylab = "Distinct Word Count", #Axis labels
                 main = "Average Unique Words Per Album", #Plot title
                 pal = "info2", #Color scheme
                 point.o = .2, #Points
                 avg.line.o = 1, #Turn on the Average/Mean line
                 theme = 0, #Theme
                 point.pch = 16, #Point `pch` type
                 point.cex = 1.5, #Point size
                 jitter.val = .1, #Turn on jitter to see the songs better
                 cex.lab = .9, cex.names = 1)#Axis label size
    })
    
    output$piratePlot2 <- renderPlot({
      pirateplot(formula =  word_count ~ x2, #Formula
                 data = word_summary2, #Data frame
                 xlab = 'Album', ylab = "Distinct Word Count", #Axis labels
                 main = "Unique Words Per Album", #Plot title
                 pal = "info2", #Color scheme
                 point.o = .2, #points
                 bar.f.o = .6,
                 avg.line.o = 1, #Turn on the Average/Mean line
                 theme = 0, #Theme
                 point.pch = 16, #Point `pch` type
                 point.cex = 1.5, #Point size
                 jitter.val = .1, #Turn on jitter to see the songs better
                 cex.lab = .9, cex.names = 1) #Axis label size      
    })
    

    output$sentiment1 <- renderPlot({
      lyrics_nrc_album1 <- lyrics_nrc[ which(lyrics_nrc$album_name==input$chosenAlbum1), ]
      nrc_plot1 <- lyrics_nrc_album1 %>%
        group_by(sentiment) %>%
        summarise(word_count = n()) %>%
        ungroup() %>%
        mutate(sentiment = reorder(sentiment, word_count)) %>%
        #Use `fill = -word_count` to make the larger bars darker
        ggplot(aes(sentiment, word_count, fill = -word_count)) +
        geom_col() +
        guides(fill = FALSE) + #Turn off the legend
        theme_lyrics() +
        labs(x = NULL, y = "Word Count") +
        scale_y_continuous(limits = c(0, 700)) + #Hard code the axis limit
        ggtitle(input$chosenAlbum1) +
        coord_flip()
      nrc_plot1
    })
    output$sentiment2 <- renderPlot({
      lyrics_nrc_album2 <- lyrics_nrc[ which(lyrics_nrc$album_name==input$chosenAlbum2), ]
      nrc_plot2 <- lyrics_nrc_album2 %>%
        group_by(sentiment) %>%
        summarise(word_count = n()) %>%
        ungroup() %>%
        mutate(sentiment = reorder(sentiment, word_count)) %>%
        #Use `fill = -word_count` to make the larger bars darker
        ggplot(aes(sentiment, word_count, fill = -word_count)) +
        geom_col() +
        guides(fill = FALSE) + #Turn off the legend
        theme_lyrics() +
        labs(x = NULL, y = "Word Count") +
        scale_y_continuous(limits = c(0, 700)) + #Hard code the axis limit
        ggtitle(input$chosenAlbum2) +
        coord_flip()
      nrc_plot2
    })
    
    albums <- reactive({
      input$update2
      isolate({
        getSpotifyPlot2(input$album1, input$album2, input$album3, input$album4)
      })
    })
    albums1 <- reactive({
      input$update2
      isolate({
        getSpotifyPlot3(input$album1, input$album2, input$album3, input$album4)
      })
    })
    albums2 <- reactive({
      input$update2
      isolate({
        getSpotifyPlot4(input$album1, input$album2, input$album3, input$album4)
      })
    })
    albums3 <- reactive({
      input$update2
      isolate({
        getSpotifyPlot5(input$album1, input$album2, input$album3, input$album4)
      })
      
    })
    albums4 <- reactive({
      input$update2
      isolate({
        getSpotifyPlot6(input$album1, input$album2, input$album3, input$album4)
      })
      
    })
   
    output$SpotifyPlot <-  renderPlot({
    x <- albums()
    x
    })
    output$SpotifyPlot1 <- renderPlot({
      x1 <- albums1()
      x1
    })
    output$SpotifyPlot2 <- renderPlot({
      x2 <- albums2()
      x2
    })
    output$SpotifyPlot3 <- renderPlot({
      x3 <- albums3()
      x3
    })
    output$SpotifyPlot4 <- renderPlot({
      x4 <- albums4()
      x4
    })
    
    
    }
        
  
    
# Run the application 
shinyApp(ui = ui, server = server)
