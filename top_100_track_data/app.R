# Load all necessary libraries

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gt)
library(forcats)
library(plotly)
library(ggthemes)
library(wordcloud)

# Read the rds file containing the data analyses conducted

df <- read_rds("top100_data.rds")

# Define UI for application. I applied the shinytheme "flatly" for a more
# minimalistic and sleek design.

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("What it takes to be a Billboard Hot 100 Song",
                           
                           tabPanel("Overview",
                                    
                                    h2("Objective"),
                                    
                                    h4("The goal of this project is to determine what qualities encompass a Hot 100 song. I wanted to study how different track characteristics -- like song key, danceability, or energy -- changed over the decades. Using these observations, we might be able to figure out how to create the next Billboard Top 100 song!"),
                                    
                                    h2("The Data"),
                                    
                                    h4("This project used the Billboard Hot 100 charts dating back from 1960 until 2016."),
                                    
                                    br(),
                                    
                                    h2("Here are some quick snapshots:"),
                                    
                                    h4("Most common words in the lyrics of Billboard Hot 100 songs"),
                                    
                                    plotOutput("cloud"),
                                    
                                    br(),
                                    
                                    h4("Artists with the most appearance on the Billboard Hot 100 since 1960"),
                                    
                                    DT::dataTableOutput("artists"),
                                    
                                    br()
                                    
                           ),
                           
                           tabPanel("Characteristics",
                                    tabsetPanel(
                                     
                                        tabPanel("Acousticness",
                                                 
                                                 h3("Song Acousticness Trend from 1960 to 2016"),
                                                 
                                                 h5("A rating of 1.0 indicates a higher probability that the track is acoustic."),
                                                 
                                                 br(),
                                                 
                                                 plotOutput("acoustic")
                                                 
                                                 ),
                                        
                                        tabPanel("Danceability",
                                                 
                                                 h3("Song Danceability Trend from 1960 to 2016"),
                                                 
                                                 h5("Danceability describes how suitable a song is for dancing. The calculation is based on a combination of elements like tempo, rhythm stability, beat strength, and overall regularity. A value of 1.0 indicates songs that are most danceable."),
                                                 
                                                 br(),
                                                 
                                                 plotOutput("dance")
                                                 
                                        ),
                                        
                                        tabPanel("Duration",
                                                 
                                                 h3("Average Song Duration from 1960 to 2016"),
                                                 
                                                 h5("Duration indicates how long the track is in milliseconds."),
                                                 
                                                 plotOutput("duration")
                                                 
                                                 ),
                                        
                                        tabPanel("Energy",
                                                 
                                                 h3("Song Energy Trend from 1960 to 2016"),
                                                 
                                                 h5("Energy represents a perceptual measure of intensity and activity. The calculation is based on a combination of elements like dynamic range, perceived loudness, timbre, onset rate, and general entropy. A value of 1.0 indicates songs that are most energetic and feel fast, loud, and noisy (e.g., death metal)."),
                                                 
                                                 plotOutput("energy")
                                                 
                                                 ),
                                        
                                        tabPanel("Instrumentalness",
                                                 
                                                 h3("Song Instrumentalness Trend from 1960 to 2016"),
                                                 
                                                 h5("Predicts whether a track contains no vocals. 'Ooh' and 'aah' sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly 'vocal'. The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0."),
                                                 
                                                 plotOutput("instrument")
                                                 
                                                 ),
                                        
                                        tabPanel("Liveness",
                                                 
                                                 h3("Song Liveness Trend from 1960 to 2016"),
                                                 
                                                 h5("Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
                                                 
                                                 plotOutput("live")
                                                 
                                                 ),
                                        
                                        tabPanel("Loudness",
                                                 
                                                 h3("Song Loudness Trend from 1960 to 2016"),
                                                 
                                                 h5("The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db."),
                                                 
                                                 plotOutput("loud")
                                                 
                                                 ),
                                        
                                        tabPanel("Speechiness",
                                                 
                                                 h3("Song Speechiness Trend from 1960 to 2016"),
                                                 
                                                 h5("Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks."),
                                                 
                                                 plotOutput("speech")
                                                 
                                                 ),
                                        
                                        tabPanel("Tempo",
                                                 
                                                 h3("Average Song Tempo from 1960 to 2016"),
                                                 
                                                 h5("The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."),
                                                 
                                                 plotOutput("tempo")
                                                 
                                                 ),
                                        
                                        tabPanel("Valence",
                                                 
                                                 h3("Song Valence Trend from 1960 to 2016"),
                                                 
                                                 h5("A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."),
                                                 
                                                 plotOutput("valence")
                                                 
                                                 )
                                    )
                           ),
                           
                           tabPanel("Song Key",
                                    
                                    tabsetPanel(
                                        
                                        tabPanel("By Song Key",
                                                 
                                                 h3("Yearly Trend in Song Key Popularity"),
                                                 
                                                 h5("This figure shows what proportion of the Top 100 list for a given year is the selected song key."),
                                                 
                                                 br(),
                                                 
                                                 sidebarPanel(
                                                     
                                                     selectInput("key", "Song Key:", unique(df$data$keys))
                                                     
                                                 ),
                                                 
                                                 mainPanel(
                                                     
                                                     plotOutput("pop_key")
                                                     
                                                 ),
                                                 
                                                 br(),
                                                 
                                                 h5("After looking at each individual song key, there appeared to be no strong trend indicating a shift in song key preference. The C and G are the most popular song keys on the list.")
                                                 
                                        ),
                                    
                                        tabPanel("Mode",
                                                 
                                                 h3("Major vs. Minor Key"),
                                                 
                                                 h5("Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived."),
                                                 
                                                 plotOutput("mode")
                                                 
                                        ),
                                        
                                        tabPanel("Overall",
                                                 
                                                 h3("How frequently did each chord appear on the list?"),
                                                 
                                                 plotOutput("chord_freq")
                                        )
                                    )
                           ),
                           
                           tabPanel("Predictor",
                                    
                                    sidebarPanel(
                                        
                                        h3("Traits"),
                                        
                                        sliderInput("input_danceability", "Danceability", min = 0, max = 1, value = 0),
                                        sliderInput("input_energy", "Energy", min = 0, max = 1, value = 0),
                                        sliderInput("input_loudness", "Loudness (dB)", min = -60, max = 0, value = 0),
                                        sliderInput("input_speechiness", "Speechiness", min = 0, max = 1, value = 0),
                                        sliderInput("input_acousticness", "Acousticness", min = 0, max = 1, value = 0),
                                        sliderInput("input_instrumentalness", "Instrumentalness", min = 0, max = 1, value = 0),
                                        sliderInput("input_liveness", "Liveness", min = 0, max = 1, value = 0),
                                        sliderInput("input_valence", "Valence", min = 0, max = 1, value = 0)
                                    ),
                                    
                                    mainPanel(
                                        
                                        h3("The predicted Billboard Hot 100 ranking for those characteristics is:"),
                                        
                                        h2(textOutput("predictor"))
                                    )
                           ),
                           
                           tabPanel("About",
                                    mainPanel(
                                        
                                        h3("The Data"),
                                        
                                        h5("This project seeks to illustrate the trends in song key popularity throughout the years."),
                                        h5("The data for this project is from the ", a("Billboard", href = "https://github.com/mikkelkrogsholm/billboard"), "package and the ", a("chorrrds", href = "https://github.com/r-music/chorrrds"), "package."), 
                                        h5("The Billboard package contains data on the top 100 charts from 1960 - 2016."),
                                        h5("Chorrrds is used to extract the music chords of different songs."),
                                        
                                        br(),
                                        
                                        h3("About Me"),
                                        
                                        h5("Hi, my name is Mariah Dimalaluan, and I am a senior at Harvard studying Biomedical Engineering. I have a new-found love for data science!"),
                                        h5("If you have any questions, you can reach me at mdimalaluan@college.harvard.edu.")
                                    )
                           )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$acoustic <- renderPlot(
        df$acoustic
    )
    
    output$artists <- DT::renderDataTable(
        df$popular_artists, 
        colnames = c("Artist", "Frequency")
    )
    
    output$chord_freq <- renderPlot(
        df$chord_freq
    )
    
    output$cloud <- renderPlot({
        
        tidy_lyrics <- df$data %>%
            select(title, artist_name, year, decade, lyrics) %>% 
            unnest_tokens("word", lyrics) %>% 
            anti_join(stop_words) %>% 
            filter(!(word %in% c("chorus", "verse", "hook")))
        
        words <- tidy_lyrics %>%
            #group_by(decade) %>% 
            count(word) %>%
            filter(n >= 150) %>% 
            #arrange(decade) %>% 
            arrange(-n)
        
        set.seed(10)
        wordcloud(words = words$word,
                  freq = words$n,
                  random.order = FALSE,
                  min.freq = 250,
                  rot.per = 0.35,
                  colors = brewer.pal(n = 12, name = "Paired"))
    })
    
    output$dance <- renderPlot(
        df$dance
    )
    
    output$duration <- renderPlot(
        df$duration
    )
    
    output$energy <- renderPlot(
        df$energy
    )
    
    output$instrument <- renderPlot(
        df$instrumental
    )
    
    # Filter by the user selected song key
    
    output$pop_key <- renderPlot({
        df$key_pop_decades %>%
            filter(keys == input$key) %>% 
            ggplot(aes(x = year, y = prop)) +
            geom_col(fill = '#E24E42') +
            labs(
                y = "Proportion",
                x = "Year"
            ) +
            scale_x_continuous(breaks = seq(1960, 2020, 10))
    })
    
    output$live <- renderPlot(
        df$live
    )
    
    output$loud <- renderPlot(
        df$loud
    )
    
    output$mode <- renderPlot(
        df$mode
    )
    
    output$speech <- renderPlot(
        df$speech
    )
    
    output$tempo <- renderPlot(
        df$tempo
    )
    
    output$valence <- renderPlot(
        df$valence
    )
    
    # This function finds the predicted chart rankings based on the model that I
    # calculated. I will let the user modify the different traits.
    
    sum <- reactive({
        
        tidy_model <- df$tidy_model
        
        round(tidy_model$`(Intercept)` + tidy_model$acousticness * input$input_acousticness + tidy_model$danceability * input$input_danceability + tidy_model$energy * input$input_energy + tidy_model$loudness * input$input_loudness + tidy_model$speechiness * input$input_speechiness + tidy_model$instrumentalness * input$input_instrumentalness + tidy_model$liveness * input$input_liveness + tidy_model$valence * input$input_valence)
    })
    
    output$predictor <- renderText({
        sum()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
