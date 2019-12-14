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
library(tidytext)
library(ggwordcloud)

# Read the rds file containing the data analyses conducted

df <- read_rds("top100_data.rds")

# Define UI for application. I applied the shinytheme "flatly" 
# for a more minimalistic and sleek design.

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("Can you make it to next year's Billboard Hot 100 Chart?",
                           
                           tabPanel("Overview",
                                    
                                    h2("Objective"),
                                    
                                    h4("The goal of this project is to determine what qualities — both musical and non-musical characteristics — encompass a Hot 100 song. By studying how different track characteristics (e.g., song key, danceability, song energy, etc.) changed over the years, we can gain more insight on how people's song preferences changed overtime. Using these observations, we might be able to figure out how to create the next Billboard Top 100 song!"),
                                    
                                    h2("The Data"),
                                    
                                    h4("This project used the Billboard Hot 100 charts dating back from 1960 until 2016. In a separate data set, there is information and scores on different song characteristics. In another data sets, there is information on the song lyrics as well. "),
                                    
                                    br(),
                                    
                                    tags$video(id="video2", type = "video/mp4", src = "SampleVideo_1280x720_1mb.mp4", controls = "controls")
                                    
                                    
                           ),
                           
                           # These are just some of the song characteristics
                           # that are included in the data. These would help me
                           # figure out how people's music preferences changed
                           # overtime.
                           
                           tabPanel("Characteristics",
                                    tabsetPanel(
                                        
                                        tabPanel("Summary",
                                                 
                                                 h3(" Overall Trends"),
                                                 
                                                 br(),
                                                 
                                                 h4("Over time, songs tended to be less acoustic, filled with instrumentals, have the feeling of a live recording, or have low valence scores. On the other hand, more and more songs in the yearly Billboard Hot 100 charts tended to increase in danceability, duration, energy, explicitness, loudness, and speechiness. Overall, the average tempo per year has remained about the same."),
                                                 
                                                 h4("An interesting trend is the increase in popularity of danceable and energetic songs over time. This trend seems to be in some conflict with the valence trend, which suggests that more and more songs in the year's top charts, on average, sound more 'negative.'"),
                                                 br(),
                                                 
                                                 h2("Here are some quick snapshots of the data:"),
                                                 
                                                 h4("Most common words in the lyrics of Billboard Hot 100 songs"),
                                                 
                                                 h5("Unsurprisingly, the word 'love' is the most frequently used word in these songs' lyrics."),
                                                 
                                                 plotOutput("cloud"),
                                                 
                                                 br(),
                                                 
                                                 h4("Artists with the most appearance on the Billboard Hot 100 since 1960"),
                                                 
                                                 DT::dataTableOutput("artists"),
                                                 
                                                 br()
                                        ),      
                                        
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
                                        
                                        tabPanel("Explicit",
                                                 
                                                 h3("Explicit Songs from 1960 to 2016"),
                                                 
                                                 h5("A song's explicitness is based on its song lyrics."),
                                                 
                                                 plotOutput("explicit")
                                                 
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
                                                 
                                                 plotOutput("mode"),
                                                 
                                                 br(),
                                                 
                                                 h5("Overtime, more and more songs that make it to the Top 100 charts are written in the minor key.")
                                                 
                                        ),
                                        
                                        tabPanel("Overall",
                                                 
                                                 h3("How frequently did each chord appear on the list?"),
                                                 
                                                 plotOutput("chord_freq"),
                                                 
                                                 br(),
                                                 
                                                 h3("General Observations"),
                                                 
                                                 h5("In the chart above, the song keys C and G still were the most popular song keys over all. However, upon closer inspection, it seemed like those keys are not as popular in recent years. As a whole, it seemed like song keys have no major trends that are pointing to any music preference changes."),
                                                 
                                                 h5("Interestingly, the mode that a song key is in seems like there might be some preference changes overtime. In 1969, less than 15% of the songs on the chart were written in the minor key. In 2015, this number has increased to almost 40%. Song in minor keys tend to be perceived as more melancholy. The increase in popularity of songs in minor keys may indicate an increase in popularity of sad music.")
                                        )
                                    )
                           ),
                           
                           tabPanel("Predictor",
                                    
                                    sidebarPanel(
                                        
                                        h3("Traits"),
                                        
                                        h5("Instructions: Use the slider to input your song's characteristics to determine how well it might do on the Billboard Top 100 list!"),
                                        
                                        sliderInput("input_key", "Key", min = 0, max = 11, value = 0),
                                        h6("0 = C, 1 = C#/Db, 2 = D, 3 = D#/Eb, 4 = E, 5 = F, 6 = F#/Gb, 7 = G, 8 = G#/Ab, 9 = A, 10 = A#/Bb, 11 = B"),
                                        sliderInput("input_mode", "Mode (Major Key)", min = 0, max = 1, value = 0, step = 1),
                                        h6("A '1' indicates a major key."),
                                        sliderInput("input_danceability", "Danceability", min = 0, max = 1, value = 0),
                                        sliderInput("input_energy", "Energy", min = 0, max = 1, value = 0),
                                        sliderInput("input_loudness", "Loudness (dB)", min = -60, max = 0, value = 0),
                                        sliderInput("input_speechiness", "Speechiness", min = 0, max = 1, value = 0),
                                        sliderInput("input_acousticness", "Acousticness", min = 0, max = 1, value = 0),
                                        sliderInput("input_instrumentalness", "Instrumentalness", min = 0, max = 1, value = 0),
                                        sliderInput("input_liveness", "Liveness", min = 0, max = 1, value = 0),
                                        sliderInput("input_valence", "Valence", min = 0, max = 1, value = 0)
                                    ),
                                    
                                    sidebarPanel(
                                        
                                        h3("Your predicted ranking is:"),
                                        
                                        h1(textOutput("predictor"))
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
        df$wordcloud
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
    
    output$explicit <- renderPlot(
        df$explicit
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
        
        rank = round(tidy_model$`(Intercept)` + tidy_model$key * input$input_key + tidy_model$mode * input$input_mode + tidy_model$acousticness * input$input_acousticness + tidy_model$danceability * input$input_danceability + tidy_model$energy * input$input_energy + tidy_model$loudness * input$input_loudness + tidy_model$speechiness * input$input_speechiness + tidy_model$instrumentalness * input$input_instrumentalness + tidy_model$liveness * input$input_liveness + tidy_model$valence * input$input_valence)
        
        if (rank <= 0) {
            rank = 1
        }
        
        rank
    })
    
    output$predictor <- renderText({
        sum()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
