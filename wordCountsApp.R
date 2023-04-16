library(shiny)

ui <- 
  tagList(
    # shinythemes::themeSelector(),
    
    fluidPage(
      theme = "cyborg" ,
      # Title of the app
      titlePanel("Wordcloud!"),
      
      sidebarLayout(
        sidebarPanel(
          
          strong("Settings:"),
          
          # Checkboxes for the wordcloud settings
          checkboxInput("checkbox3", label = "Document stemming", value = TRUE),
          checkboxInput("checkbox4", label = "Random Order", value = FALSE),
          checkboxInput("checkbox2", label = "Repeatable", value = TRUE),
          
          # Slider input for frequency change
          sliderInput("slider1", "Minimum Frequency:",
                      min = 1, max = 50, value = 5),
          
          # Slider input for rotation change
          sliderInput("slider3", "Rotation:",
                      min = 0.0, max = 1.0, value = 0.35),
          
          # Slider input for number of words change
          sliderInput("slider2", "Max words:",
                      min = 10, max = 1000, value = 100),
          
          # Text box to input the text
          textInput("text", "Text input:"),
          
          # Text file uploader
          fileInput("file", "Text file", accept=c("text/plain", ".txt"))),
        
        mainPanel(
          # Image download button
          downloadButton("wordcloud_img", "Download Image"),
          # CSV download button
          downloadButton("freq_csv", "Download Freq CSV"),
          # Wordcloud image rendered
          imageOutput("wordcloud"))
      )
    )

)



library(shiny)
library(wordcloud)
library(tm)
library(SnowballC)
# This above package is loaded by the tm by default but is not installed
# when the above packages were being installed
# so naturally shinyapp.io doesn't install this too.
# So, it is explicitly loaded here to be installed by the shinyapps server.

 
  server <- function(input, output) {
  datainput <- reactive({
    
    # Outputs a helpful message when neither text is entered nor
    # the text file uploaded.
    validate(
      need((input$text != "") || (!is.null(input$file)),
           "Please give me some text to work upon!"
      )
    )
    
    # If text input is not empty then get the corpus
    # else load text from the text file uploaded.
    # case of both text box and file uploader being empty was
    # covered by the above validate function.
    if (nchar(input$text) > 0){
      words <- Corpus(VectorSource(input$text))
    }
    else if (!is.null(input$file)){
      a <- input$file$datapath
      a <- substr(a, 1, nchar(a) - 1)
      words <- Corpus(DirSource(a))
    }
    
    # Cleaning & normalizing the corpus.
    words <- tm_map(words, stripWhitespace)
    words <- tm_map(words, content_transformer(tolower))
    words <- tm_map(words, removeWords, stopwords("SMART"))
    words <- tm_map(words, removeNumbers)
    words <- tm_map(words, removePunctuation)
  })
  
  # Reactive element to transform the data on the basis of
  # (de)selection of checkbox3 in ui.R
  finalinput <- reactive({
    if (input$checkbox3) datainput <- tm_map(datainput(), stemDocument)
    datainput()
  })
  
  # Reactive element to transform the data on the basis of
  # (de)selection of checkbox2 in ui.R
  asdas <- reactive({
    if (input$checkbox2) wordcloud_rep <- repeatable(wordcloud)
    else wordcloud_rep <- wordcloud
  })
  
  # Reactive element to generate the wordcloud and save it as a png
  # and return the filename.
  make_cloud <- reactive ({
    wordcloud_rep <- asdas()
    
    png("wordcloud.png", width=10, height=8, units="in", res=350)
    w <- wordcloud_rep(finalinput(),
                       scale=c(5, 0.5),
                       min.freq=input$slider1,
                       max.words=input$slider2,
                       random.order=input$checkbox4,
                       rot.per=input$slider3,
                       use.r.layout=FALSE,
                       colors=brewer.pal(8, "Dark2"))
    dev.off()
    
    filename <- "wordcloud.png"
  })
  
  # Download handler for the image.
  output$wordcloud_img <- downloadHandler(
    filename = "wordcloud.png",
    content = function(cloud) {
      file.copy(make_cloud(), cloud)
    })
  
  # Download handler for the csv.
  output$freq_csv <- downloadHandler(
    filename = "freq.csv",
    content = function(freq) {
      a <- DocumentTermMatrix(finalinput())
      b <- sort(colSums(as.matrix(a)), decreasing=TRUE)
      write.csv(b, freq)
    })
  
  # Sending the wordcloud image to be rendered.
  output$wordcloud <- renderImage({
    list(src=make_cloud(), alt="Image being generated!", height=600)
  },
  deleteFile = FALSE)
}


shinyApp(ui, server)
