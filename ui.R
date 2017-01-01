
library(shiny)


palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))



ui <- fluidPage(
    headerPanel('Coursera Capstone Project - Word Prediction App'),
    sidebarPanel(
        p("First, to get things started:"),
        
        actionButton("dictionartButton", "Load dictionary"),
        br(),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        textOutput('textLoadReady'),
        br(),
        br(),
        p("The basic goal for the Coursera Capstone Project is the building predictive model of English text.
          In the app you can enter a word or a sentence, and let the algorithm predict your next word. "),
        
        br(),

        p("Loading might take a while, so please be patient."), 
        tags$head(tags$style("#textLoadReady{color: black;
                                 font-size: 15px;
                             font-style: bold;
                             }"
                            )
        )



    ),
    
    mainPanel(
        
        p("Around the world, people are spending an increasing amount of 
          time on their mobile devices for email, social networking, banking
          and a whole range of other activities. But typing on mobile devices
          can be a serious pain. This app is built to make this easier, by prediction what you want to type for you!"),
        br(),
        p("Give it a try!"),
        br(),
        textInput("inputSentence",label = "Enter the first part of a sentence:", value = "", width ="100%"),
        actionButton("submitButton", "Predict next word"),
        br(),
        br(),
        p(
            strong("Predicted word:"),                 
            ##textOutput('text2'),
            textOutput('text1')),

        
        textOutput('text3'),
        tags$head(tags$style("#text2{color: black;
                                 font-size: 20px;
                             font-style: italic;
                             }"
                            )
        ),
        tags$head(tags$style("#text1{color: blue;
                                 font-size: 20px;
                             font-style: italic;
                             }"
                         )
        )

    )
)