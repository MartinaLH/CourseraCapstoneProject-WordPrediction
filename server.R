server <- function(input, output, session) {



    loading2 <- eventReactive(input$dictionartButton,{
        library(tm)
        library(RWeka)
        library(stringr)
        library(dplyr)
        library(tidyr)
        ## To make sure the results are reproducible, a seed is set
        set.seed(12345)
        
        # Load the twitter dataset
        EN_twitter_file <- "en_US/en_US.twitter.txt"
        EN_twitter <-  readLines(EN_twitter_file)
        
        ##Remove punctuation 
        EN_twitter <- gsub("[[:punct:]]", "", EN_twitter) 
        ##Change all characters to lower case
        EN_twitter <- tolower(EN_twitter)
        
        ##Get a sample dataset
        EN_twitter_sample <- sample(EN_twitter, length(EN_twitter)*0.005, replace = FALSE, prob = NULL)
        
        ##Create the corpus
        corpus <- Corpus(VectorSource(EN_twitter_sample))
        
        ##Remove whitespaces
        corpus <- tm_map(corpus, stripWhitespace) 
        
        ##Converting corpus to data drame
        corpus <- Corpus(VectorSource(corpus))
        cleantext<-data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
        
        
        ##Create n-grams
        onegrams <- NGramTokenizer(cleantext, Weka_control(min = 1, max = 1))
        twograms <- NGramTokenizer(cleantext, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!"))
        trigrams <- NGramTokenizer(cleantext, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))
        quagrams <- NGramTokenizer(cleantext, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!"))
        
        ##Sort the n-grams by their frequency
        one <- data.frame(table(onegrams))
        two <- data.frame(table(twograms))
        tri <- data.frame(table(trigrams))
        qua <- data.frame(table(quagrams))
        onesorted <- one[order(one$Freq,decreasing = TRUE),]
        twosorted <- two[order(two$Freq,decreasing = TRUE),]
        trisorted <- tri[order(tri$Freq,decreasing = TRUE),]
        quasorted <- qua[order(qua$Freq,decreasing = TRUE),]
        
        ##Split the sorted n-grams in columns per word
        
        twosorted <- separate(twosorted, twograms, c("word1","word2"), " ")
        trisorted <- separate(trisorted, trigrams, c("word1","word2","word3"), " ")
        quasorted <- separate(quasorted, quagrams, c("word1","word2","word3","word4"), " ")
        
        
        paste("Dictionary is loaded")
    })
    
    prediction <- eventReactive(input$submitButton,{
        if(is.na(corpus) ){
            paste("Dictionary is not yet loaded" )
            
        } else {
                
         ## Load the necessary libraries
        library(tm)
        library(RWeka)
        library(stringr)
        library(dplyr)
        library(tidyr)
        
        ## Read input sentence 
        inputText <- input$inputSentence
        
        ##Remove punctuation 
        inputText <- gsub("[[:punct:]]", "", inputText)     
        ##Change all characters to lower case
        inputText <- tolower(inputText)
        
        ##Select the last 3 words
        word3 <- word(inputText,-1)
        word2 <- word(inputText,-2)
        word1 <- word(inputText,-3)

        
        
    
        predictedWord <- NA
        
        ##If three words are entered search for ngram with these three words
        if (!is.na(word1)) {
            ##search most frequent Ngram with 3 words
            index <- head(which(quasorted$word3==word3 & quasorted$word2==word2 & quasorted$word1==word1),1)
            if(length(index) > 0) {
                predictedWord <- as.character(quasorted$word4[index])
            }
            
        } 
        
        ##If 2 words are entered or thee word ngram does not exist,
        ##search for ngram with these two words
        if(is.na(predictedWord)) {
            if (!is.na(word2)){
                ##search most frequent Ngram with at least 2 words
                index <- head(which(trisorted$word2==word3 & trisorted$word1==word2),1)
                if(length(index) > 0) {
                    predictedWord <- as.character(trisorted$word3[index])
                }
                
            }
        } 

        ##If 1 word is entered or the 2 word ngram does not exist
        ##search for ngram with this 1 words
        if(is.na(predictedWord)) {
            if (!is.na(word3)){
                ##search most frequent Ngram with 1 word
                index <- head(which(twosorted$word1==word3),1)
                if(length(index) > 0) {
                    predictedWord <- as.character(twosorted$word2[index])
                }
            }
        } 
        
        ##If no ngrams can be found, choose the most frequent word     
        if(is.na(predictedWord)) {
            ##Take most frequent word
            predictedWord <- as.character(onesorted[1,1])
        }
    

        updateTextInput(session, "inputSentence", value=paste(input$inputSentence,predictedWord ))
        paste(predictedWord)
        }

    })
    
    selection1 <- eventReactive(input$submitButton,{        
        if(is.na(corpus) ){
        paste("Predicted word:")
        }
    })
    
    

    output$textLoadReady <- loading2
    output$text2 <- selection1
    output$text1 <- prediction
    

}