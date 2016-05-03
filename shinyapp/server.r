# Author: Ryan Zhang
# Email: ryanzjlib@gmail.com

# the server.r defines backend functionality of the app

# install.packages("shiny")

# load libraries
library(shiny)
library(randomForest)
library(wavethresh)
library(imager)
library(jpeg)
library(png)
# load data, function and model

shinyServer(function(input, output) {
    load("data.Rdata")

    showText <- eventReactive(
        input$action, # the button that triggers the event handler
        {# the event handling code
            
            # some text making the app appears to be intelligent
            prompt = c("Something is telling me that it is a",
                       "Ah, it is a tricky one, but I think I got it. Ist must be a",
                       "It is a very cute",
                       "Based on our randomforest model, the image you provided is a",
                       "It gotta be a",
                       "Very cool features of this")
            
            if (!is.null(input$newImage)){
                # get the file
                inFile <- input$newImage
                # make prediction
                pred <- makePrediction(extractWaveletDecompCoefs(greyResize(inFile$datapath)))
                # cast predicted result into "Kitten" or "Puppy"
                pred <- ifelse(pred == "cat", "Kitten", "Puppy")
                # return the result string
                paste(sample(prompt,1), pred)
            }
            else{
                paste("You haven't upload an valid image yet....")
            }
        })
    
    output$prediction <- renderText(
        showText()
    )
    
    output$debug <- renderText(
        if (!is.null(input$newImage)){
            # file.copy(input$newImage$datapath, "/upload")
            # paste("Debug", input$newImage, list.files("/upload"))
    })
    
    output$examplePlot <- renderPlot({
        plot(demoCat, axes = F, ann = F)
    })
    
    output$uploadedOriginal <- renderPlot({
        if (!is.null(input$newImage)){
            img <- load.image(input$newImage$datapath)# the image is a cimg object
            plot(img, axes = F, ann = F)
        }
    })
    
    output$processedImage <- renderPlot({
        if (!is.null(input$newImage)){
            plot(convertToImage(greyResize(input$newImage$datapath)), axes = F, ann = F)
        }
    })
    
    output$similar1 <- renderPlot({
        if (!is.null(input$newImage)){
            imgVec <- greyResize(input$newImage$datapath)
            index <- findKNN(newseries = extractWaveletDecompCoefs(imgVec),
                               dataset = coefficientsMatrix,
                               k = 3)[1]
            plot(convertToImage(imageMatrix[index,]), axes = F, ann = F)
            }
    })
    
    output$similar2 <- renderPlot({
        if (!is.null(input$newImage)){
            imgVec <- greyResize(input$newImage$datapath)
            index <- findKNN(newseries = extractWaveletDecompCoefs(imgVec),
                             dataset = coefficientsMatrix,
                             k = 3)[2]
            plot(convertToImage(imageMatrix[index,]), axes = F, ann = F)
        }
    })
    
    output$similar3 <- renderPlot({
        if (!is.null(input$newImage)){
            imgVec <- greyResize(input$newImage$datapath)
            index <- findKNN(newseries = extractWaveletDecompCoefs(imgVec),
                             dataset = coefficientsMatrix,
                             k = 3)[3]
            plot(convertToImage(imageMatrix[index,]), axes = F, ann = F)
        }
    })
    
    
})