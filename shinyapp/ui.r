# Author: Ryan Zhang
# Email: ryanzjlib@gmail.com

# the ui.r defines the layout and appearance of the shinyapp

# install.packages("shiny")

# load libraries

library(shiny)

# the ui will contain a file upload, allow user to upload a image
# there will be a submit button, once clicked, image file will be processed and be classified
shinyUI(
    bootstrapPage(
        
        # Application Title
        titlePanel("Kitten or Puppy?"),
        
        # use sidebarLayout to devide the ui into two panels
        sidebarLayout(
            # Side bar, contain the upload and submit
            sidebarPanel(
                width = 3,
                # a file upload widget
                h2("Upload an Image File Here"),
                fileInput("newImage", label = "Accept .jpg and .png image files",
                          accept = c(".jpg", ".jpeg", ".png")),
                br(), # new line
                # the button triggers processing and classification
                actionButton("action", label = "Tell Me"),
                hr(style = "border-top: 1px solid #080808"), # line break
                h1("An Example Image: "), # hint text
                plotOutput("examplePlot", width = "256px", height = "312px")
                
            ), # end of sidebarPanel
            
            # main panel, will display images
            mainPanel(
                # first row, display the prediction
                fluidRow(
                    h2(textOutput("prediction"))
                ),
                # second row showing the original image, processed image, features
                fluidRow(
                       h3("The Uploaded Image"),
                       plotOutput("uploadedOriginal", width = "256px", height = "312px")
                
                ), 
                fluidRow(
                    h3("As far as I can recall, I have seem some creatures similar to your query"),
                    h2(textOutput("debug"))
                ),
                fluidRow(
                    column(
                        width = 4,
                        plotOutput("similar1", width = "256px", height = "312px")
                    ),
                    column(
                        width = 4,
                        plotOutput("similar2", width = "256px", height = "312px")
                    ),
                    column(
                        width = 4,
                        plotOutput("similar3", width = "256px", height = "312px")
                    )
                )

            ) # end of main Panel
        )# end of sidebarLayout
    )# end of bootstrapPage
)