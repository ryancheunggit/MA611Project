# Author: Ryan Zhang
# Email: ryanzjlib@gmail.com

# load library
library(imager)
# change to the working directory
setwd("D:/Study/Bentley/MA611/Project")

# batch processing code
path = "images/"  # put images in the images folder under your working directory
files <- list.files(path) # get a list of filenames within the images folder

# filter the filenames, only keep those ends with .jpg, .jpeg or .png
filterFiles <- function(files){
    imageFiles <- vector()
    for (i in 1:length(files)){
        filename <- files[i]
        if (sum(grep('.jpg', filename), grep('.jpeg', filename), grep('.png', filename)) > 0){
            imageFiles <- c(imageFiles, filename)
        }
    }
    return(imageFiles)
}

files <- filterFiles(files)

# place holders
imageLabels <- vector()
imageMatrix <- list()

# batch process the images files
for (i in 1:length(files)){
    filename <- files[i]
    print(paste("processing file:", filename))
    # get the label, either "cat" or "dog"
    imageLabels <- c(imageLabels, substr(filename, 0,3))
    # load the image file
    img <- load.image(paste(path, filename, sep = ""))
    # if the image is not in grayscale, try convert it into gray scale
    try(img <- grayscale(img), silent = T)
    # resize (convert to 128 * 128) and renomalize (each cell value is within 0 to 255)
    img <- resize(img, size_x = 128, size_y = 128)
    img <- renorm(img)
    # get the vector representation ( need the t() wrapper, other wise the convertion is by column)
    img <- as.vector(t(as.matrix(img)))
    # append to list
    imageMatrix[[i]] <- img
}

# conver to matrix, each row is a image, each column is a pixel position in image
imageMatrix <- matrix(unlist(imageMatrix ), nrow = length(imageMatrix), byrow = TRUE)

# see the dimension of the dataset
dim(imageMatrix)

# get a demostration cat image
demoCat <-  load.image("D:/Study/Bentley/MA611/Project/images/cat2.png")
# plot(demoCat) # take a look at it if you wish

# save data to file, use the next line to save data on directory
save(imageLabels, imageMatrix, demoCat, file = "processedData.RData")



