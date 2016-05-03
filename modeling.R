# Author: Ryan Zhang
# Email: ryanzjlib@gmail.com

# load library
library(wavethresh)
library(imager) # require imageMagick to use CIMG utility
library(caret)
library(randomForest)
library(class)
# set working directory
setwd("D:/Study/Bentley/MA611/Project")
# load data
load("processedData.RData")

# ============= function definitions ====================

# helper function to convert a vector representation of image to matrix representation
convertToMatrix <- function(vector){
    return(matrix(vector, nrow = sqrt(length(vector)), byrow = T))
}

# helper function to convert a vector representation of image to cimg object
convertToImage <- function(vector){
    return(as.cimg(convertToMatrix(vector)))
}

# helper function to load an image file, convert it to grey scale, resize to 128 x 128 and renorm values
greyResize <- function(filepath){
    # load image
    img <- load.image(filepath)
    # try to convert image to grayscale
    try(img <- grayscale(img), silent = T)
    # resize (convert to 128 * 128) and renomalize (each cell value is within 0 to 255)
    img <- resize(img, size_x = 128, size_y = 128)
    img <- renorm(img)
    # conver to vector
    img <- as.vector(t(as.matrix(img)))
    return(img)
}

# wrapper function that extract wavelet decomposition coefficients for the first certain level (default set to 7)
extractWaveletDecompCoefs <- function(series, level = 8){
    return(accessD(wd(series), level, 0))
}

# wrapper function for generating predictions
makePrediction <- function(waveCoefs, model = rf.model, varnames = varnames){
    df <- rbind.data.frame(waveCoefs)
    names(df) <- varname
    pred <- as.character(predict(model, df))
    return(pred)
}

# helper function return the indices of rows whithin the dataset that are the 
# k most closest to the target new data in euclidean distance sense
findKNN <- function(newseries, dataset = coefficientsMatrix, k = 7){
    n <- nrow(dataset) + 1
    distances <- as.matrix(dist(rbind(dataset, newseries)))[n,-n]
    return(which(distances %in% sort(distances)[1:k]))
}

# ================== Exploratory data analysis ====================
# dimension of the dataset
dim(imageMatrix)

# 100 dogs and 100 cats
table(imageLabels)

# not run
# model <- glm(factor(imageLabels)~.imageMatrix, family = binomial)

# plot 9 images randomly
set.seed(0306)
par(mfrow = c(3,3))
for (i in sample(1:nrow(imageMatrix),9)){
    plot(convertToImage(imageMatrix[i,]), axes = F, ann = F)
}
par(mfrow = c(1,1))

## display the 'average' dog and cat
image <- apply(imageMatrix[imageLabels == "cat",], 2, mean)
plot(convertToImage(image), axes = F, ann = F)

image <- apply(imageMatrix[imageLabels == "dog",], 2, mean)
plot(convertToImage(image), axes = F, ann = F)

#=============== wavelet decompose
# uncomment the section below to see results, can be slow
# find out approprate level
# set.seed(0306)
# for (l in 1:10){
#    coefficientsMatrix <- t(apply(imageMatrix, 1, function(vector){
#        return(extractWaveletDecompCoefs(vector, l))
#    }))
#    rf <- train(coefficientsMatrix, imageLabels, method = "rf",
#                trControl = trainControl(method = "cv", number = 5),
#                tuneGrid = expand.grid(mtry = c(1,2,4,8,16,32,64,128)),
#                prox=T,
#    print(paste("level: ", l))
#    print(max(rf$results[,"Accuracy"]))
#}


# wavelet decompose on the matrix   
set.seed(0306)
coefficientsMatrix <- t(apply(imageMatrix, 1, function(vector){
    return(extractWaveletDecompCoefs(vector, 8))
}))

# ==================== Model building ==================================
set.seed(0306)
## use cross validation to pick mtry parameter for random forest model
rf <- train(coefficientsMatrix, imageLabels, method = "rf",
            trControl = trainControl(method = "cv", number = 5),
            tuneGrid = expand.grid(mtry = c(1,2,4,8,16,32,64,128)),
            prox=T,
            allowParallel=T)
rf

# fit the final random forest model
df <- cbind.data.frame(imageLabels, coefficientsMatrix)
df$imageLabels <- as.factor(df$imageLabels)
names(df) <- make.names(names(df))

set.seed(0306)
ntree <- 1000
rf.model <- randomForest(imageLabels~., data = df, mtry = as.numeric(rf$bestTune), ntree = ntree)
# plot the oob error rate estimates with respect to number of trees in the model
plot(1:ntree, rf.model$err.rate[,1], type = "l", col = "black", ylim = c(0,1))
points(1:ntree, rf.model$err.rate[,2], type = "l", col = "red")
points(1:ntree, rf.model$err.rate[,3], type = "l", col = "blue")
# find the decision boundary using the one standard error rule
boundary <- min(rf.model$err.rate[,1]) + sd(rf.model$err.rate[,1])
# find the ideal number of trees
which(rf.model$err.rate[,1] <= boundary)

# decide the number of trees
set.seed(0306)
rf.model <- randomForest(imageLabels~., data = df, mtry = as.numeric(rf$bestTune), ntree = 230)
rf.model

# ================ Make Prediction on Test data ======================
# get variable names
varname <- make.names(names(cbind.data.frame(imageLabels, coefficientsMatrix)))
varname <- varname[-1]
# test
plot(convertToImage(greyResize("testImage/unknown1.png")), axes = F, ann = F)
makePrediction(extractWaveletDecompCoefs(greyResize("testImage/unknown1.png")))
plot(convertToImage(greyResize("testImage/unknown2.png")), axes = F, ann = F)
makePrediction(extractWaveletDecompCoefs(greyResize("testImage/unknown2.png")))
# all correct


# ================= KNN  ==============================
# Find the best K for KNN method
set.seed(0306)
knn.train <- train(imageLabels~., data = df, method = "knn",
      trControl = trainControl(method="cv",number = 5),
      tuneGrid = expand.grid(k = 1:12))
knn.train
# the best k is picked
indices <- findKNN(newseries = extractWaveletDecompCoefs(greyResize("testImage/unknown1.png")),
        dataset = coefficientsMatrix,
        k = as.numeric(knn.train$bestTune))

pred <-knn(train = coefficientsMatrix, test = coefficientsMatrix, cl = imageLabels, k = knn.train$bestTune)
ctb <- table(pred, imageLabels)
sum(diag(ctb))/sum(ctb)

# plot the k most similar images
par(mfrow = c(1,1))

for (i in 1:length(indices)){
    plot(convertToImage(imageMatrix[indices[i],]), axes = F, ann = F)
}

# save objects needed for the shiny app
save(imageMatrix,               # training data
     coefficientsMatrix,        # training features
     imageLabels,               # training labels
     rf.model,                  # fitted model
     convertToImage,            # function convert series to cimg object
     convertToMatrix,           # function convert series to matrix
     greyResize,                # load image, transform to gray scale, resize and renorn, return series
     extractWaveletDecompCoefs, # get first certain level wavelet decomposition coefficients
     varname,                   # variable names of the dataframe when making prediction 
     makePrediction,            # generate predicted label for a given image
     findKNN,                   # find the knn for a given wave coefs representation of image
     demoCat,                   # a cimg demo cat image
     file = "shinyapp/data.Rdata")

