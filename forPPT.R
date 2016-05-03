# Author: Ryan Zhang
# Email: ryanzjlib@gmail.com


library(imager)
library(forecast)
library(wavethresh)
library(tseries)

setwd("D:/Study/Bentley/MA611/Project/numbers")

number1 <- resize(load.image("number1.png"), size_x = 12, size_y = 12)
number3 <- resize(load.image("number3.png"), size_x = 12, size_y = 12)

convertToMatrix <- function(vector){
    return(matrix(vector, nrow = sqrt(length(vector)), byrow = T))
}

matrix1 <- convertToMatrix(number1)
matrix3 <- convertToMatrix(number3)

vector1 <- as.vector(t(matrix1))
vector3 <- as.vector(t(matrix3))

par(mfrow = c(2,2))
plot(number1, axes = F, ann = F)
Acf(vector1, lag.max = 18, main = "Acf plot for image of 1", lwd =2, xaxt = 'n')
axis(1, at = c(1,6,12,18), labels = c(1,6,12,18))
plot(number3, axes = F, ann = F)
Acf(vector3, lag.max = 18, main = "Acf plot for image of 3", lwd =2, xaxt = 'n')
axis(1, at = c(1,6,12,18), labels = c(1,6,12,18))

setwd("D:/Study/Bentley/MA611/Project/images")

par(mfrow = c(1,1))
cat <- load.image("cat52.jpg")
try(cat <- grayscale(cat))
cat <-resize(cat, size_x = 128, size_y = 128)
cat <- renorm(cat)
catMatrix <- convertToMatrix(cat)
catVector <- as.vector(t(catMatrix))

plot(cat)

INTC <- get.hist.quote(instrument="INTC", start="2013-01-01", end= "2014-12-31", 
                       quote="Close",provider="yahoo", origin="1970-01-01", 
                       compression= "d", retclass="zoo", quiet = T)
# get time
t <- time(INTC)
# get value
value <- as.numeric(INTC)
# construct a data frame
df <- cbind.data.frame(time = 1:504, value = value)
df$Year <- as.factor(c(rep(0, 252), rep(1,252)))
df$halfYear <- as.factor(c(rep(0,124), rep(1, 128), rep(2,124), rep(3,128)))
df$Quartley <- as.factor(c(rep(0,60), rep(1,64), rep(2, 64), rep(3, 64), rep(4,60), rep(5,64), rep(6, 64), rep(7, 64)))

# only on time
plot(x = df$time, y = df$value, xlab = "Time", ylab = "Price ($)", 
     main = "Daily Closing Prices of Intel", lwd = 2, type = "l",
     xaxt = 'n')  
axis(1, at=c(1,252,504), labels = c(2013, 2014, 2015))
model0 <- lm(value~time, df)
pred0 <- model0$fitted.values
points(x = 1:504, y = pred0, type = "l", lwd = 2, col = "blue")

# year indicator variable
plot(x = df$time, y = df$value, xlab = "Time", ylab = "Price ($)", 
     main = "Daily Closing Prices of Intel", lwd = 2, type = "l",
     xaxt = 'n')  
axis(1, at=c(1,252,504), labels = c(2013, 2014, 2015))
model1 <- lm(value~time+Year, data = df)
pred1 <- model1$fitted.values
abline(v = 0.5, lty = 2, col = "red", lwd = 2)
points(x = 1:252, y = pred1[1:252], type = "l", lwd = 2, col = "blue")
abline(v = 252.5, lty = 2, col = "red", lwd = 2)
points(x = 253:504, y = pred1[253:504], type = "l", lwd = 2, col = "blue")
abline(v = 504.5, lty = 2, col = "red", lwd = 2)

# half year indicator variable
plot(x = df$time, y = df$value, xlab = "Time", ylab = "Price ($)", 
     main = "Daily Closing Prices of Intel", lwd = 2, type = "l",
     xaxt = 'n')  
axis(1, at=c(1,252,504), labels = c(2013, 2014, 2015))
model2 <- lm(value~time+halfYear, df)
pred2 <- model2$fitted.values
abline(v = 0.5, lty = 2, col = "red", lwd = 2)
points(x = 1:124, y = pred2[1:124], type = "l", lwd = 2, col = "blue")
abline(v = 124.5, lty = 2, col = "red", lwd = 2)
points(x = 125:252, y = pred2[125:252], type = "l", lwd = 2, col = "blue")
abline(v = 252.5, lty = 2, col = "red", lwd = 2)
points(x = 253:376, y = pred2[253:376], type = "l", lwd = 2, col = "blue")
abline(v = 376.5, lty = 2, col = "red", lwd = 2)
points(x = 377:504, y = pred2[377:504], type = "l", lwd = 2, col = "blue")
abline(v = 504.5, lty = 2, col = "red", lwd = 2)

# quarterly indicator variable
plot(x = df$time, y = df$value, xlab = "Time", ylab = "Price ($)", 
     main = "Daily Closing Prices of Intel", lwd = 2, type = "l",
     xaxt = 'n')  
axis(1, at=c(1,252,504), labels = c(2013, 2014, 2015))
model3 <- lm(value~time+Year+halfYear+Quartley, df)
pred3 <- model3$fitted.values
abline(v = 0.5, lty = 2, col = "red", lwd = 2)
points(x = 1:60, y = pred3[1:60], type = "l", lwd = 2, col = "blue")
abline(v = 60.5, lty = 2, col = "red", lwd = 2)
points(x = 61:124, y = pred3[61:124], type = "l", lwd = 2, col = "blue")
abline(v = 124.5, lty = 2, col = "red", lwd = 2)
points(x = 125:188, y = pred3[125:188], type = "l", lwd = 2, col = "blue")
abline(v = 188.5, lty = 2, col = "red", lwd = 2)
points(x = 189:252, y = pred3[189:252], type = "l", lwd = 2, col = "blue")
abline(v = 252.5, lty = 2, col = "red", lwd = 2)
points(x = 253:312, y = pred3[253:312], type = "l", lwd = 2, col = "blue")
abline(v = 312.5, lty = 2, col = "red", lwd = 2)
points(x = 313:376, y = pred3[313:376], type = "l", lwd = 2, col = "blue")
abline(v = 376.5, lty = 2, col = "red", lwd = 2)
points(x = 377:440, y = pred3[377:440], type = "l", lwd = 2, col = "blue")
abline(v = 440.5, lty = 2, col = "red", lwd = 2)
points(x = 441:502, y = pred3[441:502], type = "l", lwd = 2, col = "blue")
abline(v = 504.5, lty = 2, col = "red", lwd = 2)


# wavelet decomposition on an image and the diagnoal edge extracted
myt <- function(x) x^0.5+20

greycol <- grey((0:255)/255)

plot(catIMWD, col = greycol, transform = T, tfunction = myt)

plot(catIMWD)


# wavelet decompose to the data
INTC <- get.hist.quote(instrument="INTC", start="2012-12-23", end= "2015-01-06", 
                       quote="Close",provider="yahoo", origin="1970-01-01", 
                       compression= "d", retclass="zoo", quiet = T)

INTCwd <- wd(INTC)
plot(INTCwd)
