#library--------------------------------------------------------
library(magick)
library(imager)
library(openxlsx)
library(wvtool)
library(readbitmap)
#what about average intensity of all, RGB colors?
#functions------------------------------------------------------
MSE <- function(iMSmall){
  mu <- mean(iMSmall)
  d1 <- dim(iMSmall)[1]
  d2 <- dim(iMSmall)[2]
  muMat <- matrix(rep(mu,d1*d2), nrow = d1)
  dif <- iMSmall - muMat
  dif2 <- dif**2
  mse <- sum(dif2)/(d1*d2)
  return(mse)
}
MSE3 <- function (imMat){
  return(list(
    R = MSE(imMat[,,1]),
    G = MSE(imMat[,,2]),
    B = MSE(imMat[,,3])
  ))
}
MSEAll <- function(imMat){
  mu <- mean(imMat[,,1]) + mean(imMat[,,2]) + mean(imMat[,,3])
  d1 <- dim(imMat[,,1])[1]
  d2 <- dim(imMat[,,1])[2]
  n <- d1*d2*3
  muMat1 <- matrix(rep(mu,d1*d2), nrow = d1)
  dif1 <- imMat[,,1] - muMat1
  dif2 <- imMat[,,2] - muMat1
  dif3 <- imMat[,,3] - muMat1
  dif12 <- dif1**2
  dif22 <- dif2**2
  dif32 <- dif3**2
  mse <- (sum(dif12) + sum(dif22) + sum(dif32))/n
  return(mse)
}
#data-----------------------------------------------------------
setwd("/Users/chelseaparlett/Desktop/Desktop/GitHub/CancerClassification/PH2Dataset/PH2 Dataset images")
diagDat <- read.csv(file.choose())

ims <- Sys.glob("IMD[0-9][0-9][0-9]/IMD[0-9][0-9][0-9]_Dermoscopic_Image/IMD[0-9][0-9][0-9].bmp")
imsList <- lapply(ims,FUN = load.image) #numbered list of image objects
names(imsList) <- lapply(ims,function(x) substr(x,33,38))

masks <- Sys.glob("IMD[0-9][0-9][0-9]/IMD[0-9][0-9][0-9]_lesion/IMD[0-9][0-9][0-9]_lesion.jpg")
maskList <- lapply(masks,FUN = load.image) #numbered list of image objects
names(maskList) <- lapply(masks,function(x) substr(x,22,27))
maskMatrix <- lapply(maskList,FUN = as.matrix)


#---show--------------------------------------------------------
plot(imsList[[round(runif(1,1,200))]])
plot(maskList[[round(runif(1,1,200))]])

#---looking at full Image---------------------------------------
MSEallCols <- sapply(imsList, MSEAll)
MSEcolbycol <- sapply(imsList, MSE3)
MSEcolbycol["R",1]


mat <- matrix(MSEcolbycol,ncol =3)
df <- data.frame(mat)
df2 <- cbind(names(imsList),df,MSEallCols)
names(df2) <- c("names","R","G","B","all")
dfAll <- merge(df2,diagDat, by.x = "names", by.y = "Name")

head(dfAll)
#---edgeDetection------------------------------------------------
test <- imsList[[1]]
dim(test)

test2 <- grayscale(test)
plot(test2)
hist(test2)

#HAIR!
test2[test2 < 0.2 ] <- 1
hist(test2)

test3 <- test2 < 0.4
plot(test3)

k <- cannyEdges(test2,0.005, 0.02)
plot(k)
#---withMask-----------------------------------------------------

#tests------------------------------------------------------------

