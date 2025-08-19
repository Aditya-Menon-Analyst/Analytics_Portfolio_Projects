source("DataAnalyticsFunctions.R")
source("PerformanceCurves.R")
installpkg('tree')
installpkg('glmnet')
installpkg('randomForest')
installpkg('libcoin')
installpkg('partykit')
installpkg('dplyr')
installpkg('ggplot2')
installpkg('rlang')
installpkg('gridExtra')
installpkg('ggrepel')
installpkg('class')
installpkg('caret')
installpkg('reshape2')
installpkg('corrplot')
installpkg('factoextra')
installpkg('cluster')
installpkg('pROC')
library(tree)
library(glmnet)
library(randomForest)
library(libcoin)
library(partykit)
library(dplyr)
library(ggplot2)
library(rlang)
library(gridExtra)
library(ggrepel) 
library(class)
library(caret)
library(reshape2)
library(corrplot)
library(factoextra)
library(cluster)
library(pROC)
set.seed(1) #seed seed for replicability

#################################################################data cleaning
options(scipen = 999)
data <- read.csv('Spotify_Youtube.csv')
d <- data[,-1] 
any(duplicated(d)) # check duplicated
data <- data[, c(-3, -7, -19, -25)] # remove columns:url(spotify, youtube), uri, description
summary(data)
sum(is.na(data)) # 2178 NA values
which(colSums(is.na(data)) > 0)  # identify columns with NA
colnames(data)[1] <- "id"
#colnames(data)

## Spotify Data
spotify <- data[, c(1:16, 24)] %>%
  na.omit() %>%
  mutate("Duration" = Duration_ms/60000, #Minute unit
         "Stream" = Stream/1000000) #Million unit
spotify$Has_Feature <- as.integer(grepl("\\(feat\\. .*?\\)", spotify$Track)) #add feature column
spotify$Is_Special <- as.integer(grepl("Version|Edition|Deluxe", spotify$Album, ignore.case = TRUE))
boxplot(spotify$Stream)
IQR(spotify$Stream)
boxplot_result <- boxplot(spotify$Stream, plot = FALSE)  # Set plot = FALSE to avoid displaying the plot
# Extract quartiles
quartiles <- boxplot_result$stats[, 1]
qr3 <- quartiles[4]
qr3
names(quartiles) <- c("Min", "1st Qu.", "Median", "3rd Qu.", "Max")
# Display quartiles
print(quartiles)
#Add hit = T if Stream > 3rd quartile
spotify$hit <- spotify$Stream
spotify[spotify$Stream>=qr3,]$hit <- 1
spotify[spotify$Stream<qr3,]$hit <- 0
#special edition
#summary(spotify)

## Youtube Data
yt <- data[, c(1, 17:23)] %>%
  na.omit() %>%
  mutate("Views" = Views/1000000,   #Millions unit
         "Likes" = Likes/1000,      #Thousands unit
         "Comments" = Comments/1000)#Thousands unit
#summary(yt)



#Together
#Clean spotify data
data <- data %>%
  na.omit() %>%
  mutate("Duration" = Duration_ms/60000, #Minute unit
         "Stream" = Stream/1000000) #Million unit

## Clean Youtube Data
data <- data %>%
  na.omit() %>%
  mutate("Views" = Views/1000000,   #Millions unit
         "Likes" = Likes/1000,      #Thousands unit
         "Comments" = Comments/1000)#Thousands unit
data$official_video[data$official_video == 'True'] <- 1
data$official_video[data$official_video == 'False'] <- 0
data$official_video <- as.numeric(data$official_video)
data$Licensed[data$Licensed == 'True'] <- 1
data$Licensed[data$Licensed == 'False'] <- 0
data$Licensed <- as.numeric(data$Licensed)
data<-data[,-16] #remove duplicate duration_ms col
#######################################################end data cleaning
## Numerical Vairables  

plot_Danceability <- 
  ggplot(spotify, aes(x = !!sym("Danceability"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Danceability), max(spotify$Danceability)) +
  ggtitle(paste("Distribution of Danceability")) +
  xlab("Danceability") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

plot_Energy <- 
  ggplot(spotify, aes(x = !!sym("Energy"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Energy), max(spotify$Energy)) +
  ggtitle(paste("Distribution of Energy")) +
  xlab("Energy") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Loudness <- 
  ggplot(spotify, aes(x = !!sym("Loudness"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Loudness), max(spotify$Loudness)) +
  ggtitle(paste("Distribution of Loudness")) +
  xlab("Loudness(db)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Speechiness <- 
  ggplot(spotify, aes(x = !!sym("Speechiness"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Speechiness), max(spotify$Speechiness)) +
  ggtitle(paste("Distribution of Speechiness")) +
  xlab("Speechiness") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Acousticness <- 
  ggplot(spotify, aes(x = !!sym("Acousticness"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Acousticness), max(spotify$Acousticness)) +
  ylim(0,2500) +
  ggtitle(paste("Distribution of Acousticness")) +
  xlab("Acousticness") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Instrumentalness <- 
  ggplot(spotify, aes(x = !!sym("Instrumentalness"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Instrumentalness), max(spotify$Instrumentalness)) +
  ylim(0,600) +
  ggtitle(paste("Distribution of Instrumentalness")) +
  xlab("Instrumentalness") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Liveness <-
  ggplot(spotify, aes(x = !!sym("Liveness"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Liveness), max(spotify$Liveness)) +
  ggtitle(paste("Distribution of Liveness")) +
  xlab("Liveness") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Valence <-
  ggplot(spotify, aes(x = !!sym("Valence"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Valence), max(spotify$Valence)) +
  ggtitle(paste("Distribution of Valence")) +
  xlab("Valence") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Tempo <-
  ggplot(spotify, aes(x = !!sym("Tempo"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Tempo), max(spotify$Tempo)) +
  ggtitle(paste("Distribution of Tempo")) +
  xlab("Tempo(BPM)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 


plot_Duration_10min <-
  ggplot(spotify[spotify$Duration < 10,], aes(x = !!sym("Duration"))) +  
  geom_histogram(bins = 40, fill = "lightgreen", color = "black") +
  xlim(min(spotify[spotify$Duration < 10,]$Duration), max(spotify[spotify$Duration < 10,]$Duration)) +
  ggtitle("Distribution of Duration(below 10min)") +
  xlab("Duration(minute)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 
plot_Duration_above10min <- 
  ggplot(spotify[spotify$Duration >= 10,], aes(x = !!sym("Duration"))) +  
  geom_histogram(bins = 40, fill = "lightgreen", color = "black") +
  xlim(min(spotify[spotify$Duration >= 10,]$Duration), max(spotify[spotify$Duration >= 10,]$Duration)) +
  ylim(0,20) +
  ggtitle(paste("Distribution of Duration(above 10min)")) +
  xlab("Duration(minute)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

plot_Stream <-
  ggplot(spotify, aes(x = !!sym("Stream"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(spotify$Stream), max(spotify$Stream)) +
  ylim(0, 6000) +
  ggtitle(paste("Distribution of Stream")) +
  xlab("Stream(Million)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 
plot_Stream

plot_Views <-
  ggplot(yt, aes(x = !!sym("Views"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(yt$Views), max(4000)) +
  ggtitle(paste("Distribution of Views")) +
  ylim(0, 1000) +
  xlab("Views(Million)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

plot_Likes <-
  ggplot(yt, aes(x = !!sym("Likes"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(yt$Likes), max(30000)) +
  ggtitle(paste("Distribution of Likes")) +
  ylim(0, 1000) +
  xlab("Likes(Thousands)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

plot_Comments <-
  ggplot(yt, aes(x = !!sym("Comments"))) +  
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  xlim(min(yt$Comments), max(10000)) +
  ggtitle(paste("Distribution of Comments")) +
  ylim(0, 100) +
  xlab("Comments(Thousands)") +
  ylab("Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
hist(spotify$Stream, breaks = 50)
boxplot(spotify$Stream)


#Unsupervised Learning PCA:
pca.spotify <- prcomp(spotify[,-c(1:5,8,16,22)], scale=TRUE)
summary(pca.spotify)
t(round(pca.spotify$rotation,2))

#lines(c(0,50),c(10,50), col="#cccccc", lwd=2, lty=2)  
#lines(c(20,8),c(10,60), col="#cccccc", lwd=2, lty=2)  
#text(c(30,15),c(35,40), c("PC1","PC2"), col="red", cex=1)

### Lets compute the (Full) PCA
spotify.numeric <- spotify[,-c(1:5,8,16,22)]
pca.spotify <- prcomp(spotify.numeric, scale=TRUE)
### Lets plot the variance that each component explains
plot(pca.spotify,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
###
### It seems like the first 1 components are responsible for almost all variation
###
### We can see the PCscore of each car for each Principal Component
### we can see which track has a lot of PC1 (we will interpret PC1 in a little bit via the loadings)
spotifypc <- predict(pca.spotify) # scale(car)%*%pca.car$rotation
### This will give you the PC score of each song on each Principal Component
spotifypc
track.labels <- spotify$Track

### Lets plot so see how the tracks look like in these components
### Lets plot the first 4 PCscores for each track
plot(spotifypc[,1:2], pch=21,  main="")
text(spotifypc[,1:2], track.labels, col="blue", cex=1)
#text(spotifypc[,1:2], col="blue", cex=1)

## We see that tracks are similar but some different 
### when we look at them via the PCs (i.e. latent features)
### In particular Blinding lights, Shape of You, rockstar are very different in the PC1,2

## Interpreting the four factors
#################
## Next we will interpret the meaning of the latent features (PCs)
## to do that we look at the "loadings" which gives me
## the correlation of each factor with each original feature
## Note that it is important to look at the larger correlations (positively or negatively) 
## to see which are the original features that are more important 
## in a factor. We will do it for the first 4 factors.
loadings <- pca.spotify$rotation[,1:4]
## pca.data$rotation is a matrix. Each column corresponds
## to a PC and each row to an original feature.
## the value of the entry is the correlation between them (i.e. loading)
##
### For each factor lets display the top features that 
### are responsible for 3/4 of the squared norm of the loadings
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:27],1]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### Looking at which are large positive and large negative
#### First factor is Quiet/Low energy/Acoustic tracks, Instrumental tracks
####Understanding PC1: The first principal component (PC1) appears to represent a contrast between high energy/loudness tracks and those that are more acoustic or instrumental.
#Dimensionality: Given that PC1 captures a lot of variance, this distinction is likely central to the variation within the dataset.

#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:27],2]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### This is unpopular songs

###################################################################end unsupervised
#Modeling


Spot <- spotify[,-c(1:4,16,17,22)] #remove certain columns for regression
Spot$hit <- factor(Spot$hit)
set.seed(1)
summary(Spot)
ncol(Spot)
nrow(Spot)
sum(complete.cases(Spot))
paste("Missing values in ",nrow(Spot)-sum(complete.cases(Spot)), "observations out of ",nrow(Spot)) #Check for zero missing values

#### Lets run Lasso
#### First lets set up the data for it
#### the features need to be a matrix ([,-1] removes the first column which is the intercept)
Mx<- model.matrix(hit ~ .^2, data=Spot)[,-1]
My<- Spot$hit == "1"
lasso <- glmnet(Mx,My, family="binomial")
lassoCV <- cv.glmnet(Mx,My, family="binomial")

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.hit <- sum(My)
w <- (num.hit/num.n)*(1-(num.hit/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
summary(lassoTheory)
support(lassoTheory$beta)

#features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
#features.min <- support(lassoTheory$beta)
#length(features.min)
#data.min <- data.frame(Mx[,features.min],My)


features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory) #23 features using theoretical lambda

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)


###

###
### prediction is a probability score
### we convert to 1 or 0 via prediction > threshold
#Choose threshold .5
PerformanceMeasure <- function(actual, prediction, threshold=.5) {
  1-mean( abs( (prediction>threshold) - actual ) )  
  #R2(y=actual, pred=prediction, family="binomial")
  #1-mean( abs( (prediction- actual) ) )  
}
#10-fold Cross Validation
n <- nrow(Spot)
nfold <- 10
OOS <- data.frame(m.null =rep(NA,nfold), m.lr=rep(NA,nfold), m.lr.l=rep(NA,nfold), m.lr.pl=rep(NA,nfold), m.tree=rep(NA,nfold), m.rf=rep(NA,nfold), m.average=rep(NA,nfold)) 
#names(OOS)<- c("Logistic Regression", "Lasso on LR with Interactions", "Post Lasso on LR with Interactions", "Classification Tree", "Average of Models")
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  ### Null
  m.null <-glm(hit~1, data=Spot, subset=train,family="binomial")
  pred.null <- predict(m.null, newdata=Spot[-train,], type="response")
  OOS$m.null[k] <- PerformanceMeasure(actual=My[-train], pred=pred.null)
  
  
  ### Logistic regression
  m.lr <-glm(hit~., data=Spot, subset=train,family="binomial")
  pred.lr <- predict(m.lr, newdata=Spot[-train,], type="response")
  OOS$m.lr[k] <- PerformanceMeasure(actual=My[-train], pred=pred.lr)
  
  ### the Post Lasso Estimates
  m.lr.pl <- glm(My~., data=data.min, subset=train, family="binomial")
  pred.lr.pl <- predict(m.lr.pl, newdata=data.min[-train,], type="response")
  OOS$m.lr.pl[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.lr.pl)
  
  ## the Lasso estimates
  #choose lambda
  m.lr.l  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  pred.lr.l <- predict(m.lr.l, newx=Mx[-train,], type="response")
  OOS$m.lr.l[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.lr.l)
  
  ### the classification tree
  m.tree <- tree(hit~ ., data=Spot, subset=train) 
  pred.tree <- predict(m.tree, newdata=Spot[-train,], type="vector")
  pred.tree <- pred.tree[,2]
  OOS$m.tree[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.tree)
  
  ### random forest
  m.rf <- randomForest(hit~., data=Spot, subset=train, nodesize=5, ntree = 500, mtry = 3)
  pred.rf <- predict(m.rf,type = "prob")[,2]
  OOS$m.rf[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.rf)
  
  ### average
  pred.m.average <- rowMeans(cbind(pred.tree, pred.lr.l, pred.lr.pl, pred.lr, pred.lr))
  OOS$m.average[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.m.average)
  
  print(paste("Iteration",k,"of",nfold,"completed"))
  
  
}    
OOS
colMeans(OOS)


### Logistic regression
#m.lr <-glm(hit~., data=Spot, family="binomial")
# Assuming `n` is defined as the number of rows in `Spot`
table(Spot$hit)

n <- nrow(Spot)
# Train-test split
train <- sample(1:n, size = 0.8 * n, replace = FALSE)
X_train <- Spot[train, -c(15)]
y_train <- Spot$hit[train]
X_test <- Spot[-train, -c(15)]
y_test <- Spot$hit[-train]
# Fit logistic regression model
m.lr <- glm(hit ~ ., data = Spot, subset = train, family = "binomial")
summary(m.lr)

# Make predictions
#predicted_level <- predict(m.lr, newdata = X_test, type = "response")
# Create confusion matrix
#predicted_classes <- as.factor(predicted_level > .5)
#actual_classes <- as.factor(y_test)
#confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)
#print(confusion_matrix)
# Calculate accuracy
#accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
#cat("Accuracy:", accuracy, "\n")
#sum(y_test == "1")

predicted_level <- predict(m.lr, X_test, type = 'response')
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n",
      xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
fpr_values <- c()
tpr_values <- c()
for ( val in seq(from = 0, to = 1, by = 0.05) ){
  values <- FPR_TPR((predicted_level >= val), y_test == '1')
  points( values$FPR , values$TPR ,col='red', pch=16, cex = 1.5)
  if(val<.55 & val >.1){
  text(values$FPR, values$TPR, labels = round(val, 2), pos = 4, col = "black", cex = 0.8)
  }
  cat(val, "Accuracy:", values$ACC, "\n")
  # Store the FPR and TPR for each threshold
  fpr_values <- c(fpr_values, values$FPR)
  tpr_values <- c(tpr_values, values$TPR)
}
lines(fpr_values, tpr_values, col = "blue", lwd = 2)

confusion_matrix <- table(Predicted = predicted_level >= .25, Actual = y_test) #set threshold here
confusion_matrix <- confusion_matrix[c("TRUE", "FALSE"), c("1", "0")]
print(confusion_matrix)
sum(diag(confusion_matrix)) / sum(confusion_matrix) 
#Can move threshold but it doesn't to better than the random forest later on. Either more false positives than true positives, or very few true positives. This model is having a harder time in deployment



#top <- test[predicted_level > .8 & spotify$hit=='1',] #top predictions
top <- spotify[-train & predicted_level>.25,]
top <- top[top$hit=='1',]
#sum(predicted_level>.25)

# Fit logistic regression with LASSO model
Mx <- model.matrix(hit ~ .^2, data = Spot)[, -1]  # Design matrix
My <- Spot$hit == "1"  # Binary response
data.theory <- data.frame(Mx[, features.theory], My)
# Fit LASSO model
m.lr.l <- glmnet(Mx[train, ], My[train], family = "binomial", lambda = lambda.theory)
predicted_level <- predict(m.lr.l, Mx[-train,], type = 'response')
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n",
      xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
fpr_values <- c()
tpr_values <- c()
for ( val in seq(from = 0, to = 1, by = 0.05) ){
  values <- FPR_TPR((predicted_level >= val), y_test == '1')
  points( values$FPR , values$TPR , col = 'red', pch = 16, cex = 1.5)
  if(val<.55 & val >.1){
    text(values$FPR, values$TPR, labels = round(val, 2), pos = 4, col = "black", cex = 0.8)
  }
  cat(val, "Accuracy:", values$ACC, "\n")
  # Store the FPR and TPR for each threshold
  fpr_values <- c(fpr_values, values$FPR)
  tpr_values <- c(tpr_values, values$TPR)
}
lines(fpr_values, tpr_values, col = "blue", lwd = 2)
m.lr.l$beta
length(features.theory)

confusion_matrix <- table(Predicted = predicted_level >= .25, Actual = y_test) #set threshold here
confusion_matrix <- confusion_matrix[c("TRUE", "FALSE"), c("1", "0")]
print(confusion_matrix)
sum(diag(confusion_matrix)) / sum(confusion_matrix) 
#Similar results to linear model


#Trees are performing badly, basically a null model no matter what parameters I try and add :(
m.tree <- tree(hit~., data=Spot)
summary(m.tree)
plot(m.tree)
text(m.tree, pretty = 0)
#As a result we will try expanding on our research with random forest instead

### random forest
m.rf <- randomForest(hit~., data=Spot, subset=train, nodesize=5, ntree = 500, mtry = 3)
m.rf
#pred.rf <- predict(m.rf,type = "prob")[,2]
summary(m.rf)
importance(m.rf)
importance_df <- data.frame(Feature = rownames(importance(m.rf)),
                            Importance = importance(m.rf)[,1])
importance_df <- importance_df %>% arrange(desc(Importance))

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  ggtitle("Feature Importance from Random Forest") +
  xlab("Features") +
  ylab("Importance") +
  theme_minimal() 
#Loudness, Duration, Acousticness are most important in RF. Others important are Valence, Energy, Tempo, Danceability, Speechiness, Liveness.

#Plots
predicted_level <- predict(m.rf, X_test, type = 'prob')[,2]
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n",
      xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
fpr_values <- c()
tpr_values <- c()
for ( val in seq(from = 0, to = 1, by = 0.05) ){
  values <- FPR_TPR((predicted_level >= val), y_test == '1')
  points( values$FPR , values$TPR , col = 'red', pch = 16, cex = 1.5)
  if(val<.55 & val >=.1){
    text(values$FPR, values$TPR, labels = round(val, 2), pos = 4, col = "black", cex = 0.8)
  }
  cat(val, "Accuracy:", values$ACC, "\n")
  fpr_values <- c(fpr_values, values$FPR)
  tpr_values <- c(tpr_values, values$TPR)
}
lines(fpr_values, tpr_values, col = "blue", lwd = 2)

#length(y_test)
predicted_level <- predict(m.rf, X_test, type = 'prob')[,2]
#length(predicted_level)
#predicted_level
confusion_matrix <- table(Predicted = predicted_level >= .25, Actual = y_test) #set threshold here
confusion_matrix <- confusion_matrix[c("TRUE", "FALSE"), c("1", "0")]
print(confusion_matrix)
sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix <- table(Predicted = predicted_level >= .45, Actual = y_test) #set threshold here
confusion_matrix <- confusion_matrix[c("TRUE", "FALSE"), c("1", "0")]
print(confusion_matrix)
sum(diag(confusion_matrix)) / sum(confusion_matrix) 
#Accuracy
#.5 level appears best at not identifying too much, but if you wanted a better TPR at the sacrifice of worse FPR you could take it lower to like .4 and not be overwhelmed with misses. In the context of the music industry, more true positives is helpful to identify big hits and learn what to focus marketing resources on. However, more false positives can be dangerous because promoting strongly for songs that are not recieved well can harm artists reputation. Context Matters: The decision might depend on the artist's current standing, market conditions, and specific business strategies. For example, an established artist might afford to take more risks with new releases compared to a newcomer.
#A good balance is key, so we choose .5 threshold as the best here.

#############################################################################
#KNN
X <- spotify %>%
  cbind(model.matrix(~Album_type-1, spotify), model.matrix(~Key-1, spotify)) %>%
  select(-id, -Artist, -Track, -Album, -Album_type, -Key, -Duration_ms, -Stream)
y <- ifelse(spotify$Stream >= qr3, 1, 0) # Stream level 
knn_v2 <- function(v1, v2, k = 5, p = 1/2) {
  X[,v1] <- scale(X[,v1])
  X[,v2] <- scale(X[,v2])
  x<- cbind( (X[,v1]-mean(X[,v1]))/sd(X[,v1]) ,
             (X[,v2]-mean(X[,v2]))/sd(X[,v2]) )
  ### this sets the target variable for the training set
  g <- factor(y)
  ### this will be used to define the testing set
  px1 <- seq(from=min(x[,1]), to=max(x[,1]), by=0.05)
  px2 <- seq(from=min(x[,2]), to=max(x[,2]), by=0.05)
  xnew <- expand.grid(x=px1, y=px2)
  mod5 <- knn(train=x, test=xnew, cl=g, k=k, prob=TRUE)
  prob <- attr(mod5, "prob")
  prob <- ifelse(mod5=="1", prob, 1-prob)
  prob5 <- matrix(prob, length(px1), length(px2))
  ### Lets do some plotting
  ### fist we plot the data
  ## creates a plot without labels (xlab="",ylab="")
  plot(x, label="",xlab="",ylab="")
  ## provides abox around the points
  box()
  #############################
  ### Now the regions 
  ###
  ### first for for prob <1/2 rule
  contour(px1, px2, prob5, levels=p, labels="", xlab=v1, ylab=v2, 
          main=paste("5-nearest neighbors (p<", round(p,2), "rule)"), axes=TRUE)
  points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
  gd <- expand.grid(x=px1, y=px2)
  points(gd, pch=".", cex=1.2, col=ifelse(prob5>p, "coral", "cornflowerblue"))
}

knn_v2("Energy","Loudness")


# Number of folds for cross-validation
num_folds <- 10

# Create folds for cross-validation
folds <- createFolds(y, k = num_folds, list = TRUE)

# Set possible k values (odd numbers to avoid ties)
k_values <- seq(1, 20, by = 2)  # Odd values between 1 and 19

# To store cross-validation errors for each k
cv_errors <- numeric(length(k_values))

# Cross-validation loop over different k
for (i in seq_along(k_values)) {
  k <- k_values[i]  # Current value of k
  fold_errors <- numeric(num_folds)  # Store errors for each fold
  
  for (j in seq_along(folds)) {
    # Get training and test indices for the current fold
    test_idx <- folds[[j]]
    train_idx <- setdiff(seq_len(nrow(X)), test_idx)
    
    # Split the data into training and test sets
    X_train <- X[train_idx, ]
    X_test <- X[test_idx, ]
    y_train <- y[train_idx]
    y_test <- y[test_idx]
    
    # Apply k-NN on the current fold
    knn_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)
    
    # Calculate error rate for this fold (misclassification rate)
    fold_errors[j] <- mean(knn_pred != y_test)
  }
  
  # Average error across all folds for this k
  cv_errors[i] <- mean(fold_errors)
}

# Find the best k (the one with the lowest cross-validation error)
best_k <- k_values[which.min(cv_errors)]
best_k
cv_errors
# Output the best k value
cat("The best k is:", best_k, "\n")

# Plot the cross-validation errors vs. k values
plot(k_values, cv_errors, type = "b", col = "blue", pch = 19,
     xlab = "k (Number of Neighbors)", ylab = "Cross-Validation Error",
     main = "k-NN Cross-Validation Error vs. k")

# Test k = 1
knn_v2("Energy", "Loudness", k = 1, p = 0.5)

# Test k = 3
knn_v2("Energy", "Loudness", k = 3, p = 0.5)

# Test k = 5
knn_v2("Energy", "Loudness", k = 5, p = 0.5)

table(y) # there are three times as many not hits. Can we undersample the majority of not hits so they are even?
# Assuming y is your binary target variable (1 = hit, 0 = not hit)
# Separate the hits and not hits
hitss <- which(y == 1)
not_hits <- which(y == 0)
# Randomly sample 1/3 of the not hits
undersample_not_hits <- sample(not_hits, length(hitss))
# Combine the hits and the undersampled not hits
balanced_indices <- c(hitss, undersample_not_hits)
# Create the balanced dataset
X_balanced <- X[balanced_indices, ]
y_balanced <- y[balanced_indices]

# Check the distribution
table(y_balanced)

# Now use this balanced dataset to train  k-NN model
knn_v2_balanced <- function(v1, v2, k = 5, p = 1/2) {
  X_balanced[,v1] <- scale(X_balanced[,v1])
  X_balanced[,v2] <- scale(X_balanced[,v2])
  x<- cbind( (X_balanced[,v1]-mean(X_balanced[,v1]))/sd(X_balanced[,v1]) ,
             (X_balanced[,v2]-mean(X_balanced[,v2]))/sd(X_balanced[,v2]) )
  ### this sets the target variable for the training set
  g <- factor(y_balanced)
  ### this will be used to define the testing set
  px1 <- seq(from=min(x[,1]), to=max(x[,1]), by=0.05)
  px2 <- seq(from=min(x[,2]), to=max(x[,2]), by=0.05)
  xnew <- expand.grid(x=px1, y=px2)
  mod5 <- knn(train=x, test=xnew, cl=g, k=k, prob=TRUE)
  prob <- attr(mod5, "prob")
  prob <- ifelse(mod5=="1", prob, 1-prob)
  prob5 <- matrix(prob, length(px1), length(px2))
  ### Lets do some plotting
  ### fist we plot the data
  ## creates a plot without labels (xlab="",ylab="")
  plot(x, label="",xlab="",ylab="")
  ## provides abox around the points
  box()
  #############################
  ### Now the regions 
  ###
  ### first for for prob <1/2 rule
  contour(px1, px2, prob5, levels=p, labels="", xlab=v1, ylab=v2, 
          main=paste(k,"-nearest neighbors (p<", round(p,2), "rule)"), axes=TRUE)
  points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
  gd <- expand.grid(x=px1, y=px2)
  points(gd, pch=".", cex=1.2, col=ifelse(prob5>p, "coral", "cornflowerblue"))
}

knn_v2_balanced("Loudness", "Valence", k = 5, p = 0.5)

