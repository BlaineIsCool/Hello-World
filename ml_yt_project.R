#### Importing data and organizing####
##Import data from excel sheet
library(readxl)
#Laptop
YT <- read_excel("YTVid_ML_datasheet.xlsx")

##Check Data
View(YT)
str(YT)

##rename fields
YT.df<- data.frame(YT)
names(YT.df) <- c("video.id", "category.id", "rating.disabled","publish.year",
                  "publish.month","total.dislikes", "total.likes", "total.views",
                  "days.trending", "total.comments", "tag.count","title.nchar",
                  "title.nwords")

##Change year & month published and category.id variables to factors
YT.df$publish.year <- as.factor(YT.df$publish.year)
YT.df$publish.month <- as.factor(YT.df$publish.month)
YT.df$category.id <- as.factor(YT.df$category.id)

##Drop video.id
names(YT.df[-1])
YT.df <- YT.df[-1]

##Check for missing datapoints
sum(is.na(YT.df))

#### Data review ####
## attach YT dataframe for analysis
attach(YT.df)

#review data
library(ggplot2)

##Check summary of data
summary(YT.df)

## Share of videos with more than 250 million views
table(YT.df$total.views >= 25000000)
prop.table(table(YT.df$total.views >= 25000000))
dim(YT.df)

##DROP videos with more than 250 million views
YT.df <- YT.df[-which(YT.df$total.views >= 25000000),]
summary(YT.df$total.views)
dim(YT.df)

## Share of videos with ratings disabled
table(YT.df$rating.disabled)
prop.table(table(YT.df$rating.disabled))
dim(YT.df)

## Remove rows with ratings disabled
YT.df <- subset(YT.df, rating.disabled == FALSE)
dim(YT.df)

## Remove rows with no comments
table(YT.df$total.comments == 0)
YT.df <- YT.df[YT.df$total.comments !=0, ]
dim(YT.df)

## Remove rows with no likes
table(YT.df$total.likes == 0)
YT.df <- YT.df[YT.df$total.likes !=0, ]
dim(YT.df)

## Remove rows with no dislikes
table(YT.df$total.dislikes == 0)
YT.df <- YT.df[YT.df$total.dislikes !=0, ]
dim(YT.df)

## Check new data summary
summary(YT.df)

#### Data visualizations ####

##Total views review
ggplot(YT.df, aes(x = total.views)) + geom_histogram(binwidth = 1000000)
summary(YT.df$total.views)
prop.table(table(YT.df$total.views >= 1000000)) #34% of vids with more than one million views

##Video category review
prop.table(table(YT.df$category.id))


##Time variables
plot(YT.df$publish.year)
plot(YT.df$publish.month)
summary(days.trending)
ggplot(YT.df, aes(x = days.trending)) + geom_histogram(binwidth = 1)

##Total comments highly correlated with total views
summary(total.comments)
plot(log(YT.df$total.comments), log(YT.df$total.views))


## Total likes and dislikes highly correlated with total views
plot(log(total.likes), log(total.views))
plot(log(total.dislikes), log(total.views))

####Variable transformations ####

###No videos have only one like, very few (< 1%) have only one dislike
sum(YT.df$total.likes == 1)
sum(YT.df$total.dislikes == 1)

### Create binary variable for 1 million+ view videos
YT.df$milli.views <- ifelse(YT.df$total.views >= 1000000, 1, 0)
YT.df$milli.views <- as.factor(YT.df$milli.views)
table(YT.df$milli.views)
prop.table(table(YT.df$milli.views))

##Divide likes, dislikes views and comments by number of days video is trending
YT.df$total.likes.pd <- (YT.df$total.likes)/YT.df$days.trending
YT.df$total.dislikes.pd <- (YT.df$total.dislikes)/YT.df$days.trending
YT.df$total.comments.pd <- (YT.df$total.comments)/YT.df$days.trending
YT.df$total.views.pd <- (YT.df$total.views)/YT.df$days.trending


## Natural log of likes, dislikes and comments
YT.df$ln.total.likes.pd <- log(YT.df$total.likes.pd)
YT.df$ln.total.dislikes.pd <- log(YT.df$total.dislikes.pd)
YT.df$ln.total.comments.pd <- log(YT.df$total.comments.pd)

## Create variables exploring engagement and how controversial a video is
YT.df$engage <- log(YT.df$total.comments.pd/YT.df$total.likes.pd)
YT.df$contro <- log(YT.df$total.dislikes.pd/(YT.df$total.dislikes.pd +YT.df$total.likes.pd))

str(YT.df)

## Remove unneccesary columns
YT.df2 <- subset(YT.df, 
                 select = -c(rating.disabled, total.likes, total.dislikes, 
                             total.comments, total.views, 
                             total.likes.pd,total.dislikes.pd, total.comments.pd,
                             total.views.pd, days.trending))


detach(YT.df)

## Run diagnostic reports
dim(YT.df2)
summary(YT.df2)
#install.packages("DataExplorer")
library(DataExplorer)
#create_report(YT.df2,y= "milli.views)


#### Exploratory Data Analysis of final dataset####

#1 box plots
#tag count
boxplot(tag.count ~ milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Tag count: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "tag count")
#Number of characters in title
boxplot(title.nchar~milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Chars. in title: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "title char count")
#Number of words in title
boxplot(title.nwords~ milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Words in title: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "title word count")

#net likes
boxplot(contro~milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Controversy: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "controversy")

#engagement
boxplot(engage ~ milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Engagement: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "engagement")

#log total likes
boxplot(ln.total.likes.pd ~ milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Likes: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "ln(likes per day)")

#log total dislikes
boxplot(ln.total.dislikes.pd ~ milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Dislikes: low views vs. high views",
        xlab="low views = 0, 1M views = 1",
        ylab = "ln(dislikes per day")

#comments
boxplot(ln.total.comments.pd ~ milli.views, data=YT.df2,
        col=(c("red","green")),
        main = "Comments: low views vs. high views",
        xlab="low views = 0, 1M views  = 1",
        ylab = "ln(comments per day)")

#2 frequency charts
#install.packages("ggplot2")
library(ggplot2)


#category id
ggplot(YT.df2, aes(x=category.id, fill=milli.views)) +
  geom_bar()

#publish year
ggplot(YT.df2, aes(x=publish.year, fill=milli.views)) +
  geom_bar()

#publish month
ggplot(YT.df2, aes(x=publish.month,  fill=milli.views)) +
  geom_bar()

#3 correlations
#install.packages("corrplot")
library(corrplot)
attach(YT.df2)
cor_vars <- data.frame(tag.count,title.nchar,
                      title.nwords, engage, contro,
                      ln.total.likes.pd, ln.total.dislikes.pd,
                      ln.total.comments.pd)
correlations <- cor(cor_vars)
corrplot(correlations)

##Modeling test 1
library(caret)

#### Creating train/test split ####
set.seed(1234)
index<- createDataPartition(YT.df2$milli.views, times = 1,
                            p = 0.7, list = FALSE)

train <- YT.df2[index,]
test <- YT.df2[-index,]
prop.table(table(YT.df2$milli.views))
prop.table(table(train$milli.views))
prop.table(table(test$milli.views))

#### Training the model####

## Setting up cross-validation
train_control <- trainControl(method ="repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              search = 'grid')

## Training Model
model <- train(milli.views~ . -ckviews.pd, 
               data = train, 
               method = 'rf',
               trControl = train_control,
               metric = "Accuracy",
               tuneLength = 7)


## Check model properties
model
varImpPlot(model$finalModel)
plot(model)

####Test and evaluate the model####
preds <- predict(model, test)

confusionMatrix(preds, test$milli.views)


### Save Model for later use
saveRDS (model, file = 'OneDrive - NBCUniversal/YTmodel.Rds')
model <- readRDS('YTmodel.Rds')

plot(YTmodel)

#### Alternative Models####

### SVM
library(e1071)
YT.svm <- subset(YT.df2, 
                 select = -c(category.id, publish.year, publish.month, ckviews.pd))

set.seed(1234)
index2<- createDataPartition(YT.svm$milli.views, times = 1,
                            p = 0.7, list = FALSE)

train2 <- YT.svm[index2,]
test2 <- YT.svm[-index2,]


svm.fit <- svm(milli.views ~ ., data = train2, kernel = "radial", gamma = 0.5,
               cost = 10)
summary(svm.fit)
pred.svm <- predict(svm.fit, test2)
confusionMatrix(pred.svm, test2$milli.views)

### LDA
library(MASS)
lda.fit <- lda(milli.views ~ ., data = train2)
plot(lda.fit)
lda.pred <- predict(lda.fit, test2)
lda.class <- lda.pred$class
confusionMatrix(lda.class, test$milli.views)
confusionMatrix(preds,test$milli.views)
