
# Import library

library(raster)

library(sp)

library(rgeos)

library(rgdal)

# Import Sentinel-2 data

Sentinel<-list.files(pattern = "tif$",full.names = T)

Sentinel<-stack(Sentinel)

# Remove the value of -999

S<-Sentinel!=-999

Sentinel2<-Sentinel*S

names(Sentinel2)<-c("Band1","Band2","Band3","Band4")

Sentinel2
# Remove NA data from Sentinel2

NAvalue(Sentinel2)<-0

plot(Sentinel2)

# Plot a natural color image

plotRGB(Sentinel2, r="Band3",g="Band2",b="Band1",stretch="hist")

# Import traning data

train<-shapefile(file.choose()) # Import dataset called "Ser-training'

head(train)

dim(train)

# Randomly select a number of train data points

train@data$CLASS_NAME<-as.factor(train@data$CLASS_NAME)

levels(train@data$CLASS_NAME)

# Set seed to make sure to have a consistant result

set.seed(123)

Agriculture<-train[sample(which(train@data$CLASS_NAME=="Agriculture"),7500),]

Forest<-train[sample(which(train@data$CLASS_NAME=="Forest"),7500),]

Mining<-train[sample(which(train@data$CLASS_NAME=="Mining"),7500),]

Urban<-train[sample(which(train@data$CLASS_NAME=="Urban"),7500),]

Water<-train[sample(which(train@data$CLASS_NAME=="Water"),7500),]

# combining rows

train<-rbind(Agriculture,Forest,Mining,Urban,Water)

train<-data.frame(train)

row.names(train)<-NULL

head(train)

dim(train)

# To form a spatial point dataframe

coordinates(train)<-~coords.x1+coords.x2

proj4string(train)<-CRS("+proj=utm +zone=48 +datum=WGS84")

train # check the structure of data

dim(train@data) # check the number of row and column

head(train@data)
# Showing the distribution of training data points on the satellite map
par(mar=rep(2,4))

plot(train,add=T,col=as.numeric(train@data$CLASS_NAME), axes=T, main="Distribution of Training Data Points")

# Extract the reference data points

train_ext<-extract(Sentinel2, train,df=T)

# Form a data frame 

train_df<-data.frame(train@data,train_ext)

# Check the train-df

head(train_df)

levels(train_df$CLASS_NAME) # Check the level of 'CLASS_NAME' 


# Split data.frame into training and test sets

library(caTools)

split<-sample.split(train_df$CLASS_NAME, SplitRatio = 0.9)

# training set

training_set<-subset(train_df, split==T)

test_set<-subset(train_df, split==F)

dim(training_set)

dim(test_set)

# Building a random forest model on `training set`
library(randomForest)

rf_model<-randomForest(CLASS_NAME~Band1+Band2+Band3+Band4, data = training_set, ntree=300, importance=T,prox=T)

# Predict land cover

Sen_output<-predict(Sentinel2, rf_model, type="class", progress="window")

plot(Sen_output)

#Save this classified map

writeRaster(Sen_output,filename = "Sentinel_RF.tif")

#################################################################################
# making accuraccy assessment 

as1<-predict(rf_model, newdata=test_set[,-3])

head(as1)

# Making confusion matrix

library(e1071)
library(caret)

cm1<-confusionMatrix(as1,test_set$CLASS_NAME)

cm1
###################################################################################
# Using actual collected ground control points

shp<-shapefile(file.choose())

df<-spTransform(shp,CRS("+proj=utm +zone=48 +datum=WGS84"))

df<-data.frame(df)

# Tidying test dataset

library(tidyverse)

df_test<- df %>% select(Land_Cover, matches("RAST"))

names(df_test)<-c("CLASS_NAME","Band1","Band2","Band3","Band4")

df_test$CLASS_NAME<-as.factor(df_test$CLASS_NAME)

# Predicting classes using ground control points

S_pred<-predict(rf_model,newdata=df_test[,-1])

cm2<-confusionMatrix(S_pred,df_test$CLASS_NAME)

cm2
# plot the effect of tree size 

library(tidyverse)

attributes(rf_model) # check all parameters of the model

# Create a data.frame indicating the effect of tree size

tree_size<-data.frame(rf_model$err.rate, Tree=1:nrow(rf_model$err.rate))

head(tree_size)

# Tidying this dataset

tree_size <- tree_size %>% gather(Error_class, Error_value,-Tree)

head(tree_size)

dim(tree_size)

# Plot the effect of tree

ggplot(data=tree_size, aes(x=Tree, y=Error_value)) + geom_line(aes(color=Error_class)) + ylab(" Error Rate") + 
  xlab("Number of Trees") + ggtitle("The effect of Trees on Model Error rate") + 
  theme(plot.title = element_text(hjust=0.5)) +theme_bw()

# plot the importance of variables

varImpPlot(rf_model,type=2,pch=16)

