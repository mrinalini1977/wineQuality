library(ggplot2)
library(grid)
library(gridExtra)

#importing the data file into R

setwd("C:\\Jigsaw\\R\\wine")
wineData = read.csv("wine_data.csv", header = TRUE)

#exploring the data

#structure 
str(wineData)

#histograms

par(mfrow = c(7, 2))  

p1  = ggplot(data = wineData, aes(x = wineData$fixed_acidity) )+ 
  geom_histogram(breaks = seq(4,20,by=.5), col="red", fill = "green", alpha=.2) + 
labs(title = "Histogram for fixed acidity") +
labs(x = "Fixed Acidity", y = "Count") 

p2 =ggplot(data = wineData, aes(x = wineData$volatile_acidity) )+ 
geom_histogram(breaks = seq(0,1.6,by=.05), col="red", fill = "green", alpha=.2) +
labs(title = "Histogram for volatile acidity") +
labs(x = "Volatile Acidity", y = "Count")

p3= ggplot(data = wineData, aes(x = wineData$citric_acid) )+ 
  geom_histogram(breaks = seq(0,1,by=.1), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for citric acid") +
  labs(x = "CitricAcid", y = "Count")
 
 p4 = ggplot(data = wineData, aes(x = wineData$chlorides) )+ 
  geom_histogram(breaks = seq(0,.7,by=.01), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for Chlorides") +
  labs(x = "Chlorides", y = "Count")

p5 = ggplot(data = wineData, aes(x = wineData$residual_sugar) )+ 
  geom_histogram(breaks = seq(0,16,by = .5), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for residual sugar") +
  labs(x = "Residual Sugar", y = "Count")

p6 = ggplot(data = wineData, aes(x = wineData$free_sulfur_dioxide) )+ 
  geom_histogram(breaks = seq(0,75,by = 3), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for free sulfur dioxide") +
  labs(x = "Free Sulfur Dioxide", y = "Count")

p7 = ggplot(data = wineData, aes(x = wineData$total_sulfur_dioxide) )+ 
  geom_histogram(breaks = seq(0,289,by = 10), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for total sulfur dioxide") +
  labs(x = "Total Sulfur Dioxide", y = "Count")

p8 = ggplot(data = wineData, aes(x = wineData$density ) )+ 
  geom_histogram(breaks = seq(0.95,1.1,by = .005), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for density") +
  labs(x = "density", y = "Count")


p9 = ggplot(data = wineData, aes(x = wineData$sulphates ) )+ 
  geom_histogram(breaks = seq(0.3,2,by = .05), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for sulphates") +
  labs(x = "Sulphates", y = "Count")


p10 = ggplot(data = wineData, aes(x = wineData$alcohol ) )+ 
  geom_histogram(breaks = seq(8,15,by = .5), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for alcohol") +
  labs(x = "Alcohol", y = "Count")

p11 = ggplot(data = wineData, aes(x = wineData$quality ) )+ 
  geom_histogram(breaks = seq(3,10,by = 1), col="red", fill = "green", alpha=.2)+
  labs(title = "Histogram for quality") +
  labs(x = "Quality", y = "Count")

grid.arrange(p1, p2, p3, p4, p5,  ncol = 2)
grid.arrange(p6, p7, p8, p9, p10, p11, ncol = 2)

#boxplots

summary(wineData$fixed_acidity)

ggplot(data=wineData, aes(x = factor(0), y=wineData$fixed_acidity)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for fixed acidity") +
  labs(x =" ", y = "Fixed Acidity")

ggplot(data=wineData, aes(x = factor(0), y=wineData$volatile_acidity)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for volatile acidity") +
  labs(x =" ", y = "Volatile Acidity")

ggplot(data=wineData, aes(x = factor(0), y=wineData$citric_acid)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for citric acid") +
  labs(x =" ", y = "Citric Acid") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$residual_sugar)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for residual sugar") +
  labs(x =" ", y = "Residual Sugar") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$chlorides)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for chlorides") +
  labs(x =" ", y = "Chlorides") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$free_sulfur_dioxide)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for free sulfur dioxide") +
  labs(x =" ", y = "Free Sulfur Dioxide") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$total_sulfur_dioxide)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for total sulfur dioxide") +
  labs(x =" ", y = "Total Sulfur Dioxide") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$density)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for density") +
  labs(x =" ", y = "Density") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$pH)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for pH") +
  labs(x =" ", y = "pH") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$sulphates)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for sulphates") +
  labs(x =" ", y = "Sulphates") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$alcohol)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for alcohol") +
  labs(x =" ", y = "Alcohol") 

ggplot(data=wineData, aes(x = factor(0), y=wineData$quality)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = .2) +
  labs(title = "Boxplot for quality") +
  labs(x =" ", y = "Quality") 

str(wineData)

pairs(~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+alcohol, data=wineData)

pairs(~chlorides+free_sulfur_dioxide+density+pH+sulphates, data=wineData)

summary(wineData$alcohol)

# Removing the outliers
limout <- rep(0,11)

for (i in 1:11){
  t1 <- quantile(wineData[,i], 0.75)
  t2 <- IQR(wineData[,i],0.75)
  limout[i] <- t1 + 1.5*t2
}

rows <- nrow(wineData)
outlIndex <- c()

class(outlIndex)

for (i in 1:11)
{
  for (j in 1:rows)
  {
    if (wineData[j,i] > limout[i])
    {
      if(!is.element(c(j),outlIndex))
      {
        outlIndex <- union(outlIndex,c(j))
      }
    }
  }
  
}

wineData <- wineData[-index,]
nrow(wineData)


#wineIndex <- matrix(0,1599,11)
#wineIndex

#for (i in 1:rows)
#{
 # for (j in 1:11)
  #{
    #if (wineData[i,j] > limout[j])
    #{
     # wineIndex[i,j] <- 1
    #}
  #}
  
#}


#wwind <- apply(wineIndex,1, sum)
#wwind

#indexes <- rep(0,400)

#j <- 1

#for(i in 1:rows)
#{
 # if(wwind[i] > 0 )
  #{
   # indexes[j] <- i
    #j <- j+ 1
  #}
  #else
  #{
   # j<- j 
  #}
#}

#length(indexes)

#indexes

### Split the data into training and validation dataset ###
index <- c()
index <- sample(nrow(wineData), 0.70 * nrow(wineData), replace = FALSE)
train <- wineData[index,]
test <- wineData[-index,]

### Running linear regression to create model

fit <- lm ( quality ~ volatile_acidity+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+pH+sulphates+alcohol, data=wineData)
summary(fit)


