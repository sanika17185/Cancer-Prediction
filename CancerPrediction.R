# Installing and loading required libraries
install.packages("mlbench")
library(mlbench)

install.packages("randomForest")
library(randomForest)

# Loading the data into bcancer
bcancer <- read.csv("BreastCancer.csv", header= TRUE, sep= ",")
bcancer

# Checking the dimensions of the dataset
dim(bcancer)

# Checking the structure of R objects in the dataset
str(bcancer)

# The column "Bare Nuclei" contains NULL values represented by "?". There are a total of 699 records, out of which 
# only 16 records have "?" (NULL values), we are removing these records.

#Removing records with special character 
bcancer_ne<-subset(bcancer, Bare.Nuclei!="?")
bcancer_ne
dim(bcancer_ne)


#Dividing the data into training and testing sets
set.seed(1)
trainIndex = sample(1:nrow(bcancer_ne),round(0.8*nrow(bcancer_ne)),replace=FALSE)
train = bcancer_ne[trainIndex,]
testData = bcancer_ne[-trainIndex,]

length(train)
length(testData)
names(train)
names(testData)

dim(train)
dim(testData)

# The training dataset has 546 records and the testing dataset has 137 records

##### Logistic Regression

# Now, we are fitting a logistic regression model for class status by using all the other variables except "Class"
# as predictors.We are ignoring Sample.code.number from predictors as it is not contributing towards prediction.

glm.fit = glm(as.factor(Class)~Clump.Thickness+Uniformity.of.Cell.Size+Uniformity.of.Cell.Shape+Marginal.Adhesion+Single.Epithelial.Cell.Size+Bare.Nuclei+Bland.Chromatin+
                Normal.Nucleoli+Mitoses,data=train,family=binomial)
glm.fit
summary(glm.fit)

# Now, let us predict the probabilities of breast cancer on our testing set. 
glm.pred=predict(glm.fit,testData,type="response")
glm.pred[1:10] 

# Let us now predict and classify classes based on whether the cancer is benign(2) or malignant(4)
# For glm.pred value >0.5, breast cancer is malignant(4) and benign(2) is less than 0.5
predicted.classes <- ifelse(glm.pred > 0.5, 4,2)
head(predicted.classes)

# Checking the model's accuracy
mean(predicted.classes == testData$Class)

# As we can see, the model's accuracy is about 97% and the misclassification error rate is 3%

##### Random Forest
# We are ignoring Sample.code.number from predictors as it is not contributing towards prediction.
bcancer_RF <- randomForest(as.factor(Class)~Clump.Thickness+Uniformity.of.Cell.Size+Uniformity.of.Cell.Shape+Marginal.Adhesion+Single.Epithelial.Cell.Size+
                             Bare.Nuclei+Bland.Chromatin+Normal.Nucleoli+Mitoses,data=train, mtry=4,ntree=5000)
bcancer_RF

# Checking which variable is the most important variable for making predictions about breast cancer status
importance(bcancer_RF)

# As we can see from above results, Uniformity of cell size and uniformity of cell shape are the most important variables
# while making predictions.

# Predicting the Class status on the test set based on the fitted random forest model.

rForest_pred=predict(bcancer_RF,testData)
rForest_pred[1:10]

# As we can see from above, the model is able to predict the class correctly.


#### Data Visualization

# Checking correlation between different columns of the dataset
library(ggplot2)
library(GGally)
ggpairs(bcancer_ne)

# Removing axis labels due to overplotting issue
ggpairs(bcancer_ne) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# According to the correlation graph,correlation between variable - Class, Uniformity of cell shape and size is higher as
# compared to others, we are plotting visualizations for these variables.


library(ggplot2)

# Building a scatterplot of class and uniformity of cell size (Multivariate)
theme_set(theme_bw())
ggplot(bcancer_ne, aes(x = as.factor(Class), y =as.factor(Uniformity.of.Cell.Size)))+
  ggtitle("Inspection Results for different food categories")+
  theme(plot.title=element_text(hjust=0.5))+
  geom_point(color='blue',size=3)

  
# Creating scatterplots between variables - Class, Uniformity of cell shape and Uniformity of cell size (Multivariate)
# using cowplot library  
library(cowplot)
x <- ggplot(bcancer_ne, aes(x = as.factor(Class), y =as.factor(Uniformity.of.Cell.Size))) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()
x

y <- ggplot(bcancer_ne, aes(x = as.factor(Class), y =as.factor(Uniformity.of.Cell.Shape))) +
  geom_jitter(aes(color = as.factor(Class)), alpha = 0.7) +
  theme_light()
y

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Correlation between uniformity of cell shape and size with reference to the class", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


# Checking the count/frequency of benign and malignant breast cancers (Univariate)
ggplot(bcancer_ne, aes(x= as.factor(Class), y= frequency(Class),fill=as.factor(Class))) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( title="Count of types of Cancer (Benign/Malignant)", y= "Count", x= "Class") +
  theme(plot.title= element_text(hjust = 0.5)) +
  theme(legend.position="none")+
  theme_classic()

# As we can see from the results, benign cancer has been recorded the most in our dataset.

