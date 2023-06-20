                               # Final_Capstone_Project(Census_dataset)

data=read.csv("C:\\Users\\Sairam\\Desktop\\Final_Capstone_project\\adult.data.csv")->censusdata
View(censusdata)

# Task to be done:
                                       # 1). Data Preprocessing


  # a). Replace all the missing values with NA.
  # b). Remove all the rows taht contain NA values.
  # c). Remove all whitespaces from the columns.

sum(is.null(censusdata))
sum(is.na(censusdata))
# since there are no values like "NULL" and "NA" in the Censusdata set, looking for any other kind of values in the data, convert them to "NA" values and removing those.


# converting factor columns to character

as.character(censusdata$workclass)->censusdata$workclass
as.character(censusdata$occupation)->censusdata$occupation
as.character(censusdata$native.country)->censusdata$native.country
as.character(censusdata$education)->censusdata$education
as.character(censusdata$marital.status)->censusdata$marital.status
as.character(censusdata$relationship)->censusdata$relationship
as.character(censusdata$race)->censusdata$race
as.character(censusdata$sex)->censusdata$sex
as.character(censusdata$X)->censusdata$X

censusdata$workclass[28]

#Replacing " ?" with NA

NA->censusdata[censusdata == " ?"]

sum(is.na(censusdata))

# Removing NA values

na.omit(censusdata)->censusdata

# Removing white spaces

censusdata$education[1] # There are spaces in the columns, so we have to remove them.

library(dplyr)
library(stringr)

censusdata %>% mutate_if(is.character, str_trim)->censusdata

View(censusdata)

# Again converting character cloumns back to factor

as.factor(censusdata$workclass)->censusdata$workclass
as.factor(censusdata$occupation)->censusdata$occupation
as.factor(censusdata$native.country)->censusdata$native.country
as.factor(censusdata$education)->censusdata$education
as.factor(censusdata$marital.status)->censusdata$marital.status
as.factor(censusdata$relationship)->censusdata$relationship
as.factor(censusdata$race)->censusdata$race
as.factor(censusdata$sex)->censusdata$sex
as.factor(censusdata$X)->censusdata$X

str(censusdata)

########################################################################################################----------------------------



                                                # 2).Data Manipulation:

library(dplyr)

#a).Extract the "education" column and store it in "census_ed".
censusdata$education->census_ed
census_ed

#b).Extract all the columns from "age" to "relationship" and store it in "census_Seq".

censusdata[,c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship")]->census_seq
View(census_seq)

#c).Extract the column numbers "5","8","11" and store it in "census_col".
censusdata[,c(5,8,11)]->census_col
View(census_col)

#d).Extract the male employees who work in state-gov and store it in "male_gov".
censusdata %>% filter(sex=="Male"&workclass=="State-gov")->male_gov
View(male_gov)

#e)	Extract all the 39 year olds who either have a bachelor's degree or 
#who are native of United States and store the result in "census_us".
censusdata %>% filter(age==39 & (education=="Bachelors" | native.country=="Uniter-States"))->census_us
View(census_us)

#f)	Extract 200 random rows from the "census" data frame and store it in "census_200".
censusdata %>% sample_n(200)->census_200
View(census_200)

#g)	Get the count of different levels of the "workclass" column.
censusdata %>% count(workclass)

#h)	Calculate the mean of "capital.gain" column grouped according to "workclass".
censusdata %>% group_by(workclass) %>% summarise(mean(capital.gain))
 

################################################################################################################



                                              # 3. Data Visualization:

library("ggplot2")

# a).Build a bar-plot for the "relationship" column and fill the bars according to the "race" column.
ggplot(data = censusdata, aes(x = relationship, fill = race))+geom_bar()

#i.	Set x-axis label to 'Categories of Relationships'.
#ii.	Set y-axis label to 'Count of Categories'.
ggplot(data = censusdata, aes(x = relationship, fill = sex))+geom_bar()+labs(x = "Categories of Relationships", y = "Count of Categories")

#iv.	Set the position of the bars to "dodge".
ggplot(data = censusdata, aes(x=relationship, fill=sex))+geom_bar(position = "dodge")+ labs(x="Categories of Relationships", y = "Count of Categories")

#v.	Set the title of plot to be 'Distribution of Relationships by Sex"
ggplot(data = censusdata,aes(x = relationship, fill = sex))+geom_bar(position = "dodge")+ labs(x="Categories of Relationships",y="Count of Categories",title = "Distribution of Relationships by Sex")

#b). Build a Histogram for the "age" column with number of bins equal to 50.

ggplot(data = censusdata, aes(x=age))+geom_histogram(bins = 50)

#i)	Fill the bars of the histogram according to yearly income column i.e., "X"
ggplot(data = censusdata,aes(x=age, fill = X))+geom_histogram(bins = 50)

#ii)	Set the title of the plot to "Distribution of Age".
ggplot(data = censusdata,aes(x=age, fill = X))+geom_histogram(bins = 50)+labs(title = "Distribution of Age")

#iii)Set the legend title to "Yearly income".
ggplot(data = censusdata,aes(x=age, fill = X))+geom_histogram(bins = 50)+ labs(title = "Distribution of Age", fill = "Yearly income")

#iv) Set the theme of the plot to black and white.
ggplot(data = censusdata,aes(x=age, fill = X))+geom_histogram(bins = 50)+labs(title = "Distribution of Age", fill = "Yearly income")+theme_bw()


#c)	Build a scatter-plot between "capital.gain" and "hours.per.week". Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.
ggplot(data = censusdata,aes(x = capital.gain, y = hours.per.week ))+geom_point()

#i)	Set the transparency of the points to 40% and size as 2.
ggplot(data = censusdata,aes(x = capital.gain, y = hours.per.week))+geom_point(alpha = 0.6, size = 2)

#ii)	Set the color of the points according to the "X" (yearly income) column. 
ggplot(data = censusdata,aes(x = capital.gain, y = hours.per.week, col = X))+geom_point(alpha = 0.6, size = 2)

#iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title
#to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly Income".
ggplot(data = censusdata,aes(x = capital.gain, y = hours.per.week, col = X))+geom_point(alpha = 0.6,size = 2)+labs(x = "Capital Gain", y="Hours per Week", title = "Capital Gain vs Hours per Week by Income",fill ="Yearly Income")

#d)	Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
#"age" on the y-axis.
ggplot(data = censusdata,aes(x = education, y = age))+geom_boxplot()

#i)	Fill the box-plots according to the "sex" column.
ggplot(data = censusdata,aes(x = education, y = age, fill = sex))+geom_boxplot()

#ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(data = censusdata,aes(x = education, y = age, fill = sex))+geom_boxplot()+labs(title = "Box-Plot of age by Education and Sex")


###############################################################################################################



                                         #4.	Linear Regression:
library("caTools")

#a)	Build a simple linear regression model as follows:

#i)	Divide the dataset into training and test sets in 70:30 ratio.

sample.split(censusdata, SplitRatio = 0.70)->split_tag
split_tag

subset(censusdata, split_tag==T)->train
subset(censusdata, split_tag==F)->test
nrow(train)
nrow(test)

#ii)	Build a linear model on the test set where the dependent variable is
#"hours.per.week" and independent variable is "education.num".
lm(hours.per.week~education.num, data = train)->lmodel1
summary(lmodel1)

#iii)	Predict the values on the train set and find the error in prediction. 
predict(lmodel1,newdata = test)->pred_val
head(pred_val)
cbind(Actual=test$hours.per.week, Predicted=pred_val)->final_data
View(final_data)  
  
#iv)Find the root-mean-square error (RMSE).
as.data.frame(final_data)->final_data
final_data$Actual - final_data$Predicted ->error
cbind(final_data,error)->final_data
head(final_data)
sqrt(mean((final_data$error)^2))


#################################################################################################################



                                            #5.	Logistic Regression:
library("caTools")
library("ROCR")
library("caret")

#a)	Build a simple logistic regression model as follows:
#i)	Divide the dataset into training and test sets in 65:35 ratio.
sample.split(censusdata, SplitRatio = 0.65)->split_tag1
split_tag1
subset(censusdata, split_tag1==T)->train1
subset(censusdata, split_tag1==F)->test1
nrow(train1)
nrow(test1)
View(censusdata)
#ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variable is "occupation".
glm(X~occupation, data = train1, family = "binomial")->log_mod
summary(log_mod)

#iii)	Predict the values on the test set.
predict(log_mod, newdata = test1, type = "response")->pred_val1
head(pred_val1)
range(pred_val1)

#iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off.
prediction(pred_val1,test1$X)->pred_log
performance(pred_log,"acc")->acc
plot(acc)
table(censusdata$X)
ifelse(pred_val1 > 0.47, ">50k", "<=50k")->lm.pred

#v)	Build a confusion matrix and find the accuracy.
table(lm.pred,test1$X)->tab
tab
sum(diag(tab))/sum(tab)

#vi)	Plot the ROC curve and find the auc(Area Under Curve).
performance(pred_log,"tpr","fpr")->roc_curve
plot(roc_curve)
plot(roc_curve, colorize=T)
performance(pred_log, "auc")->auc
View(auc)

#b)Build a multiple logistic regression model as follows:
#i)	Divide the dataset into training and test sets in 80:20 ratio.
sample.split(censusdata,SplitRatio = 0.80)->split_tag2
split_tag2
subset(censusdata, split_tag2==T)->train2
subset(censusdata, split_tag2==F)->test2
nrow(train2)
nrow(test2)

#ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variables are "age", "workclass", and "education".
glm(X~age+workclass+education, data = train2, family = "binomial")->log_mod1
summary(log_mod1)


#iii)	Predict the values on the test set.
predict(log_mod1, newdata = test2, type = "response")->pred_val2
head(pred_val2)
range(pred_val2)

#iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off. 
prediction(pred_val2,test2$X)->pred_log1
performance(pred_log1, "acc")->acc
plot(acc)
table(censusdata$X) 
ifelse(pred_val2>0.45,">50K","<=50K")-> lm.pred1
lm.pred1

#v)Build a confusion matrix and find the accuracy.
table(lm.pred1,test2$X)->tab1
tab1
sum(diag(tab1))/sum(tab1)

#vi)Plot the ROC curve and calculate the auc(Area Under Curve).
performance(pred_log1,"tpr","fpr")->roc_curve1
plot(roc_curve1)
plot(roc_curve1,colorize=T)
performance(pred_log1,"auc")->auc
View(auc)


#################################################################################################################


                                             #6.	Decision Tree:

library("caTools")
library("rpart")
library("tree")
library("rpart.plot")

#a)	Build a decision tree model as follows:
#i)	Divide the dataset into training and test sets in 70:30 ratio.
sample.split(censusdata, SplitRatio = 0.70)->split_tag3
split_tag3
subset(censusdata, split_tag3==T)->train3
subset(censusdata, split_tag3==F)->test3
nrow(train3)
nrow(test3)

#ii)	Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables.
rpart(formula = X~., data = train3, method = "class")->mod_tree
mod_tree

#iii)	Plot the decision tree.
rpart.plot(x=mod_tree, type = 5,tweak = 1.5)

#iv)	Predict the values on the test set.
predict(mod_tree,newdata = test3,type = "class")->result
class(result)

#v)	Build a confusion matrix and calculate the accuracy.
table(test3$X,result)->cm
cm
sum(diag(cm))/sum(cm)->acc
acc


#################################################################################################################


                                              #7.	Random Forest:

library("randomForest")
library("caTools")

#a)	Build a random forest model as follows:
#i)	Divide the dataset into training and test sets in 80:20 ratio.
sample.split(censusdata, SplitRatio = 0.80)->split_tag4
split_tag4
subset(censusdata, split_tag4==T)->train4
subset(censusdata,split_tag4==F)->test4
nrow(train4)
nrow(test4)

#ii)	Build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
randomForest(X~.,data = train4,mtry=2,ntree=300)->mod_forest
View(mod_forest)
importance(mod_forest)
varImpPlot(mod_forest)

#iii)	Predict values on the test set.
predict(mod_forest,newdata = test4,type = "class")->result_forest
head(result_forest,10)

#iv)	Build a confusion matrix and calculate the accuracy.
table(Actual=test4$X, Predicted = result_forest)->cm1
cm1
sum(diag(cm1))/sum(cm1)->acc1
acc1

