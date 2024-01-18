#Experiment 1
#Correlation Analysis using scatter diagram, Karl Pearson's correlation 
#coefficient and drawing inferences for a given dataset

iris
library(tidyverse)
ggplot(iris,aes(x=Sepal.Width,y=Sepal.Length))+facet_wrap(~Species,scales="free_x")+
  geom_point()+geom_smooth(formula = y~x,method="lm",se = FALSE)+
  labs(x="Sepal Width",y="Sepal Length",title="Sepal Width vs. Sepal Length",subtitle="Grouped by species")

#Correlation Coefficient
my_data=mtcars
my_data
cor_1=cor.test(my_data$wt,my_data$mpg,method = "pearson")
cor_1

#H0: rho=0 vs H1: rho !=0
#As p-value is less than 0.05, we reject Ho
#There is a significant linear relationship between the variables
#r=-0.86765

#Correlation Matrix
my_data=mtcars[,c(1,3,4,5)]
#my_data
head(my_data,5)
cor_2=round(cor(my_data,method="pearson"),4)
cor_2

#Visualizing correlation matrix
#Correlogram
library(corrplot)
my_data=mtcars[,c(1,3,4,5)]
cor_3=corrplot(cor_2,type="upper",order="hclust",tl.col="black",tl.srt = 45)

#Performance Matrix
library(PerformanceAnalytics)
cor_4=chart.Correlation(my_data,histogram=T,method="pearson")

#Symbolic Representation
cor_5=cor(mtcars[,c(1,3,4,5)])
cor_5
symnum(cor_5,abbr.colnames = F)


#GG correlation plot
cor_6=round(cor(my_data),4)
cor_6
library(ggcorrplot)
ggcorrplot(cor_6)
ggcorrplot(cor_6,hc.order = T,type="lower",lab = T)


#-----------------------------------#
#             Exercise              #
#-----------------------------------#

# Use the inbuilt data set "trees" and performance the following:
trees_data <- trees
trees_data

# libraries used
library(ggplot2)                # plot()
library(corrplot)               # corrplot()
library(PerformanceAnalytics)   # chart.Correlation()
library(ggcorrplot)             # ggcorrplot()

# (i) Obtain the scatter diagram for the attributes Girth and Height. Interpret your findings.
scatter.smooth(trees_data$Girth, trees_data$Height)

plot(trees_data$Girth, trees_data$Height, main = "Trees Height vs Trees Girth", xlab = "Girth", ylab = "Height")
abline(lm(trees_data$Height~trees_data$Girth))

summary(lm(trees_data$Height~trees_data$Girth))

# (ii) Obtain the correlation between the attributes Height and Volume. Interpret your output.
cor_height_vol <- cor.test(trees_data$Height, trees_data$Volume, method = "pearson")
cor_height_vol

# (iii) Obtain the correlation matrix for the given dataset by restricting to 5 rows and perform various plots for the correlation matrix.
partial_trees_data <- head(trees_data,5)
partial_trees_data

corr_matrix <- round(cor(partial_trees_data),4)
corr_matrix

corrplot(corr_matrix,type="lower",order="hclust",tl.col="black",tl.srt = 45)
chart.Correlation(corr_matrix,histogram=T,method="pearson")
ggcorrplot(corr_matrix,hc.order = T,type="lower",lab = T)
