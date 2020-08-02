#Initialize Libraries
library(ggplot2)
library(stargazer)
library(dplyr)
library(corrplot)
#Session-Set Working Directory- To Source File Location

#Load the data (included with R)
data(mtcars)

#Explore the dataset
str(mtcars) #transform relevant numerics into factors! 

#Clean variables
mtcars$model <- row.names(mtcars)
mtcars[mtcars$am == 1,]$am <- "Manual"
mtcars[mtcars$am == 0,]$am <- "Automatic"
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <- as.factor(mtcars$cyl)
str(mtcars)

#Transform variables
mtcars <- mtcars %>%
  mutate(fcon = mpg / 2.352)%>% #creates new variable
  mutate(wt = wt / 2.205) #overwrites a variable

#Descriptive Statistics (saved in directory)
stargazer(mtcars, type = "text", title="Descriptive statistics", digits=1, out="descriptives.txt")

#Select continuous variables
cars_cont <- mtcars[,c("fcon", "hp", "wt", "qsec", "disp"
                       ,"drat")]
res <- cor(cars_cont)
res <- round(res, 2) #round to two decimals

#Correlogram
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 50)

#Save as CSV in Directory
res <- as.data.frame(res)
write.csv(res, "correlations.csv")

#Select relevant variables (no multicolinearity)
mydata <- mtcars[,c("fcon", "cyl", "hp", "wt", "am", "model")]

#Explore relationships between variables
xtabs(~ am + cyl, data=mydata)
pairs(mydata[,c("fcon", "hp", "wt", "cyl")], lower.panel = NULL)

#Scatter Plot
mydata %>%
  ggplot(aes(x= wt, y=fcon))+
  geom_point(aes(color=am, size=cyl, shape=cyl))+
  geom_text(aes(label= ifelse(wt == max(wt), as.character(model), "")),hjust=1, vjust=0) +
  geom_text(aes(label= ifelse(wt == min(wt), as.character(model), "")),hjust=0, vjust=0) +
  geom_smooth(method=lm)+
  geom_vline(xintercept = mean(mydata$wt), linetype = "dashed")+
  geom_hline(yintercept = mean(mydata$likm), linetype = "dotted")+
  labs(x="Weight (tons)", y="Km. per Liter")
  #save in working directory as PNG

#Lineal Models
m1 <- lm(fcon~wt, data = mydata)
summary(m1)
m2 <- lm(fcon~wt+hp, data = mydata)
m3 <- lm(fcon~wt+hp+am, data = mydata)
m4 <- lm(fcon~wt+hp+am+cyl, data = mydata)
summary(m4)

#Model Results

stargazer(m1, m2, m3, m4, type="text",
            title="Fuel Economy",
            dep.var.labels=c("Km. per Liter"),
            covariate.labels=c("Weight (kg)", "Gross horsepower", "Type of transmission (manual=1)",
                             "Number of Cylinders"), out="models.txt")

#######################  In Latex  ########################

stargazer(m1, m2, m3, m4, 
          title="Fuel Economy",
          dep.var.labels=c("Km. per Liter"),
          covariate.labels=c("Weight (kg)", "Gross horsepower", "Type of transmission (manual=1)",
                             "Number of Cylinders"))
stargazer(mtcars, title="Descriptive statistics", digits=1)
