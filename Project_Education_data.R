library(GGally)
library(ggplot2)
library(gridExtra)
library(car)

# Load data 
ScoreData<-read.csv(file.choose(), header=TRUE)
str(ScoreData)
View(ScoreData)

# Change variable(s) to factor
ScoreData$school.id <- as.factor(ScoreData$school.id)
ScoreData$employ <- as.factor(ScoreData$employ)
ScoreData$ID <- as.factor(ScoreData$ID)
ScoreData$grade <- as.factor(ScoreData$grade)
#str(ScoreData)

# Attach data set 
attach(ScoreData)

# Summary Statistics 
summary(ScoreData)

# Reorder columns 
ScoreData <- ScoreData[c(1,5,4,6,7,2,8,3)]
#View(ScoreData)

# Pairwise Scatterplots using ggplot2
ggpairs(ScoreData[c(-1,-2)]) + ggtitle("Pairwise Scatterplots")

# Ques 1a: x=english, y=t.score, based on employ
g1 <- ggplot(ScoreData, aes(x=english, y=t.score)) + 
  labs(title="Plot of Test Score by English Score", x="English portion of score", y="test score")
geng0 <- g1 + geom_point() + geom_smooth(method="loess")
geng <- g1 + geom_point() + geom_smooth(method="loess") + 
  facet_grid(.~employ) + ggtitle("Plot of Test Score by English Score based on Parents' Employment")

# Ques 1b: x=math, y=t.score, based on employ
g11 <- ggplot(ScoreData, aes(x=math, y=t.score)) + 
  labs(title="Plot of Test Score by Math Score", x="math portion of score", y="test score")
gmath0 <- g11 + geom_point() + geom_smooth(method="loess")
gmath <- g11 + geom_point() + geom_smooth(method="loess") + 
  facet_grid(.~employ) + ggtitle("Plot of Test Score by Math Score based on Parents' Employment")

# Combine Plots of Test Score by English Score and by Math Score
grid.arrange(geng0, gmath0, nrow=2)
grid.arrange(geng, gmath, nrow=2)

# Ques 2: x=grade, y=t.score, based on employ
g2 <- ggplot(ScoreData, aes(x=grade, y=t.score, color=grade)) + 
  labs(title="Plot of Test Score by Grade", y="test score")
g2 + geom_jitter()
g2 + geom_jitter() + facet_grid(.~employ) + 
  ggtitle("Plot of Test Score by Grade based on Parents' Employment")
g2 + geom_boxplot()
g2 + geom_boxplot() + facet_grid(.~employ) +
  ggtitle("Plot of Test Score by Grade based on Parents' Employment")

# Ques 3: x=gender, y=t.score, based on employ
g3 <- ggplot(ScoreData, aes(x=gender, y=t.score, color=gender)) + 
  labs(title="Plot of Test Score by Gender", y="test score")
g3 + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=0.5) + 
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")
g3 + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=0.5) + 
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") + 
  facet_wrap(~employ) + 
  ggtitle("Plot of Test Score by Gender based on Parents' Employment")

# Fit simple linear regression model for Ques 1
lmeng <- lm(t.score~english)
lmmath <- lm(t.score~math)
summary(lmeng, type=3)   
summary(lmmath, type=3)  

lmengc <- lm(t.score~english+employ)
lmmathc <- lm(t.score~math+employ)
Anova(lmengc, type=3)    
Anova(lmmathc, type=3)   

# Fit simple linear regression model for Ques 2
lmgrade <- lm(t.score~grade)
lmgradec <- lm(t.score~grade+employ)
Anova(lmgrade, type=3)   
Anova(lmgradec, type=3)  

# Fit simple linear regression model for Ques 3
lmgender <- lm(t.score~gender)
lmgenderc <- lm(t.score~gender+employ)
Anova(lmgender, type=3)  
Anova(lmgenderc, type=3) 
