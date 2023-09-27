
# Data simulation
# The following codes create a dataset containing five
# variables, including a binary mortality outcome mort.y, two
# factor variables and two numeric variables. The smoking
# variable has three levels “never”, “ever” and “smoking”.
# The gender variable has two levels “male” and “female”.


{r}
library(dummies)   #1
set.seed(666)   #2
n <- 1500   #3
lac<- round(rnorm(n,mean=5,sd=1.1),1)   #4
age<- round(rnorm(n,mean=67,sd=10))   #5
smoking<- as.factor(sample(x=c("never","ever","smoking"),   #6
size=n,   #7
replace=TRUE,   #8
prob=c(0.5,0.3,0.2)))   #9
smoking<-relevel(smoking,ref="never")   #10
gender<- as.factor(sample(x = c("male","female"),   #11
size = n,   #12
replace = TRUE,   #13
prob = c(60,40)))   #14
lp<-cbind(1,dummy(smoking)[,-1]) %*% c(0.07,1.5,3.2)+   #15
cbind(1, dummy(gender)[, -1]) %*% c(0.07,1.5)-   #16
0.2*age+0.003*age^2+   #17
3*lac-0.25*lac^2-11   #18
pi.x<- exp(lp) /(1 + exp(lp))   #19
mort.y <- rbinom( n = n, size = 1, prob = pi.x)   #20
df <- data.frame(mort.y, smoking, gender,lac,age)   #21
df$dataset<-sample(x=c("train","validate"),   #22
size=n,   #23
replace=TRUE,   #24
prob=c(0.75,0.25))   #25
{r}


# This code separates the numeric variables from factor variables because they
# would be treated differently. It then uses a for loop to generate LOESS
# smoothing curves for each numeric variable in the training dataset. The
# curves are saved as jpeg images in a specified directory.

df.cont<- df[df$dataset=="train", c("mort.y","lac","age")]   #1
df.cat<- df[df$dataset=="train", c("smoking","gender")]   #2
ymark<-seq(0,1,0.1)   #3
library(ggplot2)   #4
for(var in names(df.cont)[-1]){   #5
  xvar<-seq(min(df.cont[,var]), max(df.cont[,var]), length.out=10)   #6
  mypath <- file.path("/Users/apple/score", paste(paste("plot",var,sep = "_"), "jpg", sep = "."))   #7
  jpeg(mypath)   #8
  ggplot(df.cont, aes_string(var, "mort.y")) + geom_point() + stat_smooth(method = "loess") + scale_y_continuous(breaks = ymark) + theme_bw()   #9
  dev.off()   #10
}


# This code defines cut points and base references for the variables 'age'
# and 'lac'. It then converts the continuous variables to categorical variables
# based on the defined cut points. The base reference is set to the range with
# the lowest mortality risk.

agecut<-c(50,60,70,75,80)   #1
ageb<-"[40,50]"   #2
laccut<-c(3,3.7,4.7,6.6)   #3
lacb<-"[1.5,3]"   #4
cont.to.cat<-data.frame(id=c(1:nrow(df.cont)))   #5
for (var in names(df.cont)[-1]) {   #6
  cat<-cut(df.cont[,var],   #7
  breaks=c(min(df.cont[,var]),   #8
  get(paste(var,"cut",sep="")),   #9
  max(df.cont[,var])),include.lowest= TRUE)   #10
  cat<-relevel(cat,ref=get(paste(var,"b",sep="")))   #11
  cont.to.cat<-cbind(cont.to.cat,cat)   #12
}   #13



# This code renames the factor variables that have just been converted from
# numeric variables. Then it combines the converted factor variables and
# original categorical variables to form a data frame that will be used in
# logistic regression model. The logistic regression model is fit with glm
# () function. The scores for each level are obtained by rounding the
# coefficients of the logistic regression model.

df.cont.to.cat<-cont.to.cat[,-1]   #1
names(df.cont.to.cat)<-names(df.cont)[-1]   #2
df.final<-cbind(cbind(df.cat,df.cont.to.cat),   #3
mort.y=df.cont$mort.y)   #4
mod<-glm(mort.y~.,   #5
df.final, family="binomial")   #6
score<-round(coef(mod)[-1])   #7
score.cat<-score[1:3]   #8
score.cont<-score[4:length(score)]   #9


# This code is manipulating strings in R to handle the variable names in the
# score.cont vector. It reduces the names in score.cont to their original
# variable names, assigns scores to each interval of the numeric variables, and
# replaces NA with 0.

library(stringr)   #1  
var.cont<-as.character(1)   #2  
for(var in names(score.cont)){   #3  
  var.red<-sub("(\\(|\\[)[0-9]+.*", "", var)   #4  
  var.cont<-c(var.cont,var.red)   #5  
}   #6  
var.cont<-unique(var.cont)[-1]   #7  
for(var in var.cont){   #8  
  df[,paste(var,"points",sep=".")]<-as.numeric(NA)   #9  
}   #10  
for (var in names(score.cont)){   #11  
  var.red<-sub("(\\(|\\[)[0-9]+.*", "", var)   #12  
  var.low<-as.numeric(str_extract(var,'(?<=(\\(|\\[))[0-9]+\\.*[0-9]*(?=\\,)'))   #13  
  var.upper<-as.numeric(str_extract(var,'(?<=\\,)[0-9]+\\.*[0-9]*(?=\\])'))   #14  
  df[,paste(var.red,"points",sep=".")]<-ifelse(df[,var.red]<=var.upper&df[,var.red]>=var.low, score[var],df[,paste(var.red,"points",sep=".")])   #15  
}   #16  
for(var in var.cont){   #17  
  df[,paste(var,"points",sep=".")]<-ifelse(is.na(df[,paste(var,"points",sep=".")]),0,df[,paste(var,"points",sep=".")])   #18  
}   #19



# This code calculates scores for factor variables in a similar way to numeric
# variables. It assigns scores to each level of the factor variables and
# replaces NA with 0. Finally, it sums all the points to get a total score for
# each observation.


var.cat<-names(df.cat) for(var in var.cat){ df[,paste
(var,"points",sep=".")]<-as.numeric(NA) }

for(var in names(score.cat)){   #1  
  df[,paste(var,"points",sep=".")]<-as.numeric(NA)   #2  
}   #3  
for(var in names(score.cat)){   #4  
  for(level in levels(df[,var])){   #5  
    df[,paste(var,"points",sep=".")]<-ifelse(df[,var]==level, score[paste(var,level,sep=".")],df[,paste(var,"points",sep=".")])   #6  
  }   #7  
}   #8  
for(var in names(score.cat)){   #9  
  df[,paste(var,"points",sep=".")]<-ifelse(is.na(df[,paste(var,"points",sep=".")]),0,df[,paste(var,"points",sep=".")])   #10  
}   #11  

df$score<-rowSums(df[,grepl("\\.+points",names(df))])   #12  
head(df[,7:11])   #13


# This code fits a logistic regression model using the glm() function
# with "binomial" as the family argument. It then predicts the probability of
# outcome events given the scores of individual patients. The predicted
# probabilities and the number of survivors and non-survivors for each score
# value are stored for further use.

glmod<-glm(mort.y~score,   #1  
df[df$dataset=="train",],   #2  
family="binomial")   #3  
newx<-seq(min(df[df$dataset=="train",]$score),   #4  
max(df[df$dataset=="train",]$score))   #5  
prd<-predict(glmod,   #6  
newdata=data.frame(score=newx),   #7  
type="response",   #8  
se.fit=T)   #9  
count<-as.matrix(table(cut(df[df$dataset=="train",]$score,   #10  
breaks=seq(min(df[df$dataset=="train",]$score),   #11  
max(df[df$dataset=="train",]$score)),   #12  
include.lowest = T),   #13  
df[df$dataset=="train",]$mort.y))   #14  


# This code generates a bar plot to show the number of survivors and
# non-survivors, stratified by scores. It also depicts the predicted
# probability of outcome events on the same plot. The function par() is used to
# set graphical parameters so that the number of lines on four sides of the
# plot can be specified.


par(mar=c(5,4,4,5)+.1)   #1  
barplot(t(count),   #2  
main="Scores versus probability of death",   #3  
xlab="Scores",   #4  
ylab="Observed number of patients",   #5  
space=0,   #6  
col=c("yellow","lightblue"))   #7  
legend("topleft",fill=c("yellow","lightblue",NA),   #8  
lty = c(NA,NA,1),lwd=c(NA,NA,2),   #9  
legend=c("Survivors","Non-survivors","Predicted Prob."),   #10  
col=c("black"),   #11  
border = c("black","black",NA))   #12  
par(new=TRUE)   #14  
plot(prd$fit~newx,   #15  
type="l",col="black",   #16  
lwd=2,xaxt="n",yaxt="n",   #17  
xlab="",ylab="")   #18  
polygon(c(rev(newx), newx),   #19  
c(rev(prd$fit+1.96*prd$se.fit),prd$fit-1.96*prd$se.fit),   #20  
col = adjustcolor('grey80',alpha=0.5),   #22  
border = NA)   #23  
lines(newx, prd$fit+1.96*prd$se.fit,   #24  
lty = 'dashed', col = 'red')   #25  
lines(newx, prd$fit-1.96*prd$se.fit,   #26  
lty = 'dashed', col = 'red')   #27  
axis(4)   #28  
mtext("Predicted probability of death",side=4,line=3)   #30  



# This code uses the rms package for the evaluation of model validation. The
# datadist() function determines summaries of variables for effect and plotting
# ranges. The distribution summaries for all variables are stored before model
# fit, and are applicable for subsequent plots. Logistic regression model is
# fit for the training dataset with lrm() function. The predict() function
# returns predicted probability for the validation cohort. The val.prob
# () function is used to validate predicted probability against binary
# outcomes.

# Part 10
library(rms)   #1  
ddist <- datadist(df)   #2  
options(datadist='ddist')   #3  
f.score<-lrm(mort.y~score,   #4  
df[df$dataset=="train",],   #5  
x=TRUE,y=TRUE)   #6  
phat.score<-predict(f.score,   #7  
df[df$dataset=="validate",],   #8  
type="fitted")   #9  
v.score<-val.prob(phat.score,   #10  
df[df$dataset=="validate",]$mort.y,   #11  
m=20)   #12  
