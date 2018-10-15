STLFSI.df <- read.csv(file.path("C:/Users/Noe/Downloads/STLFSI.csv"),sep=",")
SP500.df <- read.csv(file.path("C:/Users/Noe/Downloads/SP500.csv"),sep=",")

#1) Working with dates in R is not particularly easy. In order to make working with dates easier 
#let’s begin by installing the lubridate package
# install the lubridate package;
install.packages('lubridate', dependencies=TRUE)

#(2) Let’s also make our file reads easier by defining some file parameters
my.path <- 'C:/Users/Noe/Downloads/';
file.1 <- paste(my.path,'STLFSI.csv',sep='');
file.2 <- paste(my.path,'SP500.csv',sep='');

#(3) Read in the STLFSI data using read.csv(). In addition we need to create an R date object for 
#the date. Notice that when we read in the file, the field DATE is a string, not a date. 
#We will also create three new dates +1, +2, +3 months in the future using the lubridate package
fsi <- read.csv(file.1);
head(fsi)
tail(fsi)
str(fsi)

sp <- read.csv(file.2);
head(sp)
tail(sp)
str(sp)


# Create an R date from the field DATE;
fsi$RDATE0 <- as.Date(fsi$DATE,'%m/%d/%Y');
str(fsi)
head(fsi)

# Increment dates using the lubridate package;
library(lubridate)

# create R date plus 1 month
fsi$RDATE1 <- fsi$RDATE0 + months(1);
# create R date plus 2 months
fsi$RDATE2 <- fsi$RDATE0 + months(2);
# create R date plus 3 months
fsi$RDATE3 <- fsi$RDATE0 + months(3);
# Check the dates;
head(fsi)

#(4) Now let’s read in the S&P 500 data. Just like we did with the STLFSI data, we will use the 
#DATE field to create the R date RDATE. In addition we will create the variables SP500_0, SP500_1, 
#SP500_2, SP500_3. Can you guess why we need these fields?
sp500 <- read.csv(file.2);
head(sp500)
str(sp500)

# create an R date from the date string;
sp500$RDATE <- as.Date(sp500$DATE,'%m/%d/%Y');
# Add replicate columns for merging;
sp500$SP500_0 <- sp500$SP500;
sp500$SP500_1 <- sp500$SP500;
sp500$SP500_2 <- sp500$SP500;
sp500$SP500_3 <- sp500$SP500;

# Check your dataframe and its components
str(sp500)
head(sp500)

#(5) Use the merge() function to perform repeated merging of the data sets. Note that the merge() 
#function will produce ‘inner joins’.
merge.0 <- merge(x=fsi,y=sp500[,c('RDATE','SP500_0')],by.x='RDATE0',by.y='RDATE');
head(merge.0)

merge.1 <- merge(x=merge.0,y=sp500[,c('RDATE','SP500_1')],by.x='RDATE1',by.y='RDATE');
head(merge.1)

merge.2 <- merge(x=merge.1,y=sp500[,c('RDATE','SP500_2')],by.x='RDATE2',by.y='RDATE');
head(merge.2)

merge.3 <- merge(x=merge.2,y=sp500[,c('RDATE','SP500_3')],by.x='RDATE3',by.y='RDATE');
head(merge.3)

# Compare the merge results to the original SP500 file;
head(sp500)

#Compare the results from the merges to the original SP500 file. Do the S&P 500 values seem to be 
#in the correct place? For each RDATE0 we should have the S&P 500 value for RDATE0, RDATE1, RDATE2,
#and RDATE3. We can use these future S&P 500 values to compute the returns over these windows. 
#These will be our response variables. They are defined to have forward looking 
#performance windows of +1, +2, and +3 months.


#(6) Finish the data build by computing the log-returns over the performance windows. 
#We need to have some returns to validate the STLFSI.
returns.df <- merge.3;
returns.df$R1 <- log(returns.df$SP500_1)-log(returns.df$SP500_0);
returns.df$R2 <- log(returns.df$SP500_2)-log(returns.df$SP500_0);
returns.df$R3 <- log(returns.df$SP500_3)-log(returns.df$SP500_0);
returns.df$minR <- pmin(returns.df$R1,returns.df$R2,returns.df$R3);

##Check
head(returns.df)

#We now have four different returns computed over the 1-3 month performance windows.
#R Note: Do we know how the pmin() and pmax() functions operate? How do they differ from 
#the functions min() and max()? Why did we use pmin() instead of min()?

#Part 2: Exploratory Data Analysis
#Now that we have a basic data set built, we will begin our exploratory data analysis. 
#What types of EDA might be useful on this type of data? What might we be trying to show?
  
#(7) Let’s begin by making a panel of boxplots.

# Discretize index values;
returns.df$discreteSTL <- cut(returns.df$STLFSI,breaks=c(-2,-1,0,1,2,3,4,5));
# Use par(mfrow) to make a panel of plots in R;
par(mfrow=c(1,4))
boxplot(R1 ~ discreteSTL,data=returns.df, ylim=c(-0.4,0.2), col = "lightblue", main='1 Month Return');
boxplot(R2 ~ discreteSTL,data=returns.df, ylim=c(-0.4,0.2), col = "lightblue",main='2 Month Return');
boxplot(R3 ~ discreteSTL,data=returns.df, ylim=c(-0.4,0.2), col = "lightblue",main='3 Month Return');
boxplot(minR ~ discreteSTL,data=returns.df, ylim=c(-0.4,0.2), col = "lightblue",main='Min Return');

#Is this panel of boxplots particularly useful? What does it show? What does it not show? 
#What might we want to know that we cannot discern from these boxplots?

#(8) Make a Table: For simple data sets tables still help you understand the data regardless of the
#number of observations. Intuitively, 
#we might think that higher STLFSI values should predict declines in the S&P 500 Index. 
#We might make a graph, but our data set is so simple that a table would be far more precise 
#than a graph. From this table we can see where the majority of our data lies, 
#and we can compute empirical probabilities.

# Create indicator variables for positive returns;
returns.df$drop5.R1 <- ifelse(-0.05 > returns.df$R1,1,0);
returns.df$drop5.R2 <- ifelse(-0.05 > returns.df$R2,1,0);
returns.df$drop5.R3 <- ifelse(-0.05 > returns.df$R3,1,0);
returns.df$drop5.minR <- ifelse(-0.05 > returns.df$minR,1,0);
# Note: Cannot have returns.df$R1 < -0.05;
# R will interpret as the assignment operator!
# returns.df$drop5.minR <- ifelse(returns.df$minR<-0.05,1,0);
head(returns.df)

# Cross-tab of drop5.R1 and discreteSTL;
table(returns.df$drop5.R1,returns.df$discreteSTL)
table(returns.df$discreteSTL)
# drop5.R1 In percent format;
table(returns.df$drop5.R1,returns.df$discreteSTL)/rbind(table(returns.df$discreteSTL),
                                                        table(returns.df$discreteSTL))
# drop5.R2 In percent format;
table(returns.df$drop5.R2,returns.df$discreteSTL)/rbind(table(returns.df$discreteSTL),
                                                        table(returns.df$discreteSTL))
# drop5.R3 In percent format;
table(returns.df$drop5.R3,returns.df$discreteSTL)/rbind(table(returns.df$discreteSTL),
                                                        table(returns.df$discreteSTL))
# drop5.minR In percent format;
table(returns.df$drop5.minR,returns.df$discreteSTL)/rbind(table(returns.df$discreteSTL),
                                                          table(returns.df$discreteSTL))
# Cross-tab of drop5.R1 and discreteSTL;
table(returns.df$drop5.minR,returns.df$discreteSTL)


#####################################################################Part 3: Model Building
#Let’s now consider three different models for modeling STLFSI. Here are some code snippets that 
#define the three models. We have also included some hints about the models.
model.1 <- glm(drop5.minR ~ STLFSI, family=binomial, data=returns.df)
summary(model.1)

# What information is saved in model.1?
names(model.1)

returns.df$model1.scores <- model.1$fitted.values;
aggregate(x=returns.df$model1.scores, by=list(discreteSTL=returns.df$discreteSTL), FUN=mean)
aggregate(x=returns.df$drop5.minR, by=list(discreteSTL=returns.df$discreteSTL), FUN=mean)

#######################################################################################Model #2
model.2 <- glm(drop5.minR ~ discreteSTL, family=binomial, data=returns.df)
summary(model.2)

# What information is saved in model.1?
names(model.2)


model.2$coef
XB <- model.2$coef[1] + c(0,model.2$coef[2:7]);
pi <- exp(XB)/(1+exp(XB));

#######################################################################################Model #3
model.3 <- lm(drop5.minR ~ STLFSI, data=returns.df)
summary(model.3)

model.3$coef
XB1 <- model.3$coef[1] + c(0,model.3$coef[2:7]);
pi2 <- exp(XB1)/(1+exp(XB1));

returns.df$model.3.scores <- model.3$fitted.values;
aggregate(x=returns.df$model.3.scores, by=list(discreteSTL=returns.df$discreteSTL), FUN=mean)
aggregate(x=returns.df$drop5.minR, by=list(discreteSTL=returns.df$discreteSTL), FUN=mean)

plot(model.1$residuals)


plot(model.1$fitted.values)
