
setwd("~/FALL 2022/STAT 5474- DATA MINING")#My current working directory
baseball <-read.table(file="baseball.dat",header = T, sep = ",");colnames(baseball)=c("salary", "batting.avg", "OBP", "runs", "hits",
                                                          "doubles", "triples", "homeruns", "RBI", "walks", "strike.outs",
                                                          "stolen.bases", "errors", "free.agency.elig", "free.agent.91",
                                                          "arb.elig", "arb.91", "name")

head(baseball)
dim(baseball)#The data has 18 dimension(variables) with 337 observations

#(1)--a. EDA - DATA PREPARATION-Obtain the histograms of both salary
hist(baseball$salary, xlab = "Salary ( thousands of dollars)",
     main = "Histogram of 1992 baseball Salary")#The result obtained shows the
#positively skewed of the data as seen below

# Take the logarithm (natural base) of salary
baseball[,1] <-log(baseball$salary)
head(baseball)
tail(baseball)

#Histogram  of log transformation of salary
hist(baseball$salary, xlab = "Log transformation of Salary",
     main = "Histogram of  Log (Salary)")#The log transformation 
#of histogram plot below shows the plot is nearly skewed even though the plot(itself)
#is somehow flat

#--- b.Inspect the data and answer these questions: 
missing_data <- data.frame()
row_numbr<- nrow(baseball)
column_numbr <- ncol(baseball)
Variable_name <- variable.names(baseball)
for (k in 1:column_numbr) {
  df<- is.null((baseball[,k]))
  na_prcnt <- (df/row_numbr)*100 #percentage of missing value
  result <- list(missing_value= df, percentage_missing = na_prcnt, Variable = Variable_name[k])
  missing_data <- rbind(missing_data, result, stringsAsFactors = F)
}
missing_data
# The result below shows the number of missing value percentage
#and this no missing value in the baseball data

#Among all the predictors, how many of them are continuous, 
#integer counts, and categorical, respectively

# i number of integer counts
print("number of integer counts")
sum(sapply(baseball, FUN = is.integer))
#The result obtained below shows 14 integers among
#the predictor

# ii--# number of categorical Predictors
print("Total number categorical predictors are")
sum(sapply(baseball, FUN = is.factor))
#The result obtained shows we have no categorical variable 
#among the predictors

#iii number of continuous predictors in the data
print("Number of continuous variables")
sum(sapply(baseball, FUN = is.double))
#we have three continuous variables in the baseball data

# 2. Linear Regression with Variable Selection:

#(a) Partition the data randomly into two sets:
#the training data D and the test data D0 with a ratio of about 2:1.
set.seed(38311)
 fit_sample<- sample(nrow(baseball), (2.0/3.0)*nrow(baseball), replace = FALSE)
 #Partitioning  into 2:1 
 
# training data
D <- baseball[fit_sample, ] #Training data

# test data
D0 <- baseball[-fit_sample, ]#Test data 

#b) Using the training data D, apply three variable selection methods 
#of your choice and identify your ???best??? models accordingly
fit_data <- (salary ~ batting.avg  + OBP + runs+ hits + doubles + triples + homeruns + RBI + walks+ strike.outs + stolen.bases + arb.91+errors + free.agency.elig + free.agent.91 + arb.elig-1)
y <- D[, all.vars(fit_data)[1]]
X <- model.matrix(as.formula(fit_data),D)
X <- as.data.frame(scale(X, center = TRUE, scale = TRUE));
y <- scale(y, center = TRUE, scale = FALSE);
dat <- as.data.frame(cbind(X, y))	

#THE FULL MODEL WITH ALL PREDICTORS INCLUDED,THE FULL MODEL WITH
#ALL PREDICTORS INCLUDED
fit.full <- lm(fit_data, data=D); 
summary(fit.full)#Looking at the adjusted R-squared , the model fit data 
#with about 98%, overall P-Value is Statistically significant
BIC(fit.full)#using Bayesian Information Criterion
#For small sample size,higher result BIC is not bad even though large sample
# is required to obtain the best result
AIC(fit.full) #Using Akaike Information criterion, smaller result for AIC is not 
# bad even though higher AIC is better for large sample 

n = nrow(D) #considering Bootstrap 
res = predict(fit.full, newdata =D, interval = "none")

bootstrap = replicate(500, mean(res[sample(1:n, n, replace = TRUE)]^2))

c(mean(bootstrap)) + c(-1, 1)*sd(bootstrap)/sqrt(500)


## DETERMINE THE BEST SUBSET SELECTION -

require(bestglm); 
 y <- D[,names(D)==as.character(fit.full)[1]]; # ALTERNATIVE WAY
y <- D[, all.vars(fit_data)[1]]
X <- as.data.frame(model.matrix(as.formula(fit_data),D))
D.tmp <- as.data.frame(cbind(X, y))	
result.bestBIC <- bestglm(Xy=D.tmp, 
                          IC="BIC", intercept=TRUE, TopModels=5);   
# Setting intercept=TRUE means the intercept term is always included
names(result.bestBIC)
result.bestBIC$BestModels  
result.bestBIC$BestModel

fit.subset <- result.bestBIC$BestModel
summary(fit.subset)
beta.hat <- fit.subset$"coefficients"; beta.hat

terms <- (names(beta.hat))[-1]
formula.bestBIC <- as.formula(paste(c("salary ~ ", terms), collapse=" + "))
fit.bestBIC <- lm(formula.bestBIC, data =D)
summary(fit.bestBIC)#The result shows the prediction power we can get without 
#over-parameterization. It means this predictor could be useful without looking 
#at the model if it is a good fit however there is a high possibility of being
#correlated.



# ===============
# LASSO METHOD--1
# ===============

library(ncvreg);
set.seed(38311)
#10-FOLD CV FOR SELECTING THE TUNING PARAMETER
cvfit.L <- cv.ncvreg(X=X,y=y, nfolds=10, family="gaussian", 
                      penalty="lasso", lambda.min=.005, nlambda=100, eps=.001, max.iter=1000) 
# The design matrix X does not need to have an intercept. ncvreg standardizes the data and includes an intercept by default.
plot(cvfit.L)
names(cvfit.L)
beta.hat <- coef(cvfit.L)  # THE LASSO COEFFICIENTS WITH MINIMUM CV ERROR

# NEXT, WE REFIT THE MODEL USING OLS WITH VARIABLES SELECTED BY LASSO
# SINCE LASSO DOES NOT ENJOY THE ORACLE PROPERTY
cutoff <- 0.0001
terms <- names(beta.hat)[abs(beta.hat) > cutoff]
formula.LASSO <- as.formula(paste(c("salary~ ", terms[-1]), collapse=" + "))
fit.L <- lm(formula.LASSO, data = D)
summary(fit.L)# This model performed poorly with the adjusted R-squared approximately
# 77%  which less than 80%  as presume to be the benchmark,we can can not take this into consideration 
#as good predictive model even though the entire data is statistically significant

# =========================
# SCAD & MCP Method---2
# =========================

library(ncvreg); 
cvfit <- cv.ncvreg(X=X,y=y, nfolds=10,
                   family="gaussian", penalty="SCAD") 	# SCAD
# cvfit <- cv.ncvreg(X=X,y=y, nfolds=10,
# family="gaussian", penalty="MCP") 	# MCP
plot(cvfit)
result.SCAD <- cvfit$fit
beta.hat <- as.vector(result.SCAD$beta[-1, cvfit$min])
cutoff <- 0
terms <- colnames(X)[abs(beta.hat) > cutoff]; terms
formula.SCAD <- as.formula(paste(c("salary ~ 1", terms), collapse=" + "))
fit.SCAD <- lm(formula.SCAD, data =D)
summary(fit.SCAD)#The summary of data below shows the adjusted model for about 
#approximately 77% fit data , statistically we require for at least 80%, so it is
#hard to conclude that this model fit the data.

# ===================
# ADAPTIVE LASSO METHOD-3
# ===================
set.seed(38311)
library(MESS)
library(glmnet)
wt <- adaptive.weights(x=X, y=y, weight.method="univariate")
cv.fit1 <- cv.glmnet(x=as.matrix(X), y=y, family="gaussian", alpha=1, nlambda=100,
                    penalty.factor=as.numeric(wt$weights), standardize=FALSE)
plot(cv.fit1)
# beta.hat <- coef(cv.fit, s="lambda.min")	# 0SE
beta.hat <- coef(cv.fit1, s="lambda.1se")  	# 1SE FOR BETTER SELECTION

# AGAIN, LET'S FIT OLS MODEL WITH ALASSO SELECTED VARIABLES
# ALTHOUGH THIS IS UNNECESSARY SINCE ALASSO ENJOYS THE ORACLE PROPERTY
cutoff <- 0
terms <- names(X)[abs(as.vector(beta.hat[-1])) > cutoff]; terms
formula.ALASSO <- as.formula(paste(c("salary~ ", terms), collapse=" + "))
fit.ALASSO <- lm(formula.ALASSO, data =D)
summary(fit.ALASSO)
#The summary of data below shows the adjusted model for about 
#approximately 74% fit data

# ==========================================
# CROSS-VALIDATED ERRORS - MODEL COMPARISON
# ==========================================

summary(fit.L)
summary(fit.SCAD)
summary(fit.ALASSO)
#The summary result show the best variable selection for each model as it is given 
#below , Lasso and Adaptive Lasso seem to fit the model better when compared 
#with SCAD and MCP method

#(c)  Report the essential steps and/or key quantities involved in the variable 
#selection procedure that you choose.               

#i)LASSO METHOD:Least absolute shrinkage and selection operator basically 
#summarizes how Lasso regression works. Lasso does regression analysis using a 
#shrinkage parameter where data are shrunk to a certain central point
#and performs variable selection by forcing the coefficients of ???not-so-significant??? 
#variables to become zero through a penalty.


# ii) SCAP AND MCP METHOD: The method uses convex penalized selection???the LASSO and elastic
#net methods???to select an optimal model. In comparison,It explores model 
#selection by using folded concave penalized selection???the smoothly clipped absolute 
#deviation (SCAD) and minimax concave penalty (MCP)???to select an optimal model.
#The SCAD method elects the model that yields the lowest average square error
#(ASE) value for the validation data.
               

#iii) ALASSO METHOD : Adaptive Lasso is an evolution of the Lasso that has 
#the oracle properties (for a suitable choice of Lambda ).Adaptive Lasso, as a 
#regularization method, avoids over fitting penalizing large
#coefficients. Besides,It has the same advantage that Lasso: it can shrink some 
#of the coefficients to exactly zero, performing thus a selection of attributes with the regularization.
#This method also uses the weight vector, the edge of adaptive lasso, it adjust the 
#penalty differently for each coefficient 

#(d) Output the necessary fitting results for each ???best??? model, e.g., 
#in particular, selected variables and their corresponding slope parameter estimates.
        
#(i)--- FOR LASSO
#Outputting the best fit for the LASSO selections method.
set.seed(38311)
fit_Lasso<- lm(salary~hits + RBI+ triples+strike.outs+
                  + free.agency.elig + free.agent.91 + arb.elig , data=D )
summary(fit_Lasso)# The best fit model for Lasso shows the same result with the
#Initial Lasso which is also statistically significant and approximately 76% for
# the adjusted R-squared

#(ii)--- FOR SCAD AND MCP METHOD
##Outputting the best fit for the SCAD AND MCP selections method.
fit_SCAD_MCP<- lm(salary~ hits + doubles+ triples
        + RBI +walks +strike.outs+ stolen.bases+ free.agency.elig + free.agent.91 + arb.elig , data=D )
summary(fit_SCAD_MCP)# The best fit model for SCAP AND MCP shows the same result with the
#Initial SCAP and MCP which is also statistically significant and approximately 77% for
# the adjusted R-squared

#(iii)--- FOR ADAPTIVE LASSO METHOD
#Outputting the best fit for the ADAPTIVE LASSO selections method.
fit_Adaptive_Lasso <- lm(salary~ RBI+ free.agency.elig + arb.elig, data=D )
summary(fit_Adaptive_Lasso)
#The best fit model for ALASSO looks similar with the adjusted R-squared is approximately 
#75% compare to the initial ALASSO model approximately 75% for adjusted R-square
#and they are both statistically significant 


#(e) Apply your ???best??? models to the test data D0 Output the sum of squared prediction error
#(SSPE). Let???s consider the one yielding the minimum SSPE as the final model

#(a)---SSPE  FOR LASSO

fit_Lasso_D0<- lm(salary~ hits+ RBI +strike.outs+
                   + free.agency.elig + free.agent.91 + arb.elig , data=D )
summary(fit_Lasso_D0)
pred_Lasso_D0<-predict(fit_Lasso_D0,newdata = D0)
head(pred_Lasso_D0)
sum((D0$salary-pred_Lasso_D0)**2)#square prediction error(SSPE) FOR LASSO

#(b)--- SSPE FOR SCAD AND MCP METHOD

fit_SCAD_MCP_D0<- lm(salary~  + hits  
                  + RBI +walks +strike.outs+ stolen.bases + free.agency.elig + free.agent.91 + arb.elig , data=D )
summary(fit_SCAD_MCP_D0)
pred_SCAD_MCP_D0<-predict(fit_SCAD_MCP_D0,newdata = D0)#Test data prediction
head(pred_SCAD_MCP_D0)
sum((D0$salary-pred_SCAD_MCP_D0)**2)#square prediction error(SSPE) FOR SCAD AND MCP



#(c)--- SSPE  FOR ADAPTIVE LASSO METHOD

fit_Adaptive_Lasso_D0 <- lm(salary~RBI  + free.agency.elig + arb.elig, data=D )
summary(fit_Adaptive_Lasso_D0)
pred_Adaptive_Lasso_D0<-predict(fit_Adaptive_Lasso_D0,newdata = D0)#Test data prediction
head(pred_Adaptive_Lasso_D0)
sum((D0$salary-pred_Adaptive_Lasso_D0)**2)#square prediction error(SSPE) FOR ADAPTIVE LASSO

#CONCLUSION: considered the SSPE for each method, Adaptive Lasso turns out to be the 
#best model with the lease SSPE of 23.8937 compare to the two other methods.

#--(3)
#Refit your final model using the entire data, i.e., D ???D0
# Call it fit.final. Provide the output
#from your final model with summary(fit.final)

fit.final<- lm(fit_data, data=baseball)
summary(fit.final)#The final model seems to be a good fit by looking at the adjusted 
# R squared with about 98.45% which shows the model fit the data since we can say for 
# any fit from 80% above.Aside that, the overall p_value  for the  is 
# the model is statistically significant since it is less than 0.05 by default 

#--(4) 
#We next perform model diagnostics on the final model.
#(a)(a) (Check Normality) Obtain the studentized jackknife residuals and check if they follow
#the standard normal distribution

# =============================
# I. ASSUMPTION CHECKING
# =============================

# OBTAIN THE STUDENTIZED JACKKNIFE RESIDUALS 
r.jack <- rstudent(fit.final)
# NORMALITY
# -----------

par(mfrow=c(1,2),mar=c(8,4,8,4)) 
# The fisrt plot: Histogram 
hist(r.jack, xlab="Jackknife Residual", col="green4",
     main="(a) Histogram") 
#Histogram plot can be relatively conclude as being close to Normal distribution
#with most of the data mostly centered
# USING PACKAGE {car}
# install.packages("car")
library(car)
# A fancier qq plot for studentized jackknife residuals 
qqPlot(fit.final, pch=19, cex=.8, col="blue", main="(b) Q-Q Plot") 
#The result obtained from the Q-Q plot violated normality assumption with a slight
# deviation and obviously there is presence of outliers
# THE SHAPIRO-WILKS NORMALITY TEST: A LARGE P-VALUE WOULD JUSTIFY NORMALITY
shapiro.test(r.jack) 
# The Shapiro test shows the statistical significant of the final model as well

# (Check Homoscedasticity) Plot Absolute Jackknife Residuals vs. Fitted values using the R
#function spreadLevelPlot() in Package {car}. Apply the Breusch-Pagan Test to check
#for non-constant error variance.
# HOMOSCEDASTICITY
# --------------------
# USING PACKAGE {car}
library(car)

# homoscedasticity

# The Breusch-Pagan Test for Non-Constant Error Variance 
ncvTest(fit.final) 
# A LARGE P-VALUE (>0.05) JUSTIFIES EQUAL VARIANCE but from the result obtained
#we can not conclude that there is equal variance since the P-value is less than 
#<0.05

# Plot Absolute Jackknife Residuals vs. Fitted values 
# Power Box-Cox Transformation on the response Y is suggested 
par(mfrow=c(1,1),mar=c(4, 4, 4, 4)) 
spreadLevelPlot(fit.final, pch=18, cex=0.5, col="red",
                main="HV Model on Baseball Salary: Heteroscedasticity")
# IF THE LINES ARE FLAT, THEN EQUAL VARIANCE IS JUSTIFIED. However the 
#result obtained from this plot concluded there is an unequal variance since the lines 
#are not flat


#(c) (Check Independence) Apply the Durbin-Watson test to check for
#auto-correlated errors.

# INDEPENDENCE--#In independence, we look for some time series structure,It 
#assume the data comes in the nature order
# -----------------
# Test for Autocorrelated Errors
durbinWatsonTest(fit.final)
durbinWatsonTest(fit.final,reps = 5000)
# LARGE P-VALUE (>0.05) JUSTIFIES INDEPENDENCE, But the result obtained do not
#justify independence since the P-value is <0.05, Hence the data does not comes 
#in the nature order.


#(d)---(Check Linearity) 
# LINEARITY
# ------------

# Evaluate Nonlinearity VIA THE component + residual plot (partial residual)
crPlots(fit.final, main="Partial Residual Plots")

# leverage plots or partial regression plot
leveragePlots(fit.final, main="Partial Regression (Leverage) Plots") 

# Ceres plots - a generalization of component+residual (partial residual) 
# plots that are less prone to leakage of non-linearity among the predictors.
#library(car)
#ceresPlots(fit.final) 

#Conclusion:The plot obtained does not show linearity in the model data ,conclusively it 
#does not satisfy linearity

#(e) (Outlier Detection)-Identify outliers that are outlying in terms of predictors, response,
#and being influential by using the leverage hii
# ============================
# OUTLIER DETECTION
# ============================

infl <- influence.measures(fit.final); 
infl.mat <- as.data.frame(infl$infmat)
n <- nrow(baseball); 
p <- length(coef(fit.final))-1 
# Cook's Distance
cook.d <- infl.mat$cook.d
infl <- summary(influence.measures(fit.final));infl
write.csv(infl, file="Influennce-Mat.csv", row.names=TRUE)
outlierTest(fit.final) # Bonferonni p-value for most extreme obs

# Plot of Cook's Distance
cutoff <- 4/(n-p-2)
plot(fit.final, which=4, cook.levels=cutoff, col="gray65", lwd=1.5)
points(1:n, cook.d, pch=1, cex=1, col="blue") 
#Since I obtained my cutoff to be 0.01556, we can boldly say 179 and 268 is an 
# outlier

# EXTRACT INFLUETIAL POINTS
baseball[cook.d > 0.05, ]   # HIGH COOK'S DISTANCE


# Interactive Plot for Identifying Influential Points
# Press ESC to stop when you are done with identification
influencePlot(fit.final, id.method="identify", 
              col="blue", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's d")

# The result obtained from the influential point shows that 268,284,80,179 and
#303 are all outliers making it 5 of them.

# A BARBLE PLOT ON OUTLIER DETECTION
# --------------------------------------

infl <- influence.measures(fit.final); 
infl.mat <- as.data.frame(infl$infmat)
h <- infl.mat$hat
cook.d <- infl.mat$cook.d

# A BARBLE PLOT OF THREE DIAGNOSTIC MEASURES
par(bg="white", mar=c(5, 5, 5, 5), mfrow=c(1, 1), xaxt="s")
plot(x=c(min(h), max(h)), y=c(min(r.jack), max(r.jack)), xlab=expression(h[ii]), 
     ylab=expression(r[(-i)]), cex.lab=1.5, type="n")
symbols(h, r.jack, circles=cook.d, inches=0.35, fg="white", bg="green", add=T)
abline(h=0, col="black", lwd=1)
abline(h=qt(.975, n-p-2), col="blue", lwd=2)
abline(h=qt(.025, n-p-2), col="blue", lwd=2)
text(x=.12, y=qt(.975, n-p-2)+.3, labels=expression(t[.975]^(n-p-2)),  col="blue")
text(x=.12, y=qt(.025, n-p-2)+.3, labels=expression(t[.025]^(n-p-2)),  col="blue")
abline(v=2*(p+1)/n, lwd=4, col="grey")
text(2*(p+1)/n+.008, 3.0, labels="2(p+1)/n", col="grey")

# IDENTIFY OUTLIERS
t0 <- qt(.975, n-1-(p+1)); t0
#subset1 <- (cook.d >=.0325)|(h>0.06)|(abs(r.jack) > 3) 
subset1 <- (cook.d >=0.065)|(h> 2*(p+1)/n)|(abs(r.jack) > t0) 
symbols(h[subset1], r.jack[subset1], circles=cook.d[subset1], inches=0.35, 
        fg="white", bg="orangered", add=T)
text(h[subset1], r.jack[subset1], (1:n)[subset1], cex=0.5)


cbind(id=(1:n)[subset1],  "r.jack"= abs(r.jack[subset1]) > t0, "h"= h[subset1]> 2*(p+1)/n, 
      "cook.d" = cook.d[subset1] >= 0.065)

# Conclusion: The result obtained from the Barble plot confirm that303,268,179,205,135 are all
#outliers


#(f) (Multicollinearity) Assess multicollinearity by obtaining the condition number of the
#design matrix X and the variance inflation factor (VIF) measures.

fit_M<- lm(salary ~ batting.avg  + OBP + runs+ hits + doubles + triples + homeruns + RBI + walks+ strike.outs + stolen.bases
          + errors + free.agency.elig + free.agent.91 + arb.elig + arb.91,data=baseball, x=TRUE)
kappa(fit_M$x);
#The kappa result obtained is 9199.991 which happens to be greater than the 
#threshold 100, we therefore conclude that, there exist multicollinearity  between the variables

# COMPUTE VIF USING FUNCTION vif DIRECTLY 
vif(fit_M)#The variance inflation factor threshold is 10 as given,meaning that any predictor
#greater than 10 should be removed in the model,looking at the result we can
#conclude that predictor such as "runs","hits","RBI" should
# be removed as they determine the presence of multicollinearity in our model


#--5. Model Deployment Apply your final model to predict the log-salary for the new data set in
#the file bb92-test.csv

new_data<- read.table(file = "bb92-test.csv",sep=",", header = T, na.strings = c("NA", "", " "),
                  stringsAsFactors = T)
dim(new_data)#It has 20 observations with 16 (dimension) variables

#NEW data prediction
pred_ict <- predict(fit.final, new_data , interval="prediction")
head(pred_ict)#The predicted output(head) for the new data on interval basis

#Taking exponential of predicted values for the new data

test.pred<-data.frame(pred_ict)
test.pred.exp<- exp(test.pred)
head(test.pred.exp) #The result of exponential prediction on interval

library(ggplot2)#Determine the error plot
dat.plot <- data.frame(player=1:20, exp(pred_ict)); names(dat.plot)
ggplot(dat.plot, aes(x=player, y=fit)) + geom_errorbar(aes(ymin=lwr, ymax=upr)) + geom_point()

#The error plot below shows the error bar of the new data prediction with lower 
#and upper tail in each data point 

