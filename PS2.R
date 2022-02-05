library(data.table)
library(bit64)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fastDummies)
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1data/dataind")
datind2009 <- fread("datind2009.csv",header=T)
#1.1 Calculate the correlation between Y and X.
#Although X contains a row of 1, I temporarily discard this row when 
#I compute the correlation. The reason is a row of 1 generates sd=0,
#causing the denominator to be 0.
#Case1: When I compute the correlation, NAs are discarded.
Y <- datind2009$wage
varX <- datind2009$age
df1_1<-data.frame(Y,varX)
cor(df1_1, method = "pearson", use = "complete.obs")
#Case2: I also discard data with wage=0,
#For the rest of PS2, I discard the data with wage=0.
datind2009_exclude <- datind2009 %>% filter(wage!=0)
Y_no0 <- datind2009_exclude$wage
varX_no0 <- datind2009_exclude$age
df1_1_no0<-data.frame(Y_no0,varX_no0)
cor(df1_1_no0, method = "pearson", use = "complete.obs")
#1.2 Calculate the coefficients on this regression
X <- cbind(rep(1,length(Y_no0)),varX_no0)
df1_2<-data.frame(Y_no0,X)
df1_2noNA <- df1_2[complete.cases(df1_2), ]
X_matrix <- as.matrix(df1_2noNA[, c("V1","varX_no0")])
Y_matrix <-as.matrix(df1_2noNA[,"Y_no0"]) 
coefficient1_2 <-solve(t(X_matrix)%*%X_matrix)%*%
                  (t(X_matrix)%*%Y_matrix)
#1.3 Calculate the standard errors
#(i) Use the standard formulas of the OLS. 
ster<-function(y,x){
  coefficient1_2 <-solve(t(x)%*%x)%*% (t(x)%*%y)
  #Calculate residuals first
  residuals <- y-x%*%coefficient1_2
  p <- ncol(x) - 1 
  df <- nrow(x) - p - 1 
  # Residual variance 
  res_var <- sum(residuals^2) / df 
  # Calculate covariance matrix of estimate 
  beta_cov <- res_var * solve(t(x)%*%x) 
  # Square root of the diag 
  beta_se <- sqrt(diag(beta_cov))
  return(beta_se)
}
ster(Y_matrix,X_matrix)
#(ii)bootstrap with 49 and 499 replications respectively. 
R1=49;R2=499
boot1 <- matrix(NA,R1,2)
for (i in 1:R1) {
  whichone1<-sample(1:nrow(df1_2noNA),nrow(df1_2noNA),rep=TRUE)
  wholedata1 <-df1_2noNA[whichone1,]
  x_data1<-as.matrix(wholedata1[,2:3])
  y_data1<-as.matrix(wholedata1[,1])
  coeffR1<-solve(t(x_data1)%*%x_data1)%*% (t(x_data1)%*%y_data1)
  boot1[i,]<-coeffR1
}
bootsd1<-apply(boot1,2,sd)
#R2=499
boot2 <- matrix(NA,R2,2)
for (i in 1:R2) {
  whichone2<-sample(1:nrow(df1_2noNA),nrow(df1_2noNA),rep=TRUE)
  wholedata2 <-df1_2noNA[whichone2,]
  x_data2<-as.matrix(wholedata2[,2:3])
  y_data2<-as.matrix(wholedata2[,1])
  coeffR2<-solve(t(x_data2)%*%x_data2)%*% (t(x_data2)%*%y_data2)
  boot2[i,]<-coeffR2
}
bootsd2<-apply(boot2,2,sd)
#Comment on the difference between the two strategies.
#Exercise2 Consider the same application as exercise 1 
#but using a pooled version of individual data from 2005 to 2018.
temp = list.files(pattern="*.csv")
appendind = lapply(temp, fread, colClasses = c(idind="character",
                                               idmen="character"))
datind0419 <- do.call("rbind", appendind)
datind0518 <- datind0419 %>% filter(year==c(2005:2018))
#By table(datind0518$age), we know age ranges from -1 to 101.
#Also, we note the data with age=-1 has wage=NA, so it would be 
#discarded in the next step. That is, we don't need to deal with it now.
table(datind0518$age)
datind0518_age <- datind0518 %>% filter(age!="NA"& wage!="NA") %>%
  filter(age>=18 & age<=101)%>%
  filter(wage!=0)%>%
  dplyr::select(c("age","wage","year")) %>%
  mutate(ag = cut(age,breaks = c(17,25,30,35,40,45,50,55,60,101)))
#Plot the wage of each age group across years. Is there a trend? Yes.
rawplot <- datind0518_age %>%
  ggplot(aes(x=ag,y=wage,fill=factor(year)))+
  geom_boxplot()
rawplot
#2.3 After including a time fixed effect, 
#how do the estimated coefficients change?
datind0518_2_2 <- dummy_cols(datind0518, select_columns ='year',
             remove_first_dummy = TRUE)
df2_2noNA <- datind0518_2_2%>%filter(wage!="NA"& wage>0)
df2_2noNA$constant <- rep(1,nrow(df2_2noNA))
X_matrix2_2 <- as.matrix(df2_2noNA[, c("age","year_2006",
                                            "year_2007","year_2008",
            "year_2009","year_2010","year_2011","year_2012","year_2013",
            "year_2014","year_2015","year_2016","year_2017","year_2018",
            "constant")])
Y_matrix2_2 <-as.matrix(df2_2noNA[,"wage"]) 
coefficient2_2 <-solve(t(X_matrix2_2)%*%X_matrix2_2)%*%
  (t(X_matrix2_2)%*%Y_matrix2_2)
ster(Y_matrix2_2,X_matrix2_2)
#Compute t-value for both models to see the difference
tvalue11_OLS <-coefficient1_2[2]/ster(Y_matrix,X_matrix)[2]
tvalue22time_OLS <- coefficient2_2[1]/ster(Y_matrix2_2,X_matrix2_2)[1]
#summary(plm(wage ~ age, data = df2_2noNA, 
    #model = "within", index = "year"))
#We are interested in the effect of age on labor market participation.
#We consider this problem using the data from 2007.
#Consider a probit model.
#Exclude all individuals who are inactive.
datind2007 <- fread("datind2007.csv",header=T)
datind2007_exclude <- datind2007 %>%  
  filter(empstat!="Inactive") %>% filter(empstat!="Retired")%>%
  filter(wage!=0)
#Write a function that returns the likelihood of the probit 
#of being employed.You might want to write Xβ first. 
#Then, calculate F(XB) and the log likelihood.
#Remember, for the probit model, 
#F(x) is the standard normal distribution function.
#Here,I create a dummy called employed.
#employed=1 if employed =0 o.w. unemployed
datind2007_exclude$employed <- ifelse(datind2007_exclude$empstat==
                                        "Employed",1,0)
#reg3 <- glm(datind2007_exclude$employed~datind2007_exclude$age,
            #family = binomial(link = "probit"))
flike = function(par,x1,x2,yvar)
{xbeta <- par[1]*x1 + par[2]*x2
  prob_XB <- pnorm(xbeta)
  prob_XB[prob_XB>0.999999] = 0.999999
  prob_XB[prob_XB<0.000001] = 0.000001
  return(-sum((1 - yvar) * log(1 - prob_XB) + yvar * log(prob_XB)))
  }
#test_par = reg3$coefficients
datind2007_exclude$constant <- rep(1,nrow(datind2007_exclude))
x1=datind2007_exclude$constant 
x2=datind2007_exclude$age
yvar=datind2007_exclude$employed
#Optimize the model and interpret the coefficients. 
#You can use pre-programmed optimization packages.
times = 100
like3_4 <- c()
result3_4 = mat.or.vec(times,2)
for (i in 1:times)
{start= runif(2,-11,11)
res= optim(start,fn=flike,method="BFGS",
                 control=list(trace=6,maxit=1000),
                 x1=x1,x2=x2,yvar=yvar)
like3_4[i] <- flike(res$par,x1,x2,yvar)
#output saves the optimization result
result3_4[i,] = res$par
}
result3_4[which.min(like3_4),]
trytry <- as.data.frame(cbind(result3_4,like3_4))
ggplot(trytry, aes(x=like3_4)) + geom_histogram()
#Method 2. I manually try different set.seed values many times, 
#and 120 is the value which brings us the lowest flike.
#The coefficient obtained here is nearly the same as Method 1.
#set.seed(120)
#start = runif(2)
#resM2  = optim(start,fn=flike,method="BFGS",
             #control=list(trace=6,REPORT=1,maxit=1000),
             #x1=x1,x2=x2,yvar=yvar,hessian=TRUE)
#resM2$par
#3.4 Can you estimate the same model including wages
#as a determinant of labor market participation? Explain.
#No. Most of 
x3=datind2007_exclude$wage
#reg3_4 <- glm(datind2007_exclude$employed~datind2007_exclude$age+
                #datind2007_exclude$wage,
            #family = binomial(link = "probit"))
#summary(reg3_4)
flike3_4 = function(par,x1,x2,x3,yvar)
{xbeta <- par[1]*x1+par[2]*x2+par[3]*x3
prob_XB <- pnorm(xbeta)
prob_XB[prob_XB>0.999999] = 0.999999
prob_XB[prob_XB<0.000001] = 0.000001
return(-sum((1 - yvar) * log(1 - prob_XB) + yvar * log(prob_XB)))
}
#Method1
ntry=499
likeage <- c()
outage = mat.or.vec(ntry,3)
for (i0 in 1:ntry)
{ start =runif(3,-0.4,0.4)
#res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
resage= optim(start,fn=flike3_4,method="BFGS",
                 control=list(trace=6,maxit=1000),
                 x1=x1,x2=x2,x3=x3,yvar=yvar)
likeage[i0] <- flike3_4(res$par,x1,x2,x3,yvar)
#output saves the optimization result
outage[i0,] = resage$par
}
outage[which.min(likeage),]
#Method2. 
set.seed(162)
start = runif(3)
res  = optim(start,fn=flike3_4,method="BFGS",
             control=list(trace=6,REPORT=1,maxit=1000),
             x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)
fisher_info = solve(res$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma
est = cbind(summary(reg3_4)$coefficients[, 1],
            summary(reg3_4)$coefficients[, 2],res$par,prop_sigma)
#Exercise4
#We are interested in the effect of age on labor market participation.
#Use the pooled version of the data
#from 2005 to 2015. Additional controls include time-fixed effects.
datind0515 <- datind0419 %>% filter(year>=2005 & year<=2015) 
yeardummy <- dummy_cols(datind0515, select_columns ='year',
           remove_first_dummy = TRUE)
#Exclude all individuals who are inactive.
datind0515_exclude <- datind0515 %>% mutate(yeardummy)%>% 
  filter(empstat!="Inactive")%>%filter(empstat!="Retired")%>%
  filter(wage!=0) %>%
  mutate(employed=ifelse(empstat=="Employed",1,0))
#Part (i) Write LPM
xelement <- cbind(contant=rep(1,nrow(datind0515_exclude)),
      datind0515_exclude[,c(9,11:20)])
x_LPM_Matrix <- as.matrix(xelement)
y_LPM_Matrix <- as.matrix(datind0515_exclude$employed)
coefficient4_LPM <-solve(t(x_LPM_Matrix)%*%x_LPM_Matrix)%*%
  (t(x_LPM_Matrix)%*%y_LPM_Matrix)
std.error_LPM <- ster(y_LPM_Matrix,x_LPM_Matrix)
t_value_LPM <- coefficient4_LPM/std.error_LPM
pvalue_LPM =1-abs(pt(t_value_LPM,
                     df=nrow(x_LPM_Matrix)-ncol(x_LPM_Matrix))-1/2)*2
flike5LPM = function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,yvar)
{xbeta <- par[1]*x1 + par[2]*x2+par[3]*x3+par[4]*x4+par[5]*x5+
  par[6]*x6+par[7]*x7+par[8]*x8+par[9]*x9+par[10]*x10+par[11]*x11+
  par[12]*x12
prob_XB <- pnorm(xbeta)
prob_XB[prob_XB>0.999999] = 0.999999
prob_XB[prob_XB<0.000001] = 0.000001
p1=log(prob_XB)
po=log(1-prob_XB)
return(-sum((1 - yvar)*po+ yvar*p1))
}
ntry=4
out = mat.or.vec(ntry,12)
like5_LPM <- c()
set.seed(123)
for (i0 in 1:ntry)
{start = runif(12,-2,2)
resLPM = optim(start,fn=flike5LPM,method="BFGS",
                  control=list(trace=6,maxit=1000),
                  x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                  x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                  x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
out[i0,] = resLPM$par
like5_LPM[i0]<- flike5LPM(resLPM$par,x1,x2,x3,x4,x5,x6,
                               x7,x8,x9,x10,x11,x12,yvar)
}
out[which.min(like5_LPM),]
#Check my answer
#CHECK <- summary(plm(employed~age, data = datind0515_exclude, 
#model = "within", index = "year"))
#Part (ii) Probit model
#reg4_2 <- glm(employed~age+year_2006+year_2007+year_2008+year_2009+
        #year_2010+year_2011+year_2012+year_2013+year_2014+year_2015,
          #data=datind0515_exclude,family = binomial(link = "probit"))
flike5prob = function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,yvar)
{xbeta <- par[1]*x1 + par[2]*x2+par[3]*x3+par[4]*x4+par[5]*x5+
  par[6]*x6+par[7]*x7+par[8]*x8+par[9]*x9+par[10]*x10+par[11]*x11+
  par[12]*x12
prob_XB <- pnorm(xbeta)
prob_XB[prob_XB>0.999999] = 0.999999
prob_XB[prob_XB<0.000001] = 0.000001
return(-sum((1 - yvar) * log(1 - prob_XB) + yvar * log(prob_XB)))
}
datind0515_exclude$constant <- rep(1,nrow(datind0515_exclude))
x1=datind0515_exclude$constant 
x2=datind0515_exclude$age
x3=datind0515_exclude$year_2006
x4=datind0515_exclude$year_2007
x5=datind0515_exclude$year_2008
x6=datind0515_exclude$year_2009
x7=datind0515_exclude$year_2010
x8=datind0515_exclude$year_2011
x9=datind0515_exclude$year_2012
x10=datind0515_exclude$year_2013
x11=datind0515_exclude$year_2014
x12=datind0515_exclude$year_2015
yvar=datind0515_exclude$employed
#Method1 28350.27
ntry=4
out = mat.or.vec(ntry,12)
like5_1probit <- c()
set.seed(123)
for (i0 in 1:ntry)
{start = runif(12,-2,2)
resprobM1 = optim(start,fn=flike5prob,method="BFGS",
                 control=list(trace=6,maxit=1000),
                 x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                 x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                 x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
out[i0,] = resprobM1$par
like5_1probit[i0]<- flike5prob(resprobM1$par,x1,x2,x3,x4,x5,x6,
                                 x7,x8,x9,x10,x11,x12,yvar)
}
out[which.min(like5_1probit),]
#Method2 28350.26 
set.seed(32)
start = runif(12,-1,1)
res_probit= optim(start,fn=flike5prob,method="BFGS",
             control=list(trace=6,REPORT=1,maxit=1000),
             x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
             x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
             x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
fisher_info = solve(res_probit$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
#prop_sigma
zvalue=(res_probit$par)/prop_sigma
est_probit = cbind(res_probit$par,prop_sigma,zvalue,
            #pvalue_probit=1-abs(pnorm(zvalue_prob)-1/2)*2,
            pvalue_probit=2*pnorm(-abs(zvalue)))
est_probit
#Part (iii) Logit model
#reg4_3 <- glm(employed~age+year_2006+year_2007+year_2008+year_2009+
      #year_2010+year_2011+year_2012+year_2013+year_2014+year_2015,
    #data=datind0515_exclude,family = binomial(link = "logit"))
flogit = function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,yvar)
{xbeta <- par[1]*x1 + par[2]*x2+par[3]*x3+par[4]*x4+par[5]*x5+
  par[6]*x6+par[7]*x7+par[8]*x8+par[9]*x9+par[10]*x10+par[11]*x11+
  par[12]*x12
prob_XB= exp(xbeta)/(1+exp(xbeta))
prob_XB[prob_XB>0.999999] = 0.999999
prob_XB[prob_XB<0.000001] = 0.000001
return(-sum((1 - yvar) * log(1 - prob_XB) + yvar * log(prob_XB)))
}
#Method1
#Method1 28326.31
ntry=4
out = mat.or.vec(ntry,12)
like5_1logit <- c()
set.seed(123)
for (i0 in 1:ntry)
{start = runif(12,-2,2)
#res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
reslogitM1 = optim(start,fn=flogit,method="BFGS",
                  control=list(trace=6,maxit=1000),
                  x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                  x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                  x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
#output saves the optimization result
out[i0,] = reslogitM1$par
like5_1logit[i0] <- flogit(reslogitM1$par,x1,x2,x3,x4,x5,x6,
                                 x7,x8,x9,x10,x11,x12,yvar)
}
out[which.min(like5_1logit),]
#Method2 28326.31 (manually choose the set.seed value by the criteria
#that gives me the lowest like5_1logit)
set.seed(120)
start = runif(12)
res_logit = optim(start,fn=flogit,method="BFGS",
             control=list(trace=6,REPORT=1,maxit=1000),
             x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
             x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
             x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
fisher_info = solve(res_logit$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma
zvalue_logit=res_logit$par/prop_sigma
est_logit = cbind(res_logit$par,prop_sigma,
            #pvalue_logit=1-abs(pnorm(zvalue_logit)-1/2)*2,
            pvalue_logit=2*pnorm(-abs(zvalue_logit)))
est_logit
#Interpret and compare the estimated coefficients. 
#Part(i):LPM
#How significant are they? (compute p-value given alpha=0.05)
#Discussed in the .pdf.
#Exercise5
#5.1 Compute the marginal effect of the previous probit and logit models.
#Here, I choose to compute MEM (marginal effect at the mean)
#Probit
par_probit<-res_probit$par
ME_Probitpdf<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y){
  xbeta<-par[1]*mean(x1)+par[2]*mean(x2)+par[3]*mean(x3)+
    par[4]*mean(x4)+par[5]*mean(x5)+par[6]*mean(x6)+
    par[7]*mean(x7)+par[8]*mean(x8)+par[9]*mean(x9)+
    par[10]*mean(x10)+par[11]*mean(x11)+par[12]*mean(x12)
  pdf_probit<-dnorm(xbeta)
  return(pdf_probit)
}
mem_probit<-c()
for (i in 1:12) {
  mem_probit[i]<-ME_Probitpdf(par_probit,x1,x2,x3,x4,x5,x6,x7,
                     x8,x9,x10,x11,x12,yvar)*par_probit[i]
}
#Logit_Analytical
par_logit<-res_logit$par
ME_Logitpdf<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y){
  xbeta<-par[1]*mean(x1)+par[2]*mean(x2)+par[3]*mean(x3)+
    par[4]*mean(x4)+par[5]*mean(x5)+par[6]*mean(x6)+
    par[7]*mean(x7)+par[8]*mean(x8)+par[9]*mean(x9)+
    par[10]*mean(x10)+par[11]*mean(x11)+par[12]*mean(x12)
  pdf_logit<-dlogis(xbeta)
  return(pdf_logit)
}
mem_logit<-c()
for (i in 1:12) {
  mem_logit[i]<-ME_Logitpdf(par_logit,x1,x2,x3,x4,x5,x6,x7,
                              x8,x9,x10,x11,x12,yvar)*par_logit[i]
}
#Logit_Numerical
h=0.0001
(ME_Logitpdf(par_logit,x1,x2+h,x3,x4,x5,x6,x7,
            x8,x9,x10,x11,x12,yvar)-
    ME_Logitpdf(par_logit,x1,x2,x3,x4,x5,x6,x7,
                x8,x9,x10,x11,x12,yvar))/h
#5.2 Construct the standard errors of the marginal effects. 
#Hint: Boostrap may be the easiest way.
#Probit case
R1=9
mem_probit_5_2_1<-matrix(NA,R1,12)
for (i in 1:R1) {
  whichone_5_2prob<-sample(1:nrow(datind0515_exclude),
                           nrow(datind0515_exclude),rep=TRUE)
  wholedata_5_2_1 <-datind0515_exclude[whichone_5_2_1,]
  x_data_521<-as.matrix(wholedata_5_2_1[,c(9,11:20,22)])
  y_data_521<-as.matrix(wholedata_5_2_1[,21])
  start = runif(12)
  res_probit = optim(start,fn=flike5prob,method="BFGS",
                    control=list(trace=6,REPORT=1,maxit=1000),
                    x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                    x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                    x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
  parprobit5_2_1 <- res_probit$par
  for (j in 1:12){
    mem_probit_5_2_1[i,j]<-ME_Logitpdf(parlogit5_2_1,x1,x2,x3,x4,x5,x6,x7,
                                      x8,x9,x10,x11,x12,yvar)*
      parprobit5_2_1[j]
  }
}
bootsdmepro5_2_1<-apply(mem_probit_5_2_1,2,sd)
#Logit case
mem_logit_5_2_1<-matrix(NA,R1,12)
for (i in 1:R1) {
    whichone_5_2_1<-sample(1:nrow(datind0515_exclude),
                           nrow(datind0515_exclude),rep=TRUE)
  wholedata_5_2_1 <-datind0515_exclude[whichone_5_2_1,]
  x_data_521<-as.matrix(wholedata_5_2_1[,c(9,11:20,22)])
  y_data_521<-as.matrix(wholedata_5_2_1[,21])
  start = runif(12)
  res_logit = optim(start,fn=flogit,method="BFGS",
                    control=list(trace=6,REPORT=1,maxit=1000),
                    x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                    x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                    x11=x11,x12=x12,yvar=yvar,hessian=TRUE)
  parlogit5_2_1 <- res_logit$par
  for (j in 1:12){
  mem_logit_5_2_1[i,j]<-ME_Logitpdf(parlogit5_2_1,x1,x2,x3,x4,x5,x6,x7,
                                    x8,x9,x10,x11,x12,yvar)*
    parlogit5_2_1[j]
}
}
bootsdlog5_2_1<-apply(mem_logit_5_2_1,2,sd)