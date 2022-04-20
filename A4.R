setwd("C:/Users/LISA/Desktop/Duke/ECON613")
library(data.table)
library(panelr)
library(AER)
library(survival)
library(nnet)
library(dplyr)
library(ggplot2)
library(sampleSelection)
library(tinytex)
library(marginaleffects)
library(stringr)
library(snow)
library(margins)
library(nloptr)
library(boot)
library(mlogit)
dat_A4 <- fread("dat_A4.csv",header=T)
#1.1 Create additional variable for the age of the agent ¡¨age¡¨,
#total work experience measured in years ¡¨work exp¡¨.
#work_exp0=0 actually represents an individual has NA for all 11 columns.
#However, they should be denoted as NA rather than 0 since I think NA is
#different from 0. Getting NA might result from the situation that the 
#individual doesn't know how to measure his working hrs for some reason. 
dat_A4new <- dat_A4 %>% mutate(age=2019-KEY_BDATE_Y_1997)%>%
  group_by(V1)%>% mutate(work_hrs = sum(CV_WKSWK_JOB_DLI.01_2019,
      CV_WKSWK_JOB_DLI.02_2019,CV_WKSWK_JOB_DLI.03_2019,
      CV_WKSWK_JOB_DLI.04_2019,CV_WKSWK_JOB_DLI.05_2019,
      CV_WKSWK_JOB_DLI.06_2019,CV_WKSWK_JOB_DLI.07_2019,
      CV_WKSWK_JOB_DLI.08_2019,CV_WKSWK_JOB_DLI.09_2019,
      CV_WKSWK_JOB_DLI.10_2019,CV_WKSWK_JOB_DLI.11_2019,
                             na.rm=TRUE))%>%
  mutate(work_exp0=work_hrs/52,work_exp=na_if(work_exp0, 0))
head(dat_A4new[,c("V1","age","work_exp")])
#1.2 Create additional education variables indicating total years of 
#schooling from all variables related to education 
#(eg, ¡¨BIOLOGICAL FATHERS HIGHEST GRADE COMPLETED¡¨) in our dataset.
#There are 5 variables related to education.
#CV_HGC_BIO/RES_MOM/DAD & YSCH.3113_2019
#Notice that 95 denotes ungraded, so we need to convert it to 0
dat_A4new[which(dat_A4new$CV_HGC_BIO_DAD_1997 == 95),
          "CV_HGC_BIO_DAD_1997"] <- 0
dat_A4new[which(dat_A4new$CV_HGC_BIO_MOM_1997 == 95),
          "CV_HGC_BIO_MOM_1997"] <- 0
dat_A4new[which(dat_A4new$CV_HGC_RES_DAD_1997 == 95),
          "CV_HGC_RES_DAD_1997"] <- 0
dat_A4new[which(dat_A4new$CV_HGC_RES_MOM_1997 == 95),
          "CV_HGC_RES_MOM_1997"] <- 0
#Lastly,YSCH.3113_2019 is a categorical variable. E.g. 2 denotes GED, and 
#3 denotes High school diploma. Therefore, we need to convert them to 
#corresponding educational years.
#The following is my definition of each correponding educational year:
#1 None => 0
#2 GED => 12
#3 High school diploma (Regular 12 year program)=>12
#4 Associate/Junior college (AA) => 14
#5 Bachelor's degree (BA, BS) => 16
#6 Master's degree (MA, MS) => 18
#7 PhD => 22
#8 Professional degree (DDS, JD, MD) => 20
dat_A4new <- dat_A4new %>%                              
  mutate(indedu = recode(YSCH.3113_2019, `1` = 0, `2` = 12, `3` = 12,
            `4` = 14, `5` = 16, `6` = 18,`7` = 22,`8` = 20))
head(dat_A4new[,c("V1","indedu","CV_HGC_BIO_DAD_1997","CV_HGC_BIO_MOM_1997",
                  "CV_HGC_RES_DAD_1997","CV_HGC_RES_MOM_1997")])
#1.3Provide the following visualizations.
#1.3.1 Plot the income data (where income is positive) by 
#i) age groups, ii) gender groups and iii) number of children
dat_income <- dat_A4new %>% filter(YINC_1700_2019>0)
plotbyage <- dat_income %>%
  ggplot(aes(x=age,y=YINC_1700_2019,fill=factor(age)))+
  geom_boxplot()
plotbyage
dat_income$gender[dat_income$KEY_SEX_1997 == 2] <- "female"
dat_income$gender[dat_income$KEY_SEX_1997 == 1] <- "male"
plotbygender <- dat_income %>%
  ggplot(aes(x=gender,y=YINC_1700_2019,fill=factor(gender)))+
  geom_boxplot()
plotbygender
plotbychild<- dat_income %>% filter(CV_BIO_CHILD_HH_U18_2019!="NA")%>%
  ggplot(aes(x=CV_BIO_CHILD_HH_U18_2019,y=YINC_1700_2019,
             fill=factor(CV_BIO_CHILD_HH_U18_2019)))+
  geom_boxplot()
plotbychild
## of individual having 8 and 9 children under 18 is 2 and 1 respectively. 
#That's the reason why we see two lines in the box plot.
#1.3.2 Table the share of ¡¨0¡¨ in the income data by i) age groups, 
#ii) gender groups, iii) number of children and marital status
table321 <- dat_A4new %>% filter(YINC_1700_2019!="NA")%>% group_by(age)%>%  
  mutate(income0=ifelse(YINC_1700_2019>0,1,0))%>%
  summarize(numerator=sum(income0),
            demoniator=n( ),share=1-(numerator/demoniator))
table321
dat_A4new$gender[dat_A4new$KEY_SEX_1997 == 2] <- "female"
dat_A4new$gender[dat_A4new$KEY_SEX_1997 == 1] <- "male"
table322 <- dat_A4new %>% filter(YINC_1700_2019!="NA")%>%group_by(gender) %>%  
  mutate(income0=ifelse(YINC_1700_2019>0,1,0))%>%
  summarize(numerator=sum(income0),
            demoniator=n( ),share=1-(numerator/demoniator))
table322
table323 <- dat_A4new %>% filter(YINC_1700_2019!="NA")%>%
                             filter(CV_BIO_CHILD_HH_U18_2019!="NA") %>%
            filter(CV_MARSTAT_COLLAPSED_2019!="NA")%>%
            group_by(CV_BIO_CHILD_HH_U18_2019,CV_MARSTAT_COLLAPSED_2019)%>%  
            mutate(income0=ifelse(YINC_1700_2019>0,1,0))%>%
            summarize(numerator=sum(income0),
            demoniator=n( ),share=1-(numerator/demoniator))
table323
#1.3.3 interpret the visualizations from above
#From the first boxplot, we can see individuals aged 41 have the highest
#income, but the overall distribution is nearly the same across 5 age groups.
#From the second boxplot, we can see there is a big gender income gap. To
#be more specific, the median of female income is only higher than the 25th
#percentile of male income by a litte. 
#From the third boxplot, we can see individuals having two children under 18
#have the highest among all groups. Also, individuals having one or three
#children under 18 has the second highest income in terms of the median.
#2.1 Specify and estimate an OLS model to explain the income variable
#(where income is positive).
#Here, I also recode marital status into two groups, single and married.
#1 Married -> 1; 0 single, 2 separated, 3 divorced, 4 widowed ->0
dat_income <- dat_income%>%
  mutate(mardummy = recode(CV_MARSTAT_COLLAPSED_2019, `0` = 0, `1` = 1,
                           `2` = 0,`3` = 0,`4` = 0))
reg21 <- lm(YINC_1700_2019~age+KEY_SEX_1997+work_exp+CV_HGC_BIO_DAD_1997+
          indedu+mardummy,data=dat_income)
summary(reg21)
#¡V Interpret the estimation results
#Holding others constant, an additional year in education increases 
#income by 2165.95 dollars when we focus on those whose income >0. I treat
#age, gender, working experience, and other variables as control variables.
#Explain why there might be a selection problem when estimating
#an OLS this way
#We might argue that whether an individual has a positive income is not random. 
#It might be affected by one's family background(e.g. father's educationa level)
#and one's marital status.For example, it's more likely that one would 
#enter into the job market when he is married. 
#2.2 Explain why the Heckman model can deal with the selection problem.
#In the first stage, the criteria of choosing what to include is these variables
#affect whether an individual has income >0 or not. However, 
#By computing Inverse Mills Ratio, we can 
#2.3 Estimate a Heckman selection model. Interpret the results from the 
#Heckman selection model and compare the results to OLS results. Why does
#there exist a difference?
#2.3 Interpret the results from the Heckman selection model and compare the results to OLS results.
#Why does there exist a difference?
#If the coefficient of imr is not significant, then OLS model is not 
#misspecified. Here, we can see p-value of imr >0.05, 
#so it's not significant under alpha=0.05. However, I doubt the 
#potential reason is high multicollinearity leading to blowing high s.e.. 
#Hence, we will get the insignificant coefficient, getting wrong 
#statistical inference. If we want to check the result, we should perform sensitivity analysis since selection models can be fragile. Compared with the results with that of OLS, we can see the magnitude of indedu,work_exp, and gender is all higher. 
#In the first stage, we want to put variables correlated with 
#having income or not, but not correlated with
#the level of income. 
dat_2 <- dat_A4new %>% filter(YINC_1700_2019!="NA")%>%
  filter(CV_HGC_RES_DAD_1997!="NA") %>%  
  filter(CV_BIO_CHILD_HH_U18_2019!="NA")%>%  
  mutate(incomedummy=ifelse(YINC_1700_2019>0,1,0))%>%
mutate(mardummy = recode(CV_MARSTAT_COLLAPSED_2019, `0` = 0, `1` = 1,
                         `2` = 0,`3` = 0,`4` = 0))
observed_index  = dat_2$YINC_1700_2019 > 0
probit <-  glm(observed_index ~ 
            CV_HGC_RES_DAD_1997+mardummy,
             data = dat_2,
             family = binomial(link = 'probit'))
summary(probit)
probit_lp = predict(probit)
mills0 = dnorm(probit_lp)/pnorm(probit_lp)
#summary(mills0)
imr = mills0[observed_index]
#Second stage (put IMR into the regression) focus on those whose
#income is > 0
Heck2 <- lm(YINC_1700_2019~age+work_exp+indedu+imr+KEY_SEX_1997,
            data=dat_2[observed_index, ])
summary(Heck2)
#Exercise3
#Note that the ¡¨YINC-1700¡¨ variable is censored because of privacy issues. 
#In otherwords, highwages are top-coded in this data set.
#3.1 Plot a histogram to check whether the distribution of the income 
#variable. What might be the censored value here?
ggplot(dat_income, aes(x=YINC_1700_2019))+
  geom_histogram(color="darkblue", fill="lightblue")
#From the graph, I think 10,000 is the censored value.
#3.2 Propose a model to deal with the censoring problem.
#I choose tobit models since it's designed to estimate linear relationships between 
#variables when there is either left- or right-censoring 
#in the dependent variable. In this case, we have right-censoring in income
#where the threshold is 100,000.
#Since the Heckman selection model falls into the Type II tobit, we can also
#use the similar method as Q2 above to deal with the censored case.
#3.3 Estimate the appropriate model with the censored data (please write 
#down the likelihood function and optimize yourself without 
#using the pre-programmed package)
dat_3 <- dat_A4new %>%
  filter(YINC_1700_2019!="NA") %>%
  filter(CV_HGC_RES_DAD_1997!="NA") %>%  
  filter(CV_BIO_CHILD_HH_U18_2019!="NA")%>%
  mutate(mardummy = recode(CV_MARSTAT_COLLAPSED_2019, `0` = 0, `1` = 1,
                           `2` = 0,`3` = 0,`4` = 0))   
dat_3$censordummy <- 0
dat_3$censordummy[which(dat_3$YINC_1700_2019<100000)] <- 1
observed_index3  = dat_3$censordummy > 0
probit3 <-  glm(observed_index3 ~ 
                 indedu+gender+work_exp+age,
               data = dat_3,
               family = binomial(link = 'probit'))
summary(probit3)
probit_lp3 = predict(probit3)
mills03 = dnorm(probit_lp3)/pnorm(probit_lp3)
#summary(mills0)
imr3 = mills03[observed_index3]
#Second stage (put IMR into the regression) focus on those whose
#income is < 100,000
censor3 <- lm(YINC_1700_2019~age+work_exp+indedu+imr3+KEY_SEX_1997+mardummy,
            data=dat_3[observed_index3, ])
summary(censor3)
#3.4 Interpret the results above and compare to those when not 
#correcting for the censored data
dat34 <- dat_A4new%>%
  mutate(mardummy = recode(CV_MARSTAT_COLLAPSED_2019, `0` = 0, `1` = 1,
                           `2` = 0,`3` = 0,`4` = 0)) 
OLS3 <- lm(YINC_1700_2019~age+work_exp+indedu+KEY_SEX_1997+mardummy,
              data=dat34)
summary(OLS3)
#Since the coefficient of age is not significant, we don't take it into 
#consideration in the comparison. For the rest of the variables, 
#the magnitude of the coefficient in OLS is higher than that in tobit model.
#Exercise 4
#We are interested in the effect of education, marital status, 
#experience and education on wages.
#4.1 Explain the potential ability bias when trying to explain to 
#understand the determinants of wages
#4.2 Exploit the panel dimension of the data to propose a model to 
#correct for the ability bias. Estimate the model using the 
#following strategy.
#B1 hat=(X'_{new}X_{new})^(-1)*(X'_{new}Y_{new})
#Method1
dat_A4panel <- fread("dat_A4_panel.csv",header=T)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1998=CV_HIGHEST_DEGREE_9899_1998)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1999=CV_HIGHEST_DEGREE_9900_1999)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2000=CV_HIGHEST_DEGREE_0001_2000)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2001=CV_HIGHEST_DEGREE_0102_2001)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2002=CV_HIGHEST_DEGREE_0203_2002)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2003=CV_HIGHEST_DEGREE_0304_2003)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2004=CV_HIGHEST_DEGREE_0405_2004)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2005=CV_HIGHEST_DEGREE_0506_2005)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2006=CV_HIGHEST_DEGREE_0607_2006)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2007=CV_HIGHEST_DEGREE_0708_2007)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2008=CV_HIGHEST_DEGREE_0809_2008)
dat_A4panel <- dat_A4panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2009=CV_HIGHEST_DEGREE_0910_2009)
dat_A4trans <- long_panel(dat_A4panel,prefix = '_',begin = 1997,end=2019,
                          label_location = "end")
dat_A4trans <- subset(dat_A4trans,wave!='2012'& wave!='2014'& wave!='2016'
                      & wave!='2018')
dat_A4done <- dat_A4trans%>% filter(CV_HIGHEST_DEGREE_EVER_EDT!="NA")
dat_A4matrix <- as.matrix(dat_A4done)
#Marital status is col 9 [only divide into two categories: single/married]
#The method is the same as what we've done in Q2 & Q3. 
dat_A4matrix[which(dat_A4matrix[,9]==2),9] <-0
dat_A4matrix[which(dat_A4matrix[,9]==3),9] <-0
dat_A4matrix[which(dat_A4matrix[,9]==4),9] <-0
#Edu col 22 [categorical => continuous]
#The transformation criteria follows Q1.2
dat_A4matrix[which(dat_A4matrix[,22]==1),22] <-0
dat_A4matrix[which(dat_A4matrix[,22]==2),22] <-12
dat_A4matrix[which(dat_A4matrix[,22]==3),22] <-12
dat_A4matrix[which(dat_A4matrix[,22]==4),22] <-14
dat_A4matrix[which(dat_A4matrix[,22]==5),22] <-16
dat_A4matrix[which(dat_A4matrix[,22]==6),22] <-18
dat_A4matrix[which(dat_A4matrix[,22]==7),22] <-22
#Generate Working exp 10~16 & 23~30 =>31
dat_A4matrix <- matrix(as.numeric(dat_A4matrix),    # Convert to numeric matrix
                  ncol = ncol(dat_A4matrix))
dat_A4Matrix <- cbind(dat_A4matrix,rowSums(dat_A4matrix[, c(10:16,23:30)], na.rm=TRUE)) 
dat_A4df <- as.data.frame(dat_A4Matrix[,c(1,2,5,9,22,31)])
colnames(dat_A4df) <- c("id", "year","wage","marrydummy","edu","exp")
#For the following three strategies, I would like to control ethnicity and
#gender. However, both characteristics remain the same for every individual
#across sample periods. Therefore, these two variables will be canceled out.
#¡V Between Estimator
#yi bar=alpha i + beta*xi bar+ ei bar
Q422_df <- dat_A4df %>%  na.omit() %>% group_by(id) %>%
  summarize(meanwage = mean(wage, na.rm=TRUE),
            meanmaritalstatus= mean(marrydummy, na.rm=TRUE),
            meanedu = mean(edu, na.rm=TRUE),
            meanexp = mean(exp, na.rm=TRUE))
Q422reg <- lm(meanwage~meanmaritalstatus+meanedu+meanexp,data=Q422_df)
summary(Q422reg)
Between<- plm(wage ~ marrydummy+edu+exp, data=dat_A4df, 
              index=c("id", "year"), model="random")
summary(Between)
#¡V Within Estimator. (i.e. Fixed Effect)
#yit=B1Xit+ai+uit (For each i, average over time for each individual)
#yi bar=B1Xi bar+ai+ui bar
#(yit-yi)=B1(Xit-Xi bar)+(uit-ui bar)---no constant term
Q421_df <- left_join(dat_A4df,Q422_df, by = "id")%>%
          mutate(diffwage=wage-meanwage,
                 diffmarital=marrydummy-meanmaritalstatus,
                 diffedu=edu-meanedu,diffexp=exp-meanexp)
Q421reg <- lm(diffwage~diffmarital+diffedu+diffexp+0,data = Q421_df)
summary(Q421reg)
library(plm)
Within <- plm(wage ~ marrydummy+edu+exp, data=Q421_df, 
    index=c("id", "year"), model="within")
summary(Within)
#¡V Difference (any) Estimator Filter out NA
dat43raw <- dat_A4df %>% filter(!is.na(wage)) %>% 
  filter(!is.na(marrydummy))%>%filter(!is.na(edu))%>%filter(!is.na(exp))
Q423_dflag <- dat43raw %>% group_by(id) %>% 
  mutate(lwage = wage-dplyr::lag(wage))%>%
  mutate(lmaritalstatus = marrydummy-dplyr::lag(marrydummy))%>%
  mutate(ledu = edu-dplyr::lag(edu))%>%
  mutate(lexp = exp-dplyr::lag(exp))
Q423_df <- Q423_dflag %>% filter(!is.na(lwage)) %>% 
  filter(!is.na(lmaritalstatus))%>%filter(!is.na(ledu))%>%filter(!is.na(lexp))
Q423reg <- lm(lwage~lmaritalstatus+ledu+lexp+0,data = Q423_df)
summary(Q423reg)
#4.3 Interpret the results from each model and explain why different 
#models yield different parameter estimates.
#Within (FE model)
#Coefficients in three models can be interpreted in the same way as in OLS. 
#For example, the coefficient of marrydymmy in FE model is 9300.984 means married 
#individuals have higher wage by 9300.984 dollars compared with single individuals.
#Between estimator.The coefficient of marrydymmy in Between model is 6610.419 means
#married individuals have higher wage by 6610.419 dollars compared with single
#individuals. First difference.The coefficient of marrydymmy in FD model is 
#3504.5554 means married individuals have higher wage by 3504.5554 dollars 
#compared with single individuals.As we use between estimator, implying we assume 
#no unobserved heterogeneity between individuals. In other words, it uses just 
#cross-sectional variation. The first difference and within estimators are 
#consistent under all models (pool, RE, and FE). As T=2, both models are expected 
#to yield the same result. However, now we have T>2, so it's not surprising that 
#we do not yield the same result. In conclusion, the estimates of the parameter 
#differs across three methods are quite different. The reason is Between models 
#discards the time variation in data, but FE model completely discards the 
#individual variation in the data. As for FD, the requirement is it only uses 
#observations for which sample which does not containing NA for all variables. 
#Therefore, from the perspective of remaining data, it is common that three models 
#yield very different results. By the result of within and between, 
#we can infer the coefficient of RE will lie between these two coefficients 
#obtained from within and between. RE is the weighted average of between and 
#within estimates.  
