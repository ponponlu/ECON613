setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS3")
library(data.table)
library(survival)
library(nnet)
library(dplyr)
library(tinytex)
library(marginaleffects)
library(stringr)
library(snow)
library(margins)
library(nloptr)
library(boot)
library(mlogit)
datjss <- fread("datjss.csv",header=T)
datsss <- fread("datsss.csv",header=T)
datstu <- fread("datstu_v2.csv",header=T)
#1.1 Number of students, schools, programs
#We obtain number of students and programs from datstu.
nrow(datstu)
pro_c1 <- unique(datstu$choicepgm1)
pro_c2 <- unique(datstu$choicepgm2)
pro_c3 <- unique(datstu$choicepgm3)
pro_c4 <- unique(datstu$choicepgm4)
pro_c5 <- unique(datstu$choicepgm5)
pro_c6 <- unique(datstu$choicepgm6)
length(Reduce(union,list(pro_c1,pro_c2,pro_c3,pro_c4,pro_c5,pro_c6)))
#We obtain number of schools from datsss.
length(unique(datsss$schoolcode))
#1.2 Number of choices (school, program)
#(Hint: Create a matrix of school, programs. Convert data from Wide to Long)
#datstu %>% 
  #select(schoolcode1,schoolcode2,schoolcode3,
         #schoolcode4,schoolcode5,schoolcode6,
        #choicepgm1,choicepgm2,choicepgm3,
         #choicepgm4,choicepgm5,choicepgm6,) %>%
  #pivot_wider(names_from=year, values_from=mobile_subs) 
#V <- crossprod(table(datstu[5:16]))
sc1_6<-datstu[,c(5:10)]
sc1_6paste <- data.frame(unlist(sc1_6))
cp1_6 <-datstu[,c(11:16)]
cp1_6paste <- data.frame(unlist(cp1_6))
sccp <- cbind(sc1_6paste,cp1_6paste)
#Each individual has 6 rows in data
id <-rep(datstu$V1,6)
score <-rep(datstu$score,6)
agey <-rep(datstu$agey,6)
male <-rep(datstu$male,6)
jssdistrict <-rep(datstu$jssdistrict,6)
rankplace <-rep(datstu$rankplace,6)
Q1_2df <- data.frame(id, score, agey, male, sccp, jssdistrict,rankplace)
with(Q1_2df, table(sccp))
#crossprod(table(Q1_2df[5:6]))
#1.3 Number of students applying to at least one senior high schools 
#in the same district to home
#I remove NA and "" in datsss
#Also,since some schools have the same code but different name, I 
#only keep the entry with the max length of schoolname.
#codedis <- as.data.frame(unique(datsss[ , c("schoolcode","sssdistrict")]))
datsssnona <- datsss%>% filter(ssslong!="NA" & ssslat!="NA")%>%
  filter(schoolname!="")%>%
  select(c("schoolname","schoolcode","sssdistrict",
                                    "ssslong","ssslat"))%>%distinct()%>%
  group_by(schoolcode)%>% mutate(wordlength=str_length(schoolname))%>%
  slice(which.max(wordlength))
colnames(Q1_2df)[5] <-"schoolcode"
dfssstu <- left_join(Q1_2df,datsssnona, by = "schoolcode")
dfssstu$samelog <- dfssstu$jssdistrict == dfssstu$sssdistrict
dfssstu$samenum <- ifelse(dfssstu$samelog==TRUE,1,0)
nrow(dfssstu %>% group_by(id) %>% 
       summarise(number=sum(samenum),.groups = 'drop') %>%
  filter(number>=1))
#1.4 Number of students each senior high school admitted
Q1_4df <- Q1_2df  %>%
  filter(rankplace>=1 & rankplace<=6) %>% arrange(rankplace,id)
Q1_4g1 <- subset(Q1_4df, rankplace == 1)
v1 <- seq(1,nrow(Q1_4g1),by=6)
Q1_4g1complete <- Q1_4g1[v1,]
Q1_4g2 <- subset(Q1_4df, rankplace == 2)
v2 <- seq(2,nrow(Q1_4g2),by=6)
Q1_4g2complete <- Q1_4g2[v2,]
Q1_4g3 <- subset(Q1_4df, rankplace == 3)
v3 <- seq(3,nrow(Q1_4g3),by=6)
Q1_4g3complete <- Q1_4g3[v3,]
Q1_4g4 <- subset(Q1_4df, rankplace == 4)
v4 <- seq(4,nrow(Q1_4g4),by=6)
Q1_4g4complete <- Q1_4g4[v4,]
Q1_4g5 <- subset(Q1_4df, rankplace == 5)
v5 <- seq(5,nrow(Q1_4g5),by=6)
Q1_4g5complete <- Q1_4g5[v5,]
Q1_4g6 <- subset(Q1_4df, rankplace == 6)
v6 <- seq(6,nrow(Q1_4g6),by=6)
Q1_4g6complete <- Q1_4g6[v6,]
Q1_4comp <- rbind(Q1_4g1complete,Q1_4g2complete,Q1_4g3complete,
                  Q1_4g4complete,Q1_4g5complete,Q1_4g6complete)
table(Q1_4comp$schoolcode)
#1.5 The cutoff of senior high schools (the lowest score to be admitted)
Q1_4comp %>% group_by(schoolcode)%>%
  summarise(cutoff = min(score))
#1.6 The quality of senior high schools 
#(the average score of students admitted)
Q1_4comp %>% group_by(schoolcode)%>%
  summarise(quality = mean(score))
#Exercise 2 Data
#Create a school level dataset, where each row corresponds to a
#(school,program) with the following variables.
#2.1 the district where the school is located
#2.2 the latitude and longitude of the district
#2.3 cutoff (the lowest score to be admitted)
#2.4 quality (the average score of the students admitted)
#2.5 size (number of students admitted)
dfsclevel <- left_join(datsssnona,Q1_4comp,by= "schoolcode")%>%
group_by(schoolcode,unlist.cp1_6.)%>% mutate(cutoff=min(score))%>%
  mutate(quality=mean(score))%>%mutate(size=n())%>%
  distinct(schoolcode,unlist.cp1_6.,.keep_all = TRUE)%>% 
  select(schoolname,schoolcode,sssdistrict,ssslong,ssslat,
         unlist.cp1_6.,cutoff,quality,size)
#Exercise 3 Distance
#You should generate a value of distance for each of students choices.
step1df <- left_join(Q1_2df,datsssnona,by= "schoolcode")
dfjssstud <- left_join(step1df,datjss,by= "jssdistrict")
dist <- function(ssslong,ssslat,jsslong,jsslat){
  term1=69.172*(ssslong-jsslong)*cos(jsslat/57.3)
  term2=69.172*(ssslat-jsslat)
  distance=sqrt(term1^2+term2^2)
  return(distance)
}
dfjssstuddist<- dfjssstud %>% mutate(distance=
                dist(dfjssstud$ssslong,dfjssstud$ssslat,
     dfjssstud$point_x,dfjssstud$point_y))
head(dfjssstuddist)
#Exercise 4 Dimensionality Reduction
#4.1 Recode the schoolcode into its first three digits (substr).
#Call this new variable scode_rev.
dfsclevel$scode_rev=substr(dfsclevel$schoolcode,1,3)
#4.2 Recode the program variable into 4 categories: arts
#(general arts and visual arts), economics (business
#and home economics), science (general science) and others. 
#Call this new variable pgm rev.
#Divide all choice program into 4 groups
dfsclevel$pgm_rev=ifelse(
            dfsclevel$unlist.cp1_6.%in% c("General Arts","Visual Arts"),
       "arts",ifelse(dfsclevel$unlist.cp1_6.%in% c("Business","Home Economics")
,"economics",ifelse(dfsclevel$unlist.cp1_6.%in% "General Science",
"science","others")))
Q1_2df$pgm_rev=ifelse(
  Q1_2df$unlist.cp1_6.%in% c("General Arts","Visual Arts"),
  "arts",ifelse(Q1_2df$unlist.cp1_6.%in% c("Business","Home Economics")
      ,"economics",ifelse(Q1_2df$unlist.cp1_6.%in% "General Science",
              "science","others")))
Q1_2df$scode_rev=substr(Q1_2df$schoolcode,1,3)
Q1_2df$choice_rev<-paste(Q1_2df$scode_rev,Q1_2df$pgm_rev)
#4.3 Create a new choice variable choice_rev.
#i.e. Recoded schoolcode + recoded program.
dfsclevel$choice_rev <-paste(dfsclevel$scode_rev,dfsclevel$pgm_rev) 
head(dfsclevel)
#4.4 Recalculate the cutoff and the quality for each recoded choice.
#New quality=weighted average (summation size*quality/total size)
completedfsclevel <- dfsclevel %>% group_by(choice_rev) %>%
  filter(!is.na(cutoff))%>% 
  mutate(newcutoff=min(cutoff))%>% 
  mutate(eachquality=quality*size)%>% 
  mutate(total=sum(size))%>% 
  mutate(newquality=sum(eachquality,na.rm = T)/total)
#Consider the 20,000 highest score students.
Q1_4comp20000 <- Q1_4comp%>%arrange(desc(score))%>% slice(1:20000)
#From the result, I found 355 is the cutoff, but there are still some 
#people whose scores are also 355 are not included. Total=20249
Q1_4comp20000 <- Q1_4comp%>%arrange(desc(score))%>% filter(score>=355)
dfsclevel20000 <- left_join(datsssnona,Q1_4comp20000,by= "schoolcode")%>%
  group_by(schoolcode,unlist.cp1_6.)%>% mutate(cutoff=min(score))%>%
  mutate(quality=mean(score))%>%mutate(size=n())%>%
  distinct(schoolcode,unlist.cp1_6.,.keep_all = TRUE)%>% 
  select(schoolname,schoolcode,sssdistrict,ssslong,ssslat,
         unlist.cp1_6.,cutoff,quality,size)
dfsclevel20000$scode_rev=substr(dfsclevel20000$schoolcode,1,3)
dfsclevel20000$pgm_rev=ifelse(
  dfsclevel20000$unlist.cp1_6.%in% c("General Arts","Visual Arts"),
  "arts",ifelse(dfsclevel$unlist.cp1_6.%in% c("Business","Home Economics")
                ,"economics",ifelse(dfsclevel$unlist.cp1_6.%in% "General Science",
                                    "science","others")))
dfsclevel20000$choice_rev <-paste(dfsclevel20000$scode_rev,
                                  dfsclevel20000$pgm_rev) 
completedfsclevel20000 <- dfsclevel20000 %>% group_by(choice_rev) %>% 
  filter(!is.na(cutoff))%>%
  mutate(newcutoff=min(cutoff))%>%
  filter(newcutoff!=Inf)%>% 
  mutate(eachquality=quality*size)%>% 
  mutate(total=sum(size))%>% 
  mutate(newquality=sum(eachquality,na.rm = T)/total)
#The rest of the assignment uses the recoded choices 
#and the 20,000 highest score students.
#Exercise 5 First Model
#Using the new data with recoded choices, 
#we want to understand the effect of the student test score on
#his first choice.
#5.1 Propose a model specification. Write the Likelihood function.
#Since test score is invariant for an individual across alternatives.
#I would choose multinomial logit model.
df5_1 <- Q1_2df %>% group_by(id) %>% slice(1)
df5_1try <- df5_1%>% dplyr::select(id,score,choice_rev)%>%
  filter(!is.na(score))
df5_120000 <- df5_1try%>%arrange(desc(score))%>% filter(score>=355)%>%
 mutate(choice_rev_fac=as.factor(choice_rev))%>%
  mutate(choice_rev_num=as.numeric(choice_rev_fac))
#likelihood function
###three inputs in my function
like_fun = function(param,data,choice)
{score= data$score
ch = data$choice_rev_num
ni = nrow(data)
ut = mat.or.vec(ni,choice)
#I choose the first choice as my reference group
nj_ref <- choice-1
pn1    = param[1:nj_ref]
pn2    = param[(nj_ref+1):(2*nj_ref)]
ut[, 1] = rep(0, ni)
# multinomial logit
for (j in 2:choice)
{
  ut[,j] = pn1[(j-1)]+score*pn2[(j-1)]
}
prob  = exp(ut)       
prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
# match prob to actual choices
probc = NULL
for (i in 1:ni)
{
  probc[i] = prob[i,ch[i]]
}
probc[probc>0.999999] = 0.999999
probc[probc<0.000001] = 0.000001
like = sum(log(probc))
return(-like)
}
#testing using package
mymodel = multinom(choice_rev_num~score,data=df5_120000,maxit=10000)
cheatvec <- c(coef(mymodel))
restry = optim(cheatvec,fn=like_fun,method="BFGS",
            control=list(trace=6,REPORT=1,maxit=10000),data=df5_120000,
            choice=nrow(unique(df5_120000[,5])))
#Check whether the parameter estimated by my own function 
#is same as paackage
multiresult <- data.frame(cheatvec,restry$par)
head(multiresult)
#5.2 Estimate parameters and compute the marginal effect 
#of the proposed model. Since the sample size is too big to run
#the result efficiently, I only use 100 samples to compute ME. 
set.seed(123)
samplenum <-sample(1:nrow(df5_120000),100)
sample <-df5_120000[samplenum,]
sample$choice_rev_num <- as.factor(sample$choice_rev)
sample$choice_rev_num <- as.numeric(sample$choice_rev_num)
mymodelsmall = multinom(choice_rev_num~score,data=sample,maxit=10000)
cheatvecsample <- c(coef(mymodelsmall))
ressample = optim(cheatvecsample,fn=like_fun,method="BFGS",
                  control=list(trace=6,REPORT=1,maxit=10000),data=sample,
                  choice=nrow(unique(sample[,5])))
m=length(unique(sample$choice_rev_num))
#Marginal Effect
#use package
mfxmulti <- marginaleffects(mymodelsmall, type = "probs")
mfxmulti$dydx
#I found the package discards the 8th choice.
#However, in my own function, I choose the 1st choice 
#as the reference group
c_constant  <- c(0,cheatvecsample[1:m-1])
c_score     <- c(0,cheatvecsample[m:68])
mlogit_prob_matrix = function(param,data) 
{ 
  score = data$score 
  ch = data$choice_rev_num 
  ni = nrow(data) 
  nj = nrow(unique(data[,3])) 
  nj_ref <- nj-1
  ut = mat.or.vec(ni,nj) 
  # multinomial logit pij matrix 
  pn1 = param[1:nj_ref] 
  pn2 = param[(nj_ref+1):(2*nj_ref)] 
  ut[,1]<-rep(0,ni) 
  for (j in 2:nj) 
  { 
    ut[,j] = pn1[j-1] + score*pn2[j-1] 
  } 
  prob = exp(ut)  
  prob = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  return(prob[,-1])
} 
prob_matrix<-mlogit_prob_matrix(ressample$par,sample) 
prob_matrix 
colnames(prob_matrix) <- c(2:m) 
#Computation by formula 
mlogit_ME=matrix(0,nrow=100, ncol=m-1) 
for (i in 1:100) { 
  beta_bar <- sum(prob_matrix[i,]*c_score[-1]) 
  mlogit_ME[i,]=prob_matrix[i,]*(c_score[-1]-beta_bar) 
} 
#compare results 
#The result (multicompare) is similar from the 701th row. 
#Write the sequence of each group explicitly, then we can easily see why.
#mfxmulti:  1,2,3,4,5,6,7,9,10,...
#mlogit_ME: 2,3,4,5,6,7,8,9,10,...
multicompare <- data.frame(mfxmulti$dydx[101:3400],
                           c(mlogit_ME)[-c(701:800)])
#The result is similar but it's not exactly the same since me and package
#choose the different choice as the reference group.
head(multicompare)
mlogit_me=apply(mlogit_ME, 2, mean) 
mlogit_me
#Exercise 6 Second Model
#Using the new data with recoded choices, 
#we want to understand the effect of the school quality on the
#first choice.
#6.1 Propose a model specification.
#Using the new data with recoded choices, 
#we want to understand the effect of the school quality on the
#first choice.
#Here, I choose conditional logit model since the school quality varies among choices. (For the simplicity, we only focus on one choice. 
#The following table is just an illustration,so I only have two choices)
#Although the most ideal case is have both variations across choices and 
#person, having a variation in each row is enough to use conditional logit model. 
# id    nj1  nj2     ch       id   nj1   nj2  ch
#  1    200   NA      1  =>    1   200   150   1  
#  2    NA    150     2        2   200   150   2
# Write the Likelihood function.
schoolscore <- completedfsclevel%>%select(choice_rev,newquality)%>%
  distinct()
schoolscore20000 <- completedfsclevel20000%>%
  select(choice_rev,newquality)%>%distinct()
#df6_1 <- left_join(df5_1,schoolscore,by= "choice_rev")%>%
#select(id,choice_rev,newquality)%>%
#filter(choice_rev!="NA others")
df6_120000 <- left_join(df5_120000,schoolscore,by= "choice_rev")%>%
  select(id,choice_rev,newquality)%>%
  filter(choice_rev!="NA others")%>%
  arrange(choice_rev)%>%
  mutate(choice_rev_fac=as.factor(choice_rev))%>%
  mutate(choice_rev_num=as.numeric(choice_rev_fac))
#df6_1wide <- df6_120000%>% pivot_wider(names_from=choice_rev_num,
#values_from=newquality)
#df6_1_long <- df6_1wide %>% 
#pivot_longer(cols = -id, names_to = "choice_rev_num", 
#values_to = "newquality")
#melt(df6_120000, id.vars=c("id","choice_rev",
#"choice_rev_fac","choice_rev_num"))
sample6_1 <-df6_120000[samplenum,]
sample6_1$choice_rev_num <- as.factor(sample6_1$choice_rev)
sample6_1$choice_rev_num <- as.numeric(sample6_1$choice_rev_num)
m6_1=length(unique(sample6_1$choice_rev_num))
#m6=length(unique(df6_120000$choice_rev_num))
clike_fun = function(param,data)
{quality= data$newquality
ch = data$choice_rev_num
ni = nrow(data)
nj =nrow(unique(data[,5]))
ut = mat.or.vec(ni,nj)
# conditional logit
pn1    = param[1:(nj-1)]
#pn2    = param[(nj+1):(2*nj)]
for (j in 2:nj)
{
  ut[,j] =pn1[j-1]+quality[j]*param[nj]
}
prob  = exp(ut)       
prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
# match prob to actual choices
probc = NULL
for (i in 1:ni)
{
  probc[i] = prob[i,ch[i]]
}
probc[probc>0.999999] = 0.999999
probc[probc<0.000001] = 0.000001
like = sum(log(probc))
return(-like)
}
#6.2 Estimate parameters and compute the marginal effect 
m6_1=length(unique(sample6_1$choice_rev_num))
start6_1=runif(m6_1,-0.05,0.05)
restrycon = optim(start6_1,fn=clike_fun,method="BFGS",
                  control=list(trace=6,REPORT=1,maxit=10000),data=sample6_1)
#restrycon$par
#Marginal effect
co_quality <- restrycon$par[1]
clogit_prob_matrix = function(param,data){
  quality     =  data$newquality
  ch          =  data$choice_rev_num
  ni = nrow(data)
  nj = nrow(unique(data[,5]))
  ut = mat.or.vec(ni,nj)
  data_unique=distinct(data,choice_rev_num,.keep_all = TRUE)
  quality_unique = data_unique[order(data_unique$choice_rev_num),]$newquality
  # conditional logit
  for (j in 2:nj)
  {
    pn1 = param[1:(nj-1)]
    ut[,j] = pn1[j-1] + param[nj]*quality[j]
  }
  prob  = exp(ut)       
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  return(prob)
}
#Pij matrix for the conditional logit
prob_matrix_clogit <- clogit_prob_matrix(restrycon$par,sample6_1)
#Compute ME by the formula on the slide
#If j=k, delta=1. Otherwise, delta=0.
deltaijk=array(0,dim = c(100,m6_1,m6_1))
for (i in 1:100) {
  diag(deltaijk[i,,]) <- 1
}
clogit_ME=array(0,dim=c(100,m6_1,m6_1))
for (i in 1:100) {
  for (j in 1:m6_1) {
    for (k in 1:m6_1) {
      clogit_ME[i,j,k]=prob_matrix_clogit[i,j]*(deltaijk[i,j,k]-prob_matrix_clogit[i,k])*co_quality
      
    }
  }
}
clogit_me=apply(clogit_ME,c(2,3),mean)
clogit_me
#Exercise 7
#In this exercise, we are interested in the effect of excluding choices 
#where the program is others.
#7.1 Explain and justify, which model (first or second model) you think 
#is appropriate to conduct this exercise.
#I think first model is more appropriate.i.e. multinomial model
#I think school qualitiy is endogenous because it is determined
#by the average score of admitted students. 
#Using multinomial model can study the impact of score on choice 
#after "others" are excluded.
like_fun72 = function(param,data,choice)
{score= data$score
ch = data$choice_rev_num
ni = nrow(data)
ut = mat.or.vec(ni,choice)
#I choose the first choice as my reference group
nj_ref <- choice-1
pn1    = param[1:nj_ref]
pn2    = param[(nj_ref+1):(2*nj_ref)]
ut[, 1] = rep(0, ni)
# multinomial logit
for (j in 2:choice)
{
  ut[,j] = pn1[(j-1)]+score*pn2[(j-1)]
}
prob  = exp(ut)       
prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
return(prob)
}
prob4choices <- like_fun72(ressample$par,sample,nrow(unique(sample[,5])))
head(prob4choices)
#7.3
#But now I exclude "others" and compute the probability of each choice.
#The difference is that the denominator is smaller compared with 5.2
#since some choices coded as "others" are excluded. 
#The probability is computed by the revised function from 5.2. (see below)
df7 <- sample %>% filter(!grepl('others', choice_rev))
df7$choice_rev_fac <- as.factor(df7$choice_rev)
df7$choice_rev_num <- as.numeric(df7$choice_rev_fac)
filter <- sample %>% filter(grepl('others', choice_rev))
filterdf <- filter[!duplicated(filter$choice_rev),]
othersnum <- filterdf$choice_rev_num
prob72_like_fun = function(param,data,choice)
{score= data$score
ch = data$choice_rev_num
ni = nrow(data)
ut = mat.or.vec(ni,choice)
#I choose the first choice as my reference group
nj_ref <- choice-1
pn1    = param[1:nj_ref]
pn2    = param[(nj_ref+1):(2*nj_ref)]
ut[, 1] = rep(0, ni)
# multinomial logit
for (j in 2:choice)
{
  ut[,j] = pn1[(j-1)]+score*pn2[(j-1)]
}
newut <- ut[,-othersnum]
prob  = exp(ut)
newtotalprob=exp(newut)
prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(newtotalprob))
return(prob)
}
prob7exclude <- prob72_like_fun(ressample$par,sample,
                                nrow(unique(sample[,5])))
head(prob7exclude)
