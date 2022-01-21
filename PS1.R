###A1_01/19/2022
library(data.table)
library(bit64)
library(ggplot2)
library(dplyr)
library(tidyr)
#library(esquisse)
#The goal of this first exercise is to introduce
#the students to basic data manipulations. Open the corresponding
#dataset, and report the following statistics:
#I put household-level data into the file called datahh and
#I put individual-level data into the file called dataind
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1/datahh")
#1.1 Number of households surveyed in 2007
dathh2007 <- fread(file="dathh2007.csv", 
                  header=T)
n_occurhh2007 <- data.frame(table(dathh2007$idmen))
#Check whether there are repeated item with the same idmen in data
n_occurhh2007[n_occurhh2007$Freq > 1,]
#The output tells me there are no repeated idmen.
length(dathh2007$idmen)
#1.2 Number of households with a marital status ¡§Couple, with kids¡¨ in 2005.
dathh2005 <- fread(file="dathh2005.csv", 
                      header=T)
n_occurhh2005 <- data.frame(table(dathh2005$idmen))
#Check whether there are repeated item with the same idmen in data
n_occurhh2005[n_occurhh2005$Freq > 1,]
#length(which(dathh2005$mstatus=="Couple, with Kids"))
nrow(dathh2005 %>% filter(mstatus=="Couple, with Kids"))
#1.3 Number of individuals surveyed in 2008.
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1/dataind")
datind2008 <- fread("datind2008.csv",header=T)
#Check whether there are repeated item with the same idind in data
n_occurind2008 <- data.frame(table(datind2008$idind))
n_occurind2008 [n_occurind2008$Freq>1,]
length(datind2008$idind)
#1.4 Number of individuals aged between 25 and 35 in 2016.
#I assumed between 25 and 35, exclusive
datind2016 <- fread("datind2016.csv",header=T)
#length(which(datind2016$age<35 & datind2016$age>25))
nrow(datind2016 %>% filter(age<35&age>25))
#1.5 Cross-table gender/profession in 2009.
#source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
datind2009 <- fread(file="datind2009.csv", 
                       header=T)
table(datind2009$gender,datind2009$profession)
#crosstab(datind2009, row.vars = "gender", col.vars = "profession")
#1.6 Distribution of wages in 2005 and 2019. 
#Report the mean, the standard deviation, the inter-decile ratio
#D9/D1 and the Gini coefficient. Write a function to compute
library(DescTools)
datind2005 <- fread(file="datind2005.csv", 
                       header=T)
datind2019 <- fread(file="datind2019.csv", 
                       header=T)
datind2005 %>%summarize(mean=mean(wage,na.rm = T),
                        sd=sd(wage, na.rm = T),
                        D9D1=quantile(wage,prob=c(0.1,0.9),na.rm = T)[[2]]/
                          quantile(wage,prob=c(0.1,0.9),na.rm = T)[[1]])
datind2019 %>%summarize(mean=mean(wage,na.rm = T),
                        sd=sd(wage, na.rm = T),
                        D9D1=quantile(wage,prob=c(0.1,0.9),na.rm = T)[[2]]/
                          quantile(wage,prob=c(0.1,0.9),na.rm = T)[[1]])
#Gini(datind2005$wage, unbiased = TRUE, conf.level = NA, na.rm = T)
#mean(datind2019$wage,na.rm = T)
#sd(datind2019$wage, na.rm = T)
#quantile(datind2019$wage,prob=c(0.1,0.9),na.rm = T)[[2]]/
  #quantile(datind2019$wage,prob=c(0.1,0.9),na.rm = T)[[1]]
###Gini
Gini <- function (x, corr = FALSE, na.rm = TRUE) 
{if (!na.rm && any(is.na(x))) 
    return(NA_real_)
  x <- as.numeric(na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- sum(x * 1:n)
  G <- 2 * G/sum(x) - (n + 1)
  if (corr) 
    G/(n - 1)
  else G/n
}
Gini(datind2005$wage)
Gini(datind2019$wage)
d1_62005 <- density(datind2005$wage,na.rm = T) # returns the density data
plot(d1_62005) # plots the results
d1_62019 <- density(datind2019$wage,na.rm = T) # returns the density data
plot(d1_62019) # plots the results
#1.7 Distribution of age in 2010. Plot an histogram. 
#Is there any difference between men and women?
datind2010 <- fread(file="datind2010.csv", 
                       header=T)
hist(datind2010$age)
# Kernel Density Plot
d <- density(datind2010$age) # returns the density data
plot(d) # plots the results
#grp <- cut(datind2010$age, breaks = seq(min(datind2010$age),
                                        #max(datind2010$age),7), 
           #include.lowest = TRUE)
#barchart(datind2010$age~grp, groups =datind2010$gender)
ggplot(datind2010, aes(x=age, color=gender)) +
  geom_histogram(fill="white", alpha=0.5,bins=30,
                 position="identity")
#Roughly speaking,there are no gender differences in every age cut.
#1.8 Number of individuals in Paris in 2011.
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1/datahh")
dathh2011 <- fread("dathh2011.csv", 
                      header=T)
#First, read dathh2011.csv to know which household lives in Paris
#Also, we collect those idmen number and save it in Paris2011_num
Paris2011_num <- as.numeric(dathh2011[location=="Paris",idmen])
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1/dataind")
datind2011 <- fread(file="datind2011.csv", 
                       header=T)
#However, we know # of households is not equal to # of individuals
#Therefore, we need to find out how many times do Paris2011_num
#appear in individual level data ,that is datind2011.
datind2011_idmennum <- as.numeric(unlist(datind2011$idmen))
#In other words, we find common elements on two different length vectors
#using which(%in%)
length(which(datind2011_idmennum %in% Paris2011_num))
###Exercise 2 Merge Datasets
# In the first part of this exercise, we will learn how to merge datasets.
# 2.1 Read all individual datasets from 2004 to 2019. 
#Append all these datasets.
library(purrr)
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1/dataind")
temp = list.files(pattern="*.csv")
appendind = lapply(temp, fread, colClasses = c(idind="character",
                                               idmen="character"))
datind0419 <- do.call("rbind", appendind)
#2.2 Read all household datasets from 2004 to 2019. 
#Append all these datasets.
setwd("C:/Users/LISA/Desktop/Duke/ECON613/PS1/datahh")
hh = list.files(pattern="*.csv")
appendhh = lapply(hh, fread,colClasses = c(idmen="character"))
dathh0419 <- do.call(rbind.data.frame, appendhh)
#2.3 List the variables that are simultaneously present 
#in the individual and household datasets.
#Since the first column of every data set has no column names,
#R names it as V1 automatically. Here, I assume V1 is also a variable.
#Therefore, there are 3 variables simultaneously present in both datasets.
intersect(colnames(datind0419), colnames(dathh0419))
#2.4 Merge the appended individual and household datasets.
#Now, V1 is useless, so it should be discarded
#for the sake of appending both datasets.
dathh0419_delv1 <- dathh0419[,-1]
datind0419_delv1 <- datind0419[,-1]
merge0419raw <- merge(datind0419_delv1,dathh0419_delv1,
      by=c("idmen","year"),all = TRUE)
#Check whether every idind in each year is not double counted
#I found there are 32 repeated idinds in year 2013.I suppose
#those data are not reliable and decide to discard those data.
merge0419time <- merge0419raw %>% group_by(year,idind) %>% 
  mutate(time = n())
#merge0419 is the dataframe filtering out the repeated inind in 2013
#That is, only the data with time=1 will be left in merge0419
merge0419 <- merge0419time %>%filter(time<2)
#2.5 Number of households in which there are more than four family members
merge0419_byyear = merge0419 %>% group_by(year)
number <- c()
for(i in 1:16){
  hhind <- data.frame(table(group_split(merge0419_byyear)[[i]][1]))
  number[i] <- nrow(hhind[hhind$Freq >4,])
}
year <- c(2004:2019)
rbind(year,number)
#2.6 Number of households in which at least one member is unemployed
allunemp <- merge0419 %>% filter(empstat == "Unemployed")%>% 
  group_by(year)
number_unemp <- c()
for(i in 1:16){
  number_unemp[i] <- nrow(unique(group_split(allunemp)[[i]][1]))
}
rbind(year,number_unemp)
#2.7 Number of households in which 
#at least two members are of the same profession
#We first replace blank by NA
merge0419[which(merge0419$profession == ""),6] <- NA 
#We get the table telling us how many times each profession
#appear in each idmen and year.
count <- count(merge0419 %>% group_by(year,idmen),profession)
#In this case, we don't consider two NAs as the same profession.
countwithoutna <- count %>% drop_na(profession)
countallyear <- countwithoutna%>% filter(n >=2)%>% group_by(year)
num <- c()
for(i in 1:16){
  num[i] <- nrow(unique(group_split(countallyear)[[i]][2]))
}
#The final result
rbind(year,num)
#2.8 Number of individuals in the panel 
#that are from household-Couple with kids
allcoupwkids <- merge0419 %>% filter(mstatus=="Couple, with Kids")%>% 
  group_by(year)
number_allcoupwkids <- c()
for(i in 1:16){
  number_allcoupwkids[i] <- nrow(group_split(allcoupwkids)[[i]])
}
rbind(year,number_allcoupwkids)
#2.9 Number of individuals in the panel that are from Paris.
allParis <- merge0419 %>% filter(location=="Paris")%>% 
  group_by(year)
number_allParis <- c()
for(i in 1:16){
  number_allParis[i] <- nrow(group_split(allParis)[[i]])
}
rbind(year,number_allParis)
#2.10 Find the household with the most number of family members. 
#Report its idmen.
merge0419_byyear = merge0419 %>% group_by(year)
maxnum <- c()
for(i in 1:16){
  hh <- data.frame(table(group_split(merge0419_byyear)[[i]][1]))
  maxnum[i] <-max(hh$Freq) 
}
maxnum==max(maxnum)
#maxnum tells us 2007 and 2010 have the most number of family members
#4 and 7 are TRUE, so both 2007 and 2010 have 14 members in a household
df2007 <- data.frame(table(group_split(merge0419_byyear)[[4]][1]))
df2007[which(df2007$Freq==14),1]
df2010 <- data.frame(table(group_split(merge0419_byyear)[[7]][1]))
df2010[which(df2010$Freq==14),1]
#2.11 Number of households present in 2010 and 2011.
nrow(unique(group_split(merge0419_byyear)[[7]][1]))
nrow(unique(group_split(merge0419_byyear)[[8]][1]))
#group_split(merge0419_byyear)[[7]][2]
#group_split(merge0419_byyear)[[8]][2]
nrow(intersect(group_split(merge0419_byyear)[[7]][1],
               group_split(merge0419_byyear)[[8]][1]))
###Exercise3
#3.1 Find out the year each household enters and exit the panel. 
#Report the distribution of the time spent 
#in the survey for each household.
merge0419 %>% group_by(idmen)%>%
  summarize(enter=min(year),exit=max(year))
table <- as.data.frame(table(dathh0419_delv1$year,dathh0419_delv1$idmen))
df3_1 <- table %>% group_by(Var2)%>%summarize(timespent=sum(Freq))
hist(df3_1$timespent)
#3.2 Based on datent, identify whether or not a household moved into its 
#current dwelling at the year of survey. 
#Report the first 10 rows of your result and 
#plot the share of individuals in that situation across years.
#Idea: Compare year and datent
list <- merge0419 %>% group_by(idmen,year)%>%
  summarize(sum(year==datent))
#TF is a dummy variable. TF=1 indicates year=datent.
list$TF <- ifelse(list$`sum(year == datent)`==0,0,1)
#numerator is the number of individuals with TF=1
#denominator is the total number of individuals in each year's data
dftest <- list %>% group_by(year)%>%
  summarize(numerator=sum(TF,na.rm = T),
            demoniator=n( ),
            share=numerator/demoniator)
ggplot(data=dftest, aes(x=year, y=share)) +
  geom_line()
datentresult <- list[1:10,]
#In the first 10 rows of my result, we can see only one household
# idmen=1200010066630100
#moved into its current dwelling at the year of survey (i.e.2005).
#3.3 Based on myear and move, identify whether or not household migrated
#at the year of survey. 
#Report the first 10 rows of your result 
#and plot the share of individuals in that situation across years.
#For the former part, check whether year==myear
#move=1:lives at the same place as in the previous survey
#move=2: the household has moved since last survey
#Df <- merge0419 %>% filter(!is.na(move))
#Df <- Df %>% arrange (idmen,idind,year, move) %>% 
#select (c("idmen","idind", "year", "move"))
merge0419$movebefore2014 <- ifelse(merge0419$year==merge0419$myear,1,0)
merge0419$moveafter2014 <- ifelse(merge0419$move==2,1,0)
condition <- merge0419$year==merge0419$myear|merge0419$move==2
merge0419$movedummy <- ifelse(condition,1,0)
df3_3 <- merge0419 %>%
  select(c("idmen","year","movebefore2014","moveafter2014"))%>%unique()
#movebefore2014=1 represents the household migrated at the year of survey. 
print(df3_3[1:10,])
df3_3_plot <- merge0419 %>% group_by(year)%>%
  summarize(numerator=sum(movedummy,na.rm = T),
            demoniator=n( ),
            share=numerator/demoniator)
ggplot(data=df3_3_plot, aes(x=year, y=share)) +
  geom_line()
#3.4 Mix the two plots you created above in one graph, 
#clearly label the graph. Do you prefer one method
#over the other? Justify
#Put two together
table1 <- df3_3_plot %>% select(c("year","share"))%>%
  mutate(type="myear/move")
table2 <- dftest %>% select(c("year","share"))%>%
  mutate(type="datent")
df3_4plot <- rbind(table1,table2)
#The blue line indicates the share we computed in 3.3
#The red line indicates the share we computed in 3.2
ggplot(data=df3_4plot, aes(x=year, y=share,color=type)) +
  geom_line()
#I prefer the method defined in 3.2 (i.e. Datent) over that in 3.3 
#since I care more about which exact year does an individual move 
#rather than whether one moves or not during the period. 
#3.5 For households who migrate, find out how many households 
#had at least one family member changed his/her profession
#or employment status.
#Idea: diff(vector)
#I choose definition 1 of migration (i.e.datent) here.
mergemove <- merge0419 %>% group_by(idmen) %>%
  mutate(move_TF=ifelse(year==datent,1,0)) %>%
  filter(move_TF==1)
#Case 1, I consider the change over 16 years.
#mergefinal <- merge0419 %>% group_by(idind)%>% 
  #mutate(prochange=length(unique(profession)))%>% 
  #mutate(empchange=length(unique(empstat)))
#nrow(mergefinal %>% filter(prochange >1|empchange > 1) %>%
       #mutate(move_TF=ifelse(year==datent,1,0))%>%
       #filter(move_TF==1)%>%
  group_by(idmen)%>% summarise(COUNT = n()))
#Case 2, I change the period to (2004,2005),(2005,2006),...
change <- matrix(data=NA,15,2)
for(i in 2004:2018){
  subdata35 <- subset(merge0419,year==i|year==i+1)
  subdata35_final <- subdata35 %>% group_by(idind)%>% 
    mutate(prochange=length(unique(profession)))%>% 
    mutate(empchange=length(unique(empstat)))
  change[i-2003,2] <- nrow(subdata35_final %>% filter(prochange >1|empchange > 1) %>%
                      mutate(move_TF=ifelse(year==datent,1,0)) %>%
                      filter(move_TF==1)%>%group_by(idmen)%>% 
                        summarise(COUNT = n()))
}
change[,1] <- c(2005:2019)
change
#Exercise4
#Compute the attrition across each year, where attrition is defined as
#the reduction in the number of individuals staying in the data panel. 
#Report your final result as a table in proportions.
#Hint: Construct a year of entry and exit for each individual.
#Idea:Suppose we have a dummy =1 if an individual stays in the panel. 
#=0 if an  individual doesn't stay in the panel.
#By definition, we only care about the situation 1 0
#This combination 1 0 means the reduction in an individual
#staying in the panel in year i and leaving in year i+1
df4 <- data.frame(table(merge0419$idind,merge0419$year))
rateofattrition <- matrix(data=NA,15,2)
for (i in 2004:2018) {
  subdata<-subset(df4,Var2==i|Var2==i+1)
  att<-subdata %>% group_by(Var1) %>% summarise(diff=Freq[2]-Freq[1])
  denominator <- nrow(subdata %>% filter(Freq==1 & Var2==i))
  rateofattrition[i-2003,2]<-length(which(att$diff==-1))/denominator
                             }
rateofattrition[,1] <- c(2005:2019)
print(rateofattrition)