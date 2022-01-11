install.packages("BB")
library(BB)
source("A1.R")
?for
dir()
1+1
2/2
save.image("misc.RDATA")
1:10
30%%4
###1.2
packages=c("Hmisc", "gdata,boot","xtable","MASS","moments",
           "snow","mvtnorm")
install.packages(packages)
###1.3
setwd("C:/Users/LISA/Desktop/Duke/ECON613")
###1.4
dir()
ls()
###1.5
678%%9
###1-2
vec0 = NULL
vec1 = c(1,2,3,4)
vec2 = 1:4
vec3 = seq(1,4,1)
vec4 = rep(0,4)
sum(vec1)
str(vec1)
prod(vec1)
mat1 = mat.or.vec(2,2)
mat.or.vec(1,3)
mat2 = matrix(0,ncol=2,nrow=2,byrow=T)
mat3 = cbind(c(0,0),c(0,0))
mat4 = rbind(c(1,1),c(0,0))
mat5 = matrix(1:20,nrow=5,ncol=4)
arr1 = array(0,c(2,2))
dim(mat4)
dim(vec2)
length(vec2)
length(mat1)
class(mat4)
###Exercise2
install.packages("titanic")
library(titanic)
table(Titanic)
data(Titanic)
Titanic_df <- as.data.frame(Titanic)
###2.a
summary(Titanic)
sum(Titanic)
###2.b
dimnames(Titanic)
#Method2
sum(Titanic[,,"Adult",])
###2.c
adult <- Titanic[,,"Adult",]
crew_adult <- adult[[4]]+adult[[8]]+adult[[12]]+adult[[16]]
child <- Titanic[,,"Child",]
crew_child <- child[[4]]+child[[8]]+child[[12]]+child[[16]]
crew=crew_adult+crew_child
#Method2
sum(Titanic["Crew",,,])
###2.d 3rd class children
thirdclass_child <- child[[3]]+child[[7]]+child[[11]]+child[[15]]
#Method2
sum(Titanic["3rd",,"Child",])
###2.e 2nd adult female
secondadfe <- adult[[6]]+adult[[14]]
#Method2
sum(Titanic["2nd","Female","Adult",])
###2.f f1st class children male
firstchildmale <- child[[1]]+child[[9]]
sum(Titanic["1st","Male","Child",])
###2.g Female Crew survivor
Survived <- Titanic[,,,"Yes"]
femcrewsur <- Survived[[8]]+Survived[[16]]
#Method2
sum(Titanic["Crew","Female",,"Yes"])
###2.h 1st class adult male survivor
firstadultsur <- Survived[[1]]+Survived[[9]]
#Method2
sum(Titanic["1st","Male", ,"Yes"])
###2.2(a)proportion of survivors among first class, male, adult
suradult <- Titanic[,,"Adult","Yes"]
prop.table(suradult)[1]
#a Method2
prop.table(Titanic["1st","Male","Adult",])
###2.2(b)proportion of survivors among first class, female, adult
prop.table(suradult)[5]
#b Method2
prop.table(Titanic["1st","Female","Adult",])
###2.2(c)proportion of survivors among first class, male, child
surchild <- Titanic[,,"Child","Yes"]
prop.table(surchild)[1]
#c Method2
prop.table(Titanic["1st","Male","Child",])
###2.2.(d)proportion of survivors among third class, female, adult
prop.table(suradult)[7]
#d Method2
prop.table(Titanic["3rd","Female","Adult",])
###Exercise3
#1. Use three different ways, to create the vectors
#(a) a = 1, 2, . . . , 50
#(b) b = 50, 49, . . . , 1
#Hint : rev
a1 <- c(1:50)
b1 <- c(50:1)
a2 <- seq(1,50)
b2 <- seq(50,1)
b3 <- rev(a1)
a3 <- rev(b1)
#2. Create the vectors
#(a) a = 10, 19, 7, 10, 19, 7, . . . , 10, 19, 7 with 15 occurrences of 10,19,7
#(b) b = 1, 2, 5, 6, . . . , 1, 2, 5, 6 with 8 occurrences of 1,2,5,6
#Hint : rep
a <- rep(c(10,19,7),15)
b <- rep(c(1,2,5,6),8)
#Create a vector of the values of log(x)sin(x) at x = 3.1, 3.2, . . . , 6
x <- seq(3.1,6,0.1)
vectorlogsin <- log(x)*sin(x)
#Using the function sample, draw 90 values between (0,100) 
#and calculate the mean. Re-do
#the same operation allowing for replacement.
mean(sample(0:100,size=90,replace=FALSE))
mean(sample(0:100,size=90,replace=TRUE))
#3.5.a
a <- c(1:20)
b <- c(1:15)
numerator <- rep((exp(sqrt(a))*log(a^5)),15)
denommatrix <- matrix(NA,20,15)
for(j in 1:length(a)){
  for(i in 1:length(b)){
    denommatrix[j,i]=5+(cos(j)*sin(i))
  }
}
denominator <- c(denommatrix)
sum(numerator/denominator)
#Method2(sol)
a = 1:20
b = t(c(1:15))
sum(exp(sqrt(a))*log(a^5)/(5+cos(a)%*%sin(b)))
#3.5.b
#6. Create a vector of the values of exp(x) cos(x) at x = 3, 3.1, ...6
x <- seq(3,6,0.1)
vecexpcos <- exp(x)*cos(x)
#4.1 Create two vectors xVec and yVec 
#by sampling 1000 values between 0 and 999.
xVec <- sample(0:999,size=1000,replace=FALSE)
yVec <- sample(0:999,size=1000,replace=FALSE)
#Create the vector (y2 ??? x1, . . . , yn ??? xn???1) denoted by zVec
zVec <- c()
for (i in 1:999){
  zVec[i]=yVec[i+1]-xVec[i]
}
#create a vector wVec siny1/cosx2,...,sinyn-1/cosxn
wVec <- c()
num <- sin(yVec)
den <- cos(xVec)
for (i in 1:999){
  wVec[i]=num[i]/den[i+1]
}
#(c) Create a vector subX which consists of the values 
#of xVec which are ?‰¥ 200.
subX <- xVec[c(which(xVec>=200))]
#(d) What are the index positions in yVec 
#of the values which are ?‰¥ 600
which(yVec>=600)
#Exercise5 Create the matrix A 
A <- matrix(data=c(1,1,3,5,2,6,-2,-1,-3),3,3,byrow=T)
#(a) Check that A3=0 (matrix 0)
A%*%A%*%A==0
#(b) Bind a fourth column as 
#the sum of the first and third column
B <- matrix(data=NA,3,4,byrow=T)
B[,4] <- A[,1]+A[,3]
B[,1:3] <- A[,1:3]
print(B)
#(c) Replace the third row by the sum of the first and second row
A[3,] <- A[1,]+A[2,]
print(A)
#(d) Calculate the average by row and column.
A <- matrix(data=c(1,1,3,5,2,6,-2,-1,-3),3,3,byrow=T)
print(rowMeans(A)) 
print(colMeans(A))
#5.2
#2x + y + 3z = 10 (1)
#x + y + z = 6 (2)
#x + 3y + 2z = 13 (3)
r1 <- c(2,1,3)
r2 <- c(1,1,1)
r3 <- c(1,3,2)
LHS <- rbind(r1,r2,r3)
RHS <- c(10,6,13)
solve(LHS,RHS)
#Exercise6
#6.1 Write a function fun1 which takes two arguments (a,n) 
#where (a) is a scalar and n is a
#positive integer, and returns a/1+a^2/2+a^3/3+...+a^n/n
fun1 <- function(a,n){
  denominator <- c(1:n)
  numerator <- c(a^(1:n))
  answer=sum(numerator/denominator)
  return(answer)
}
#6.2 Consider the function
#E.g.Multiple conditions
if(score>=90){
  print("Àu¨q")
}else if(score>=60){
  print("¤Î®æ")
}else{
  print("¤£¤Î®æ")
}
func2 <- function(input){
  if(input<0){
    answer=input^2+2*input+abs(input)
  }else if(input>=2){
    answer=input^2+4*input-14
  }else{
    answer=input^2+3+log(1+input)
  }
  return(answer)
}
func2(-3)
func2(0)
func2(3)
#Exercise7
#1. Sample 36 values between 1 and 20 and name it v1
v1 <- sample(1:20,size=36,replace=T)
#2. Use two different ways to create the subvector of elements 
#that are not in the first position
#of the vector. Hint: which and subset can not be used. 
#Check x[a] and x[-a].
#7.2 Method1
v1[-1]
#7.2 Method2
v1[2:36]
#7.3 Create a logical element (TRUE or FALSE), v2, 
#which is true if v1 > 5. Can you convert
#this logical element into a dummy 1 (TRUE) and 0 (FALSE)?
v2 <- c()
for(i in 1:36){
  if (v1[i]>5) v2[i]=TRUE else v2[i]=FALSE
}
if (v2[i]==TRUE) v2[i]=1 else v2[i]=0
#ifelse single line
#e.g. if(3 > 2) TRUE else FALSE
#7.4 Create a matrix m1 [6 ?? 6] which is filled 
#by row using the vector v1.
m1 <- matrix(data=v1, nrow=6, ncol=6, byrow = T)
m1
#7.5 Create the following object
#x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10))
x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
#7.6 Test for the position of missing values, and non-finite values. 
#Return a subvector free of missing and non-finite values.
which(x==NaN)
which(x==NA)
condition = is.na(x) + is.infinite(x)
x[!condition]
#Exercise8
#8.1 Load the library AER, and the dataset (data(?€GSOEP9402?€?)) 
#to be named dat.
install.packages("AER")
library(AER)
data("GSOEP9402", package = "AER")
dat = GSOEP9402
#8.2 What type of object is it? Find the number of rows and column? 
#Can you provide the names of the variables?
class(dat)
dim(dat)
variable.names(dat)
#8.3 Evaluate and plot the average annual income by year.
#Method1
library(dplyr)
dat %>%
  group_by(year) %>%
  summarise_at(vars(income), list(name = mean))
#Method2
aggregate(dat$income, list(dat$year), FUN=mean) 
annualincome <- aggregate(income ~ year, data=dat, mean)
plot(x=annualincome$year,y=annualincome$income)
#8.4 
#Create an array that illustrates simultaneously 
#the income differences (mean) by gender,
#school and memployment.
agg1 <- aggregate(income ~ gender, data=dat, mean)
agg2 <- aggregate(income ~ school, data=dat, mean)
agg3 <- aggregate(income ~ memployment, data=dat, mean)
mfdiff <- agg1$income[1]-agg1$income[2]
HRdiff <- agg2$income[1]-agg2$income[2]
RGdiff <- agg2$income[2]-agg2$income[3]
HGdiff <- agg2$income[1]-agg2$income[3]
fpdiff <- agg3$income[1]-agg3$income[2]
pndiff <- agg3$income[2]-agg3$income[3]
nfdiff <- agg3$income[1]-agg3$income[3]
array(data=c(mfdiff,HRdiff,RGdiff,HGdiff,fpdiff,pndiff,nfdiff))
###Exercise9
#9.1 Load the dataset (data("CASchools")) to be named dat1.
data("CASchools", package = "AER")
dat1 = CASchools
#2. Using the function lm, run a regression of read on 
#the following variables: district, school,
#county, grades, students, teachers, calworks, lunch, computer, 
#expenditure, income and english. Store this regression as reg1.
#Method1
reg1 <- lm(read~district+school+county+grades+students+teachers
           +calworks+lunch+computer+expenditure+income+english
           ,data = dat1)
summary(reg1)
dat1$school= factor(dat1$school)
dat1$district= factor(dat1$district)
reg1 = lm(read ~ .-math, dat1)
summary(reg1)
#9.3 formula = y ~ x. lm(formula)
#Create reg2, that uses only the 200 first observations.
formula = y ~ x.lm(formula)
formula = y ~ x
lm(formula=formula, data=dat1[1:200,])
reg2 <- lm(formula=read~.-math, data=dat1[1:200,])
#Example
#try1_data <- matrix(data=rnorm(100),20,5)
#reg3 <- lm(data1$V1~.-V5,data=data1)
#summary(reg3)
###Exercise10
#10.1 Create a vector lu of 200 draws from a pareto distribution (1,1). 
#How many values are higher than 10.
# Replace these values by draws from a logistic distribution (6.5,0.5).
library(EnvStats)
lu <- rpareto(1,1,n=200)
length(which(lu>10))
replace <- c()
for(i in 1:200){
  if(lu[i]>10) replace[i]=rlogis(1,6.5,0.5)
  else replace[i]=lu[i]
}
#Create a vector de of 200 draws from a normal distribution (1,2). 
#Set de = log(de), and count the number of missing values or 
#negative values. Replace these values by draws from
#a normal distribution (0,1) truncated at 0. hint:truncnorm
de <- rnorm(n=200,mean=1,sd=2)
de=log(de)
count <- length(which(de<0))+length(which(de=="NaN"))
library(truncnorm)
newrnorm <- c()
for (i in 1:200){
  if(de[i]<0|de[i]=="NaN") 
    newrnorm[i]=rtruncnorm(1, a=0, b=Inf, mean = 0, sd = 1)
  else newrnorm[i]=de[i]
}
#10.3 Create two vectors, orig and dest as 200 draws 
#from a uniform distribution [0,1].
orig <- runif(200,0,1)
dest <- runif(200,0,1)
#10.4 Create two matrices, hist and dist as 200*200 draws 
#from a uniform distribution [0,1].
hist <- matrix(data=runif(40000,0,1),nrow=200,ncol = 200, byrow = T)
dist <- matrix(data=runif(40000,0,1),nrow=200,ncol = 200, byrow = T)
#10.6 Create the matrices su and se.
#Method1
su <- matrix(data=NA,nrow=200,ncol = 200)
numerator <- matrix(data=NA,nrow=200,ncol = 200)
denominator <- matrix(data=NA,nrow=200,ncol = 200)
for(j in 1:200){
  for(l in 1:200){
    numerator[j,l] <- log(orig[j]+dest[l]+dist[j,l])
    denominator[j,l] <- 1+log(orig[j]+dest[l]+dist[j,l])
    su[j,l] <- numerator[j,l]/denominator[j,l]
  }
}
#Method2
int = outer(orig, dest, "+")+dist
su1 = log(int)/(1+log(int))
int6ii <- outer(orig,dest,"+")+hist
se <- exp(int6ii)/1+exp(int6ii)
#outer example
v1 <- c(1,2,3)
v2 <- c(2,3,4)
outer(v1, v2, "+")
#10.7 Set r = 0.05. Create a function to evaluate qjl(.). 
#Evaluate qjl(9245) for all pairs (j,l).
r=0.05
obtainq <- function(w){
  firstpar <- outer(r+newrnorm,r+newrnorm,"/")
  firstfull <- w*firstpar
  second <- replace*log(w)
  third <- replace*(1+log(w))
  mid <- outer(second,third,"-")
  su_fourth <- firstpar*(rowSums(su)-diag(su))
  su_fifth <- outer(rep(1,200),rowSums(su)-diag(su),"*")
  sufull <- su_fourth-su_fifth
  se_fourth <- firstpar*(rowSums(se)-diag(se))
  se_fifth <- outer(rep(1,200),rowSums(se)-diag(se),"*")
  sefull <- se_fourth-se_fifth
  return(firstfull+mid+sufull+sefull)
}
v <- matrix(data=c(1:9),3,3)
rowSums(v)
obtainq(9245)
#10.8 Create gridw, 
#which consists of a sequence from 9100 to 55240 of length 50
gridw <- seq(9100,55240,length.out = 50)
#10.9 Using the function sapply, evaluate qjl.
#Store the ouput into an array of dimension (50 ¡Ñ200 ¡Ñ 200).
#How long does it take to evaluate qjl( ) for each value of w?
array(sapply(gridw, obtainq),dim=c(50,200,200))
system.time(sapply(gridw, obtainq))
###Example_list
li = list()
li[[1]] = mat1
li[[2]] = Titanic
li1 = list(x=mat1,y=Titanic)
li1$x
li2$y
###Example_dataframe
data=data.frame(x=rnorm(100),y=runif(100))
data
browse(data)
edit(data)
data[,1]
data[1,]
data$x
names(data)
attach(data)
x
detach(data)
y
###Example_test and conversion is.na()
is.list() as.list()
is.factor() as.factor()
is.matrix()
is.vector()
is.array()
is.finite()
a==b
a=>b
a<=b
###Exercise11
#11.1. Test if c(1, 2, 3) is an array? a vector? a matrix?
vec11 <- c(1,2,3)
is.array(vec11)
is.vector(vec11)
is.matrix(vec11)
#11.2 x0 = rnorm(1000); Using the function table() count 
#the number of occurrences of x0 > 0,
#x0 > 1, x0 > 2, x0 > 0.5, x0 < 1 and x0 > ???1
x0=rnorm(1000)
table(x0>0)[[2]]
table(x0 > 1)[[2]]
table(x0 > 2)[[2]]
table(x0 > 0.5)[[2]]
table(x0 < 1)[[2]]
table(x0 > -1)[[2]]
#11.3 g denotes number of quantile groups
library(Hmisc)
x1 = cut2(runif(100,0,1),g=10)
levels(x1)=paste("q",1:10,sep="")
#11.4 Test whether or not x1 is a factor?
is.factor(x1)
#11.5 Verify that ¡¨q1¡¨ has 10 occurences.
table(x1)
table(x1)[[1]]==10
#11.6 Convert x1 into a numeric variables. What happens to the levels?
as.numeric(x1)
class(as.numeric(x1))
#11.7
rand = rnorm(1000)
#11.8 Using the function which() find the indexes of positive values.
which(rand>0)
#11.9 Create the object w of positive values of rand using:
#(a) Which
#(b) Subset
#(c) By indexing directly the values that respect a condition
w1 <- rand[which(rand>0)]
w2 <- subset(rand,rand>0)
w3 <- rand[rand>0]
#Exercise12
#12.0 Write a program that asks the user to
#type an integer N and compute u(N) defined with :
#u(0)=1,u(1)=1,u(n+1)=u(n)+u(n-1)
u <- function(N){
  if (N==0|N==1) {return(1)}
  else{
    return(u(N-1)+u(N-2))
  }
}
#e.g.
score<-80
if(score>=60){
  print("¤Î®æ")
}else{
  print("¤£¤Î®æ")
}
#e.g.
# Create a function to print squares of numbers in sequence.
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}	
#12.1 Evaluate 1^2 + 2^2 + 3^2 + . . . 400^2.
vec12 <- c()
fun12 <- function(last){
  for(i in 1:last){
    vec12[i]<- i^2
    print(sum(vec12))
  }
}
#Method2
sum(c(1:400)^2)
#12.2 Evaluate 1 ¡Ñ 2 + 2 ¡Ñ 3 + 3 ¡Ñ 4 + ... + 249 ¡Ñ 250
vec12_1 <- c(1:249)
vec12_2 <- vec12_1+1
sum(vec12_1*vec12_2)
#Method2
sum(c(1:249) * c(2:250))
#12.3 Create a function ¡¨crra¡¨ with two arguments (c, £c) that 
# returns c^(1???£c)/1???£c .
#Add an if condition
#such that the utility is given by the log when £c ??? [0.97, 1, 03] ??? 1
crra <- function(c,theta){
  answer <- c^(1-theta)/(1-theta)
  return(answer)
}
crra <- function(c,theta){
  if (theta>=0.97&theta<=1.03) answer=log(c^(1-theta)/(1-theta))
  else answer=c^(1-theta)/(1-theta)
  return(answer)
}
crra = function(c,theta) {
  op = c^(1-theta)/(1-theta)
  if (0.97 <= theta & theta <=1.03) {return(log(op))}
  return(op)
}
#12.4 Create a function ¡¨fact¡¨ that returns the factorial of a number
fact <- function(number){
    if (n==0|n==1) {return(1)}
  else(return(prod(1:number)))
}
fact(4)
###Exercise13 Apply Functions
#Using this object,
#m = matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow = 20, ncol = 2)
#Calculate the mean, median, min, max and standard deviation 
#by row and column
m = matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow = 20, ncol = 2)
apply(m, MARGIN = 1, mean)
apply(m, MARGIN = 2, mean)
apply(m, MARGIN = 1, median)
apply(m, MARGIN = 2, median)
apply(m, MARGIN = 1, min)
apply(m, MARGIN = 2, min)
apply(m, MARGIN = 1, max)
apply(m, MARGIN = 2, max)
apply(m, MARGIN = 1, sd)
apply(m, MARGIN = 2, sd)
###E.g.apply M=1=> by row ; M=2=> by column
df <- data.frame(x = 1:4, y = 5:8, z = 10:13)
apply(df, MARGIN = 1, sum)
apply(df, MARGIN = 2, sum)
#13.2
#Using the dataset iris in the package ¡¨datasets¡¨, 
#calculate the average Sepal.Length by Species. 
#Evaluate the sum log of Sepal.Width by Species.
aggregate(iris$Sepal.Length, by=list(iris$Species), FUN=mean) 
aggregate(log(iris$Sepal.Width), by=list(iris$Species), FUN=sum)
#13.3
#y1 = NULL; for (i in 1:100) y1[i]=exp(i)
#y2 = exp(1:100)
#y3 = sapply(1:100,exp)
#(a) Check the outcome of these three operations.
#(b) Using proc.time() or system.time(), 
#compare the execution time of these three equivalent commands
ptm <- proc.time()
y1 = NULL; for (i in 1:100) y1[i]=exp(i)
proc.time() - ptm
ptm <- proc.time()
y2 = exp(1:100)
proc.time() - ptm
ptm <- proc.time()
y3 = sapply(1:100,exp)
proc.time() - ptm
system.time(for (i in 1:100) {y1[i]=exp(i)})
system.time(exp(1:100))
system.time(sapply(1:100,exp))
#Exercise 14 Simulating and Computing
#14.1 Simulate a vector x of 10,000 draws from a normal distribution. 
#Use the function summary to provide basic characteristics of x.
x <- rnorm(10000,0,1)
summary(x)
#14.2 Create a function dsummary that returns, the minimum, 
#the 1st decile, the 1st quartile,the median, the mean,
#the standard deviation, the 3rd quartile, the 9th decile, maximum.
dsummary <- function(input){
  min <- min(input)
  firstdecile1 <- quantile(input, probs = seq(.1, .9, by = .1))[[1]]
  quartile <- quantile(input)[2:4]
  mean <- mean(input)
  sd <- sd(input)
  ninthdecile1 <- quantile(input, probs = seq(.1, .9, by = .1))[[9]]
  max <- max(input)
  print(min)
  print(firstdecile1)
  print(quartile)
  print(mean)
  print(sd)
  print(ninthdecile1)
  print(max)
}
###14.3
#Suppose X ??? N (2, 0.25). Evaluate f (0.5), F (2.5), F inverse(0.95)
#p:cdf q:cdf's inverse
dnorm(0.5,mean=2,sd=0.25)
pnorm(2.5, mean=2,sd=0.25)
qnorm(0.95,mean=2,sd=0.25)
#14.4
#Repeat if X has t-distribution with 5 degrees of freedom.
dt(0.5,df=5)
pt(2.5,df=5)
qt(0.95,df=5)
#14.5 
#Suppose X ??? P (3, 1), where P is the pareto distribution. 
#Evaluate f (0.5), F (2.5), F inverse 0.95
dpareto(0.5,3,1)
ppareto(2.5,3,1)
qpareto(0.95,3,1)
###Exercise 15 Moments
V = rnorm(100, -2, 5)
###15.1 Evaluate n as the length of V.
length(V)
###15.2 Compute the mean m
m=mean(v)
#15.3 Compute the variance s2 
sd(v)
#15.4 Compute the skewness £^1
sk <- skewness(v)
#15.5 Compute the kurtosis k1 
k1 <- kurtosis(v)
###Exercise 16 OLS
#16.1 Create a matrix X of dimension (1000,10). 
#Fill it with draws from a beta distribution
#with shape1 parameter 2, and shape 2 parameter 1. 
#Make sure that there is no negative.
beta <- rbeta(10000,2,1)
X <- matrix(data=beta,1000,10)
length(X[X<0])==0
#16.2 Create a scalar denoted by £m2 and set it to 0.5. 
#Generate a vector £] of size 10. Fill it with
#draws from a Gamma distribution with parameters 2 and 1.
sigmasq <- 0.5
sigmasq_10 <- 0.01
beta <- rgamma(10,2,1)
#16.3 Create a vector epsilon of 1000 draws from a normal distribution
epsilon <- rnorm(1000,0,1)
#16.4 Create Y = X£] + sqrt(£m2) ???epsilon dimension:1000*1
Y=X%*%beta+sqrt(sigmasq)*epsilon
Y_10=X%*%beta+sqrt(sigmasq_10)*epsilon
#16.5 Recover £]hat = (X¡¬X)inverse(X¡¬Y )
betahat <- solve(t(X)%*%X)%*%(t(X)%*%Y)
betahat_10 <- solve(t(X)%*%X)%*%(t(X)%*%Y_10)
#16.6 Evaluate epsilonhat = yhat ??? y. 
#Plot the histogram (filled in grey) and the kernel density of the
#distribution of the error term.
yhat <- X%*%betahat
yhat_10 <- X%*%betahat_10 
epsilonhat <- yhat-Y
epsilonhat_10 <- yhat_10-Y_10
hist(epsilonhat,col="grey")
# Kernel Density Plot
depsilon <- density(epsilon) # returns the density data
plot(depsilon) # plots the results 
#16.7 Estimate £m2 = epsilonhat'*epsilonhat/n ??? p ??? 1
#and V(betahat) = £m2(X¡¬X)inverse
sigmasqest <- t(epsilonhat)%*%epsilonhat/(1000-10-1)
var_betahat <- as.numeric(sigmasqest)*(solve(t(X)%*%X))
#16.8Create param that binds (£](1*10),¡ÔV (£]hat (10*10))).
#Using the command lm, check these estimates.
param <- cbind(beta,var_betahat)
summary(lm(Y~0+X))
#16.9 Construct a confidence interval for £] 
confint(lm(Y~0+X))
#16.10 Redo the exercise by setting £m2 = 0.01. 
#How are your confidence intervals for £] smaller
confint(lm(Y_10~0+X))
#A0end!Week1 forces my brain to exercise a lot.