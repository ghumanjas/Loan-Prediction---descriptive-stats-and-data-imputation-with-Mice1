library(ggplot2)
getwd()
dir("C:/Users/ghumanja/Desktop/New folder/Analytics vidhya")
train <- read.csv('train_home loan.csv',na.strings=c("","NA"))
head(train)
dim(train)
typeof(train)
str(train)
table(is.na(train))
colSums(is.na(train))
summary(train)
ggplot(data=train, aes(ApplicantIncome,LoanAmount))+geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Applicant income")
ggplot(data=train, aes(Gender,ApplicantIncome))+geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + ylab("Applicant income")+xlab("Gender")
ggplot(data=train, aes(Education,ApplicantIncome))+geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + ylab("Applicant income")+xlab("Education")
summary(train$Loan_Status)
table(train$Credit_History)
ggplot(train, aes(Credit_History))+geom_bar(width = 0.5) +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red"))
head(train)
pairs(train[,7:13])
ggplot(train, aes(Credit_History,Loan_Status))+geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red"))
ggplot(train, aes(Credit_History,Loan_Status,fill=Gender))+geom_bar(position = "dodge", stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.2, color = "red"))
table(is.na(train))
colSums(is.na(train))
ggplot(data=train, aes(interaction(Education,Self_Employed),LoanAmount))+geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red"))
table(train$Self_Employed)
#Data imputation by mean, median etc#
#train$Self_Employed[which(is.na(train$Self_Employed))] <- "No"#
library(mice)
md.pattern(train)
library(VIM)
#Plot missing values and check#
aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
labels(train)

#https://rpubs.com/corey_sparks/63681 . follow this link. Good#
train1 <- mice(data = train[,c("Gender", "Married", "Dependents","Education","Self_Employed","ApplicantIncome","CoapplicantIncome","LoanAmount","Loan_Amount_Term","Credit_History","Property_Area","Loan_Status")], m=5,maxit=50,seed = 500)
summary(train1)
#This is to see the variability in the 5 different imputations for each outcome#
names(train)
train2 <- with(data=train1 ,exp=lm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area))
train2
#Pooling or combining results from all the 5 iterations#
train3 <- pool(train2)
print(train3)
