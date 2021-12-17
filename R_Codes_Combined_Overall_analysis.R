#Clear the console: Ctrl+Alt+L

#Finding the directory path
getwd()

#Change home directory to the directory Data/DataAnalysis
setwd("~/Data/Combined")

#Change back to the home directory
setwd("~")

#Install a package

install.packages("foreign")
install.packages("pastecs")

#load package
library(pastecs)

#Upload the file: Combined_Overall.csv
data<-read.csv(file="Combined_Overall.csv",head=TRUE,sep=",");

#Create an output file: Combined_Overall_Result.txt
#Write the result in the txt output file:
sink("Combined_Overall_Result.txt",append=TRUE,split=TRUE)
#To write the file back to the console: sink()


#To Force R to give the output in decimal format instead of exponential
options("scipen"=100, "digits"=2)

#Descriptive Statistics
stat.desc(data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#To paste the data in Excel or CSV file, copy and paste. Then click on data and click on text To Columns. 

names(data)

StudentsGrade<-data$StudentsGrade;
InstructorsGrade<-data$InstructorsGrade;
Difference<-data$Difference;

print("Test for Over Estimate")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")

print("Test for Under Estimate")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")


print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference for Combined Data", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

printf("Correlation Coefficient")
cor(InstructorsGrade, Difference)


print("Linear Regression Analysis")
set.seed(122)
modcomb=lm(formula=Difference~InstructorsGrade)
summary(modcomb)

#Scatter Plot
# Create the data for the chart.
png(file = "Combined_Overall.jpg")

# Plot the bar chart.
plot(StudentsGrade,type = "o",col = "red", xlab = "InstructorsGrade", ylab = "StudentsGrade",main = "InstructorsGrade Versus StudentsGrade")

lines(InstructorsGrade, type = "o", col = "blue")

# Save the file.
dev.off()

---Grade A-------------------------------
print("filter the Grade A data")
adata<-subset(data,InstructorsGrade>89)
InstructorsGrade<-adata$InstructorsGrade
StudentsGrade<-adata$StudentsGrade
Difference<-adata$Difference
print("Descriptive Statistics")
stat.desc(adata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing")
T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot")
#Scatter Plot and Regression Line
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade A", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Linear Regression
set.seed(122)
moda=lm(formula=Difference~InstructorsGrade)
summary(moda)

#Save Scatter Plot
# Create the data for the chart.
png(file = "Combined_gradeA.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade A", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#------Grade B Data---------------------
#filter the Grade A and B data
bdata<-subset(data,InstructorsGrade>79 & InstructorsGrade<90)
InstructorsGrade<-bdata$InstructorsGrade
StudentsGrade<-bdata$StudentsGrade
Difference<-bdata$Difference
#Descriptive Statistics
stat.desc(bdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade B", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modb=lm(formula=Difference~InstructorsGrade)
summary(modb)
#Save Scatter Plot
# Create the data for the chart.
png(file = "Combined_gradeB.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade B", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#----------Grade A and B

#filter the Grade A and B data
abdata<-subset(data,InstructorsGrade>79)
InstructorsGrade<-abdata$InstructorsGrade
StudentsGrade<-abdata$StudentsGrade
Difference<-abdata$Difference
#Descriptive Statistics
stat.desc(abdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade A and B", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modb=lm(formula=Difference~InstructorsGrade)
summary(modb)

# Create the data for the chart.
png(file = "Combined_gradeAnB.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grades A and B", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#-----Grade C

#filter the Grade C data
cdata<-subset(data,InstructorsGrade<80&InstructorsGrade>69)
InstructorsGrade<-cdata$InstructorsGrade
StudentsGrade<-cdata$StudentsGrade
Difference<-cdata$Difference
#Descriptive Statistics
stat.desc(cdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade C", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modc=lm(formula=Difference~InstructorsGrade)
summary(modc)

# Create the data for the chart.
png(file = "Combined_gradeC.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade C", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#--Grades B and C-----------
print("Grades B and C")

print("filter the Grade B and C")
bncdata<-subset(data,InstructorsGrade<89&InstructorsGrade>69)
InstructorsGrade<-bncdata$InstructorsGrade
StudentsGrade<-bncdata$StudentsGrade
Difference<-bncdata$Difference
print("Descriptive Statistics")
stat.desc(bncdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grades B and C", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modbnc=lm(formula=Difference~InstructorsGrade)
summary(modbnc)

# Create the data for the chart.
png(file = "Combined_gradeBnC.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade B and C", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()
#---------------------------------

#-------Between 75 and 85-----------

bcdata<-subset(data,InstructorsGrade<86&InstructorsGrade>74)
InstructorsGrade<-bcdata$InstructorsGrade
StudentsGrade<-bcdata$StudentsGrade
Difference<-bcdata$Difference
Descriptive Statistics
stat.desc(bcdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grades Between 75 and 85", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modbc=lm(formula=Difference~InstructorsGrade)
summary(modbc)

# Create the data for the chart.
png(file = "Combined_grade75n85.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference for Grades Between 75 and 85", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()
#---------------------------------

#-------Grades below C------------------
#filter the Grade Below C data
fdata<-subset(data,InstructorsGrade<70)
InstructorsGrade<-fdata$InstructorsGrade
StudentsGrade<-fdata$StudentsGrade
Difference<-fdata$Difference
Descriptive Statistics
stat.desc(fdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade Below C", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modf=lm(formula=Difference~InstructorsGrade)
summary(modf)

# Create the data for the chart.
png(file = "Combined_gradeF.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade Below C", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#---Grade D-----
#filter the Grade D
ddata<-subset(data,InstructorsGrade>=60&InstructorsGrade<70)
InstructorsGrade<-ddata$InstructorsGrade
StudentsGrade<-ddata$StudentsGrade
Difference<-ddata$Difference
#Descriptive Statistics
stat.desc(ddata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade D", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modd=lm(formula=Difference~InstructorsGrade)
summary(modd)

# Create the JPg File for the plot.
png(file = "Combined_gradeD.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade D", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#---Grade Below D-----
#filter the Grade Below D data
fdata<-subset(data,InstructorsGrade<60)
InstructorsGrade<-fdata$InstructorsGrade
StudentsGrade<-fdata$StudentsGrade
Difference<-fdata$Difference
#Descriptive Statistics
stat.desc(fdata[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

#Hypothesis Testing
#T Test
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

#Scatter Plot and Regression Line

plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade Below D", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

#Regression Analysis
set.seed(122)
modf=lm(formula=Difference~InstructorsGrade)
summary(modf)

# Create the JPg File for the plot.
png(file = "Combined_gradeBelowD.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Grade Below D", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#-----By Course---Math 1101, Math 1111, Math 1120, Math 1113, Math 191, Math192, Math 2204, Math 2470, Math 320, Math3650------------

print("Grades for Math 320")
print("filter the Grade for Math320")
m320ata<-subset(data,CourseNumber==320)
InstructorsGrade<-m320data$InstructorsGrade
StudentsGrade<-m320data$StudentsGrade
Difference<-m320data$Difference
print("Descriptive Statistics")
stat.desc(m320data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Math 320", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm320=lm(formula=Difference~InstructorsGrade)
summary(modm320)
------------------
print("Grades for Math 2470")
print("filter the Grade for Math2470")
m2470data<-subset(data,CourseNumber==2470)
InstructorsGrade<-m2470data$InstructorsGrade
StudentsGrade<-m2470data$StudentsGrade
Difference<-m2470data$Difference
print("Descriptive Statistics")
stat.desc(m2470data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Math 2470", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm2470=lm(formula=Difference~InstructorsGrade)
summary(modm2470)
------------------
print("Grades for Math 2204")
print("filter the Grade for Math2204")
m2204data<-subset(data,CourseNumber==2204)
InstructorsGrade<-m2204data$InstructorsGrade
StudentsGrade<-m2204data$StudentsGrade
Difference<-m2204data$Difference
print("Descriptive Statistics")
stat.desc(m2204data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Math 2204", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm2204=lm(formula=Difference~InstructorsGrade)
summary(modm2204)
------------------
print("Grades for Math 191")
print("filter the Grade for Math191")
m1120data<-subset(data,CourseNumber==191)
InstructorsGrade<-m191data$InstructorsGrade
StudentsGrade<-m191data$StudentsGrade
Difference<-m191data$Difference
print("Descriptive Statistics")
stat.desc(m191data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Math 191", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm191=lm(formula=Difference~InstructorsGrade)
summary(modm191)
------------------
print("Grades for Math 1120")
print("filter the Grade for Math1120")
m1120data<-subset(data,CourseNumber==1120)
InstructorsGrade<-m1120data$InstructorsGrade
StudentsGrade<-m1120data$StudentsGrade
Difference<-m1120data$Difference
print("Descriptive Statistics")
stat.desc(m1120data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Math 1120", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm1120=lm(formula=Difference~InstructorsGrade)
summary(modm1120)
------------------
print("Grades for Math 1101")
print("filter the Grade for Math1101")
m1101data<-subset(data,CourseNumber==1101)
InstructorsGrade<-m1101data$InstructorsGrade
StudentsGrade<-m1101data$StudentsGrade
Difference<-m1101data$Difference
print("Descriptive Statistics")
stat.desc(m1101data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Math 1101", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm1101=lm(formula=Difference~InstructorsGrade)
summary(modm1101)
------------------
print("Grades for Math 192")
print("filter the Grade for Math192")
m192data<-subset(data,CourseNumber==192)
InstructorsGrade<-m192data$InstructorsGrade
StudentsGrade<-m192data$StudentsGrade
Difference<-m192data$Difference
print("Descriptive Statistics")
stat.desc(m192data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Calculus II", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm192=lm(formula=Difference~InstructorsGrade)
summary(modm192)
------------------
print("Grades for Math 1113")
print("filter the Grade for Math1113")
m1113data<-subset(data,CourseNumber==1113)
InstructorsGrade<-m1113data$InstructorsGrade
StudentsGrade<-m1113data$StudentsGrade
Difference<-m1113data$Difference
print("Descriptive Statistics")
stat.desc(m1113data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Precalculus", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm1113=lm(formula=Difference~InstructorsGrade)
summary(modm1113)
----------------
print("Grades for Math 1111")
print("filter the Grade for Math1111")
m1111data<-subset(data,CourseNumber==1111)
InstructorsGrade<-m1111data$InstructorsGrade
StudentsGrade<-m1111data$StudentsGrade
Difference<-m1111data$Difference
print("Descriptive Statistics")
stat.desc(m1111data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For College Algebra", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm1111=lm(formula=Difference~InstructorsGrade)
summary(modm1111)
------------------
print("Grades for Math 3650")
print("filter the Grade for Math3650")
m3650data<-subset(data,CourseNumber==3650)
InstructorsGrade<-m3650data$InstructorsGrade
StudentsGrade<-m3650data$StudentsGrade
Difference<-m3650data$Difference
print("Descriptive Statistics")
stat.desc(m3650data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Linear Algebra", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm3650=lm(formula=Difference~InstructorsGrade)
summary(modm3650)

#---------By Gender---Male=1, Female=0
print("Grades for Females")
print("filter the Grade for Female")
m0data<-subset(data,Gender==0)
InstructorsGrade<-m0data$InstructorsGrade
StudentsGrade<-m0data$StudentsGrade
Difference<-m0data$Difference
print("Descriptive Statistics")
stat.desc(m0data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Female Students", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm0=lm(formula=Difference~InstructorsGrade)
summary(modm0)

# Create the data for the chart.
png(file = "Combined_Female.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Females", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()
-------------------
print("Grades for Males")
print("filter the Grade for Male Students")
m1data<-subset(data,Gender==1)
InstructorsGrade<-m1data$InstructorsGrade
StudentsGrade<-m1data$StudentsGrade
Difference<-m1data$Difference
print("Descriptive Statistics")
stat.desc(m1data[,c("StudentsGrade","InstructorsGrade","Difference")], basic=TRUE, desc=TRUE, norm=FALSE)

print("Hypothesis Testing: T test")

t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="greater")
t.test(StudentsGrade,InstructorsGrade,paired=TRUE, alt="less")

print("Scatter Plot and Regression Line")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Male Students", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");

print("Regression Analysis")
set.seed(122)
modm1=lm(formula=Difference~InstructorsGrade)
summary(modm1)

# Create the data for the chart.
png(file = "Combined_Male.jpg")
plot(InstructorsGrade, Difference, col="blue",xlab = "InstructorsGrade", ylab = "Difference",main = "InstructorsGrade Versus Difference For Males", pch=19);
abline(lm(Difference~InstructorsGrade), col="red");
dev.off()

#Logistic Analysis: When the preditive/dependent variable is a binary variable (0,1), we can use it. We can make the behavior variable as over estimate and #under estimate with values 1 and 0 and analyze the dependence of it with other continuous or catarorical variables.
#Independent variables: actual grades, initial preparation level, gender, semester, and so on. In this case, the actual grade is a continuous variable, #others are categorical variables so that we have to use factor(variable) to change the variable into the categorical variable. 
 
behavior<-mydata$estimate;
initialprep<-mydata$InitialPreparation;
gender<-mydata$Gender;
semester<-mydata$Semester;
grade<-mydata$InstructorsGrade;
course<-mydata$CourseLevel;
mylogit <- glm(behavior ~ initialprep+gender+semester+grade+course, family = "binomial");
summary(mylogit)