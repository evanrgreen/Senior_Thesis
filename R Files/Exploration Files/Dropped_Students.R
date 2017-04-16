setwd("/Users/evangreen/Desktop/Senior\ Thesis/Grade\ Data")

grades <- read.csv("CodeGrades.csv", as.is = T)
test_grades <- read.csv("CodeTests.csv", as.is = T)
genders <- read.csv("CodeGender.csv", as.is = T,strip.white=TRUE)

#student 86 does not appear in the grades file
#or anywhere else, I will assume student 86 dropped the class 
setdiff(genders$code, grades$student)

#sum up the last three HWs to see if students dropped or not

grades <- merge(grades,test_grades)

grades$did_drop_after4 <- rowSums(grades[,6:8]) == 0
grades$did_drop_immediately <- rowSums(grades[,2:8]) == 0
sum(grades$did_drop_immediately)
sum(grades$did_drop_after4)

#couple observations 
# There are a couple 0's in the tests that are probably due to cheating but
#probably have to deal with them separately since that will mess up the data 

table(genders$Sex)
table(genders[genders$code %in% grades$student[!grades$did_drop_immediately],"Sex"])
table(genders[genders$code %in% grades$student[!grades$did_drop_after4],"Sex"])

write.csv(grades,row.names = F, file="FullGrades.csv")

grades<-read.csv(file="FullGrades.csv",as.is=T)
grades[grades$test1==0 & grades$test2>0,]
grades[grades$test1>0 & grades$test2==0,]
View(grades[grades$did_drop_after4 & !grades$did_drop_immediately,])
