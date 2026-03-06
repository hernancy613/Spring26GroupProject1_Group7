# Analysis written by Bi Na
library(readr)
HINTSData_2020_clean <- read_csv("HINTSData_2020_clean.csv")
Group7Variables <- HINTSData_2020_clean[,c("Age", #extracted the variables assigned to our group
                                           "AvgDrinksPerWeek",
                                           "WeeklyMinutesModerateExercise",
                                           "BMI",
                                           "QualityCare",
                                           "BirthGender",
                                           "smokeStat",
                                           "RaceEthn5")]
head(Group7Variables) #printed the first couple of columns to make sure the correct variables were extracted
colSums(is.na(Group7Variables)) #confirmed none of our columns have missing information 

Group7Variables <- HINTSData_2020_clean[,c(
  "Age",
  "AvgDrinksPerWeek",
  "WeeklyMinutesModerateExercise",
  "BMI",
  "QualityCare",
  "BirthGender",
  "smokeStat",
  "RaceEthn5"
)]
head(Group7Variables)
#4.I
barplot(table(Group7Variables$QualityCare),
        main = "Distribution of QualityCare",
        col = "lightblue")
#Interpretation:
#Most respondents reported receiving good quality care.
#Only a small proportion reported poor quality care.
#The distribution suggests overall positive perception of care quality.
barplot(table(Group7Variables$RaceEthn5),
        main = "Distribution of Race/Ethnicity",
        col = "lightgreen")
#Interpretation:
#The sample is mostly in group 1(Hispanic), with smaller proportions of in 
#group 4(Non-Hispanic American Indian or Alaska Native)and 5(Non-Hispanic Asian).
#4.II
hist(Group7Variables$Age,
     main = "Distribution of Age",
     xlab="Age",
     col = "pink")
#Interpretation:
#Age appears normally distributed in 60 years old and skewed right.
hist(Group7Variables$BMI,
     main = "Distribution of BMI",
     xlab="BMI",
     col = "orange")
#Interpretation:
#BMI appears slightly right-skewed, with most values between 20-30.

#4.III
install.packages("ggplot2")
library(ggplot2)
ggplot(Group7Variables, aes(x = QualityCare)) +
  geom_bar(fill = "lightblue") +
  xlab("Quality Care Rating") +
  ggtitle("Distribution of QualityCare")
#Interpretation:
#Most respondents reported receiving good quality care.
#Only a small proportion reported poor quality care.
#The distribution suggests overall positive perception of care quality.
ggplot(Group7Variables, aes(x = RaceEthn5)) +
  geom_bar(fill = "lightgreen") +
  xlab("Race/Ethnicity Category") +
  ggtitle("Distribution of RaceEthn5")
#Interpretation:
#The sample is mostly in group 1(Hispanic), with smaller proportions of in 
#group 4(Non-Hispanic American Indian or Alaska Native)and 5(Non-Hispanic Asian).
ggplot(Group7Variables, aes(x = Age)) +
  geom_histogram(fill = "pink", bins = 30) +
  ggtitle("Distribution of Age")
#Interpretation:
#Age appears normally distributed in 60 years old and skewed right.
ggplot(Group7Variables, aes(x = BMI)) +
  geom_histogram(fill = "orange", bins = 30) +
  ggtitle("Distribution of BMI")
#Interpretation:
#BMI appears slightly right-skewed, with most values between 20-30.

#4.IV
my_visual_func_I <- function(data, varname){
  barplot(table(data[[varname]]),
          main = paste("Distribution of", varname),
          col = "lightblue",
          xlab = varname)
}
my_visual_func_I(Group7Variables, "QualityCare")
my_visual_func_I(Group7Variables, "RaceEthn5")
#Interpretation:
#The sample is mostly in group 1(Hispanic), with smaller proportions of in 
#group 4(Non-Hispanic American Indian or Alaska Native)and 5(Non-Hispanic Asian).
my_visual_func_II <- function(data, varname){
  hist(data[[varname]],
       main = paste("Distribution of", varname),
       col = "orange",
       xlab = varname)
}
my_visual_func_II(Group7Variables, "Age")
my_visual_func_II(Group7Variables, "BMI")
#Updated by Bi Na
