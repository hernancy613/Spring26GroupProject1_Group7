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
        col = "lightblue", 
        names.arg = c("Excellent", "Very Good", "Good", "Fair","Poor"),
        xlab = "Quality of Care Rating",
        ylab = "Count")
#Interpretation:
#Most respondents reported receiving good quality care.
#Only a small proportion reported poor quality care.
#The distribution suggests overall positive perception of care quality.
barplot(table(Group7Variables$RaceEthn5),
        main = "Distribution of Race/Ethnicity",
        col = "lightgreen", 
        names.arg = c("Non-Hisp White", "Non-Hisp Black/AA", "Hispanic", "Non-Hisp Asian","Non-Hisp Other"),
        xlab = "Race/Ethnicty",
        ylab = "Count",
        cex.names = 0.75,
        cex.lab = 1.2)
#Interpretation:
#The sample consists mostly of Non-Hispanic White, with smaller proportions of 
#Non-Hispanic Asian and Non-Hispanic Other participants.
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
ggplot(Group7Variables, aes(x = factor(QualityCare,
                                       levels = c(1,2,3,4,5),
                                       labels = c("Excellent",
                                                  "Very Good",
                                                  "Good",
                                                  "Fair",
                                                  "Poor"))))+
  geom_bar(fill = "lightblue") +
  xlab("Quality Care Rating") +
  ggtitle("Distribution of QualityCare")
#Interpretation:
#Most respondents reported receiving good quality care.
#Only a small proportion reported poor quality care.
#The distribution suggests overall positive perception of care quality.
ggplot(Group7Variables, aes(x = factor(RaceEthn5,
                                       levels = c(1,2,3,4,5),
                                       labels = c("Non-Hisp White",
                                                  "Non-Hisp Black/AA",
                                                  "Hispanic",
                                                  "Non-Hisp Asian",
                                                  "Non-Hisp Other")))) +
  geom_bar(fill = "lightgreen") +
  xlab("Race/Ethnicity Category") +
  ggtitle("Distribution of RaceEthn5")
#Interpretation:
#The sample consists mostly of Non-Hispanic White, with smaller proportions of 
#Non-Hispanic Asian and Non-Hispanic Other participants.
ggplot(Group7Variables, aes(x = Age)) +
  geom_histogram(fill = "pink", bins = 30) +
  ggtitle("Distribution of Age")+
  xlab("Age")
#Interpretation:
#Age appears normally distributed in 60 years old and skewed right.
ggplot(Group7Variables, aes(x = BMI)) +
  geom_histogram(fill = "orange", bins = 30) +
  ggtitle("Distribution of BMI") +
  xlab("BMI")
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
#Most respondents reported receiving good quality care.
#Only a small proportion reported poor quality care.
#The distribution suggests overall positive perception of care quality.
my_visual_func_I(Group7Variables, "RaceEthn5")
#Interpretation:
#The sample is mostly in group 1(Non-Hispanic White), with smaller proportions of in 
#group 4(Non-Hispanic Asian)and 5(Non-Hispanic Other).
my_visual_func_II <- function(data, varname){
  hist(data[[varname]],
       main = paste("Distribution of", varname),
       col = "orange",
       xlab = varname)
}
my_visual_func_II(Group7Variables, "Age")
#Age appears normally distributed in 60 years old and skewed right.
my_visual_func_II(Group7Variables, "BMI")
#BMI appears slightly right-skewed, with most values between 20-30.
#Updated by Bi Na
