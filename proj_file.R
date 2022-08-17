rm(list = ls()) #removes all the existing objects from the environment
data<- read.csv("F:\\R\\Project\\46.csv") #reads the .csv file into the data object
library(Hmisc) #used for the describe function
describe(data) #describes the distribution of the data in the csv file

View(data) #displays the entire data
mean_weight = mean(data$Weight) #finds mean of the column 'Weight' in the csv file
mean_weight
mean_height = mean(data$Height) #finds mean of the column 'Height' in the csv file
mean_height
mean_age = mean(data$Age) #finds mean of the column 'Age' in the csv file
mean_age
mode<- function(v){#user defined function to compute the mode of a column in the csv file
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_medal = mode(data$Medal) #calculation of the modal value of medal column in the csv file
mode_medal

plot(x = data$Height, y = data$Weight, col = ifelse(data$Sex == "M", "blue", "red"), pch = c(19, 17), main = "Relation between height and weight", xlab = "Height of participants(in cm)", ylab = "Weight of participants(in kg)", xlim = c(152,194), ylim = c(43,112))
# draws a scatter plot of Height variables vs the wight variables in the csv file using custom shapes and colour for each column
#col = ifelse(data$Sex =="M", "blue","red") is used to set colour blue to the points
#with sex as M and red to points with Sex as F
# pch is used to give the shape required for the points
legend("topleft", pch = c(19, 17),  c("Female", "Male"),  col = c("red", "blue")) #displays the legend for the 
# plotted scatter plot
grid() #displays the grid in the graph
data_male = subset(data, Sex == "M") #creates a subset of the entire data set into one in which Sex is male 
data_female = subset(data, Sex == "F") #creates a subset of the entire data set into one in which Sex is female
abline(lm(data_female$Weight ~ data_female$Height), col = "red") # draws the best fit line for relation 
# between weight and height for Sex = "F" 
abline(lm(data_male$Weight ~ data_male$Height), col = "blue") #draws the best fit line for male
master_data = rbind(data_male, data_female) #binds the two datasets in order to get a seperated list based on gender
View(master_data) #Views the new data set sorted by gender

data_heights = master_data$Height #column of heights of participants
data_heights = as.vector(data_heights) #converts above list into vector
data_weights = master_data$Weight #column of weights of participants
data_weights = as.vector(data_weights) #converts above list into vector
bmi_vec <- (data_weights/(data_heights * data_heights)) * 10000 #calculates the BMI of each row in the data set
master_data$BMI <- bmi_vec #adds the BMI column in the data set
View(master_data) #Views the new data set along with the BMI column

library(plyr) #used for barplot function
length_data_male = count(data_male, "Sex") #counts the number of male entries in the data set
length_data_female = count(data_female, "Sex") #counts the number of female entries in the data set
barplot(c(length_data_male$freq, length_data_female$freq), xlab = "Gender", ylab = "Number of medals", main = "Gender distribution of medals", names.arg = c("Male","Female"), border = "black", col = "blue", ylim = c(0,700))
# constructs a barplot of number of medals won based on Sex of participant
grid() #displays the grid in the graph

hist(master_data$Age, col = 'steelblue', main = 'Age distribution', xlab = 'Age') #draws a histogram of the 
# distribution of age versus frequency
grid() #draws the grid in the graph
