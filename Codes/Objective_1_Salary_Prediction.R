# import the dataset
dataset = read.csv2('Datasets/Ob_1_Salary_Data.csv')


#Split the data into Training and Test
library(caTools)
set.seed(123) #Randomly assign Training or Test.
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE) #True are for training
testing_set = subset(dataset, split == FALSE) #False are for testing

#complete set



#Train the simple regression model with the training data using the lm function
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

#obtain information about the model
summary(regressor)

#Predict results with the test data.
y_pred = predict(regressor, newdata =  testing_set)



#Training data Visualization
install.packages("ggplot2") #install the library ggplot2
library(ggplot2)
ggplot()+
  #built the diagram with points
  geom_point(aes(x = training_set$YearsExperience, 
                 y = training_set$Salary),
             colour = "red") +
  #represent the line of the regression line model
  geom_line(aes(x= training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  #Write a title for the diagram
  ggtitle("Salary VS Years Experience (Training set)")+
  xlab("Years Experience") +
  ylab("Salary ($)")



#Testing data visualization
ggplot()+
  #built the diagram with points
  geom_point(aes(x = testing_set$YearsExperience, 
                 y = testing_set$Salary),
             colour = "red") +
  #represent the line of the regression line model
  geom_line(aes(x= testing_set$YearsExperience, 
                y = predict(regressor, newdata = testing_set)),
            colour = "blue") +
  #Write a title for the diagram
  ggtitle("Salary VS Years Experience (Testing set)")+
  xlab("Years Experience") +
  ylab("Salary ($)")


#Train the simple regression model with the complete dataset
regressor = lm(formula = Salary ~ YearsExperience,
               data = dataset)

#obtain information about the model
summary(regressor)



#Obtain the desired salary that the employees must be earning a cording to the lineal model
d_salary = predict(regressor, newdata = dataset)

#add the d_salary to the dataset
dataset$D_Salary = c(d_salary)

#obtain the difference between Salary and D_Salary
dataset$Difference = dataset$D_Salary - dataset$Salary

#round the values
dataset$D_Salary = round(dataset$D_Salary, 2)
dataset$Difference = round(dataset$Difference, 2)

#save the data frame into a csv file
write.csv2(dataset, "C:/Users/willi/OneDrive/Escritorio/Proyectos/Mahine_Learning_Project/Datasets/Ob_1_Salary_Data_2.csv", row.names = F)



#banda de confianza 
#agregar stat_smooth (method = "lm",
#                      formula = y ~ x,
#                     geom = "smooth")
