# importar el dataset
dataset = read.csv('Datasets/Data.csv')

# tratar los valores NA
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)

dataset$Salary = ifelse(is.na(dataset$Salary),
                     ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Salary)

#Codificar las variables categoricas
dataset$Country = factor(dataset$Country,
                         levels = c("France", "Spain", "Germany"),
                         labels = c(1, 2, 3))

dataset$Purchased = factor(dataset$Purchased,
                           levels = c("No", "Yes"),
                           labels = c(0,1))

#Dividir los datos en conjunto de training y conjunto de test
set.seed(123) #asignar de forma aleatoria cuales serán Training o Test
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE) #Los true son para training
testing_set = subset(dataset, split == FALSE) #Los False son para testing

# Escalado de valores ya que los rangos entre age y salary son muy grandes
training_set[,2:3] = scale(training_set[,2:3]) #solo la columna 2(Age) y 3 (salary)
testing_set[,2:3] = scale(testing_set[,2:3]) # solo la columna 2(Age) y 3 (salary)




