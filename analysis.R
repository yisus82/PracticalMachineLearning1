if (!file.exists("pml-training.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile="pml-training.csv", method="curl")
}

raw_training <- read.csv("pml-training.csv");

if (!file.exists("pml-testing.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              destfile="pml-testing.csv", method="curl")
}

testing  <- read.csv("pml-testing.csv")

library(caret)

set.seed(1)
inTrain <- createDataPartition(y = raw_training$classe, p = 0.6, list = F)
training <- raw_training[inTrain, ]
cv       <- raw_training[-inTrain, ]

qplot(new_window, num_window, col = classe, data = training, alpha = 0.1)

qplot(roll_belt, pitch_belt, col = classe, data = training, alpha = 0.1)

qplot(yaw_belt, total_accel_belt, col = classe, data = training, alpha = 0.1)

qplot(roll_arm, pitch_arm, col = classe, data = training, alpha = 0.1)

qplot(yaw_arm, total_accel_arm, col = classe, data = training, alpha = 0.1)

qplot(roll_dumbbell, pitch_dumbbell, col = classe, data = training, alpha = 0.1)

qplot(yaw_dumbbell, total_accel_dumbbell, col = classe, data = training, alpha = 0.1)

qplot(total_accel_dumbbell, yaw_dumbbell, col = classe, data = training, alpha = 0.1)

qplot(roll_forearm, pitch_forearm, col = classe, data = training, alpha = 0.1)

qplot(yaw_forearm, total_accel_forearm, col = classe, data = training, alpha = 0.1)

input_vars_list <- c("new_window", "num_window", "roll_belt", "pitch_belt", "yaw_belt", 
                     "total_accel_belt", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", 
                     "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell",
                     "roll_forearm", "pitch_forearm", "yaw_forearm", 
                     "total_accel_forearm")

exp_input <- function(x){
  res = x[1]
  for (i in 2:length(x)){
    res <- paste(res, " + ", x[i], sep = "")
  }
  res
}
input_vars <- exp_input(input_vars_list)
set.seed(2)
modFit <- train(eval(parse(text = paste("classe ~", input_vars, sep = ""))), data = training, method = "rf")

missClass <- function(values, prediction){
  sum(prediction != values)/length(values)
}
missClass(training$classe, predict(modFit, training))
missClass(cv$classe, predict(modFit, cv))

answers <- predict(modFit, testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste("problem_id_", i, ".txt", sep = "")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(answers)
