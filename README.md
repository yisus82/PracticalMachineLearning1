Predict Five Types of Exercise
========================================================

## Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data
about personal activity relatively inexpensively. One thing that people regularly do is quantify how much of a
particular activity they do, but they rarely quantify how well they do it. 
In this analysis, we choose 18 variables to predict which exercise they do, and train using random forest method.
We find that the expected error rate is less than 0.3% in test sample. Therefore, we conclude that we can predict which exercise they do.

## Analysis

First, we check if the data files exists and if they does not exist we download them and then we load the data:

```r
if (!file.exists("pml-training.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile="pml-training.csv", method="curl")
}

raw_training <- read.csv("pml-training.csv")

if (!file.exists("pml-testing.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              destfile="pml-testing.csv", method="curl")
}

testing  <- read.csv("pml-testing.csv")
```

Partition of the raw training dataset into training & cross-validation datasets:

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
set.seed(1)
inTrain <- createDataPartition(y=raw_training$classe,p=0.6, list=F)
training <- raw_training[inTrain, ]
cv       <- raw_training[-inTrain, ]
```

Plotting of some of the variables. We show only 4 of them:

```r
qplot(new_window, num_window, col=classe, data=training, alpha=0.1)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) 

```r
qplot(roll_belt, pitch_belt, col=classe, data=training, alpha=0.1)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 

```r
qplot(roll_arm, pitch_arm, col=classe, data=training, alpha=0.1)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-33.png) 

```r
qplot(yaw_arm, total_accel_arm, col=classe, data=training, alpha=0.1)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-34.png) 

From this plotting analysis, we adopt the following variables as explanatory variables:

```r
input_vars_list <- c("new_window", "num_window", "roll_belt", "pitch_belt", "yaw_belt", 
  "total_accel_belt", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", 
	"roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell",
	"roll_forearm", "pitch_forearm", "yaw_forearm", 
	"total_accel_forearm");
```

Using the explanatory variables, we create models using the random forest method:

```r
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
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

## Results

Checking the error rate in the training and the cross-validation datasets. 
The error rate is less than 0.3% in the cross-validation dataset. 

```r
missClass <- function(values, prediction){
  sum(prediction != values)/length(values)
}
missClass(training$classe, predict(modFit, training))
```

```
## [1] 0
```

```r
missClass(cv$classe, predict(modFit, cv))
```

```
## [1] 0.002549
```

Using our model, we make the predictions and write them in text files.

```r
answers <- predict(modFit, testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste("problem_id_", i, ".txt", sep = "")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(answers)
```
