### This is the final exam of PROGRAMMING R.

### This exam consists of ten questions with three levels of difficulty, ranging from
### basic (*) to advance (***). Each question is worth 1 point.

### In the exercises where a final answer is requested (these questions will be indicated by
### the expression "(QUESTION X)", where X is the corresponding number of the question), 
### please fill the line "### ANSWER TO X:", which you will find at the end of the exercise 
### where QUESTION X was formulated, with the corresponding answer (there can be more than
### one QUESTION in the same exercise).

### Good luck!
######################################################################################################

#### (*) Exercise 1: Together with this quiz you received two files (in folder "files"). Read both ####
#### of them without using fread. Bear in mind that both files represent the same dataset. Read these
#### files in a correct way so

rm(list = ls());

library(data.table);

folder_path <- "/Users/ckalibnelson/Desktop/Ckalib S. Nelson/ie/Global Master in Business Analytics & Big Data/Program/Statistical Programming - R/Assignment/final_quiz/Final Exam - C. Nelson/files"

file_1 <- setDT(read.csv(file.path(folder_path,"file_1.csv"), sep = ";", as.is = TRUE, header = TRUE, stringsAsFactors = FALSE));

file_2 <- setDT(as.data.frame(readRDS(file.path(folder_path, "file_2.rds"))));  

#all.equal(file_1,file_2);

#attributes(file_1);

#attributes(file_2);

identical(file_1, file_2);

# returns TRUE

#### (*) Exercise 2: You have the following data.frame ####

rm(list = ls());

df <- data.frame(name = c("Arsenal", "Chelsea", "Man.City", "Liverpool"),
                 position = c(5, 4, 2, 1),
                 london_based = c(TRUE, TRUE, FALSE, FALSE),
                 coach = c("Arteta", "Lampard", "Guardiola", "Klopp"));

#### Use each one of the four indexation methods seen in class to retrieve the position of Arsenal.
#### You should give 4 different commands, each one of them using only one of the indexation methods.

#Integer Indexation
df[1,2];

#Logical Indexation
df[df$position >= 5, 2];

#Name Indexation
df["1","position"];

#Variable Indexation
df$position[1];

### Use indexation by name to retrieve the columns that have words on them (name, coach). Do not write the
### names of these columns manually. You should check the class of the columns at some point
### of your solution.

indexation <- sapply(df, is.factor);

df[,indexation];

#### (*) Exercise 3: Cast the diamonds dataset (includen in ggplot2) to a data.table. Then, compute the ####
#### mode of the color for each value of cut. Take into account only variables where the sum of
#### x square, y square, and z square is greater than 50. Use .SD and .SDcols (the latter to perform
#### operations only having in .SD the necessary columns).

rm(list = ls());

library(ggplot2);

dt <- as.data.table(diamonds);

calcmode <- function(x){
  x <- as.factor(x)
  ret <- names(sort(table(x), decreasing = TRUE))[1];
  return(ret)
}

dt[(x^2 +  y^2 + z^2) > 50, lapply(.SD, calcmode), by = "cut", .SDcols = c("color")];

#### (*) Exercise 4: You have the following vector ####

rm(list = ls());

v <- sample(1:100, 100, replace = TRUE);

#### Create a for loop that iterates through all the values of 'v'. When you find a position
#### where the sum of the actual value and the next one is greater than 100, you should print 
#### "Found in x", where x is the actual index, and stop using break. Notice than in the last position
#### you do not have a "next value", so code your for loop accordingly.

for (index in 1:length(v)){
  if (v[index] + v[index + 1] > 100){
    print(sprintf("Found in %s", index));
    break;
  } else {
  }
}

#### (*) Exercise 5: Try to replicate the plot given in final_quiz_plot.png together  #### 
#### with this quiz using ggplot functions. Bear in mind that the values plotted come 
#### from the diamonds dataset.

rm(list = ls());

library(ggplot2);

### Create plot
my_plot <- ggplot(diamonds, aes(cut,price)) + 
  geom_boxplot(aes(fill = color)) +
  scale_y_continuous(trans = "reverse") + 
  ggtitle("From diamonds dataset") +  
  theme(aspect.ratio = 1);

my_plot;

#### (**) Exercise 6: Create a new R Markdown and make the following ####
#### modifications:  

#### 1) Modify the header to include a new parameter called 'threshold' with a default
#### value of 1000.
#### 2) Modify the general configuration chunk so for any R block
#### the code is not computed by default.
#### 3) Create a new text block at the bottom with an inline R command showing the
#### dimensions of the diamonds dataset. 
#### 4) Create a new code block below the previous text block to compute the
#### summary of all the rows in diamonds where price is greater than threshold. This code
#### must be computed and the results shown in the output file generated.

#### You have to submit the modified .Rmd together with this .R file.

#### (**) Exercise 7: Compute k-prototypes on diamonds in a way that you reach a #### 
#### total within cluster distance of less than 3*10^11 (3e+11). If the output of your 
#### clustering is stored in a variable called "clustering" you can access this value running

rm(list = ls());

library(cluster)
library(clustMixType)
library(ggplot2)

set.seed(42)
dt <- as.data.table(ggplot2::diamonds)
clustering <- kproto(x = dt, k = 11, iter.max = 20, verbose = TRUE)

clustering$tot.withinss;

total <- clustering$tot.withinss < 3*10^11

total;

# Finally, get the median price value for each final cluster you obtained using a dt[i, j, by] command.

dt$cluster <- clustering$cluster
median_price <- dt[order(cluster), median(price), by = cluster]
print(median_price)

#### (**) Exercise 8: Create a new shiny app and make the following modifications ####
#### to the ui.R and server.R files 

#### 1) Create a new multiple selectInput called 'cut' where an user can
#### choose several values among the existing values in the cut column of diamonds.
#### 2) Use the value of this 'cut' input to plot a boxplot of the price by cut 
#### (like the one in exercise 5 but not taking into account the color) for the rows
#### in diamonds that have a value of cut among the ones chosen by the user. Use ggplot.
#### 3) If a user changes the value of 'cut' selected the plot should
#### also automatically change.
#### 4) You can show this plot at any position in your shiny app.

#### You have to submit this shiny app together with this .R file.

#### (***) Exercise 9: Check the following block of code ####

rm(list = ls());

index_selected <- c();
dt <- as.data.table(diamonds);
numeric_vars <- colnames(dt)[sapply(dt, class) %in% c("integer","numeric")];
for (col in numeric_vars){
  values <- as.numeric(t(dt[, col, with = F]));
  correlation <- abs(cor(values, dt$price));
  if (correlation > 0.9){
    index_selected <- c(index_selected, TRUE);
  } else {
    index_selected <- c(index_selected, FALSE);
  }
}
vars_selected <- numeric_vars[index_selected];
vars_selected;

#### Carry out these steps:

#### 1) Create a function 'select_variables' to compute the same operations for
#### a given vector as each iteration of the previous loop is doing for each 
#### column of dt

select_variables <- function(dtable){
  numeric_vars <- colnames(dt)[sapply(dt, class) %in% c("integer","numeric")];
  for (col in numeric_vars){
    values <- as.numeric(t(dt[, col, with = F]));
    correlation <- abs(cor(values, dt$price));
    if (correlation > 0.9){
      index_selected <- c(index_selected, TRUE);
    } else {
      index_selected <- c(index_selected, FALSE);
    }
  }
  return(numeric_vars[index_selected]);
}

select_variables(dt);

#### 2) Use this function together with any apply-type method to get 
#### the same output as the one in 'selected'

lapply(dt, select_variables);

#### 3) Try to generalize your function so this operation can be called
#### with any other combination of target (instead of dt$price)
#### and threshold (instead of 0.9). dt$price and 0.9 should be the
#### default values

selection_variables <- function(target = "price", threshold = .9, ...){
  arguments <- list(...);
  numeric_vars <- colnames(dt)[sapply(dt, class) %in% c("integer","numeric")];
  for (col in numeric_vars){
    values <- as.numeric(t(dt[, col, with = F]));
    correlation <- abs(cor(values, target));
    if (correlation > threshold){
      index_selected <- c(index_selected, TRUE);
    } else {
      index_selected <- c(index_selected, FALSE);
    }
  }
  return(numeric_vars[index_selected]);
}

select_variables(dt);

#### (***) Exercise 10: Your goal now is to predict the 'cut' column of diamonds. ####
#### Take into account only diamonds with cuts fair or good (binary classification problem).
#### Find a model with more than 0.93 of AUC for the test set.

rm(list = ls());

# Use this train/val/split

# setting seed to reproduce results of random sampling
set.seed(100); 

# Convert diamonds to data.table
library(ggplot2);
library(data.table)
library(pROC)
library(e1071)
library(randomForest)
library(dplyr)
library(cvAUC)

dat <- as.data.table(diamonds);

# Filter cuts
dat <- dat[cut %in% c("Fair", "Good")]

# row indices for training data (70%)
train_index <- sample(1:nrow(dat), 0.7*nrow(dat));  

# row indices for validation data (15%)
val_index <- sample(setdiff(1:nrow(dat), train_index), 0.15*nrow(dat));  

# row indices for test data (15%)
test_index <- setdiff(1:nrow(dat), c(train_index, val_index));

# split data
train <- dat[train_index]; 
val <- dat[val_index]; 
test  <- dat[test_index];

dim(dat);
dim(train);
dim(val);
dim(test);


# Train Random Forest Model
model <- randomForest(droplevels(cut) ~., data = train)

#Prediction for train set and validation set
predict_train <- predict(model, newdata = train)
predict_val <- predict(model, newdata = val)

#AUC for train set and validation set
auc_train <- auc(as.numeric(predict_train), as.numeric(train$cut))
auc_val  <- auc(as.numeric(predict_val), as.numeric(val$cut))

#Prediction for test set
predict_test <- predict(model, newdata = test)

#AUC of test set
auc_test <-  auc(as.numeric(predict_test), as.numeric(test$cut))
print(auc_test)

