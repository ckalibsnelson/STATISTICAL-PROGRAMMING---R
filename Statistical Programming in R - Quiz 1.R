### This is partial exam 1 of PROGRAMMING R.

### This exam consists of ten questions with three levels of difficulty, ranging from
### basic (*) to advance (***). Each question is worth 1 point.

### In the exercises where a final answer is requested (these questions will be indicated by
### the expression "(QUESTION X)", where X is the corresponding number of the question), 
### please fill the line "### ANSWER TO X:", which you will find at the end of the exercise 
### where QUESTION X was formulated, with the corresponding answer (there can be more than
### one QUESTION in the same exercise).

### Good luck!
######################################################################################################
rm(list = ls());

set.seed(14);

#### (*) EXERCISE 1: Suppose you have the following vector ####

v <- sample(1:100, 100, replace = TRUE);
v;

#### Create a loop that iterates through all the positions of 'v' and, for each
#### iteration, prints the value of 'v' in that position only if the value is greater 
#### than 10. Do this using both a for and a while loop. Each loop should contain an if
#### (or if-else) clause.

#Define x
x <- 1;
length(v);

#For loop
for (x in 1:length(v)){
  if (v[x] > 10){
    print(v[x]);
  }
}

#While loop
x <- 1;
while (x <= length(v)){
  if (v[x] > 10){
    print(v[x]);
  }
  x <- x + 1;
}

#### (*) EXERCISE 2: Assuming you have the following matrix: ####

rm(list = ls());

M <- matrix(c("Rex", "Tom", "Jerry", "Bobby",
               "dog", "cat", "mouse", "cat",
               3, 5, 2, 7), nrow = 4, ncol = 3);
rownames(M) <- 1:4
colnames(M) <- c("name", "type", "age");
M;

### Use numeric indexation to get the value of column 'age' in the second row.

#[Row,Column]
M[2,3];

### Use only logical indexation to get the rows where ''type''age' is greater than 4. Do not create
### the logical index manually, it should be the output of a logical operator (>, <, ==, !=, etc.)

#Logical indexation
M[M[, colnames(M) == "age"] > 4,];

### Use only indexation by name to get the type of Jerry. 

#Indexatiion
M[3, "type"];

#### (*) EXERCISE 3: A hidden encrypted message has been recently discovered stating: ####
### 'R is cool since 2018-03-08 11:35:20'.
### The date and time of this message is encrypted in the following variable

rm(list = ls());

encrypted <- "008R235_11;  1803...20";

### You want to know how many hours has passed since the encrypted date.
### Write the R commands required to get this number.

encrypted <- strptime(encrypted, format = "00%dR2%M_%H;  %y%m...%S");
encrypted;
timesince <- as.numeric(difftime(encrypted, Sys.time(), units = "hours"));
abs(timesince);

#### (*) EXERCISE 4: Create a function, called 'equal_classes', that returns as output  ####
#### TRUE only if the first argument, named 'var1', and the second argument, named 'var2', 
#### belong to the same class. Use default values so that the output of

rm(list = ls());

equal_classes <- function(var1 = 1, var2 = "Ckalib"){
  if(class(var1) == class(var2))
    print(T)
  else
    print(F)
}
equal_classes();

#### is FALSE

#### (*) EXERCISE 5: Suppose You have the following vectors: ####

rm(list = ls());

v1 <- c(TRUE, FALSE, TRUE);
v2 <- c("A", "B", "C")
v3 <- c(1, 2)

### Combine these three vectors in a single matrix, in a manner that v1
### corresponds to the first column in the matrix, v2 to the second and v3
### to the last colum (it is ok if you get a warning due to v2 having
### a different length).
### To which R class belong each column of this matrix? (QUESTION 1).
### v2 has only 2 elements, which value has been assigned to the third row
### of this column? Why? (QUESTION 2).

M <- matrix(c(v1, v2, v3), nrow = 3, ncol = 3);
M;

class(M[,1]);
class(M[,2]);
class(M[,3]);

### ANSWER TO 1: Character
### ANSWER TO 2: If there are too few elements in data to fill the matrix, 
###then the elements in data are recycled. In this case, it's recycling the
###first element in the first column. Since I am using matrix, instead of cbind,
###my blank value is yielding "True" not "1"

#### (**) EXERCISE 6: Suppose you have the following matrix ####

rm(list = ls());

M <- matrix(sample(1:100, 100, replace = TRUE), nrow = 10, ncol = 10);

#### Your objective now is to create a double for loop (one for the rows, one for the columns)
#### that iterates through all the positions of M. In the inner (second) loop you should check
#### if the value of M in that position (row and column) is greater than 90. If that is the case
#### print the indexes corresponding to that row and column and run a next statement.

for (row in 1:nrow(M)) {
  for (col in 1:ncol(M)) {
    if(M[row,col] > 90) {
      print("Value")
      print(M[row,col])
      print("Indexes")
      print(c(row,col))}
  next}
}

#### (**) Exercise 7: Try to replicate the plot given in quiz.png together  #### 
#### with this quiz using basic plot functions. You must use these vectors

rm(list = ls());

v1 <- iris$Sepal.Length;
v2 <- iris$Sepal.Width;
v3 <- iris$Species;

# Set plotting environment
par(mfrow=c(1,2));

#First visualization
#Green line
plot(v1, type = "l", col = "green", main = "Plot 1", ylab = "CM", xlim = c(0,150), ylim = c(0,10));
#Red scatter
lines(v2, type = "p", col = "red");

#Second visualization
v3table <- table(v3);
barplot(v3table, col = "blue", horiz = TRUE, main = "Plot 2", ylab = "Frequency", xlab = "Value");

### (**) EXERCISE 8: Source the file "auxiliary.R" that comes ####
#### with this test. Compute the function inside this script for each column 
#### of the following dummy dataset and store the result in a variable. 
#### Note: If you have memory problems, reduce the size of df.

rm(list = ls());

source('/Users/ckalibnelson/Desktop/Ckalib S. Nelson/ie/Global Master in Business Analytics & Big Data/Program/Statistical Programming - R/Intermediate Quiz 1/quiz/auxiliary.R')
df <- data.frame(v1 = sample(1:10, 10^8, replace = TRUE),
               v2 = sample(11:20, 10^8, replace = TRUE),
               v3 = sample(21:30, 10^8, replace = TRUE));

colnames(df) <- c("v1", "v2", "v3");
head(df);

#### Do this using both a while loop and sapply(). Measure the computational time
#### required for each method. Which one runs faster in your computer? (QUESTION 3)

#While Loop for averages
averages <- c();
column <- 1;
while (column <= ncol(df)){
    averages <- c(averages, compute_average(df[,column]))
    column <- column + 1;
}
averages;  

#Sapply() for averages
sapply(df, compute_average)

#Computational time for while loop for averages
system.time(
{averages <- c();
column <- 1;
while (column <= ncol(df)){
    averages <- c(averages, mean(df[,column]))
    column <- column + 1;
}}
)

#Computational time for sapply for averages
system.time(sapply(df, compute_average))

#### ANSWER 3: sapply() runs faster

#### Now create your own function, called compute_median, to perform the same operations 
#### as with compute_average but this time computing the median for each column of df.

#Median Formula
compute_median <- function(x){
  ret <- median(x);
  return(ret);
}

#While loop for medians
medians <- c();
column <- 1;
while (column <= ncol(df)){
  medians <- c(medians, compute_average(df[,column]))
  column <- column + 1;
}
medians;  

#Sapply() for medians
sapply(df, compute_median)

#Computational time for while loop for median
system.time(
  {medians <- c();
  column <- 1;
  while (column <= ncol(df)){
    medians <- c(medians, mean(df[,column]))
    column <- column + 1;
  }}
)

#Computational time for sapply for median
system.time(sapply(df, compute_median))

#Oddly enough, it seems as if my while loop was faster than the sapply for the median calculation

#### (***) Exercise 9: Read the R object "userbase.RData" provided with this exam. It represents a ####
### database of clients of an airline, where each row corresponds to a flight purchase. Taking into  
### account only flights bought on "2018-11-02", get the  5 users that have made the most
### expensive purchases, i.e if you get that the most expensive purchases are:

rm(list = ls());

# user 1 100
# user 5 90
# user 7 80
# user 5 70
# user 3 60
# user 7 50
# user 8 40

# Your code should return:

# "user1"  "user5"  "user7"  "user3"  "user8"

userbase <- readRDS('/Users/ckalibnelson/Desktop/Ckalib S. Nelson/ie/Global Master in Business Analytics & Big Data/Program/Statistical Programming - R/Intermediate Quiz 1/quiz/userbase.RData')
head(userbase);

library(dplyr)

flights <- userbase %>%
  filter(booking_date == "2018-11-02") %>%
  group_by(user) %>%
  summarise(total = sum(price)) %>%
  arrange(desc(total));

head(flights,5);

print(c(flights[1:5,"user"]));

#### (***) EXERCISE 10: Create a function called 'count_pairs_and_trios' which takes advantage of  ####
#### the ... argument functionality to return a list with two variables: the total number of objects
#### with a length equal to 2,  stored in a variable called 'n_duos', 
#### and the total number of objects with length 3, stored in a variable called 'n_trios'. For instance, the 
#### following call

rm(list = ls());

count_pairs_and_trios <- function(...){
  arguments <- list(...);
  n_duos <- 0;
  n_trios <- 0;
    for(var in arguments){
      n_duos <- n_duos + sum(length(var) == 2)
      n_trios <- n_trios + sum(length(var) == 3)
    }
  return(list(n_duos = n_duos, n_trios = n_trios));
}

count_pairs_and_trios(v1 = c("cat", "cat"), 
              v2 = letters[1:4], 
              v3 = c("1", "2"),
              v4 = c("dog"),
              v5 = factor(c("cat", "dog", "dog"), levels = c("cat", "dog")));

#### should give this output

# $n_duos
# [1] 2
# 
# $n_trios
# [1] 1


