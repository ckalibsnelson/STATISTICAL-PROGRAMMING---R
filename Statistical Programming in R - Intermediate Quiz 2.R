### This is partial exam 2 of PROGRAMMING R.

### This quiz consists of ten questions with three levels of difficulty, ranging from
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

#### (*) Exercise 1: You have the following vector of dates:  ####

dates <- c("03-22-2018", "2018/03/22", "2018-03-22", "22 03 2018");

#### Use strptime to cast these four values to objects of class date.

date <- as.Date(strptime(dates, format = c("%m-%d-%Y", "%Y/%m/%d", "%Y-%m-%d", "%d %m %Y")));

class(date);

#### Now extract the number of weeks between today and the date represented in the dates vector.

date_difference <- difftime(date, Sys.time(), units = "weeks");

as.numeric(abs(date_difference));

#### (*) Exercise 2: You have the following data.frame ####

rm(list = ls());

df <- data.frame(name = c("Arsenal", "Chelsea", "Man.City", "Liverpool"),
                 position = c(5, 4, 2, 1),
                 london_based = c(TRUE, TRUE, FALSE, FALSE));

#### Use any indexation method to get if Liverpool is london_based or not.

df$london_based[df$name == "Liverpool"]; #False

#### Use indexation by variable ($ operator) in combination with logical indexation
#### to get the name of all the teams in a top 4 position (i.e positions 1, 2, 3 or 4)
#### that are london_based.

df[(df$position <= 4 & df$london_based == TRUE),];

#### (*) Exercise 3: Create a function, called substract_dates, that returns as output  ####
#### the difference in days between two dates, passed as arguments 'date1' and 'date2' 
#### to the function. In particular we want 'date2' - 'date1' in terms of days.
#### Use default values so that the output of

rm(list = ls());

subtract_dates <- function(date1, date2 = Sys.Date(),...){
  return(as.numeric(date2 - date1))
};

subtract_dates(date1 = Sys.Date()- 2);

#### is 2

#### (*) Exercise 4: You have the following vector ####

rm(list = ls());

v <- sample(c("cat", "dog", "mouse"), 100, replace = TRUE);

#### Create a for loop that iterates through all the values of 'v' printing how many dog values
#### have you found up to that point. When you get to the fifth 'dog' value you should stop.
#### (after the corresponding print). Use break. Therefore, your for loop should print something
#### along these lines:

dog <- 0;

for (iterator in 1:length(v)){
  if (v[iterator] == "dog") { 
    print(paste(dog + 1,"n_dogs found"));
    dog <- dog + 1}
  if (dog == 5){
    break}
  };

# [1] "1 n_dogs found"
# [1] "2 n_dogs found"
# [1] "3 n_dogs found"
# [1] "4 n_dogs found"
# [1] "5 n_dogs found"

#### (**) Exercise 5:  You have the following list #### 

rm(list = ls());

l <- list(v1 = c("a", "b", "c"),  
          v2 = c("p", "r", "o", "g", "r", "a", "m", "m", "i", "n", "g", "R"),
          v3 = c("d", "e", "f"),
          v4 = letters[1:3],
          v5 = letters[3:7]);
l;

#### Count how many objects of this list have length 3. Do not use a loop.

sum(lengths(l) == 3);

#### (**) Exercise 6: Convert the iris dataset into a data.table variable ####
#### named 'dat'. Then compute the median value of all the columns except from
#### Petal.Length by Species. Take into account only flowers with Petal.Length greater than 1.5.
#### Use only one command with the format dt[i, j, by]. Use .SD and .SDcols

rm(list = ls());

#install.packages("data.table");

library(data.table);

dat <- as.data.table(iris);

dat[Petal.Length > 1.5, lapply(.SD, median), by = "Species", .SDcols = c("Sepal.Length","Sepal.Width","Petal.Width")];

#### EXPECTED OUTPUT:
#       Species Sepal.Length Sepal.Width Petal.Width
# 1:     setosa          5.0         3.4         0.2
# 2: versicolor          5.9         2.8         1.3
# 3:  virginica          6.5         3.0         2.0

#### (**) Exercise 7: Perform the same operation as in the previous exercise but this time ####
#### using directly iris as a data.frame and one single dplyr command. Use piping.

rm(list = ls());

#install.packages("dplyr");

library(dplyr);

dat <- as.data.frame(iris);

dat <- 
  dat %>% 
  group_by(Species) %>%
  summarise(median(Sepal.Length), median(Sepal.Width), median(Petal.Width));

dat;

#### EXPECTED OUTPUT:
# A tibble: 3 x 4
#    Species    Sepal.Length Sepal.Width Petal.Width
#    <fct>             <dbl>       <dbl>       <dbl>
# 1 setosa              5           3.4         0.2
# 2 versicolor          5.9         2.8         1.3
# 3 virginica           6.5         3           2  

#### (**) Exercise 8: Modify the function in auxiliary_functions.R so now it returns ####
#### the values of the argument x greater than 1 instead of the ones that are greater
#### than 5. Source this function and  then use a dt[i, j, by] operation to call it to compute
#### the values greater than 1 for all the columns in iris that are numerical. Store the output
#### in a variable called 'gr_1'. Use .SD.

rm(list = ls());

source('/Users/ckalibnelson/Desktop/Ckalib S. Nelson/ie/Global Master in Business Analytics & Big Data/Program/Statistical Programming - R/Intermediate Quiz 2/quiz2/auxiliary_functions.R');

library(data.table);

body(count_greater_than)[[2]] <- substitute(ret <- sum(x > 1)); #compute the values greater than 1 instead of 5

gr_1 <- as.data.table(iris)[, sapply(.SD, count_greater_than), .SDcols = c ('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')];

gr_1;

#### EXPECTED OUTPUT:
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 150          150          149           93 

#### (***) Exercise 9: Read the R object "userbase.RData" provided with this exam. It represents a ####
### database of clients of an airline, where each row corresponds to a flight purchase. Get the top 3 destinations
### in terms of average price for tickets sold online. Take into account only tickets
### with price greater than 50. Get these results using dt[i, j, by] chained operations.

rm(list = ls());

userbase <- readRDS('/Users/ckalibnelson/Desktop/Ckalib S. Nelson/ie/Global Master in Business Analytics & Big Data/Program/Statistical Programming - R/Intermediate Quiz 2/quiz2/userbase.RData')

#install.packages("data.table");

library(data.table);

userbase <- as.data.table(userbase);

userbase[sale_channel == "online" & price > 50, list(avg_price = mean(price)), by = "destination"][order(-avg_price)][1:3];

#### (***) Exercise 10: Create a function called 'other_function' which takes advantage of  ####
#### the ... argument functionality to return a single value, total_length, with the sum of the
#### length of all the objects passed as arguments in the ... Use sapply for this purpose.
#### For instance, the following call

rm(list = ls());

other_function <- function(...){
  arguments <- list(...);
  total_length <- 0;
  total_length <- sum(sapply(arguments, function(x)sum(length(x))))
  return(total_length)
}

other_function(v1 = c(1,2,3,4), 
              v2 = letters[1:4], 
              v3 = 101:200,
              v4 = c(5, 6, 7),
              v5 = factor(c("cat", "dog", "dog"), levels = c("cat", "dog")));

# Could also do this, but w/o sapply

other_function <- function(...){
  arguments <- list(...);
  total_length <- 0;
  for(var in arguments){
    total_length <- sum(lengths(var)) + total_length
  }
  return(total_length)
}
  
other_function(v1 = c(1,2,3,4), 
               v2 = letters[1:4], 
               v3 = 101:200,
               v4 = c(5, 6, 7),
               v5 = factor(c("cat", "dog", "dog"), levels = c("cat", "dog")));
#### should give this output

# 114
