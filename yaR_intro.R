####Chapter 3.1: Exploring data####
install.packages('yarrr')
library(yarrr)
?pirates
#Look at the first few rows of data
head(pirates)
#See the column names
names(pirates)
#See entire dataset
#View(pirates)

####chapter 3.2: Descriptive statistics####
#Mean pirate age
mean(pirates$age)
#Tallest pirate
max(pirates$height)
#How many of each sex
table(pirates$sex)
#Calculate mean age for each sex
aggregate(formula=age~sex, data=pirates, FUN=mean)

####Chapter 3.3: Plotting####
#Plot relationship between pirate's height and weight using plot() function
plot(x=pirates$height, y=pirates$weight)
#Fancier version
plot(x=pirates$height, y=pirates$weight, main='My first scatterplot of pirate data!',
     xlab='Height (in cm)', ylab='Weight (in kg)', pch=16, col=gray(.0,.1), grid())
#Create linear regression model
model<- lm(formula=weight~height, data=pirates)
abline(model, col='blue')
#Pirateplot to show distribution of pirate's age based on favorite sword
pirateplot (formula = age~sword.type, data=pirates, main="Pirateplot of ages by favorite sword")
#Pirateplot of height and sex using different theme and pony palette
pirateplot(formula = height~sex, data=pirates, main="Pirateplot of height by sex", pal="pony", theme=3)
#Show me the pony palette
piratepal(palette="pony", plot.result=TRUE, trans=.1)

####Chapter 3.4: Hypothesis tests####
#Age by headband t-test
t.test(formula=age~headband, data=pirates, alternative='two.sided')
#Significant correlation between height and weight
cor.test(formula=~height + weight, data=pirates)
#ANOVA to see if there is a difference between number of tattoos based on favorite sword
#Create tattoos model
tat.sword.lm <- lm(formula=tattoos~sword.type, data=pirates)
#Get ANOVA table
anova(tat.sword.lm)

####Chapter 3.5: Regression analysis####
#Regression to see if age, weight, and tattoos predicts how many treasure chests they've found
#Create linear regression model: DV=tchests, IV=age, weight, tattoos
tchests.model <-lm(formula=tchests~age+weight+tattoos, data=pirates)
#Show summary stats
summary(tchests.model)

####Chapter 3.6: Bayesian statistics####
#Repeat previous analyses with Bayesian verions. Install and load BayesFactor package
install.packages('BayesFactor')
library(BayesFactor)
#Bayesian version of earlier t-test of age vs. headband
ttestBF(formula=age~headband, data=pirates)

#####Chapter 4.4: Objects and functions#####
#Create a vector called tattoos
tattoos <- c(4, 67, 23, 4, 10, 35)
#Apply mean function to tatooos object
mean(tattoos)
#Create a new object called a with a value of 100
a <- 100
#Define object which has global revenue of Curse of the Black Pearl USD
blackpearl.usd <- 634954103
#Make new object to convert to euros
blackpearl.eur <- blackpearl.usd*(0.88)
blackpearl.eur
#See how much more Dead Man's Chest made compared to Black Pearl
deadman.usd <- 1066215812
deadman.usd/blackpearl.usd

####Chapter 5: Scalars and vectors####
#Crew information
captain.name <- "Jack"
captain.age <- 33
crew.names <- c("Heath", "Vincent", "Maya", "Becki")
crew.ages <- c(19, 35, 22, 44)
crew.sex <- c(rep("M", times = 2), rep("F", times=2))
crew.ages.decade <- crew.ages/10
#Earnings over first 10 days at sea
days <- 1:10
gold <- seq(from=10, to=100, by=10)
silver <- rep(50, times=10)
total <- gold + silver

####Chapter 5.2: Vectors####
#Create an object with integers from 1-5
a <- c(1, 2, 3, 4, 5)
#Create longer vector by combining vectors already defined.
#Create a vector of numbers from 1-10 by generating a vector a from 1-5 and vector b from 6-10
#Then comine into single vector x
a <- c(1, 2, 3, 4, 5)
b <- c(6, 7, 8, 9, 10)
x <- c(a, b)
#Can also create character vector by using c() function to combine character scalars into character vectors
char.vec <- c("Ceci", "nest", "pas", "une", "pipe")
char.vec
#The a:b function takes two numeric scalars "a" and "b" as arguments, and returns a vector of numbers from 
#the starting point "a" to the ending point "b" in steps of 1
1:10
10:1
2.5:8.5
#The seq() function has additional arguments: by and lengthout
#Create the numbers 1-10 in steps of 1
seq(from = 1, to = 10, by = 1)
#Integers from 0-100 in steps of 10
seq(from = 0, to = 100, by = 10)
#If using lengthout argument, the sequence will have length equal to length.out
#Create 10 numbers from 1-5
seq(from = 1, to = 5, length.out = 10)
# Three numbers from 0-100
seq(from = 0 , to =  100, length.out = 3)
#rep() function allows you to repeat a scalar (or vector) a specified number of times or desired length
rep(x = 3, times = 10)
rep(x = c(1, 2), each = 3)
rep(x = 1:3, length.out = 10)

####Chapter 5.3: Generating random data####
#Sample() funtion allows you to draw random samples of elements (scalars) from a vector
#From the integers 1:10, draw 5 numbers
sample(x = 1:10, size = 5)
#If you don't specify replace argument, R will assume sampling without replacement--each element only sampled once
#Draw 30 samples from the integers 1:5 with replacement
sample (x =1:5, size = 10, replace = TRUE)
#Use prob argument to specify how likely each element in vector x should be selected
#Length of prob argument should be as long as x argument
#Let's draw 10 samples (with replacement) from vector ["a", "b"], but make probability of selecting
#"a" to be .90 and "b" .10
sample(x = c("a", "b"), prob = c(.9, .1), size = 10, replace =TRUE)
#Ex: Simulating coin flips
#Let's simulate 10 flips of a fair coin. Because all values are equally likely, we don't need to specify prob
sample (x = c("H", "T"), size = 10, replace = TRUE)
#Simulate flips of a biased coin
sample (x = c("H", "T"), prob = c(.8, .2), size = 10, replace = TRUE)
#Sample drawing coins from chest with 20 gold, 30 silver, 50 bronze. Draw 10 random coins
#Create chest with 100 coins
chest <- c(rep("gold", 20), rep("silver", 30), rep ("bronze", 50))
#Draw 10 coins
sample (x=chest, size=10)
#See all distributions included in Base R
?Distributions
#Normal (Gaussian) distribution is most important-bell shaped with two parameters: mean and sd
#To generate samples from a normal distribution in R, use the function rnorm()
#5 samples from a Normal dist with mean = 0, sd = 1
rnorm(n = 5, mean = 0, sd = 1)
#3 samples from a Normal dist with mean =10, sd=15
rnorm(n = 3, mean = -10, sd = 15)
#Uniform distribution gives equal probability to all values between its min and max values
#Everything between its lower and upper bounds are equally likely to occur
#To generate samples from a uniform distribution, use function runif with 3 arguments
#n = the number of observations to draw from the distribution
#min = the lower bound of the uniform distrubution from which samples are drawn
#max = the upper bound of the uniform distrubution from which samples are drawn
#5 samples from uniform dist with bounds at 0 and 1
runif(n = 5, min = 0, max = 1)
#10 samples from uniform distribution with bounds at -100 and +100
runif(n = 10, min = -100, max = 100)
#Every time you draw a sample from a prob. distribution, you'll get a different result
#Draw a sample of size 5 from a normal distribution with mean 100 and sd 10
rnorm(n = 5, mean =100, sd=10)
#Again
rnorm(n = 5, mean =100, sd=10)
#Use set.seed() to control random samples
#Fix sampling seed to 100, so the next sampling functions always produce same value
set.seed(100)
#The result will always be -0.5022, 0.1315, -0.0789
rnorm(3, mean = 0, sd = 1)
#The result will always be 0.887, 0.117, 0.319
rnorm(3, mean = 0, sd = 1)
####Chapter 5.4 exercises####
#Create the vector [1,2,3,4,5,6,7,8,9,10] three ways: using c(), a:b and seq()
x <- c(1, 2, 3, 4, 5,6, 7, 8, 9, 10)
y <- (1:10)
z <- seq(from=1, to=10, by=1)
#Create vector 2.1, 4.1, 6.1, 8.1 using seq(
a <- seq(from=2.1, to=8.1, by=2)
#chapter 5.4 exercise
swords <- c(rep("scimitar", 10), rep("broadsword", 40), rep ("cutlass", 50))
sample(x=swords, size=10)

####Chapter 6: Vector functions####
#10 students from two different classes took 2 exams
#Here are three vectors showing the data
midterm <- c(62, 68, 75, 79, 55, 62, 89, 76, 45, 67)
final <- c(78, 72, 97, 82, 60, 83, 92, 73, 50, 88)
#How many students are there?
length(midterm)
#Add 5 to each midterm score (extra credit)
midterm <- midterm + 5
#Difference between final and midterm scores
final-midterm
#Each student's average score
(midterm + final) / 2
#Mean midterm grade
mean(midterm)
#SD of midterm grades
sd(midterm)
#Highest final grade
max(final)
#z-scores
midterm.z <- (midterm - mean(midterm)) / sd(midterm)
final.z <- (final - mean(final)) / sd (final)

####Chapter 6.1.2: Arithmetic operations on vectors: Pirate bake sale####
#Pirate bake sale: Let's say 5 pirates sold both pies and cookies. Record the total number of pies and cookies in 2 vectors
pies <- c(3, 6, 2, 10, 4)
cookies <- c(70, 40, 40, 200, 60)
#If you want to know how many total items each pirate sold, you can do this just by adding the two vectors
total.sold <- pies + cookies
total.sold

####Chapter 6.2: Summary statistics
#Create vector x that contains the number of tattoos from 10 random pirates
tattoos <- c(4, 50, 2, 39, 4, 20, 4, 8, 10, 100)
#Now we can calculate several descriptive stats on this vector by using summary stat functions
min(tattoos)
mean(tattoos)
sd(tattoos)

#6.2.1: length()
#Use length() function to take vector as an argument and return a scalar representing the number of elements in vector
a <- 1:10
#How many elements are in a?
length(a)
b <- seq(from = 1, to = 100, length.out = 20)
#How many elements are in b?
length(b)
length(c("This", "character", "vector", "has", "six", "elements."))
length("This character scalar has just one element.")

#6.2.2: Additional numeric vector functions
#round(x, digits)--Round elements in x to "digits" digits
round(c(2.231, 3.1415), digits =1)
#ceiling(x), floor(x)--Round elements x to the next highest (or lowest) integer
ceiling(c(5.1, 7.9))
# x %% y--Modular arithmetic (ie. x mod y)
7 %% 3

#6.2.3: Sample stats from random samples using rnorm and runif
#5 samples from a Normal dist with mean = 10 and sd = 5
x <- rnorm(n = 5, mean = 10, sd = 5)
#What are the mean and sd of the sample?
mean(x)
sd(x)
#Mean and sd are close to pop values, but not exactly the same.
#If we take a larger sample, the sample stats should get closer to pop values
y <- rnorm (n = 100000, mean = 10, sd = 5)
mean(y)
sd(y)

####Chapter 6.3: Counting statistics####
#Counting statistics- let's start with two vectors of discrete data
vec <- c(1, 1, 1, 5, 1, 1, 10, 10, 10)
gender <- c("M", "M", "F", "F", "F", "M", "F", "M", "F")
#Function unique() will tell you all the unique values in the vector, but nothing about how often each value occurs
unique(vec)
unique(gender)
#Function table() does same thing, but also tells how often each of the unique values occurs
table(vec)
table(gender)
#If you want to get a table of percentages instead of counts, you can divide result of table function by sum of the result
table(vec) / sum(table(vec))
table(gender) / sum(table(gender))

####Chapter 6.4: Missing (NA) values
a <- c(1, 5, NA, 2, 10)
mean(a)
#To tell a descriptive statistic function to ignore missing values include argument na.rm = TRUE
mean(a, na.rm = TRUE)

####Chapter 6.5: Standardization (z-score)
#Purpose of standardizing a vector is to put it on a common scale to compare it to other variables
#To standardize, simply subtract the vector by its mean, and divide the reult by the sd
#Let's say you have a vector containing some data. Assign to a new object called a, then get mean and sd
a <- c(5, 3, 7, 5, 3, 4)
mean(a)
sd(a)
#Now we create vector a.z, a standardized version of a. Subtract the mean of the vector, then divide by sd
a.z <- (a-mean(a)) /sd(a)
a.z
#The mean of a.z should now be 0 and the sd should now be 1
mean(a.z)
sd(a.z)

#6.5.1 Ex: Evaluating a competition
#Promote a pirate based on rope climbing and grogg drinking
grogg <- c(12, 8, 1, 6, 2)
climbing <- c(100, 520, 430, 200, 700)
#Since scales are very different, we'll use standardization and make new vectors
grogg.z <- (grogg - mean(grogg)) / sd(grogg)
climbing.z <- (climbing - mean(climbing)) / sd(climbing)
#Look at final results
grogg.z
climbing.z
#To find best across both events, create a combined z score to calculate average z score across both events
#Add the two performances and divide by 2
average.z <- (grogg.z + (climbing.z)) / 2
round(average.z, 1)

####Chapter 6.6 Exercises####
sober <- c(2, 0, 3, 1, 0, 3, 5)
drunk <- c(0, 0, 1, 0, 1, 2, 2)
sober.z <- (sober-mean(sober)) / sd(sober)
drunk.z <- (drunk-mean(drunk)) / sd(drunk)
difference <- (sober.z - (drunk.z))
mean(difference)
median(difference)
sd(difference)

####Chapter 7: Indexing vectors with []
#Boat sale-creating the data vectors
boat.names <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
boat.colors <- c("black", "green", "pink", "blue", "blue","green", "green", "yellow", "black", "black")
boat.ages <- c(143, 53, 356, 23, 647, 24, 532, 43, 66, 86)
boat.prices <- c(53, 87, 54, 66, 264, 32, 532, 58, 99, 132)
boat.costs <- c(52, 80, 20, 100, 189, 12, 520, 68, 80, 100)
#What was the price of the first boat?
boat.prices[1]
#What were the ages of the first 5 boats?
boat.ages[1:5]
#What were the names of the black boats?
boat.names[boat.colors=="black"]
#What were the prices of either green or yellow boats?
boat.prices[boat.colors=="green" | boat.colors=="yellow"]
#Change the price of boat "s" to 100
boat.prices[boat.names=="s"] <- 100
#What was the median price of black boats less than 100 years old?
median(boat.prices[boat.colors=="black" & boat.ages < 100])
#How many pink boats were there?
sum(boat.colors=="pink")
#What percent of boats were older than 100 years old?
mean(boat.ages < 100)

####Chapter 7.1: Numerical indexing####
#Let's use numerical indexing to get values from our boat vectors
#What is the first boat's name?
boat.names[1]
#What are the first five boat colors?
boat.colors[1:5]
#What is every second boat age?
boat.ages[seq(1, 5, by = 2)]
#You can use any indexing vector as long as it contains integers. You can even access same elements multiple times
#What is the first boat age (3 times)
boat.ages[c(1, 1, 1)]
#If it makes it clearer, you can define an indexing object before doing actual indexing. 
#Ex: let's define an object called my.index and use this object to index our data vector
my.index <- 3:5
boat.names[my.index]

####Chapter 7.2: Logical indexing#### 
#Let's create some logical vectors
#Which ages are >100?
boat.ages > 100
#Which are equal to 23?
boat.ages == 23
#Which boat names are equal to c?
boat.names == "c"
#You can also create logical vectors by comparing a vector to another vector of the same length
#Which boats had a higher price than cost?
boat.prices > boat.costs
#Which boats had a lower price than cost?
boat.prices < boat.costs
#What were the prices of boats older than 100?
boat.prices [boat.ages > 100]

#7.2.1: &, |, %in%
#Let's create a logical vector indicating which boats had a price greater than 200 OR less than 100
boat.prices > 200 | boat.prices < 100
#what were the names of these boats?
boat.names[boat.prices > 200 | boat.prices < 100]
#You can combine as many logical vectors as you want, as long as they all have the same length
#Boat names of boats with a color of black OR price >100
boat.names [boat.colors == "black" | boat.prices > 100]
#Names of blue boats with a price greater than 200
boat.names [boat.colors == "blue" & boat.prices > 200]
#Which boats were either black or brown AND had a prices less than 100
(boat.colors == "black" | boat.colors == "brown") & boat.prices < 100
#What were the names of these boats?
boat.names[(boat.colors == "black" | boat.colors == "brown") & boat.prices <100]
#The %in% operation allows you to combine multiple OR comparisons quickly. 
#Imagine you have a vector x of people's favorite letters
x <-c("a", "t", "a", "b", "z")
#Let's say you want to create a vector indicating which values are either a or b or c or d
x == "a" | x == "b" | x == "c" | x =="d"
#The %in% operation does this faster, just put it between the original vector and the new vector of possible values. It goes through every value 
#and returns TRUE if it finds it in the vector of possible values or FALSE if it doesn't
x %in% c("a", "b", "c", "d")

#7.2.2: Counts and percentages from logical vectors
#Start with vector x of length 10, with 3 positve and 5 negative numbers
x <- c(1, 2, 3, -5, -5, -5, -5, -5)
#Create a logical vector to see which values are greater than 0
x>0
#Now use sum() and mean() to see how many and what percent are positive
sum(x > 0)
mean(x> 0)

#7.2.3: Additional logical functions
#You can use is.na() to test which values of a vector are missing.Frequent ones:
#is.na(x): Which values in x are NA?
is.na(c(2, NA, 5))
#is.finite(x): which values in x are numbers?
is.finite(c(NA, 89, 0))
#duplicated(x): Which values in x are duplicated?
duplicated(c(1, 4, 1, 2))
#which(x): Which values in x are TRUE
which(c(TRUE, FALSE, TRUE))
#You can also use logical vectors to figure out which values in a vector satisfy some criteria using which() function. It will tell you which are TRUE
#A vector of sex information
sex <- c("m", "m", "f", "m", "f", "f")
#Which values of sex are m?
which(sex == "m")
#Which values of sex are f?
which (sex == "f")

####Chapter 7.3: Changing values of a vector####
#You can change specific values using assignment operation. Just assign a vector of new values to the indexed values of the original vector
#Create a vector a, which contains 10 1s
a <- rep(1, 10)
#Change the first 5 values in the vector to 9s by indexing the first five values, and assigning the value of 9
a[1:5] <- 9
a
#Now change the last five values to 0s. Index the values 6-10 and assign a value of 0
a[6:10] <- 0
a
#You can also change values of a vector using a logical indexing vector. Let's say you have a vector of numbers that should be from 1-10.
#If values are outside of this range, you want to set them to either the min or max value
#x is a vector of numbers that should be from 1-10
x <- c(5, -5, 7, 4, 11, 5, -2)
#Assign values less than one to one
x[x < 1] <- 1
#Assign values greater than ten to ten
x[x > 10] <- 10
x

#7.3.1:Ex: Fixing invalid responses to a Happiness survey
#Let's say you asked 10 people how happy they were on a scale of 1-5
happy <- c(1, 4, 2, 9999, 2, 3, -2, 3, 2, 999)
#There are invalid values (999, -2). Remove them using logical indexing to change the invalid values to NA.
#Create a logical vector indicating which values of happy are invalid using %in%. Because we want to see what's invalid, use ==FALSE condition. 
#Which values of happy are not in the set 1-5?
invalid <- (happy%in% 1:5) == FALSE
invalid
#Now we can index happy with invalid and assign the invalid values as NA
happy[invalid] <- NA
happy
#Convert all values of happy that are NOT integers from 1-5 as NA
happy[(happy %in% 1:5)== FALSE] <- NA
#Include na.rm = TRUE to ignore NA values
mean(happy, na.rm = TRUE)

####Chapter 7.4 Exercises####
movie.names <- c("Whatever Works", "It Follows", "Love and Mercy", "The Goonies", "Jiro Dreams of Sushi", 
                 "There Will Be Blood", "Moon", "Spice World", "Serenity", "Finding Vivian Maier")
movie.year <- c(2009, 2015, 2015, 1985, 2012, 2007, 2009, 1988, 2005, 2014)
movie.boxoffice <- c(35.0, 15.0, 15.0, 62.0, 3.0, 10.0, 321.0, 79.0, 39.0, 1.5)
movie.genre <- c("Comedy", "Horror", "Drama", "Adventure", "Documentary", "Drama", "Science Fiction", "Comedy", "Science Fiction", "Documentary")
movie.time <- c(92, 97, 120, 90, 81, 158, 97, -84, 119, 85)
movie.rating <- c("PG-13", "R", "R", "PG", "G", "R", "R", "PG-13", "PG-13", "Unrated")
#Name the 10th movie
movie.names[10]
#First 4 genres
movie.genre[1:4]
#Replace Spice World with The Naked Gun
movie.names[movie.names == "Spice World"] <- "The Naked Gun"
#Name movies made before 1990
movie.names[movie.year < 1990]
#How many dramas?
sum(movie.genre == "Drama")
#What percent are drama
mean(movie.genre == "Drama")
#One of the values in the time vector is invalid. Convert any invalid values in this vector to NA. Then, calculate the mean movie time
invalid <- (movie.time%in% 1:160) == FALSE
invalid
#Now we can index the movie times with invalid and assign the invalid values as NA
movie.time[invalid] <- NA
movie.time
#Convert all the movie times  that are NOT integers from between 1 and 160 minutes as NA
movie.time[(movie.time %in% 1:160)== FALSE] <- NA
#Include na.rm = TRUE to ignore NA values
mean(movie.time, na.rm = TRUE)
#What were the names of the comedy movies? What were their box office totals?
movie.names[movie.genre =="Comedy"]
movie.boxoffice[movie.genre == "Comedy"]
#What were the names of the movies that made less than 50 million AND were comedies?
movie.names [movie.genre == "Comedy" & movie.boxoffice < 50.0]
#Median box office revenue of movies rated either G or PG
median(movie.boxoffice[movie.rating == "G" | movie.rating == "PG"])
#What percent of movies were rated R OR were comedies?
mean(movie.rating=="R" | movie.genre == "Comedy")

####Chapter 8: Matrices and dataframes####
#Create a dataframe of boat sale data called bsale
bsale <- data.frame(name = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
                    color = c("black", "green", "pink", "blue", "blue", "green",
                              "green", "yellow", "black", "black"),
                    age = c(143, 53, 356, 23, 647, 24, 532, 43, 66, 86),
                    price = c(53, 87, 54, 66, 264, 32, 532, 58, 99, 132),
                    cost = c(52, 80, 20, 100, 189, 12, 520, 68, 80, 100),
                    stringsAsFactors = FALSE)
#Explore bsale dataset
#Show me the first few rows
head(bsale)
#Show me the structure of the data
str(bsale)
#Open the data in a new window
View(bsale)
#What are the names of the columns?
names(bsale)
#How many rows are in the data?
nrow(bsale)
#Calculating statistics from column vectors
#What was the mean age?
mean(bsale$age)
#How many boats were there of each color?
table(bsale$color)
#What was the maximum price?
max(bsale$price)
#Adding new columns
bsale$id <- 1:nrow(bsale)
bsale$age.decades <- bsale$age / 10
bsale$profit <- bsale$price - bsale$cost
#What was the mean price of green boats
with(bsale, mean(price[color == "green"]))
#What were the names of the boats older than 100 years?
with(bsale, name[age > 100])
#What percent of black boats had a positive profit?
with(subset(bsale, color == "black"), mean(profit > 0))
#Save only the price and cost columns in a new dataframe
bsale.2 <- bsale[c("price", "cost")]
#Change the names of the columns to "p" and "c"
names(bsale.2) <- c("p", "c")
#Create a dataframe called old.black.bsale containing only data from black boats older than 50 years
old.black.bsale <- subset(bsale, color == "black" & age >50)

####Chapter 8.2: Creating matrices and dataframes####
##Several common functions, which takes one or more vectors as inputs, and returns a matrix or dataframe
#cbind(a, b, c)--Combine vectors as columns in a matrix
cbind(1:5, 6:10, 11:15)
#rbind(a, b, c)--Combine vectors as rows in a matrix
rbind(1:5, 6:10, 11:15)
#matrix(x, nrow, ncol, byrow)--Create a matrix from a vector x
matrix(x = 1:12, nrow = 3, ncol = 4)
#data.frame()--Create a dataframe from named columns
data.frame("age" = c(19, 21), sex = c("m", "f"))

#8.2.1-cbind(), rbind
#Both create matrices by combining several vectors of the same length. 
#Let's use these functions to create a matrix with the numbers 1-30
#First create three vectors of length 5, then combine into one matrix
x <- 1:5
y <- 6:10
z <- 11:15
#Create a matrix where x, y, and z are columns
cbind(x, y, z)
#Create a matrix where x, y, and z are rows
rbind(x, y, z)

#8.2.2-matrix()
#Matrices can either contain numbers or character vectors, but not both
#Creating a matrix with numeric and character columns will make everything a character
cbind(c(1, 2, 3, 4, 5),
      c("a", "b", "c", "d", "e"))
#The matrix() function creates a matix from a single vector of data
#It has 4 main inputs
#data-a vector of data
#nrow-the number of rows you want in the matrix
#ncol-the number of columns you want in a matrix
#byrow-a logical value indicating whether you want to fill the matrix by rows. 

#Create a matrix of the integers 1:10, with 5 rows and 2 columns
matrix(data = 1:10, nrow = 5, ncol = 2)
#Now with 2 rows and 5 columns
matrix(data = 1:10, nrow = 2, ncol = 5)
#Now with 2 rows and 5 columns, but fill by row instead of columns
matrix(data = 1:10, nrow = 2, ncol = 5, byrow = TRUE)

#8.2.3-dataframe()
#To create a dataframe from vectors, use the data.frame function. It works similarly as cbind, 
#but you specify names to each of the columns as you define them. 
#Let's create a simple dataframe called survey with a mix of text and numeric columns.
survey <- data.frame("index" = c(1, 2, 3, 4, 5), 
                     "sex"= c("m", "m", "m", "f", "f"), 
                     "age" = c(99, 46, 23, 54, 23))

#8.2.3.1-stringasfactors = FALSE
#data.frame() will automatically convert any string columns to a specific type of obect called a factor
#a factor is a nominal variable that has a well specified possible set of values that it can take on
#ex: you can create a factor "sex" that can only take values "male" and "female"
#To tell R NOT to convert your string columns to factors, you need to include the argument stringasfactors=FALSE
#Let's look at the classes of the columns in the dataframe "survey" using the str() function
#Show me the structure of the survey dataframe
str(survey)
#R has converted the column sex to a factor with only two possible levels, which can cause problems
#Let's create the dataframe again using the stringasfactors=FALSE argument
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" =c(99, 46, 23, 54, 23),
                     stringsAsFactors = FALSE)
str(survey)

####Chapter 8.3: Matrix and dataframe functions####
#R has lots of functions for viewing data frames and returning info
#head(x), tail(x)--print the first or last few rows
#View(x)--open entire object in a new window
#nrow(x), ncol(x), dim(x)--count the number of rows and columns
#rownames(), colnames(), names()--show the row or column names
#str(x), summary(x)--show the structure of the dataframe (dimensions and classes) and summary stats

#8.3.1-head(), tail(), View()
#to see the first few rows of a dataframe, use head(), to see the last, use tail()
head(ChickWeight)
tail(ChickWeight)
#View opens the entire dataframe in a new window
View(ChickWeight)

#8.3.2-summary(), str()
#To get summary stats on all columns in a dataframe, use summary() function
summary(ToothGrowth)
#To learn about the classes of columns in a dataframe, use the str() function. 
str(ToothGrowth)

####Chapter 8.4: Dataframe column names####
#In a df, each column will have a name, and you can use these names to access specific columns by name
#To access, use function name(), which will return a string vector with the names of the df
names(ToothGrowth)
#To access a specific column, use the $ operator in the form df$name, which will return the column as a vector
#Return the len column of ToothGrowth
ToothGrowth$len
#Because the $ operator returns a vector, you can easily calculate stats on columns
#What is the mean of the len column of ToothGrowth
mean(ToothGrowth$len)
#Give me a table of the supp column of ToothGrowth
table(ToothGrowth$supp)
#If you want to access several columns by name, you can forgo the $ operator and put a character vector in brackets
#Give me the len and supp columns of ToothGrowth
head(ToothGrowth[c("len", "supp")])

#8.4.1: Adding new columns
#You can add new columns using $ and <-
#Let's create a df called survey with two columns: index and age
survey <- data.frame("index" =c(1, 2, 3, 4, 5), age = c(24, 25, 42, 56, 22))
survey
#Now let's add a column called sex with a vector of sex data
survey$sex <- c("m", "m", "f", "f", "m")
survey

#8.4.2-Changing column names
#To change the name of a column in a df, use a combo of names() function, indexing, and reassignment
#Change name of first column of survey to "participant.number"
names(survey)[1] <- "participant.number"
survey
#WARNING: Change column names with logical indexing to avoid errors
#What if the column you want to change isn't the first column?
#To avoid issues, it's better to change column names using a logical vector using format
#names(df)[names(df) == "old.name] <- "new.name"
#Use logical indexing to change the name of column survey$age to survey$years
names(survey)[names(survey) == "age"] <- "years"
survey

####Chapter 8.5: Slicing dataframes####
#Once you have a dataset stored, you'll want to access different parts based on criteria
#Ex: if your dataset has the results of an experiment comparing different experimental groups,
#you'll want stats for each group separately. This is called slicing

#8.5.1-Slicing with [, ]
#Just like vectors, you can access specific data using brackets
#But now we can use two indexing vectors: one for rows, one for columns
#Use notation data[rows, columns] where rows and columns are vectors of integers
#Try indexing the ToothGrowth df, which has the results of a study testing effectiveness of different
#types of supplements on the length of guinea pig's teeth
#Give me the rows 1-6 and column 1 of TG
ToothGrowth[1:6, 1]
#You can index matrices and df with longer vectors to get more data
#Give me rows 1-3 and columns 1 and 3 of TG
ToothGrowth[1:3, c(1,3)]
#If you want to look at an entire row OR colum, you can leave the corresponding index blank
#Give me the first row and all columns of TG
ToothGrowth[1, ]
#Give me the second column and all rows of TG
ToothGrowth[, 2]
#To get analyses on subsets of data, we'll use subsetting, either indexing with logical vectors or using subset()

#8.5.2-Slicing with logical vectors
#First create a logical vector containing only T and F vales
#Next, index a df using logical vector to return only values for which logical vector is T
#Create a new df with only rows of TG where supp equals VC
ToothGrowth.VC <- ToothGrowth[ToothGrowth$supp == "VC", ]
#Just like with vectors, we can make logical vectors based on multiple criteria-then index based on those
#Create a df with only the rows of TG where supp equals OJ and dose < 1
ToothGrowth.OJ.a <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose < 1, ]

#8.5.3-Slicing with subset()-more elegant
#Main arguments with subset() function:
#x--a df you want to subset
#subset--a logical vector indicating the rows to keep
#select--the columbs you want to keep
#Get rows of TG when len < 20 AND supp == "OJ" AND dose >= 1
subset(x = ToothGrowth,
      subset = len < 20 &
        supp == "OJ" &
        dose >= 1)
#If you just want certain columns, you can just name the columns in the select argument
#Get rows of TG where len > 30 AND supp =="VC", but only return len and dose column
subset(x = ToothGrowth,
       subset = len > 30 & supp == "VC",
       select = c(len, dose))

####Chapter 8.6: Combining slicing with functions####
#Now you can combine slicing and dicing with stat functions to get summary stats on groups of data
#What is the mean tooth length of guinea pigs given OJ?
#Step 1: Create a subsetted df called oj
oj <- subset(x = ToothGrowth, subset = supp == "OJ")
#Step 2: Calculate the mean of the len column from the new subsetted data set
mean(oj$len)
#Can also use logical indexing all in one step:
mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])

#8.6.1-with()
#Helps save time when using multiple columns from df
#Allows to specify a df once at the beginning of a line
#Let's create a df called "health" with some info
health <- data.frame("age" =c(32, 24, 43, 19, 43),
                     "height" =c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight"= c(70, 65, 62, 79, 85))
#Let's say we want to add a column called bmi, where bmi=weight/height(squared)
#Calculate bmi
health$weight / health$height ^ 2
#We had to retype the name of the df for each column. 
#Using with(), we can make it easier by saying name of df once
with(health, height / weight ^ 2)

####Chapter 8 exercises####
#Create the dataframe
pirate.hero <- data.frame("Name" = c("Astrid", "Lea", "Sarina", "Remon", "Letizia", "Babice", 
                                     "Jonas","Wendy", "Niveditha", "Gioia"),
                          "Sex" = c("F", "F", "F", "M", "F", "F", "M", "F", "F", "F"),
                          "Age" = c(30, 25, 25, 29, 22, 22, 35, 19, 32, 21),
                          "Superhero"= c("Batman", "Superman", "Batman", "Spiderman", "Batman",
                                         "Antman", "Batman", "Superman", "Maggott", "Superman"),
                          "Tattoos" = c(11, 15, 12, 5, 65, 3, 9, 13, 900, 0))
#What is the median age of the 10 pirates?
median(pirate.hero$Age)
#What was the mean age of the female and male pirates separately
mean(pirate.hero$Age[pirate.hero$Sex =="F"])
#What was the most number of tattoos owned by a male pirate
max(pirate.hero$Tattoos[pirate.hero$Sex =="M"])
#What percent of pirates under the age of 32 were female?
with(subset(pirate.hero, Age < 32), mean(Sex == "F"))
#What percent of female pirates are under the age of 32?
with(subset(pirate.hero, Sex == "F"), mean(Age < 32))
#Add a new column called tattoos.per.year which shows how many tattoos each pirate has for each year in their life
pirate.hero$tattoos.per.year <- pirate.hero$Tattoos / pirate.hero$Age
#Which pirate had the most number of tattoos?
pirate.hero$Name[pirate.hero$tattoos.per.year == max(pirate.hero$tattoos.per.year)]
#What are the names of the female pirates whose favorite superhero is Superman?
pirate.hero$Name[pirate.hero$Sex =="F" & pirate.hero$Superhero == "Superman"]
#What was the median number of tattoos of pirates over the age of 20 whose favorite superhero is Spiderman?
median(pirate.hero$tattoos[pirate.hero$Age > 20 & pirate.hero$Superhero == "Spiderman"])

####Chapter 9: Importing, saving, and managing data####

#### Chapter 9.1: Workspace Management Functions
#ls()-Displays all objects in the current workspace
#rm(a, b)...-Removes the objects a, b... from your workspace
#rm(list =ls())-Removes ALL objects in your workspace
#getwd()-Returns the current working directory
#setwd(file = "dir)-Changes the working directory to a specified file location
#list.files()-Returns the names of all files in the working directory
#write.table(x, file = "mydata.txt", sep)-Writes the object x to a text file called mydata.txt. Define how the columns will be separated with sep
#save(a, b..., file = "myimage.RData")-Saves objects a, b, ...to myimage.RData
#save.image(file = "myimage.RData")-Saves ALL objects in your workspace to myimage.RData
#load(file = "myimage.RData")-Loads objects in the file myimage.RData
#read.table(file = "mydata.txt", sep, header)-Reads a text file called mydata.txt, define how columns are separated with sep, and whether there is a header column. 

####Chapter 9.2: The Working Directory####
#To see your current working directory, use getwd()
get(wd)
#If you want to change your working directory, use setwd()
setwd(dir = "C:/Users/Danielle.Perez/Documents/yaRrr")

####Chapter 9.4: The workspace####

#9.4.1: ls()
#If you want to see all the objects defined in your current workspace, use ls() function
ls()

####Chapter 9.5: .RData files####
#The best way to store objects from R is with .RDatafiles, which can store as many objects as you want in a single file

#9.5.1: save()
#To save objects into one file, use save() function
#Create some objects that we'll save later
study1.df <- data.frame(id = 1:5, sex =c("m", "m", "f", "f", "m"), score = c(51, 20, 67, 52, 42))
score.by.sex <- aggregate(score ~ sex, FUN = mean, data = study1.df)
study1.htest <- t.test(score ~ sex, data = study1.df)
#Save two objects as a new .RData file in the data folder of my current working directory
save(study1.df, score.by.sex, study1.htest, file ="C:/Users/Danielle.Perez/Documents/yaRrr/data/study1.RData")

#9.5.2: save.image()
#If you have many objects you want to save, use save.image() function.
#Save my workspace to complete_image.RData in the data folder of my wd
save.image(file = "C:/Users/Danielle.Perez/Documents/yaRrr/data/projectimage.RData")

#9.5.3: load()
#To load an .RData file, use load() function
#Load objects in 1.RData into workspace
load(file = "C:/Users/Danielle.Perez/Documents/yaRrr/data/study1.RData" )
#Load objects in projectimage.RData into workspace
load(file = "C:/Users/Danielle.Perez/Documents/yaRrr/data/projectimage.RData" )

#9.5.4: rm()
#To remove objects from workspace, use rm()
#To remove all objects in workspace, enter argument list = ls()

####Chapter 9.6: .txt files####
#Sometimes you'll want to export data as a simple .txt file that other programs (Excel) can read
#Do this using write.table() function

#9.6.1: write.table()
#Arguments for the write.table functon that will save an object (usually a data frame) as a .txt file
#x-the object you want to write to a text file, usually a data frame
#file-the document's file path relative to the working directory, unless specified otherwise
#sep-a string indicating how the columns are separated. For comma separated files, use sep = ","
#row.names-a logical value (TRUE or FALSE) indicating whether or not to save the rownames in the text file
#Following code will save the pirates data frame as a tab delimited text file called pirates.txt in my wd
write.table(x = pirates, file = "pirates.txt", sep = "\t")
#If you want to save a file outside the wd, use entire directory name
#Write pirates dataframe to different folder (example used desktop, but permission denied, probably because not admin)
write.table (x = pirates, file = "C:/Users/Danielle.Perez/Documents/yaRrr/more data/pirates.txt", sep = "\t")

#9.6.2: read.table()
#If you have a .txt file you want to read into R, use read.table() function
#file-document's file path relative to working directory unless otherwise specified
#header-a logical value indicating whether the data has a header row-that is, whether the first row represents column names
#sep-string indicating how columns are separated
#strinAsFactors-logical value indicating whether or not to convert strings to factors

#9.6.3: Reading files directly from a web URL
#A feature of read.table is loading text files from web. Set file path to the document's URL
#Read a text file from the web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P', sep = '\t', header = TRUE)
#Print the result--not sure this actually works anymore...
fromweb

####Chapter 9 Exercises####
#Open a new R project, call the directory MyRProject, then select a directory (this will be project's wd)

####Chapter 10: Advanced dataframe manipulation####
#Exam data
exam <- data.frame (id = 1:5, q1 = c(1, 5, 2, 3, 2), q2 = c(8, 10, 9, 8, 7), q3 = c(3, 7, 4, 6, 4))
#Demographic data
demographics <- data.frame(id = 1:5, sex = c("f", "m", "f", "f", "m"), age = c(25, 22, 24, 19, 23))
#Combine exam and demographics
combined <- merge(x = exam, y = demographics, by = "id")
#Mean q1 score for each sex
aggregate(formula = q1 ~ sex, data = combined, FUN = mean)
#Median q3 score for each sex, but only for those older than 20
aggregate(formula = q3 ~ sex, data = combined, subset = age > 20, FUN = mean)
#Many summary statistics by sex using dplyr
library(dplyr)
combined %>% group_by(sex) %>%
  summarise(q1.mean = mean(q1), q2.mean = mean(q2),q3.mean = mean(q3), age.mean = mean(age), N = n()) 

####Chapter 10.1:Sorting data####
#To sort the rows of a dataframe according to column values, use the order() function
#This takes one or more vectors as arguments, and returns an integer vector indicating the order of vectors
#You can use the output to index a dataframe and change its order.

#Sort the pirates dataframe by height
pirates <- pirates[order(pirates$height),]
#Look at the first few rows and columns of the result
pirates[1:5, 1:4]
#By default, the order() function will sort values in ascending order.
#Sort pirates in descending order
pirates <- pirates[order(pirates$height, decreasing = TRUE),]
#Look at first few rows and columns of result
pirates[1:5, 1:4]
#To order a dataframe by several columns, just add additional arguments
#Sort the pirates dataframe by sex and then height
pirates <- pirates[order(pirates$sex, pirates$height),]
#Sort the pirates dataframe in decreasing order
pirates <- pirates[order(pirates$sex, pirates$height, decreasing = TRUE),]

####Chapter 10.2: Combining data
#One of the most common data management tasks is merging two data sets together. 
#For example, imagine you conduct a study where 5 participants are given a score from 1-5 on a risk assessment test
#Results from a risk survey
risk.survey <- data.frame("participant" = c(1, 2, 3, 4, 5), "risk.score" = c(3, 4, 5, 3, 1))
#Now imagine a second study, where participants scale their happiness from 1-100.
happiness.survey <- data.frame("participant" = c(4, 2, 5, 1, 3), happiness.score = c(20, 40, 50, 90, 53))
#Now we'd like to comine these data into one data frame using merge()
#When you merge 2 data frames, the result is a new dataframe that contains data from both data frames
#Key argument in merge is by, which specifies how rows should be matched during merge (usually name, id number, some identifier)
#Because we want to match rows by participant.id column, we'll specify by="participant.id"
#Because we want to include rows with potentially non-matching values, we'll inclue all = TRUE
#Combine risk and happiness surveys by matching participant.id
combined.survey <- merge(x = risk.survey, y = happiness.survey, by = "participant", all = TRUE)
#Print result
combined.survey

####Chapter 10.3: Grouped aggregation####
#Aggregate allows you to easily answer questions in the form: what is the value of the function FUN applied to a 
#dependent variable dv at each level of one (or more) independent variable(s) iv?
#General structure of aggregate:
aggregate(formula = dv ~ iv, #dv is the data, iv is the group
          FUN = fun, #The function you want to apply,
          data = df) #The dataframe object containing dv and iv)
#We'll use aggregate on the ChickWeight dataset to answer "what is the mean weight for each diet?"
aggregate(formula = weight ~ Diet, #DV is weight, IV is diet
          FUN = mean, #Calculate the mean of each group
          data = ChickWeight) #dataframe is ChickWeight
#Aggregate function has returned a dataframe with a column for the independent variable "diet" and a column for 
#the results of the function "mean" applied to each level of the iv. 
#You can also include a subset argument within an aggregate function to apply the function to subets of the original data.
#Calculate the mean chick weight for each value of Diet, but only when chicks are less than 10 weeks old.
aggregate(formula = weight ~ Diet, #DV is weight, IV is Diet
          FUN = mean, #Calculate the mean of each group
          subset = Time < 10, #Only when chicks are less than 10 weeks old
          data = ChickWeight) #df is ChickWeight
#You can also include multiple independent variables in the formula argument to aggregate(). 
#Calculate the mean weight for each value if Diet and Time, but only when chicks are 0, 2, or 4 weeks old.
aggregate(formula = weight ~ Diet + Time, #DV is weight, IVs are Diet and Time
          FUN = mean, #Calculate the mean of each group
          subset = Time %in% c(0, 2, 4), #Only when chicks are 0, 2, and 4 weeks old
          data = ChickWeight) #dataframe is ChickWeight

####Chapter 10.4: dplyr####
#Allows you to do all kinds of analyses quickly and easily. 
#Especially useful for creating tables of summary stats across specific groups of data
library(dplyr) 
#dplyr works by combining objects (data frames and columns in dataframes), functions (mean, median, etc), and verbs
#(special commands in dplyr). 
#In between commands is operator called pipe: %>%, which tells R that you want to continue executing some functions or verbs
#on the object you're working on. You can think about this pipe as meaning "and then..."
#Template for using dplyr
my.df %>% #Specify original dataframe
  filter(iv3 > 30) %>% #Filter condition
  group_by(iv1, iv2) %>% #Grouping variable(s)
  summarise(
    a = mean(col.a), #Calculate mean of col.a in my.df
    b = sd(col.b), #Calculate sd of column col.b in my.df
    c = max(col.c)) #Calculate max on column col.c in my.df
#When you use dply, you write code that sounds like "the original dataframe is xxx, 
#now filter the dataframe to only include rows that satisfy yyy
#now group the data at each level of the variable(s) zzz, 
#now summarize the data and calculate summary functions xxx
##EXAMPLE TIME
#Let's create a dataframe of aggregated data from the pirates dataset and filter to only include pirates who wear a headband
#Group the data according to the columns sex and college. Create several coumns of different summmary statistic of some
#data across each grouping. To create the aggregated df, use the function group_by and verb summarise. 
#Assign the result to a new dataframe called pirates.agg. 
pirates.agg <- pirates %>%  #Start with pirates dataframe
  filter(headband == "yes") %>% #Only pirates that wear headband
  group_by(sex, college) %>%
  summarise(
    age.mean = mean(age),
    tat.med = median(tattoos), 
    n = n()
  )
#Print result
pirates.agg
#Final object pirates.agg is the aggregated df we wanted which aggregates all the columns for each combo of sex and college.
#New key function is n(), which is specific to dplyr and returns a frequency of values in a summary command

#Let's do a more complex example where we combine multiple verbs into one chunch of code. We'll aggregate data from movies df
movies %>%
  filter(genre != "Horror" & time > 50) %>% #Select only these rows
  group_by(rating, sequel) %>% #Group by rating and sequel
  summarise(
    frequency = n(), #How many movies in each group?
    budget.mean = mean(budget, na.rm = T), #Mean budget?
    revenue.mean = mean(revenue.all), #Mean revenue?
    billion.p = mean(revenue.all > 1000) #Percent of movies with revenue > 1000?
  )
#Result is a dataframe wit 14 rows and 6 columns. Data are summarized from the movie dataframe, only includes values where
#the genre is NOT horror and the movie length is longer than 50 minutes, is grouped by rating and sequel, and shows stats

####Chapter 10.5: Additional aggregation functions####
#There are many methods to achieve the same result, choice often comes down to what you like most

#10.5.1: rowMeans() and colMeans()
#To easily calculate means or sumes across all rows or columns in a matrix or dataframe, use 
#rowMeans(), colMeans(), rowSums(), or colSums()
#For example, imagine we have a datafram representing scores from a quiz with 5 questions, where each row represents
#a student, and each column represents a question. Each value can either be 1 (correct) or 0 (incorrect)
#Some exam scores:
exam <- data.frame("q1" = c(1, 0, 0, 0, 0),
                   "q2" = c(1, 0, 1, 1, 0),
                   "q3" = c(1, 0, 1, 0, 0),
                   "q4" = c(1, 1, 1, 1, 1),
                   "q5" = c(1, 0, 0, 1, 1))
#Let's use rowMeans() to get the average scores for each student
#What percent did each student get correct?
rowMeans(exam)
#Use colmeans() to get the average scores for each question
#What percent of students got each question correct
colMeans(exam)

#10.5.2: apply family
#There's an entire class of "apply" functions that apply functions to groups of data
#tapply, sapply, and lapply work similarly to aggegate
#You can calculate the average length of movies by genre with tapply
with(movies, tapply(X = time, #DV is time
                    INDEX = genre, #IV is genre
                    FUN = mean, #function is mean
                    na.rm = TRUE)) #Ignore missing

####Chapter 10 Exercises####
#load dplyr
library(dplyr)
#Load dataset from web
caffeine <- read.table(file = 'https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt', 
                       sep = '\t', header = TRUE)
View(caffeine)
#Calculate mean age for each gender
aggregate(formula = age ~ gender, 
          FUN = mean, 
          data = caffeine) 
#Calculate the mean age for each drink
aggregate(formula = age ~ drink,
          FUN = mean,
          data = caffeine)
#Calculate the mean age for each combined level of both gender and drink
aggregate(formula = age ~ gender + drink, 
          FUN = mean, 
          data = caffeine) 
#Calculate median score for each age
aggregate(formula = score ~ age,
          FUN = median,
          data = caffeine)

#For men only, calculate the maximum score for each age
aggregate(formula = score ~ age,
          FUN = max,
          data = subset(caffeine, gender == "male")
          )
#Create a dataframe showing, for each level of drink, the mean, median, maximum, and sd of scores
caffeine.agg <- caffeine %>%  
  group_by(drink) %>%
  summarise(
    score.mean = mean(score),
    score.med = median(score), 
    score.max = max(score),
    score.sd = sd(score),
    n = n()
  )
caffeine.agg
#Only for females above age of 20, create a table showing, for each combined level of drink and cups, the mean, median, max
#and sd of scores. Also include a column showing how many people were in each group.
caffeine %>%
  filter(age > 20 & gender == "female") %>% 
  group_by(drink, cups) %>% 
  summarise(
    score.mean = mean(score), 
    score.median = median(score), 
    score.max = max(score),
    score.sd = sd(score),
    N = n()
  )

####Chapter 11: Plotting (I)
#A basic scatterplot
plot(x = 1:10, y = 1:10, xlab = "X Axis Label", ylab = "Y Axis Label", main = "Main Title")

####Chapter 11.1: Colors####

#11.1: Colors by name
#Easiest way to specify a color is to enter its name as a string
#col = "red" is R's default version of red, and R has lots of basic and fun colors
colors()
demo("colors")
 
#11.1.2: gray()
#level-lightness: level = 1 = totally white, level 0 = totally black
#alpha-transparency: aplpha = 0 = totally transparent, alpha = 1 = not transparent at all

#11.1.3: yarrr::transparent()
#Transparent colors can be nicer to look and help when multiple points overlap
#Base R doesn't make it easy to make transparent colors, but there's a function in yarrr package
#Basic scatterplot with standard colors
plot(x = pirates$height, y = pirates$weight, col = "blue", pc = 16, main = "col = 'blue'")
#Now using transparent function
plot(x = pirates$height, y = pirates$weight, col = yarrr::transparent("blue", trans.val = .9), pch = 16, 
     main = "col = yarrr:transparent('blue', .9)")

####Chapter 11.2: Plotting arguments####
#Most plotting functions have lots of optional arguments, or parameters to customize a plot. 
#To see them all:
?par
#Usually only need to specify a couple of things, and R will use a default value for the rest

####Chapter 11.3: Scatterplot: plot()
#plot function makes scatterplot from two vectos
#Argument: x, y-vectors of equal length specifying the x and y values of the points
#Argument: type-type of plot. "l"=lines, "p"=points, "b"=lines and points, "n"=no plotting
#Argument: main, xlab, ylab-strings giving labels for the plot title, x, and y axes
#Argument: xlim, ylim-limits to the axes. Ex: xlim = c(0, 100) will set min and max of x axis to 0 and 100
#Argument: pch-an integer indicating type of plotting symbols or a string specifying symbols as text
?points
#Argument: col-main color of the plotting symbols. 
#Argument: ces-numeric vector specifying symbol size (from 0 to Inf). Default size is 1. 
plot(x = 1:10, y = 1:10, type = "p", main = "My First Plot", 
     xlab = "This is the x-axis label", ylab = "This is the y-axis label", 
     xlim = c(0, 11), ylim = c(0, 11), col = "blue", pch = 16, cex = 1)

#11.3.1: Symbol types: pch
#When using plot(), you can specify the type of symbol with pch argument one of two ways
#If you use a string like "p", R will use that text as the plotting symbol
#If you use an integer value, you'll get the symbol that corresponds to that number

####11.4: Histogram: hist()
#Argument:x-vector of values
#Argument: breaks-how should the bin sizes be calculated? Can be specified many ways, see ?hist for details
#Argument: freq-should frequencies or probabilities be plotted? freq=TRUE shows frequencies, freq=FALSE shows probabilities
#Argument: col, border-colors of the bin filling 
#Create a histogram of the weights in the ChickWeight dataset
hist(x = ChickWeight$weight, main = "Chicken Weights", xlab = "Weight", xlim = c(0, 500))
#Can get fancier by adding additional arguments
hist(x = ChickWeight$weight, main = "Fancy Chicken Weights Histogram", xlab = "Weight", ylab = "Frequency", 
     breaks = 20, xlim = c(0, 500), col = "papayawhip", border = "hotpink")
#If you want to plot two histograms on the same plot to show the distributions of two different groups, you can use
#the argument to the second plot:
hist(x = ChickWeight$weight[ChickWeight$Diet == 1], main = "Two Histograms in One", xlab = "Weight", ylab = "Frequency", 
     breaks = 20, xlim = c(0, 500), col = gray(0, .5))
hist(x = ChickWeight$weight[ChickWeight$Diet == 2], breaks = 30, add = TRUE, col = gray(1, .8))

####Chapter 11.5: Barplot: barplot()####
#Barplot usually shows summary statistics of different groups.
#Primary argument is height:a vector of numeric values which will generate height of each bar
#To add names, use names.arg argument
#For additional arguments, look at help menu with ?barplot
barplot(height = 1:5, names.arg = c("G1", "G2", "G3", "G4", "G5"), 
        main = "Example Barplot", xlab = "Group", ylab = "Height")
#Calculate mean chick weights for each time period
diet.weights <- aggregate(weight ~ Time, data = ChickWeight, FUN = mean)
#Create barplot
barplot(height = diet.weights$weight, names.arg = diet.weights$Time,
        xlab = "Week", ylab = "Average Weight", main = "Average Chicken Weights by Time", col = "mistyrose")

#11.5.1: Clustered barplot
#If you want different bars for different groups of data, you can enter a matrix as the argument to "height"
#R will then plot each column as a separate set of bars
#Let's say there was an expreriment where we compared how fast pirates swim under four conditions: wearing clothes
#vs. being naked, and while being chased by a shark vs. not being chased by a shark:
#No shark, naked = 2.1; no shark, clothed = 1.5; shark, naked = 3.0; shark, clothed = 3.0
swim.data <- cbind(c(2.1, 3), c(1.5, 3))
colnames(swim.data) <- c("Naked", "Clothed")
rownames(swim.data) <- c("No Shark", "Shark")
swim.data
#Now, when we enter this matrix as the height = swim.data argument to barplot(), we'll get multiple bars
barplot(height = swim.data, beside = TRUE, legend.text = TRUE, 
        col = c(transparent("green", .2), transparent("red", .2)),
        main = "Swimming Speed Experiment", ylab = "Speed (in m/s)", xlab = "Clothing Condition", ylim = c(0,4))

####Chapter 11.6: pirateplot()
#Easily shows raw data, descriptive statistics, and inferential statistics in one plot
#Argument: formula-formula specifying a y-axis variable as a function of 1, 2, or 3 x-axis variable
#For example, formula = weight ~ Diet + Time will plot weight as a function of Diet and Time
#Argument: data-a dataframe containing the variables specified in formula
#Argument: theme-plotting theme can be an integer from 1-4. Setting theme = 0 will turn off all elements
#Argument: pal-the color palette. 
#Argument: cap.beans-if cap.beans = TRUE, beans will be cut off at the mx and min data values
#Create a pirateplot of the ChickWeight data. Set the DV to weight, IV to time using argument formula = weight ~ Time
yarrr::pirateplot(formula = weight ~ Time, data = ChickWeight, main = "Pirateplot of chicken weights",
                  xlab = "Diet", ylab = "Weight")

#11.6.1: Pirateplot themes
#Many different pirateplot themes which dictate the overall look of the plot. To specify, use theme = x where x is theme #
#Ex: a pirateplot of height data using theme = 3, plotting pirate heights as a function of sex and whether or not they wear
# a headband. Plot will be grayscale  by using pal= gray argument
yarrr::pirateplot(formula = height ~ sex + headband, data = pirates, theme = 3, main = "Pirate Heights", pal = "gray")

#11.6.2: Customizing pirateplots
#Basically there are a ton, refer back to this section to see them

#11.6.3: Saving output
#If you include plot = FALSE argument, function will return some values associated with each bean in the plot
pirateplot(formula = tattoos ~ sex + headband, data = pirates)
#Save data from the pirateplot to an object
tattoos.pp <- pirateplot(formula = tattoos ~ sex + headband, data = pirates, plot = FALSE)
#Now I from access the summary and inferential stats from the plot in the tattoos.pp object
#Most interesting element is $summary, which shows summary stats from each group
#Show me stats from groups in the pirateplot
tattoos.pp

####Chapter 11.7: Low-level plotting functions####
#These allow you to add elements like points or lines to an existing plot. Common ones:
#points(x, y)-add points
#abline(), segments()-adds lines or segments
#arrows()-adds arrows
#curve()-adds a curve representing a function
#rect(), polygon()-adds a rectangle or arbitrary shape
#text(), mtext()-adds text within the plot, or to plot margins
#legend()-adds a legend
#axis()-adds an axis

#11.7.1:Starting with a blank plot
#Might be useful to start by executing plot() function, but use type = n to keep it blank. Then we can add things
plot(x = 1, xlab = "X Label", ylab = "Y label", xlim = c(0, 100), ylim = c(0, 100), main = "Blank Plotting Canvas", type = "n")

#11.7.2: points()
#Let's use points() to create a plot with different symbol types for diferent data. We'll use the pirates dataset and 
#plot the relationship between age and number of tattoos. We'll create separate points for male and female pirates
#Create blank plot:
plot(x = 1, type = "n", xlim = c(100, 225), ylim = c(30, 110), pch = 16, 
     xlab = "Height", ylab = "Weight", main = "Adding points to a plot with points()")
#Add coral2 points for male data
points(x = pirates$height[pirates$sex == "male"], 
       y = pirates$weight[pirates$sex == "male"],
       pch = 16, col = transparent("coral2", trans.val = .8))
#Add steel blue points for female data
points(x = pirates$height[pirates$sex == "female"],
       y = pirates$weight[pirates$sex == "female"],
       pch = 16, col = transparent("steelblue3", trans.val = .8))

#11.7.3: abline(), segments(), grid()
#Argument: h, v-locations of horizontal and vertical lines (for abline only)
#Argument: x0, y0, x1, y1-starting and ending coordinates of lines (for segments only)
#Argument: lty-line type. 1 = solid, 2 = dashed, 3 = dotted
#Argument: lwd-width of the lines specified by a number. 1 is the default; .2 is very thin, 5 is very thick
#Argument: col-line color
#To add straight lines, us abline (across entire plot) or segments (has defined start and end points)
#Can add reference lines to a plot with abline. 
#We'll ass vertical and horizontal reference lines showing means of the variable on axes 
plot(x = pirates$weight, y = pirates$height, xlab = "weight", ylab = "height", 
     main = "Adding reference lines with abline", pch = 16, col = gray(.5, .2))
#Add horizontal line at mean height
abline(h = mean(pirates$height), lty = 2)
#Add vertical line at mean weight
abline(v = mean(pirates$weight), lty = 2)
#You can also add regression line/line of best fit to a scatterplot by entering a regression object created with lm
plot(x = pirates$height, y = pirates$weight, pch = 16, col = transparent("purple", .7),
     main = "Adding a regression line to a scatterplot()")
#Add the regression line
abline(lm(weight~ height, data = pirates), lty = 2)
#Segments function works similarly, but you specify beginning and end points
#Before and after data
before <- c(2.1, 3.5, 1.8, 4.2, 2.4, 3.9, 2.1, 4.4)
after <- c(7.5, 5.1, 6.9, 3.6, 7.5, 5.2, 6.1, 7.3)
#Create plotting space and before scores
plot(x = rep(1, length(before)), y = before, xlim = c(.5, 2.5), ylim = c(0, 11), 
      ylab = "Score", xlab = "Time", main = "Using segments() to connect points", xact = "n")
#Add after scores
points(x = rep(2, length(after)), y = after)
#Add connection with segments()
segments(x0 = rep(1, length(before)),
         y0 = before,
         x1 = rep(2, length(after)),
         y1 = after, 
         col = gray(0, .5))
#Add labels
mtext(text = c("Before", "After"),
      side = 1, at = c(1, 2), line = 1)
#The grid() function allows you to easily add grid lines to a plot
plot(pirates$age, pirates$beard.length, pch = 16, col = gray(.1, .2), main = "Add grid lines to a plot with grid()")
grid()

#11.7.4: text()
#Argument: x, y-coordinates of the labels
#Argument: labels-labels to be plotted
#Argument: cex-size of the labels
#Argument: adj-horizontal text adjustment. adj = 0 is left justified, adj = .5 is cetnered, and adj = 1 is right justified
#Argument: pos-position of the labels relative to the coordinates. pos = 1 puts label below coordinates
  #2, 3, and 4 put it to the left, top, and right of the coordinates, respectively
#Using text()ou can highlight points of interest or add information
plot(1, xlim = c(0, 10), ylim = c(0, 10), type = "n")
text(x = c(1, 5, 9), y = c(9, 5, 1), labels = c("Put", "text", "here"))     
#Create a scatterplot of data and add labels above each point by including pos = 3 argument
#Create data vectors
height <- c(156, 175, 160, 172, 159, 165, 178)
weight <- c(65, 74, 69, 72, 66, 75, 75)
id <- c("andrew", "aeidi", "becki", "madisen", "david", "vincent", "jack")
#Plot data
plot(x = height, y = weight, xlim = c(155, 180), ylim = c(65, 80), pch = 16, col = yarrr::piratepal("xmen"))
#Add id labels
text(x = height, y = weight, labels = id, pos = 3)
#If you are adding a long text string (like a sentence), may want to separate the text into separate lines
#To do this, add text \n where you want new lines to start
plot(1, type = "n", main = "The \\n tag", xlab = "", ylab = "")
#Text withoutbreaks
text(x = 1, y = 1.3, labels = "Text without \\n", font = 2)
text(x = 1, y = 1.2, labels = "Haikus are easy. But sometimes they don't make sense. Refrigerator", font = 3)
abline(h = 1, lty = 2)
#Text with breaks
text(x = 1, y = .92, labels = "Text with \\n", font = 2)
text(x = 1, y = .7, labels = "Haikus are easy\nBut sometimes they don't make sense\nRefrigerator", font = 3)

#11.7.5: Combinin text and numbers with paste()
#Helpful anytime you want to combine multiple strings, or text and strings together
#For example, you want to write text in a plot that says "the mean of these data are xxx"
#Create a plot
plot(x = ChickWeight$Time, y = ChickWeight$weight, 
     col = gray(.3, .5), pch = 16, main = "Combining text with numeric scalers using paste()")
#Add reference line
abline(h = mean(ChickWeight$weight), lty = 2)
#Add text
text(x = 3, y = mean(ChickWeight$weight), labels = paste("Mean weight = ", round(mean(ChickWeight$weight), 2)), pos = 3)

#11.7.6: curve()
#Allows you to add a line showing a specific function or equation to a plot
#Plot function x^2 from -10 to +10
curve(expr = x^2, from = -10, to = 10, lwd = 2)
#If you want to add custom function, you can define it and then use its name as the argume to expr
#Plot the normal distribtion with mean = 22 and sd = 3
#Create a function
my.fun <- function(x) {dnorm(x, mean = 2, sd = 3)}
curve(expr = my.fun, from = -10, to = 10, lwd = 2)

#11.7.7: legend()
#Add legend to the bottom right of a plot
legend("bottomright", legend = c("Females", "Males"), col = c("blue", "orange"), pch = c(16, 16))
#Create a plot with data from females
plot(x = pirates$age[pirates$sex == "female"],
     y = pirates$tattoos[pirates$sex == "female"],
     xlim = c(0, 50), ylim = c(0, 20), pch = 16, col = yarrr::transparent("red", .7),
     xlab = "Age", ylab = "Tattoos", main = "Add a legend with legend()")
#Add data from males
points(x = pirates$age[pirates$sex == "male"],
       y = pirates$tattoos[pirates$sex == "male"],
       pch = 16, col = yarrr::transparent("blue", .7))
#Add legend
legend("bottomright", legend = c("Females", "Males"), 
       col = transparent(c('red', 'blue'), .5), pch = c(16, 16), bg = "white")

####Chapter 11.8: Saving plots to a file with pdf(), jpeg(), and png()####
#Three steps in creating a pdf
#Step 1: call the pdf command to start the plot
pdf(file = "C:/Users/Danielle.Perez/Documents/yaRrr/My Plot.pdf", width = 4, height = 4)
#Step 2: Create the plot with R code
plot(x = 1:10, y = 1:10)
abline(v = 0)
text(x = 0, y = 1, labels = "Random text")
#Step 3: Run dev.off() to create the file
dev.off()
#Functions for others work the same way

####Chapter 11.9: Examples####
#Turn a boring scatterplot into a baloonplot
#Create some random correlated data
x <- rnorm(50, mean = 50, sd = 50)
y <- x + rnorm(50, mean = 20, sd = 8)
#Set up plotting space
plot(1, bty = "n", xlim = c(0, 100), ylim = c(0, 100), type = "n", xlab = "", ylab = "", 
     main = "Turning a scatterplot into a balloon plot!")
#Add gridlines
grid()
#Add strings with segmentsy
segments(x0 = x +rnorm(length(x), mean = 0, sd = .5), 
         y0 = y -10, x1 = x, y1 = y, col = gray(.1, .95), lwd = .5)
#Add balloons
points(x, y, cex = 2, pch = 21, col = "white", bg = yarrr::piratepal("basel"))

#You can use colors and point sizes to represent third variables. Plot the relationship between pirate height and weight
#But make the size and color of each point reflect how many tattoos the pirate has
#Just the first 100 pirates
pirates.r <- pirates[1:100,]
plot(x = pirates.r$height, y = pirates.r$weight, xlab = "height", ylab = "weight", 
     main = "Specifying point sizes and colors with a third variable",
     cex = pirates.r$tattoos / 8,
     col = gray(1-pirates.r$tattoos /20))
grid()

####Chapter 11 Exercises####
#Beardlengths dataframe has data on the lengths of bears from 3 different pirate ships.
#Calculate average beard length for each ship using aggregate(), then recreate the barplot
library("yarrr")
View(BeardLengths)
avg.lengths <- aggregate(Beard ~ Ship, data = BeardLengths, FUN = mean)
barplot(avg.lengths$Beard, names.arg = avg.lengths$Ship, 
        ylab = "Beard Length",
        xlab = "Ship",
        main = "Barplot of mean beard lengths by Ship",
        ylim = c(0, 25))
#Now using BeardLengths data frame, recreate pirateplot
pirateplot(formula = Beard ~ Ship, data = BeardLengths, main = "Pirateplot of beard lengths by ship")
#Using pirates dataset, recreate scatterplot showing relationship between a pirate's age and how many parrots they own
plot(x = pirates$age, y = pirates$parrots, pch = 16, col = gray(level = .5, alpha = .1),
     xlab = "Age", ylab = "Parrots", main = "Pirate age and number of parrots owned")

####Chapter 12: Plotting (II)####

####Chapter 12.1: More colors####

#12.1.1: Piratepal()
#yarrr package come with several color palettes contained in piratepal() function. To see them all:
yarrr::piratepal("all")
#To see a palette in detail, include the name of the palette in the first argument and then specify plot.result = TRUE
#Show me basel palette
yarrr::piratepal("info2", plot.result = TRUE, trans = .1)
#Once you find a palette you like, you can save colors as a vector and assign the result to an object.
#Save the brave palette to a vector
brave.cols <- piratepal(palette = "brave", trans = .2)
#Create a barplot with the brave colors
barplot(height = 1:5, col = brave.cols, border = "white", main = "Barplot with the Brave palette")

#12.1.2: RColorBrewer
#Package that is great for getting or creating palettes
library("RColorBrewer")
display.brewer.all()

#12.1.3: colorRamp2
#good way to generate colors that represent numerical data is colorRamp2 function in circlize package
#For example: you want to plot data showing relationship between # of drinks/week and resulting adverse effects
#You want to color the points as a function of the # of packs of cigarettes/week a person smokes
#0 packs = blue, 10 packs = orange, 30 packs = red
#You want the alues in between break points of 0, 10, and 30 to be a mix of colors (value of 5 would be mix of orange and blue)
#When you run the function, it will return another function that you can then use to generate colors
#Once you storethe resulting function as an object, you can apply it on numerical data to get correct color for each point
#Let's create color ramp function for smoking data points
#Create a function which takes a number as an argument and returns corresponding color
#Create color function from colorRamp2
smoking.colors <- circlize::colorRamp2(breaks = c(0, 15, 25), 
                                       colors = c("blue", "green", "red"), transparency = .2)
plot(1, xlim = c(-.5, 31.5), ylim = c(0, .3), 
     type = "n", xlab = "Cigarette Packs", 
     yaxt = "n", ylab = "", bty = "n",
     main = "colorRamp2 Example")
segments(x0 = c(0, 15, 30), y0 = rep(0, 3), x1 = c(0, 15, 30), y1 = rep(.1, 3), lty = 2)
points(x = 0:30, y = rep(.1, 31), pch = 16, col = smoking.colors(0:30))
text(x = c(0, 15, 30), y = rep(.2, 3), labels = c("Blue", "Green", "Red"))
#Create Data
drinks <- round(rnorm(100, mean = 10, sd = 4), 2) 
smokes <- drinks + rnorm(100, mean = 5, sd = 2)
risk <- 1/ (1+exp(-(drinks + smokes) / 20 +rnorm(100, mean = 0, sd = 1)))
#Create color function from colorRamp
smoking.colors <- circlize::colorRamp2(breaks = c(0, 15, 30), colors = c("blue", "green", "red"), transparency = .3)
#Bottom plot
par(mar = c(4, 4, 5, 1))
plot(x = drinks, y = risk, 
     col = smoking.colors(smokes), 
     pch = 16, cex = 1.2, main = "Plot of (Made-up Data)", 
     xlab ="Drinks", ylab = "Risk")
mtext(text = "Point color indicates smoking rate", line = .5, side = 3)

#12.4.1: Getting colors with a kuler
#Tool that allows you to determine exact RGB values for a color on a screen
#Say you want to use the exact colors in the google logo-you need an app that picks colors off a screen 
#Using rgb() function, you can convert RGB values to colors
#Store the colors of google as a vector
google.col <- c(
  rgb(19, 72, 206, maxColorValue = 255), #Google blue
  rgb(206, 45, 35, maxColorValue = 255), #Google red
  rgb(253, 172, 10, maxColorValue = 255), #Google yellow
  rgb(18, 140, 70, maxColorValue = 255)) #Google green
#Print result
google.col
#Vector google.col now contains these values, which are string values that represents colors in a way R understands
#Now I can use these colors in a plot by specifying col = google.col
plot(1, xlim = c(0, 7), ylim = c(0, 1), type = "n", main = "Using stolen colors from a webpage")
points(x = 1:6, y = rep(.4, 6), pch = 16, col = google.col[c(1, 2, 3, 1, 4, 2)], cex = 4)
text(x = 1:6, y = rep(.7, 6), labels = c("G", "O", "O", "G", "L", "E"), 
     col = google.col[c(1, 2, 3, 1, 4, 2)], cex = 3)

####Chapter 12.2: Plot margins####
#All plots in R have margins surrounding them that separate the main plotting space from the other areas
#You can adjust margin size by specifying margin parameter, but you must do so before creating the plot
#Let's create two plots with different margins. 
#First plot with small margins
par(mar = c(2, 2, 2, 2)) #Set margins on all sides to 2
plot(1:10)
mtext("Small Margins", side = 3, line = 1, cex = 1.2)
mtext("par(mar = c(2, 2, 2, 2))", side = 3)
#Second plot with large margins
par(mar = c(5, 5, 5, 5)) #Set margins on all sides to 6
plot(1:10)
mtext("Large Margins", side = 3, line = 1, cex = 1.2)
mtext("par(mar = c(5, 5, 5, 5))", side = 3)
#Can also use mai instead of mar, which sets the margins in inches

####Chapter 12.3: Arranging plots with par(mfrow) and layout()####
#R makes it easy to arrange multiple plots in same plotting space
#mfrow and mfcol parameters allow you to createa matrix of plots in same space
#Both parameters take a vector of length two as an argument, corresponding to the number of rows and columns
#Following code sets up 3x3 plotting matrix
par(mfrow = c(2, 2)) #Create a 2 x 2 plotting matrix
#Next 4 plots will be plotted next to each other
#Plot 1
hist(rnorm(100))
#Plot 2
plot(pirates$weight, pirates$height, pch = 16, col = gray(.3, 01))
#Plot 3
pirateplot(weight ~ Diet, data = ChickWeight, pal = "info", theme = 3)
#Plot 4
boxplot(weight ~ Diet, data = ChickWeight)
#Only difference is par(mfrow) puts sequential plots in plotting matrix by row, par(mfcol) will fill them by column
#When finished using a plotting matrix, rest the plotting parameter back to default state
par(mfrow = c(1, 1))

#12.3.1: Complex plot layouts with layout()
#In order to arrange plots in different sized plotting spaces, need to use the layout() function
#Not a plotting parameter, but a function all on its 
#Let's say you want to place histograms next to a scatterplot
#Begin by creating the layout matrix
layout.matrix <-matrix(c(0, 2, 3, 1), nrow = 2, ncol = 2)
layout.matrix
#You can see we've told R to put the first plot in the bottom right, the second plot on the bottom left, third plot in top
#right. Because we put a 0 in the first element, R knows that we don't plan to put anything in the top left. 
#Because our layout matrix has 2 rows and 2 columns, need to set the widths and heights of the 2 columns
#Do this using a numeric vector of length 2. Set the heights of the two rows to 1 and 2 and widths of columns to 1 and 2
layout.matrix <- matrix(c(2, 1, 0, 3), nrow = 2, ncol = 2)
layout(mat = layout.matrix, heights = c(1, 2), #Heights of the two rows
       widths = c(2, 2)) #Widths of the two columns
layout.show(3)
#Now we're ready to put the plots together
#Set plot layout
layout(mat = matrix(c(2, 1, 0, 3), nrow = 2, ncol = 2),
       heights = c(1, 2), #Heights of the 2 columns
       widths = c(2, 1)) #Widths of the two columns
#Plot 1: Scatterplot
par(mar = c(5, 4, 0, 0))
plot(x = pirates$height, y = pirates$weight, 
     xlab = "height", ylab = "weight",
     pch = 16, col = yarrr::piratepal("pony", trans = .7))
#Plot 2: Top (height) boxplot
par(mar = c(0, 4, 0, 0))
boxplot(pirates$height, xaxt = "n", yaxt = "n", bty = "n", yaxt = "n", col = "white", frame = FALSE, horizontal = TRUE)
#Plot 3: Right(weight) boxplot
par(mar = c(5, 0, 0, 0))
boxplot(pirates$weight, xaxt = "n", yaxt = "n", bty = "n", yaxt = "n", col = "white", frame = F)

####Chapter 12.4: Additional plotting parameters####
#To change background color of a plot, add command par(bg = col) prior to creating the plot.
#Following will put light gray background behind a histogram
par(bg = gray(.9))
hist(x = rnorm(100), col = "skyblue")
#More complex example
parrot.data <- data.frame(
  "ship" = c("Drunken\nMonkeys", "Slippery\nSnails", "Don't Ask\nDon'tTell", "The Beliebers"),
  "Green" = c(200, 150, 100, 175), 
  "Blue" = c(150, 125, 180, 242))
#Set background color and plot margins
par(bg = rgb(61, 55, 72, maxColorValue = 255), mar = c(6, 6, 4, 3))
plot(1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", bty = "n", type = "n", ylim = c(0,250), xlim = c(.25, 5.25))
#Add gridlines
abline(h = seq(0, 250, 50), lty = 3, col = gray(.95), lwd = 1)
#yaxis labels
mtext(text = seq(50, 250, 50), side = 2, at = seq(50, 250, 50), las = 1, line = 1, col = gray(.95))
#ship labels
mtext(text = parrot.data$ship, side = 1, at = 1:4, las = 1, line = 1, col = gray(.95))
#Blue bars
rect(xleft = 1:4 - .35 - .04 / 2,
     ybottom = rep(0, 4), 
     xright = 1:4 -.04 / 2,
     ytop = parrot.data$Blue, col = "lightskyblue1", border = NA)
#Green bars
rect(xleft = 1:4 + .04 / 2,
     ybottom = rep(0, 4), 
     xright = 1:4 +.35 + .04 / 2,
     ytop = parrot.data$Green,
     col = "lightgreen", border = NA)
legend(4.5, 250, c("Blue", "Green"),
       col = c("lightskyblue1", "lightgreen"), pch = rep(15, 2), 
       bty = "n", pt.cex = 1.5, text.col = "white")
#Additional margin text
mtext("Number of Green and Blue Parrots on 4 ships",
      side = 3, cex = 1.5, col = "white")
mtext("Parrots", side = 2, col = "White", line = 3.5)
mtext("Source: Drunken survey on 22 May 2015", side = 1, 
      at = 0, adj = 0, line = 3, font = 3, col = "white")

####Chapter 13: Hypothesis tests####
#We'll cover 1 and 2 sample null hypothesis tests: like the t-test, correlation test,
#and chi-squared test.
#load library to get pirates data
library(yarrr)
#1 sample test
#Are pirates ages different than 30 on average?
t.test(x = pirates$age, mu = 30)
#2 sample t-test
#Do females and males have different numbers of tattoos?
sex.ttest <- t.test(formula = tattoos ~ sex, data = pirates, 
                    subset = sex %in% c("male", "female"))
#Print result
sex.ttest
#Acess specific values from test
sex.ttest$statistic
sex.ttest$p.value
sex.ttest$conf.int
#Correlation test
#Is there a relationship between age and height?
cor.test(formula = ~age + height, data = pirates)
#Chi-square test
#Is there a relationship between college and favorite pirate?
chisq.test(x = pirates$college, y = pirates$favorite.pirate)

####Chapter 13.1: A short into to hypothesis tests
#Conducted a survey and asked 10 American and 10 European pirates how many body 
#piercings they had. 
#Body piercing data
american.bp <- c(3, 5, 2, 1, 4, 4, 6, 3, 5, 4)
european.bp <- c(6, 5, 7, 7, 6, 3, 4, 6, 5, 4)
#Store data in a dataframe
bp.survey <- data.frame("bp" = c(american.bp, european.bp), 
                        "group" = rep(c("American", "European"), each = 10),
                        stringsAsFactors = FALSE)
yarrr::pirateplot(bp ~ group, data = bp.survey,
                  main = "Body Piercing Survey", 
                  ylab = "Number of Body Piercings", xlab = "Group", 
                  theme = 2, point.o = .8, cap.beans = TRUE)

#13.1.1: Null v Alternative Hypothesis
#In null hypothesis test, always start with null hypothesis
#Specific hypothesis will depend on type of question you're asking
#In general, states: nothing is going on and everything is the same
#For example, in body piercing study, null hypothesis is that 
#American and European pirates have the same number of body piercings, on average.

#Alternative hypothesis is opposite of null hypothesis.
#Ie, American and European pirates do NOT have the same number of piercings on average.
#Can have different types of alternative hypotheses depending on how specific you want
#to be about your prediction.
#Can make a 1-sided (or 1 tailed) hypothesis by predicting direction of the difference
#between American and European pirates.
#Ex: European pirates have more piercinfs than American pirates
#Could make 2-sided (2 tailed) alternative hypothesis that American and European
#pirates simply differ in average number of piercings, without stating which group
#has more than the other
#Once null and alternative hypotheses are stated, calculate descriptive statistics.

#13.1.2: Descriptive statistics
#Descriptive stats (or sample stats) describe samples of data
#Ex: a mean, median or sd of a dataset is a descriptive stat of that dataset
#Let's calculate some descriptive stats on body piercing survey using summary function
#Print descriptive statistics of the piercing data
summary(american.bp)
summary(european.bp)
#Sample of 10 American pirates had 3.7 piercings on average, sample of 10 European
#pirates had 5.3 piercings on average. Is this large or small? Are we justified
#in concluding that American and European pirates in general differ in how many
#piercings they have? To answer, need to calculate test statistic.

#13.1.3: Test Statistics
#Test statistic compares descriptive statistics and determines how different they are
#Formula to calculate test stats depends on type of test (which depends on other stuff)
#Our data calls for a two sample t-test
#Conduct two sided t-test comparing vectors and save results
bp.test <- t.test(x = american.bp, y = european.bp, alternative = "two.sided")
#Print the main results
bp.test
#Test statistic is -2.52. If there was no difference, we'd see something close to 0
#In order to make decidion, need to get the p-value

#13.1.4: p-value
#p-value is a probability that reflects how consistent the test stat is with the 
#hypothesis that the groups are actually the same. OR
#Definition of p-value: assuming that the null hypothesis is true, what is the 
#probability that we would have gotten a test statistic as far away from 0 as the
#one we actually got?
#What is the p-value from our t-test?
bp.test$p.value
#We got p-value of .02, which means that, assuming the 2 populations have the same 
#number of piercings on average, the probability that we would get a test stat as large
#as -2.52 is 2.1%. That is small, but is it small enough to decide that the null hypothesis
#is not true? No definitive answer, but most use a threshold of p < .05 to determine
#if we should reject null hypothesis or not. 
#In other words: if p-value < .05, reject null hypothesis. Because ours is .02, we
#WOULD reject null hypothesis and conclude two populations are NOT the same.

#13.1.4.2: p-values are bullshit detectors against null hypothesis
#Goes off when p-values are too small
#If p-value is too small, detector goes off and says "Bullshit! No way you would get
#data like that if the groups were the same.
#If not too small, alarm stays silent, we conclude we can't reject null hypothesis

#13.1.4.3: How small a p-value is too small?
#Traditionally use p -value of 0.05 (or sometimes 0.01) to determine "statistical significance"
#Not a magic number (.06 not that different from .04)
#Arbitrary number, but used to stay consistent

#13.1.4.4: Does the p-value tell us the probability that the null hypothesis is true?
#NO. In other words, if you calculate a p-value of 0.04, this does not mean the
#probability that the null hypothesis is true is 4%.
#Rather, it means that *if the null hypothesis was true* the probability of getting
#the result you got was 4%

####Chapter 13.2: Hypothesis test objects: htest####
#R stores hypothesis tests in special object classes called htest
#htest objects contain all the major results from a hypothesis test, from the test
#statistic, to the p-value, to a confidence interval. 
#Let's create an htest object called height.htest with the results from a two sample 
#t-test comparing the heights of male and female pirates. 
height.htest <- t.test(formula = height~sex, data = pirates, 
                       subset = sex %in% c("male", "female"))
#Once you've created an htest object, you can view a print-out of the main resulsts by
#just evaluatin the objectname.
height.htest
#Just like in dataframes, you can also access specific elements of the htest objec by
#using the $. To see all the named elements in the object, run names()
#Show me all the elements in the height.htest object
names(height.htest)
#If we want to access the test statistic or p-value directly, we can use $
#Get the test statistic
height.htest$statistic
#Get the p-value
height.htest$p.value
#Get a confidence interval for the mean
height.htest$conf.int

####Chapter 13.3: T-test: t.test()####
#To compare the mean of 1 group to a specific value, or to compare the means of 
#2 groups, you do a t-test function, it can take several arguments.

#13.3.1: 1-sample t-test
#In a one-sample t-test, you compare the data from one group of data to some
#hypothesized mean. For example, if someone said that pirates on average have 5 
#tattoos, we could conduct a one-sample test comparing the data from a sample of
#pirates to a hypothesized mean of 5. 
#To conduct a one-sample t-test, enter a vector as the main argument x, and the null
#hypothesis as the argument mu. 
#We'll conduct a t-test to see if the average number of tattoos a pirate has is different than 5
tattoo.ttest <- t.test(x = pirates$tattoos, mu = 5)
#Print the result
tattoo.ttest
#Lots of info: mean was 9.43, test stat was 41.59, p-value 2e-16(basically 0)
#Because 2e-16 is less than .05, we would reject null hypothesis that true mean is 5.
#What happens if we change the null hypothesis to a mean of 9.4?
#Because the sample mean was 9.43(close to 9.4), the test stat should decrease
# and the p-value should increase.
tattoo.ttest <- t.test(x = pirates$tattoos, mu = 9.5)
tattoo.ttest
#Test stat decreased to just -0.67, p-value increased to 0.51. In other words, our 
#sample mean of 9.43 is reasonably consistent with hypothesis that the true 
#population mean is 9.5

#13.3.2: 2 Sample t-test
#in a two-sample t-test, you compare the means of two groups of data and test whether
#or not they are the same. We can specify two-sample t-tests in one of two ways. 
#If the dv and iv are in a dataframe, we can use the formula notation in form
#y ~ x, and specify the dataset. 
#Formulation of a two-sample t-test
#Method 1: Formula
t.test(formula = y ~ x, data = df)
#Method 2: Vector
t.test(x = x, y = y)
#For example, let's test a prediction that pirates who wear eye patched have fewer
#tattoos on average than those who don't wear eye patches. Because the data are in the
#pirates dataframe, we can do this using formula method.
#2-sample t-test, iv = eyepatch (0 or 1), dv = tattoos
tat.patch.htest <- t.test(formula = tattoos ~ eyepatch, data = pirates)
tat.patch.htest
#Because the p-value is greater than 0.05 (it's 0.221), we can't reject null hyp.
#Show me all the elements in the htest object
names(tat.patch.htest)
#We can access the confidence interval for the mean difference using $
tat.patch.htest$conf.int

#Chapter 13.3.2.1: Using subset to select levels of an IV
#If your independent variable has more than 2 values, the t.test function will return
#an error because it doesn't know whith 2 groups you want to compare
#Let's say we want to compare the number of tattoos of pirates of different ages
#Will return an error because there are more than 2 levels of the age IV
t.test(formula = tattoos ~ age, data = pirates)
#To fix it, need to tell function which two values of age we want to test
#Use subset argument and indicate which values of the IV want to test using %in%
#Compare the tattoos of pirates aged 29 and 30
tatage.htest <- t.test(formula = tattoos ~age, data = pirates, 
                       subset = age %in% c(29, 30))
tatage.htest
#Got a p-value of 0.79, which suggests we should fail to reject null hypothesis
#Can select any subset of data, not just primary independent variables
#Let's say we want to compare number of tattoos between pirates who wear headbands
#or not, but only female pirates
#Is there an effect of college on # of tattoos, only for female pirates
t.test(formula = tattoos ~ college, data = pirates, subset =sex == "female")

####Chapter 13.4: Correlation: cor.test()####
#Next we'll cover two sample correlation tests
#In a correlation test, you're assessing the relationship between two variables on
#a ratio or interval scale: like height and weight or income and beard length
#Test statistic in a correlation test is called a correlation coefficient and is
#represented by letter r. Coefficient can range from -1 to +1
#-1 = strong negative relationship, 0 = no relationship/null hypothesis
#+1 = strong positive relationship
#To run a correlation test between two variables, use cor.test function
#Can do it two ways
#If x and y are columns in a data frame: formula = ~ x + y
#If x and y are separate vectors not in df, use vector notation (x, y)
#Correlation test-correlating two variable x and 
#Method 1: Formula notation
cor.test(formula = ~ x + y, data = df)
#Method 2: Vector notation
cor.test(x = x, y = y)
#Let's conduct correlation test on pirates dataset to see if there's a relationship
#between a pirate's age and number of parrot's they've had in their lifetime 
#Method 1
age.parrots.ctest <- cor.test(formula = ~ age + parrots, data = pirates)
age.parrots.ctest
#Method 2
age.parrots.ctest <- cor.test(x = pirates$age, y = pirates$parrots)
age.parrots.ctest
#Positive correlation of 0.19 and a very small p-value
#To see info we can extract for this test, run command names()
names(age.parrots.ctest)
#Let's look at the confidence interval for the population correlation coefficient
age.parrots.ctest$conf.int
#Just like t.test, we can use subset argument in the cor.test function to conduct
#a test on a subset of an entire df
#Is there a correlation between age and number of parrots only for female pirates?
cor.test(formula = ~age + parrots, data = pirates,
         subset = sex == "female")
#Results pretty much identical. Strength of the relationship between a pirate's age and
#the number parrots they've owned is pretty much the same for female pirates and 
#pirates in general.

####Chapter 13.5: Chi-square: chsq.test()####
#Test whether or nor there is a difference in the rates of outcomes on a nominal scale
#Test statistic is x^2 and can range from 0 to 
#Null hypothesis is that x^2 = 0 which means no relationship
#Key difference between chisq.test and other hypothesis tests is that chisq.test 
#requires a table created using the table() function as its main argument. 

#13.5.0.1: 1-sample Chi-square test
#If you conduct a 1 sample chi-square test, you are testing if there is a difference
#in the number of members of each category in the vector
#In other words, are all category memberships equally prevalent? General form:
chisq.test(x = table(x))
#Let's conduct a chi-square test to see if all pirate colleges are equally prevalent
#in the pirates data. We'll start by creating a table of the college data
#Frequency table of pirate colleges
table(pirates$college)
#Are all colleges equally prevalent?
college.ctest <- chisq.test(x = table(pirates$college))
college.ctest
#Test stat of 99.86 and tiny p-value, we can reject the null hypothesis, certain
#colleges are more popular than others

#13.5.0.2: 2-sa#If you want to see if the frequency of one nominal variable depends 
#on a second nominal variable, you'd do a 2-sample chi-square test
#For ex, we might want to know if there is a relationship between the college
#a pirate went to, and whether or not they wear an eyepatch
#Do pirates that wear eyepatches come from different colleges than those that don't?
table(pirates$eyepatch, pirates$college)
#Is there a relationship between a pirate's college and whether or not they wear
#an eyepatch
colpatch.cstest <- chisq.test(x = table(pirates$college, pirates$eyepatch))
#x^2 = 0, pvalue = 1, fail to reject null hypothesis, not enough info to determine 
#if pirates from different colleges differ in how likely they are to wear eyepatches

#13.5.1: Getting APA style conclusions with the apa function
#APA rules specify exactly how to report the results of the most common tests
#You can read values directly from test result, but can just use apa function
#Following 2 sample t-test on the pirates dataset that compares whether or not there 
#is a significant age difference between pirates who wear headbands and those who don't
test.result <- t.test(age ~ headband, data = pirates)
test.result
#Test stat is 0.35, degrees of freedom in 135.47, p-value is 0.73. 
#Let's see how the apa function gets these values directly from test object
yarrr::apa(test.result)
#Let's see how it works on a correlation test where we correlate a pirate's 
#age with the number of parrot they've owned
#Print an apa style conclusion of the correlation between a pirate's age & #parrots
age.parrots.ctest <- cor.test(formula = ~ age + parrots, data = pirates)
age.parrots.ctest
#Print apa style conclusion!
yarrr::apa(age.parrots.ctest)

####Chapter 13 exercises####
#Do male pirates have significantly longer beards than female pirates? 
#Test this by conducting a t-test on the relevant data in the pirates dataset. 
beard.sex.htest <- t.test(formula = beard.length ~ sex,
                          subset = sex %in% c("male", "female"), 
                          data = pirates)
beard.sex.htest

#Are pirates whose favorite pixar movie is Up more or less likely to wear an eye patch 
#than those whose favorite pixar movie is Inside Out? 
#Test this by conducting a chi-square test on the relevant data in the pirates dataset. 
df <-subset(pirates, fav.pixar %in% c("Up", "Inside Out"))
pixar.ep.table <- table(df$fav.pixar, df$eyepatch)
pixar.ep.htest <- chisq.test(pixar.ep.table)
pixar.ep.htest
yarrr::apa(pixar.ep.htest)

#Do longer movies have significantly higher budgets than shorter movies?
#Conduct a correlation test on appropriate data in movies dataset
library(yarrr)
budget.time.htest <- cor.test(formula = ~ budget + time, data = movies)
budget.time.htest

#Do R rated movies earn significantly more money than PG-13 movies?
#Conduct a t-test on relevant data in movies dataset
revenue.rating.htest <- t.test(formula = revenue.all ~ rating, 
                               subset = rating %in% c("R", "PG-13"), data = movies)
revenue.rating.htest

#Are certain movie genres significantly more common than others in the dataset?
#Conduct a 1 sample chi-square test on relevant data
genre.table <- table(movies$genre)
genre.htest <- chisq.test(genre.table)
genre.htest
apa(genre.htest)

#Do sequels and non-sequels differ in their ratings. 
#Conduct a 2-sample chi-square test on relevent data
genre.sequel.table <- table(movies$genre, movies$sequel)
genre.sequel.table <- genre.table + 20
genre.sequel.htest <- chisq.test(genre.sequel.table)
genre.htest
