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
