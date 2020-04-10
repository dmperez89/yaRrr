#Create new objects called a, b, and c
a <- data.frame ("sex" = c("m", "f", "m"), age = c(19, 43, 25), "favorite movie" = c("Moon", "The Goonies", "Spice World"))
b <- mean(a$age)
c <- table(a$sex)

#Use ls() to see that the objects are stored in workspace
ls()


#Skipped 6 and 7 because URL no longer valid

#Save three objects a, b, c to an .RData file called "myobjects.RData" in data folder using save()
save(a, b, c, file = "C:/Users/Danielle.Perez/Documents/yaRrr/MyRProject/data/myobjects.RData")

#Clear workspace using rm(list = ls()). Then run the ls() function to make sure the objects are gone.
rm(list = ls())
ls()

#Open a new R script called AnalyzingObjects.R and save the script to the R folder created
