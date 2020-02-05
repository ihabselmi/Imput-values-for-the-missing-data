library(stats)

# Question 14.1.1
# Use the mean/mode imputation method to impute values for the missing data.
# Clean workspace
rm(list=ls())
cat("\014")
graphics.off()
set.seed(123)

input <- read.csv("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 10 - Missing Data And Optimization/Homework 10/Data/breast-cancer-wisconsin.data.csv", header=FALSE)
#View(input)
head(input)
summary(input)
str(input)

# obviuously column V7 has missing data. These missing data are labelled with a question mark "?"
input$V7 = gsub("?", NA, input$V7, fixed = TRUE)
missing = which(is.na(input$V7))

# list the rows that have one or more missing values
input[!complete.cases(input),]

# the sum of rows that have one or more missing values
sum(is.na(input$V7))

mean(is.na(input$V7))
# It is less than 5 percent, thus, we can use data imputation.

val <- unique(input$V7[!is.na(input$V7)])              # Values in vec_miss
mode <- val[which.max(tabulate(match(input$V7, val)))] # Mode of vec_miss

vec_imp <- input$V7                                  # Replicate vec_miss
vec_imp[is.na(input$V7)] <- mode                        # Impute by mode

input = cbind(input,vec_imp)

colnames(input)[12] <- "V7"
input <- subset( input, select = -7 )

input.no.missing = input[, colnames(input)[c(1:6,11,7:10)]]
head(input.no.missing)
# Check whether rows have one or more missing values
input.no.missing[!complete.cases(input.no.missing),]

# Obviously there is no rows with missing values


# Question 14.1.2
# Use regression to impute values for the missing data.

# Clean workspace
rm(list=ls())
cat("\014")
graphics.off()
set.seed(123)
input <- read.csv("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 10 - Missing Data And Optimization/Homework 10/Data/breast-cancer-wisconsin.data.csv", header=FALSE)
#View(input)
input$V7 = gsub("?", NA, input$V7, fixed = TRUE)
# Do not include the response variable in regression imputation
missing = which(is.na(input$V7))
input_modified = na.omit(input)

# Generate linear model using all other factors as predictors and using the data for which we removed the rows that have missing data

model <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10+V11, data = input_modified)
summary(model)

# Not all predictors are not significant as the p-value is greater than 0.05 for some of them.
# To resolve this issue, we use backwards stepwise regression for variable selection.

step(model)

# Generate the linear model that stepwise regression recommends.

model_step.1 <- lm(V7~V3 + V4 + V5 + V8 + V9 + V11, data = input_modified)
summary(model_step.1)

# Based on the summary results, we have two variables for which the p-value is greater than 0.05. We decide to discard them
# and run another regression only with variable for which the p-value is below that threshold of 0.05
model_step.2 <- lm(V7~ V4 + V5 + V8 + V11, data = input_modified)
summary(model_step.2)
model_step.3 <- lm(V7~ V5 + V8 + V11, data = input_modified)
summary(model_step.3)
# Now all predictors are signifcant. Below the selected model:
model_step <- lm(V7~ V5 + V11, data = input_modified)
summary(model_step)

# Get predictions for missing V7 values.
missing_input <- input[is.na(input$V7),]
V7_predict <- predict(model_step, newdata = missing_input)
V7_predict = round(V7_predict)
# Impute V7 for observations with missing data for V7 to predicted
# values with this linear model.

inputed.input <- input

inputed.input[missing,7] <- V7_predict
head(inputed.input)
# Check whether rows have one or more missing values
inputed.input[!complete.cases(inputed.input),]

# Obviously there is no rows with missing values

# Question 14.1.3
# Use regression with perturbation to impute values for the missing data.

set.seed(123)

library(stats)
setwd("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 10 - Missing Data And Optimization/Homework 10/Data")



input = as.data.frame(read.table("breast-cancer-wisconsin.data.txt", sep = ",", header = FALSE)) 

newinput = data.frame(matrix(nrow = 699, ncol = 11))

#remove missing values
i = 1
for (y in seq(1:length(input[, 1]))) {
  y = as.integer(y)
  if (input[y, 7] != "?") {
    newinput[i, ] = input[y, ]
    i = i + 1
  }
}
newinput = newinput[complete.cases(newinput), ]
colnames(newinput) =  c(
  "V1",
  "V2",
  "V3",
  "V4",
  "V5",
  "V6",
  "V7",
  "V8",
  "V9",
  "V10",
  "V11"
)
mn = mean(newinput[,7])
sd = sd(newinput[,7])

# Run a regression:
model_pert <- lm(V7~V5 + V11, data = newinput)
summary(model_pert)



imputed_data = input

#create the pertubations using randomly generated normal distributions

x <- rnorm(16, mn, sd); x <- x[x >= 1 & x <= 10]
random_pert = round(x)
i = 1
for (y in seq(1:length(input[, 1]))) {
  #Apply regression to model the missing data
  if (imputed_data[y, 7] == "?") {
    x = predict.lm(model_pert, input[y,]) + random_pert[i] #add pertubations to predictions
    if (x[1] >10){ #check if data is outside of scope and correct if so
      x[1] = 10
    }
    if (x[1] < 1){
      x[1] = 1
    }
    imputed_data[y, 7] = round(x[1])
    i = i + 1
  }
}
imputed_data[missing, 7]

# Check whether rows have one or more missing values
imputed_data[!complete.cases(imputed_data),]

# Obviously there is no rows with missing values

# For optional answer:
#https://rpubs.com/McCloud77/300268




