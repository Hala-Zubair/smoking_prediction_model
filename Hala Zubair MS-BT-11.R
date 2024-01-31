# Read the dataset
smoking_dataset<- read.csv("Smoking_dataset.csv")              
# Structure of the dataset after handling missing values
str(smoking_dataset)
# Check for missing values
missing_values <- colSums(is.na(smoking_dataset))
print(missing_values[missing_values > 0])
# Handle missing values by removing rows with any missing values
smoking_dataset <- smoking_dataset[complete.cases(smoking_dataset), ] 
smoking_dataset<-na.omit(smoking_dataset)
# Structure of the dataset after handling missing values
str(smoking_dataset) 
# Number of rows after handling missing values
rows_after_handling <- nrow(smoking_dataset)
# Convert character variables to factors
smoking_dataset$gender <- as.factor(smoking_dataset$gender)
smoking_dataset$oral <- as.factor(smoking_dataset$oral)
smoking_dataset$tartar <- as.factor(smoking_dataset$tartar)
# Check the structure of the data
str(smoking_dataset)
# Convert Binary variables to factors
smoking_dataset$hearing.left. <- as.factor(smoking_dataset$hearing.left.)
smoking_dataset$hearing.right. <- as.factor(smoking_dataset$hearing.right.)
smoking_dataset$Urine.protein<- as.factor(smoking_dataset$Urine.protein)
smoking_dataset$dental.caries <- as.factor(smoking_dataset$dental.caries)
smoking_dataset$smoking <- factor(smoking_dataset$smoking)
# Check the structure of the data
str(smoking_dataset)
# Calling the library
library(ggplot2)
# Box plot  for 'age'
ggplot(smoking_dataset, aes(y = age)) +
  geom_boxplot()
# Remove outliers for 'age'
Q1 <- quantile(smoking_dataset$age, 0.25)
Q3 <- quantile(smoking_dataset$age, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset<- subset(smoking_dataset, age >= lower_bound & age <= 
                            upper_bound)
# Box plot without outliers for age
ggplot(smoking_dataset,aes(y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for age")
# Box plot  for 'height.cm.'
ggplot(smoking_dataset, aes(y = height.cm.)) +
  geom_boxplot()
# Remove outliers for 'height.cm.'
Q1 <- quantile(smoking_dataset$height.cm., 0.25)
Q3 <- quantile(smoking_dataset$height.cm., 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset<- subset(smoking_dataset, height.cm. >= lower_bound & height.cm. <= 
                            upper_bound)
# Box plot without outliers for height.cm.
ggplot(smoking_dataset,aes(y = height.cm.)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for height.cm.")
# Box plot  for 'weight.kg.'
ggplot(smoking_dataset, aes(y = weight.kg.)) +
  geom_boxplot()
# Remove outliers for 'weight.kg.'
Q1 <- quantile(smoking_dataset$weight.kg., 0.25)
Q3 <- quantile(smoking_dataset$weight.kg., 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset<- subset(smoking_dataset, weight.kg. >= lower_bound & weight.kg. <= 
                            upper_bound)
# Box plot without outliers for weight.kg.
ggplot(smoking_dataset,aes(y = weight.kg.)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for weight.kg.")
# Box plot  for 'waist.cm.'
ggplot(smoking_dataset, aes(y = waist.cm.)) +
  geom_boxplot()
# Remove outliers for 'waist.cm.'
Q1 <- quantile(smoking_dataset$waist.cm., 0.25)
Q3 <- quantile(smoking_dataset$waist.cm., 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, waist.cm. >= lower_bound & waist.cm. <= 
                            upper_bound)
# Box plot without outliers for waist.cm.
ggplot(smoking_dataset, aes(y = waist.cm.)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for waist.cm.")
# Box plot  for 'eyesight.left.'
ggplot(smoking_dataset, aes(y = eyesight.left.)) +
  geom_boxplot()
# Remove outliers for 'eyesight.left.'
Q1 <- quantile(smoking_dataset$eyesight.left., 0.25)
Q3 <- quantile(smoking_dataset$eyesight.left., 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, eyesight.left. >= lower_bound & eyesight.left. <= 
                            upper_bound)
# Box plot without outliers for eyesight.left.
ggplot(smoking_dataset, aes(y = eyesight.left.)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for eyesight.left.")
# Box plot  for 'eyesight.right.'
ggplot(smoking_dataset, aes(y = eyesight.right.)) +
  geom_boxplot()
# Remove outliers for 'eyesight.right.'
Q1 <- quantile(smoking_dataset$eyesight.right., 0.25)
Q3 <- quantile(smoking_dataset$eyesight.right., 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, eyesight.right. >= lower_bound & eyesight.right. <= 
                            upper_bound)
# Box plot without outliers for eyesight.right.
ggplot(smoking_dataset, aes(y = eyesight.right.)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for eyesight.right.")
# Box plot  for 'systolic'
ggplot(smoking_dataset, aes(y = systolic)) +
  geom_boxplot()
# Remove outliers for 'systolic'
Q1 <- quantile(smoking_dataset$systolic, 0.25)
Q3 <- quantile(smoking_dataset$systolic, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, systolic >= lower_bound & systolic <= 
                            upper_bound)
# Box plot without outliers for systolic
ggplot(smoking_dataset, aes(y = systolic)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for systolic")
# Box plot  for 'relaxation'
ggplot(smoking_dataset, aes(y = relaxation)) +
  geom_boxplot()
# Remove outliers for 'relaxation'
Q1 <- quantile(smoking_dataset$relaxation, 0.25)
Q3 <- quantile(smoking_dataset$relaxation, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, relaxation >= lower_bound & relaxation <= 
                            upper_bound)
# Box plot without outliers for relaxation
ggplot(smoking_dataset, aes(y = relaxation)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for relaxation")
# Box plot  for 'fasting.blood.sugar'
ggplot(smoking_dataset, aes(y = fasting.blood.sugar)) +
  geom_boxplot()
# Remove outliers for 'fasting.blood.sugar'
Q1 <- quantile(smoking_dataset$fasting.blood.sugar, 0.25)
Q3 <- quantile(smoking_dataset$fasting.blood.sugar, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, fasting.blood.sugar >= lower_bound & fasting.blood.sugar <= 
                            upper_bound)
# Box plot without outliers for fasting.blood.sugar
ggplot(smoking_dataset, aes(y = fasting.blood.sugar)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for fasting.blood.sugar")
# Box plot  for 'Cholesterol'
ggplot(smoking_dataset, aes(y = Cholesterol)) +
  geom_boxplot()
# Remove outliers for 'Cholesterol'
Q1 <- quantile(smoking_dataset$Cholesterol, 0.25)
Q3 <- quantile(smoking_dataset$Cholesterol, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, Cholesterol >= lower_bound & Cholesterol <= 
                            upper_bound)
# Box plot without outliers for Cholesterol
ggplot(smoking_dataset, aes(y = Cholesterol)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for Cholesterol")
# Box plot  for 'HDL'
ggplot(smoking_dataset, aes(y = HDL)) +
  geom_boxplot()
# Remove outliers for 'HDL'
Q1 <- quantile(smoking_dataset$HDL, 0.25)
Q3 <- quantile(smoking_dataset$HDL, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, HDL >= lower_bound & HDL <= 
                            upper_bound)
# Box plot without outliers for HDL
ggplot(smoking_dataset, aes(y = HDL)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for HDL")
# Box plot  for 'LDL'
ggplot(smoking_dataset, aes(y = LDL)) +
  geom_boxplot()
# Remove outliers for 'LDL'
Q1 <- quantile(smoking_dataset$LDL, 0.25)
Q3 <- quantile(smoking_dataset$LDL, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, LDL >= lower_bound & LDL <= 
                            upper_bound)
# Box plot without outliers for LDL
ggplot(smoking_dataset, aes(y = LDL)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for LDL")
# Box plot  for 'hemoglobin'
ggplot(smoking_dataset, aes(y = hemoglobin)) +
  geom_boxplot()
# Remove outliers for 'hemoglobin'
Q1 <- quantile(smoking_dataset$hemoglobin, 0.25)
Q3 <- quantile(smoking_dataset$hemoglobin, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, hemoglobin >= lower_bound & hemoglobin <= 
                            upper_bound)
# Box plot without outliers for hemoglobin
ggplot(smoking_dataset, aes(y = hemoglobin)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for hemoglobin")
# Box plot  for 'serum.creatinine'
ggplot(smoking_dataset, aes(y = serum.creatinine)) +
  geom_boxplot()
# Remove outliers for 'serum.creatinine'
Q1 <- quantile(smoking_dataset$serum.creatinine, 0.25)
Q3 <- quantile(smoking_dataset$serum.creatinine, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, serum.creatinine >= lower_bound & serum.creatinine <= 
                            upper_bound)
# Box plot without outliers for serum.creatinine
ggplot(smoking_dataset, aes(y = serum.creatinine)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for serum.creatinine")
# Box plot  for 'AST'
ggplot(smoking_dataset, aes(y = AST)) +
  geom_boxplot()
# Remove outliers for 'AST'
Q1 <- quantile(smoking_dataset$AST, 0.25)
Q3 <- quantile(smoking_dataset$AST, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, AST >= lower_bound & AST <= 
                            upper_bound)
# Box plot without outliers for AST
ggplot(smoking_dataset, aes(y = AST)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for AST")
# Box plot  for 'ALT'
ggplot(smoking_dataset, aes(y = ALT)) +
  geom_boxplot()
# Remove outliers for 'ALT'
Q1 <- quantile(smoking_dataset$ALT, 0.25)
Q3 <- quantile(smoking_dataset$ALT, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, ALT >= lower_bound & ALT <= 
                            upper_bound)
# Box plot without outliers for ALT
ggplot(smoking_dataset, aes(y = ALT)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for ALT")
# Box plot  for 'Gtp'
ggplot(smoking_dataset, aes(y = Gtp)) +
  geom_boxplot()
# Remove outliers for 'Gtp'
Q1 <- quantile(smoking_dataset$Gtp, 0.25)
Q3 <- quantile(smoking_dataset$Gtp, 0.75)
IQR <- Q3-Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
smoking_dataset <- subset(smoking_dataset, Gtp >= lower_bound & Gtp <= 
                            upper_bound)
# Box plot without outliers for Gtp
ggplot(smoking_dataset, aes(y = Gtp)) +
  geom_boxplot() +
  labs(title = "Boxplot without Outliers for Gtp")

summary(smoking_dataset)
# Bar Charts for categorical variables
ggplot(smoking_dataset, aes(x=gender))+geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x=hearing.left.)) +geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x=hearing.right.)) +geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x= Urine.protein)) +geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x= oral)) +geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x= dental.caries)) +geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x= tartar)) +geom_bar(fill = "brown", alpha = 0.7)
ggplot(smoking_dataset, aes(x= smoking)) +geom_bar(fill = "brown", alpha = 0.7)
# Histogram for Numeric Variables
ggplot(smoking_dataset, aes(x = age)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x = waist.cm.)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=height.cm.)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=weight.kg.)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=waist.cm.)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=eyesight.left.)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=eyesight.right.)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=systolic)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=relaxation)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x= fasting.blood.sugar)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=Cholesterol)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=triglyceride)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=HDL)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=LDL)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=hemoglobin)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x= serum.creatinine)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=AST)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=ALT)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
ggplot(smoking_dataset, aes(x=Gtp)) +geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)
# Bar chart between smoking and categoric variables
ggplot(data = smoking_dataset, aes(x = gender, fill= smoking)) +
  geom_bar(color = "black", position = "dodge")
ggplot(data = smoking_dataset, aes(x = tartar, fill = smoking)) +
  geom_bar(color = "black", position = "dodge")
ggplot(data = smoking_dataset, aes(x = hearing.left., fill = smoking)) +
  geom_bar(color = "black", position = "dodge")
ggplot(data = smoking_dataset, aes(x = hearing.right., fill = smoking)) +
  geom_bar(color = "black", position = "dodge")
ggplot(data = smoking_dataset, aes(x = Urine.protein, fill = smoking)) +
  geom_bar(color = "black", position = "dodge")
ggplot(data = smoking_dataset, aes(x =  oral, fill = smoking)) +
  geom_bar(color = "black", position = "dodge")
ggplot(data = smoking_dataset, aes(x = dental.caries, fill = smoking)) +
  geom_bar(color = "black", position = "dodge")
# Box plot between smoking and Numeric variables
ggplot(smoking_dataset, aes(x = age, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = height.cm., y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = weight.kg., y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = waist.cm., y = smoking)) +
  geom_boxplot() 
ggplot(smoking_dataset, aes(x =  eyesight.left., y = smoking)) +
  geom_boxplot() 
ggplot(smoking_dataset, aes(x =  eyesight.right. , y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x =  systolic, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x =  relaxation, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = fasting.blood.sugar, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = Cholesterol , y = smoking)) +
  geom_boxplot() 
ggplot(smoking_dataset, aes(x = triglyceride, y = smoking)) +
  geom_boxplot() 
ggplot(smoking_dataset, aes(x = HDL, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = LDL, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = hemoglobin, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = serum.creatinine, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = ALT, y = smoking)) +
  geom_boxplot() 
ggplot(smoking_dataset, aes(x = AST, y = smoking)) +
  geom_boxplot()
ggplot(smoking_dataset, aes(x = Gtp, y = smoking)) +
  geom_boxplot()
# Chi-square test for association
result_chi2 <- chisq.test(table(smoking_dataset$gender, smoking_dataset$smoking))
print(result_chi2)
result_chi2 <- chisq.test(table(smoking_dataset$tartar, smoking_dataset$smoking))
print(result_chi2)
result_chi2 <- chisq.test(table(smoking_dataset$oral, smoking_dataset$smoking))
print(result_chi2)
result_chi2 <- chisq.test(table(smoking_dataset$hearing.left., smoking_dataset$smoking))
print(result_chi2)
result_chi2 <- chisq.test(table(smoking_dataset$Urine.protein, smoking_dataset$smoking))
print(result_chi2)
result_chi2 <- chisq.test(table(smoking_dataset$hearing.right., smoking_dataset$smoking))
print(result_chi2)
result_chi2 <- chisq.test(table(smoking_dataset$dental.caries, smoking_dataset$smoking))
print(result_chi2)
# Independent t-test for association
result_ttest <- t.test(smoking_dataset$age ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$height.cm. ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$weight.kg. ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$waist.cm. ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$eyesight.right. ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$eyesight.left. ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$systolic ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$relaxation ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$fasting.blood.sugar ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$Cholesterol ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$triglyceride ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$HDL ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$LDL ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$hemoglobin ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$serum.creatinine ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$AST ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$ALT ~ smoking_dataset$smoking)
print(result_ttest)
result_ttest <- t.test(smoking_dataset$Gtp ~ smoking_dataset$smoking)
print(result_ttest)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # for reproducibility
index <- sample(1:nrow(smoking_dataset), 0.7 * nrow(smoking_dataset))
train_data <- smoking_dataset[index, ]
test_data <- smoking_dataset[-index, ]

# Build logistic regression model
logistic_model <- glm(smoking ~ height.cm. + weight.kg.+ triglyceride + hemoglobin + 
                        ALT + Gtp + tartar + gender + dental.caries, data = train_data, family = "binomial")
summary(logistic_model)

# Make predictions on the test set
test_data$tartar <- factor(test_data$tartar, levels = levels(train_data$tartar))
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_class <- ifelse(predictions > 0.5, 1, 0)
str(train_data)
# Evaluate the model
conf_matrix <- table(test_data$smoking, predicted_class)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)
# Convert categorical variables to factors if needed
train_data$gender <- as.factor(train_data$gender)
train_data$dental.caries <- as.factor(train_data$dental.caries)
train_data$tartar <- as.factor(train_data$tartar)

# Create a Random Forest model
rf_model <- randomForest(smoking ~ ., data = train_data, ntree = 1000)

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the accuracy
rf_accuracy <- sum(rf_predictions == test_data$smoking) / nrow(test_data)
print(paste("Random Forest Accuracy:", rf_accuracy))
    