#Import data from CSV file
data <- read.csv("C:\\Users\\SHREYA NAIR\\Downloads\\student performance1111.csv")
data

#Summary from excel
head(data)
tail(data)
summary(data)
str(data)

#Libraries and Packages

library(ggplot2)
library(reshape2)
library(Metrics)

#PLOTS

plot(data)

#Pie chart of Gender Distribution
pie_chart <- ggplot(data, aes(x = " ", y =" ", fill = gender )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Gender Distribution") +
  scale_fill_manual(values = c("red", "blue"))  
print(pie_chart)

#Pie chart of Education Level Distribution
pie_chart <- ggplot(data, aes(x = " ", y =" ", fill = Education )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Education Level Distribution") +
  scale_fill_manual(values = c("darkblue", "cyan4",'darkmagenta',"violet"))  
print(pie_chart)

#Pie chart of Field Distribution
pie_chart <- ggplot(data, aes(x = " ", y =" ", fill = Field )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Field Distribution") +
  scale_fill_manual(values = c("darkolivegreen", "darkolivegreen1","deeppink3","deeppink4"))  
print(pie_chart)

#Pie chart of Preparation Distribution
pie_chart <- ggplot(data, aes(x = " ", y =" ", fill = Preparation )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Preparation Distribution") +
  scale_fill_manual(values = c("gold", "black"))  
print(pie_chart)

#Histogram of CGPA distribution
ggplot(data, aes(x = CGPA)) +
  geom_histogram(binwidth = 1, fill = "violet", color = "black") +
  labs(title = "CGPA Distribution", x = "CGPA", y = "Frequency")

#Histogram of salary distribution
ggplot(data, aes(x = salary.per.annum)) +
  geom_histogram(binwidth = 5000, fill = "cyan", color = "black") +
  labs(title = "Salary Distribution", x = "Salary", y = "Frequency")+
  scale_x_continuous(labels = scales::comma)


#ANALYSIS

#Gender CGPA Analysis
gender_cgpa <- tapply(data$CGPA, data$gender, mean)
bar_data <- data.frame(Gender = names(gender_cgpa), CGPA = gender_cgpa)
bar_colors <- c("red", "green")
ggplot(bar_data, aes(x = Gender, y = CGPA, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender vs CGPA", x = "Gender", y = "CGPA") +
  geom_text(aes(label = format(CGPA, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)

#Education CGPA Analysis
education_cgpa <- tapply(data$CGPA, data$Education, mean)
bar_data <- data.frame(Education = names(education_cgpa), CGPA = education_cgpa)
bar_colors <- c("darkgrey", "steelblue","maroon",'peru')
ggplot(bar_data, aes(x = Education, y = CGPA, fill = Education)) +
  geom_bar(stat = "identity") +
  labs(title = "Education vs CGPA", x = "Education", y = "CGPA") +
  geom_text(aes(label = format(CGPA, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#Study field CGPA Analysis
field_cgpa <- tapply(data$CGPA, data$Field, mean)
bar_data <- data.frame(Field = names(field_cgpa), CGPA = field_cgpa)
bar_colors <- c("lavender","aquamarine","magenta","purple")
ggplot(bar_data, aes(x = Field, y = CGPA, fill = Field)) +
  geom_bar(stat = "identity") +
  labs(title = "Field vs CGPA", x = "Field", y = "CGPA") +
  geom_text(aes(label = format(CGPA, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#Preparation CGPA Analysis
prep_cgpa <- tapply(data$CGPA, data$Preparation, mean)
bar_data <- data.frame(Preparation = names(prep_cgpa), CGPA = prep_cgpa)
bar_colors <- c("black", "grey")
ggplot(bar_data, aes(x = Preparation, y = CGPA, fill = Preparation)) +
  geom_bar(stat = "identity") +
  labs(title = "Preparation vs CGPA", x = "Preparation", y = "CGPA") +
  geom_text(aes(label = format(CGPA, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#Gender Salary Analysis
gender_salary <- tapply(data$salary.per.annum, data$gender, mean)
bar_data <- data.frame(Gender = names(gender_salary), Salary = gender_salary)
bar_colors <- c("goldenrod", "black")
ggplot(bar_data, aes(x = Gender, y = Salary, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender vs Salary", x = "Gender", y = "Salary") +
  geom_text(aes(label = format(Salary, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#Education Salary Analysis
education_salary <- tapply(data$salary.per.annum, data$Education, mean)
bar_data <- data.frame(Education = names(education_salary), Salary = education_salary)
bar_colors <- c("lavender","pink","cyan","yellow")
ggplot(bar_data, aes(x = Education, y = Salary, fill = Education)) +
  geom_bar(stat = "identity") +
  labs(title = "Education vs Salary", x = "Education", y = "Salary") +
  geom_text(aes(label = format(Salary, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#Study Field Salary Analysis
field_salary<- tapply(data$salary.per.annum,data$Field,mean)
bar_data <- data.frame(Field = names(field_salary), Salary = field_salary)
bar_colors <- c("darkgrey", "orange","magenta","purple")
ggplot(bar_data, aes(x = Field, y = Salary, fill = Field)) +
  geom_bar(stat = "identity") +
  labs(title = "Field vs Salary", x = "Field", y = "Salary") +
  geom_text(aes(label = format(Salary, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#Preparation Salary Analysis
preparation_salary<- tapply(data$salary.per.annum,data$Preparation,mean)
bar_data <- data.frame(Preparation = names(preparation_salary), Salary = preparation_salary)
bar_colors <- c("maroon", "navy")
ggplot(bar_data, aes(x =Preparation , y = Salary, fill = Preparation)) +
  geom_bar(stat = "identity") +
  labs(title = "Preparation vs Salary", x = "Preparation", y = "Salary") +
  geom_text(aes(label = format(Salary, big.mark = ",")), vjust = -0.5, color = "black") +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = scales::comma)


#CGPA and salary analysis
ggplot(data, aes(x = CGPA, y = salary.per.annum)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(x = "CGPA", y = "Salary per Annum", title = "Comparison of CGPA and Salary") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas

#K-MEANS CLUSTERING

# Perform clustering and relating CGPA and Salary
cluster_data <- data[, c("CGPA", "salary.per.annum")]
k <- 2  # Number of clusters
set.seed(12)  # Set seed for reproducibility
kmeans_result <- kmeans(cluster_data, centers = k)
data$cluster <- as.factor(kmeans_result$cluster)
ggplot(data, aes(x = CGPA, y = salary.per.annum, color = cluster)) +
  geom_point() +
  labs(title = "Clustering Analysis of CGPA & Salary", x = "CGPA", y = "Salary") +
  scale_color_manual(values = c("red","green")) +
  scale_y_continuous(labels = scales::comma)



# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


#LINEAR REGREESION MODEL

#Linear regression model and calculate error for CGPA
model <- lm(CGPA ~ Education + gender + Field + Preparation, data = train_data)
# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

#Linear regression model for salary and calculate error for SALARY
model1 <- lm(salary.per.annum ~ Education + gender + Field + Preparation, data = train_data)
predictions <- predict(model1, newdata = test_data)

#CGPA ERROR
rmse <- rmse(predictions, test_data$CGPA)
mae <- mae(predictions, test_data$CGPA)
cat("Root Mean Squared Error (RMSE) of CGPAv:", rmse/1000000, "\n")
cat("Mean Absolute Error (MAE) of CGPA :", mae/1000000, "\n")

#SALARY ERROR
rmse <- rmse(predictions, test_data$salary.per.annum)
mae <- mae(predictions, test_data$salary.per.annum)
cat("Root Mean Squared Error (RMSE) of Salary :", rmse/1000000, "\n")
cat("Mean Absolute Error (MAE) of Salary :", mae/1000000, "\n")

# Build a linear regression model on the entire dataset
model <- lm(CGPA ~ Education + gender + Field + Preparation, data = data)
model1 <- lm(salary.per.annum ~ Education + gender + Field + Preparation, data = data)


#FUTURE PREDICTION
new_data <- data.frame(Education = "Graduate", gender = "male", Field = "Science", Preparation = "coaching")
future_prediction <- predict(model, newdata = new_data)
cat("Predicted CGPA for male graduate studying Science with coaching:", future_prediction, "\n")

new_data <- data.frame(Education = "Post Graduate", gender = "female", Field = "Commerce", Preparation = "No coaching")
future_prediction <- predict(model1, newdata = new_data)
cat("Predicted SALARY for female post graduate studying commerce without coaching:", future_prediction, "\n")

print("THE END, THANK YOU!")

