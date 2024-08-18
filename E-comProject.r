data1<-`Data.E.commerce.(1)`
str(data1)
summary(data1)
# Handling OUTLIERS 
columns <- c("Customer_care_calls", "Customer_rating", "Cost_of_the_Product", 
             "Weight_in_gms",'Prior_purchases')
par(mfrow=c(2,3)) 
for (col in columns) {
  boxplot(data1[[col]], 
          main = paste("Boxplot for", col), 
          xlab = col, 
          ylab = "Values",
          col = "blue")
  points(boxplot.stats(data1[[col]])$out, 
         pch = 19, col = "red")
}
#Cost_of_the_Product: (done)
boxplot(data1$Cost_of_the_Product, main = "Boxplot of Cost of the Product")
plot(data1$Cost_of_the_Product)
which(data1$Cost_of_the_Product>350)
data1$Cost_of_the_Product[314]
data1$Cost_of_the_Product[320]
data1$Cost_of_the_Product[353]
data1$Cost_of_the_Product[314]<-NA
data1$Cost_of_the_Product[320]<-NA
data1$Cost_of_the_Product[353]<-NA
data1$Product_importance[data$Product_importance == ""] <- NA 
#Missing values
sum(is.na(data1))
missing_proportion <- colMeans(is.na(data1))
missing_data_plot <- data.frame(variable = names(data1), missing_proportion = missing_proportion)
ggplot(missing_data_plot, aes(x = reorder(variable, -missing_proportion), y = missing_proportion)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Proportion of missing value per variable",
       x = "Variable",
       y = "Proportion of missing values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
install.packages("VIM")
library(VIM)
colSums(is.na(data1))
data1=kNN(data1)
summary(data1)
colSums(is.na(data1))
#data transformation 
com = data1
View(com)
sum(is.na(com))
com1=subset(com,select=c("Product_importance","Prior_purchases"))
View(com1)
View(com)
names(com)
X <- subset(com, select = c("Warehouse_block", "Mode_of_Shipment", "Customer_care_calls", "Customer_rating", "Cost_of_the_Product", "Gender", "Discount_offered", "Weight_in_gms", "Reached.on.Time_Y.N"))
View(X)
X$Warehouse_block <- as.numeric(factor(X$Warehouse_block))
X$Mode_of_Shipment <- as.numeric(factor(X$Mode_of_Shipment))
X$Gender <- as.numeric(factor(X$Gender))
View(X)
Xnor<-scale(X)
View(Xnor)
plot(Xnor)
#exploitation
#shapiro.test pour vérifier numériquement les résultats graphiques
Xnor <- as.data.frame(Xnor)

for (col in names(Xnor)) {
  if (is.numeric(Xnor[[col]])) {
    result <- shapiro.test(Xnor[[col]][1:4000])
    cat("Shapiro-Wilk test for", col, ": p-value =", result$p.value, "\n")
  }
}

#shapiro.test pour vérifier numériquement les résultats graphiques
for (col in names(Xnor)) {
  if (is.numeric(Xnor[[col]])) {
    result <- shapiro.test(Xnor[[col]][4000:8000])
    cat("Shapiro-Wilk test for", col, ": p-value =", result$p.value, "\n")
  }
}

#shapiro.test pour vérifier numériquement les résultats graphiques
for (col in names(Xnor)) {
  if (is.numeric(Xnor[[col]])) {
    result <- shapiro.test(Xnor[[col]][8000:10999])
    cat("Shapiro-Wilk test for", col, ": p-value =", result$p.value, "\n")
  }
}

#Univariate Analysis
# Load necessary libraries
library(ggplot2)      # For creating plots
library(tseries)      # For time series statistical tests
library(gridExtra)    # For arranging plots

# 1. Analysis of Quantitative Variables

# a) Descriptive Statistics
# Descriptive statistics for quantitative variables
# Identify quantitative variables

impdata=subset(com,select=c("Warehouse_block", "Mode_of_Shipment", "Customer_care_calls", "Customer_rating", "Cost_of_the_Product", "Gender", "Discount_offered", "Weight_in_gms", "Reached.on.Time_Y.N"))
quantitative_vars <- sapply(impdata, is.numeric)
quantitative_vars["Reached.on.Time_Y.N"] <- FALSE
for (var in names(impdata[quantitative_vars])) {
  print(paste("descriptive statistics for ", var, ":"))
  print(summary(impdata[[var]]))
}

# Display frequency tables for the categorical variable "Reached.on.Time_Y.N"
print("Frequency table for Reached.on.Time_Y.N:")
print(data.frame(table(impdata$Reached.on.Time_Y.N)))
# - For each quantitative variable, descriptive statistics (min, 1st quartile, median, mean, 3rd quartile, max) are displayed.

# b) Histograms and Bar Plots
# Histograms for continuous variables and bar plots for discrete variables
# Initialize a list to store the plots
plots_list <- list()

# Iterate over the quantitative variables
for (var in names(impdata[quantitative_vars])) {
  if (length(unique(impdata[[var]])) > 10) {
    # Histogram for continuous variables
    plot <- ggplot(impdata, aes_string(x = var)) + 
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = paste("Histogram of ", var))
  } else {
    # bar plot for discret variables
    plot <- ggplot(impdata, aes_string(x = var)) + 
      geom_bar(fill = "orange", color = "black") +
      labs(title = paste("Bar plot for ", var))
  }
  
  
  plots_list[[length(plots_list) + 1]] <- plot
}
# Display the plots in a single window with a grid layout
grid.arrange(grobs = plots_list)




# Remove the "Reached.on.Time_Y.N" variable from quantitative variables
Xnor["Reached.on.Time_Y.N"] <- FALSE
#Xnor["ID"] <- FALSE

# Display descriptive statistics for each variable
for (var in names(Xnor[quantitative_vars])) {
  print(paste("Descriptive statistics for", var, ":"))
  print(summary(Xnor[[var]]))
}

# Display frequency tables for the categorical variable "Reached.on.Time_Y.N"
print("Frequency table for Reached.on.Time_Y.N:")
print(data.frame(table(Xnor$Reached.on.Time_Y.N)))
# - For each quantitative variable, descriptive statistics (min, 1st quartile, median, mean, 3rd quartile, max) are displayed.

# b) Histograms and Bar Plots
# Histograms for continuous variables and bar plots for discrete variables

# Specify the number of rows and columns in the grid layout
num_rows <- 2
num_cols <- ceiling(length(names(Xnor[quantitative_vars])) / num_rows)

# Create a new graphics window with multiple sections
par(mfrow = c(num_rows, num_cols))

# Initialize a list to store the plots
plots_list <- list()

# Iterate over the quantitative variables
for (var in names(Xnor[quantitative_vars])) {
  if (length(unique(Xnor[[var]])) > 10) {
    # Histogram for continuous variables
    plot <- ggplot(Xnor, aes_string(x = var)) + 
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", var))
  } else {
    # Bar plot for discrete variables
    plot <- ggplot(Xnor, aes_string(x = var)) + 
      geom_bar(fill = "orange", color = "black") +
      labs(title = paste("Bar plot of", var))
  }
  
  # Add the plot to the list
  plots_list[[length(plots_list) + 1]] <- plot
}

# Display the plots in a single window with a grid layout
grid.arrange(grobs = plots_list, ncol = num_cols)

# Interpretation:
# - Normality tests (Shapiro-Wilk) are used to assess if quantitative variables follow a normal distribution.
# - Results include the test statistic (W), p-value, and conclusion.

# 2. Analysis of Qualitative Variables
summary(impdata)
library(tidyr)
library(dplyr)
##################### Stacked Bars ########
# Select relevant variables and gather them into long format
impdata <- impdata %>%
  mutate(Reached.on.Time_Y.N = ifelse(Reached.on.Time_Y.N == 1, "NPA", "A"))

# Check the modifications
summary(impdata)
str(impdata["Reached.on.Time_Y.N"])

long_data <- impdata %>%
  select(Mode_of_Shipment, Warehouse_block, Gender, Reached.on.Time_Y.N) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

# Calculate percentages for each category in each variable
long_data <- long_data %>%
  group_by(Variable, Value) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE)) %>%
  mutate(Percent = Count / Total * 100)

# Create the stacked bar plot
ggplot(long_data, aes(x = Variable, fill = Value)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", Percent), y = Percent),
            position = position_fill(vjust = 0.5)) +  # Add percentages
  labs(title = "Distribution of categorical variables (stacked bars)", x = "", y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")

##################### Pie Chart #########
long_data <- impdata %>%
  select(Mode_of_Shipment, Warehouse_block, Gender, Reached.on.Time_Y.N) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

# Calculate percentages for each category in each variable
long_data <- long_data %>%
  group_by(Variable, Value) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE)) %>%
  mutate(Percent = Count / Total * 100)

# Create the pie chart
ggplot(long_data, aes(x = "", y = Percent, fill = Value)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~ Variable) +
  labs(title = "Distribution of categorical variables (pie chart)", x = "", y = "") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom")
# Display frequencies for qualitative variables
for (var in c("Mode_of_Shipment", "Warehouse_block", "Gender", "Reached.on.Time_Y.N")) {
  print(paste("Frequencies for", var, ":"))
  print(table(impdata[[var]]))
}



#Bivariate Analysis
library(ggplot2)
data2 <- subset(com, select = c("Warehouse_block", "Mode_of_Shipment", "Customer_care_calls", "Customer_rating", "Cost_of_the_Product", "Gender", "Discount_offered", "Weight_in_gms", "Reached.on.Time_Y.N"))
data3<-cbind(data2,com1)

# Analyze Product Importance and On-Time Delivery
table_importance <- table(data3$Product_importance, data3$Reached.on.Time_Y.N)
result_chi_square_importance <- chisq.test(table_importance)
print(result_chi_square_importance)

data3$Product_importance <- factor(data3$Product_importance, levels = c("low", "medium", "high"), ordered = TRUE)


# Bar plot for Product_importance vs. Reached.on.Time_Y.N
ggplot(data3, aes(x = reorder(Product_importance, as.numeric(Product_importance)), fill = as.factor(Reached.on.Time_Y.N))) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(title = "Effect of Product Importance on On-Time Delivery", x = "Product Importance", y = "Count") +
  scale_fill_manual(values = c("lightgreen", "lightcoral"), labels = c("On Time", "Not On Time")) +
  theme_minimal()

# Analyze Warehouse Block and On-Time Delivery
table_warehouse <- table(data3$Warehouse_block, data3$Reached.on.Time_Y.N)
result_chi_square_warehouse <- chisq.test(table_warehouse)
print(result_chi_square_warehouse)

# Bar plot for Warehouse_block vs. Reached.on.Time_Y.N
ggplot(data3, aes(x = Warehouse_block, fill = as.factor(Reached.on.Time_Y.N))) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(title = "Effect of Warehouse Block on On-Time Delivery", x = "Warehouse Block", y = "Count") +
  scale_fill_manual(values = c("lightgreen", "lightcoral"), labels = c("On Time", "Not On Time")) +
  theme_minimal()

# Wilcoxon-Mann-Whitney test for Weight_in_gms
on_time <- data3$Weight_in_gms[data3$Reached.on.Time_Y.N == 0]
not_on_time <- data3$Weight_in_gms[data3$Reached.on.Time_Y.N == 1]
result_wilcoxon_weight <- wilcox.test(on_time, not_on_time)
print(result_wilcoxon_weight)

# Box plot for Weight_in_gms vs. Reached.on.Time_Y.N
ggplot(data3, aes(x = as.factor(Reached.on.Time_Y.N), y = Weight_in_gms)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Effect of Weight on On-Time Delivery", x = "On-Time Delivery", y = "Weight in grams") +
  theme_minimal()

# Correlation between Customer_rating and Prior_purchases
correlation_achats <- cor.test(data3$Customer_rating, data3$Prior_purchases)
print(correlation_achats)

# Box plot for Discount_offered and Reached.on.Time_Y.N
ggplot(data3, aes(x = as.factor(Reached.on.Time_Y.N), y = Discount_offered)) +
  geom_boxplot(fill = c("lightgreen", "lightcoral")) +
  labs(title = "Effect of Discount Offered on On-Time Delivery", x = "On-Time Delivery", y = "Discount Offered") +
  theme_minimal()


# Correlation matrix for data3
library(corrplot)
correlation_matrix <- cor(Xnor[, c("Warehouse_block", "Mode_of_Shipment", "Customer_care_calls", "Customer_rating", "Cost_of_the_Product", "Gender", "Discount_offered", "Weight_in_gms", "Reached.on.Time_Y.N")])
print(correlation_matrix)
corrplot(correlation_matrix, method = "color")


#modeling 
df<-cbind(Xnor,com1)
reg_multi <- lm(Prior_purchases~Warehouse_block+Mode_of_Shipment+Customer_care_calls+Customer_rating+Cost_of_the_Product+Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df)
summary(reg_multi)
model1<-lm(lm(Prior_purchases~Warehouse_block+Mode_of_Shipment+Customer_care_calls+Customer_rating-Cost_of_the_Product+Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df))
summary(model1)
model2<-lm(Prior_purchases~Warehouse_block+Mode_of_Shipment+Customer_care_calls-Customer_rating-Cost_of_the_Product+Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df)
summary(model2)
model3<-lm(Prior_purchases~Warehouse_block+Mode_of_Shipment+Customer_care_calls-Customer_rating-Cost_of_the_Product-Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df)
summary(model3)
model4<-lm(Prior_purchases~-Warehouse_block+Mode_of_Shipment+Customer_care_calls-Customer_rating-Cost_of_the_Product-Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df)
summary(model4)
model5<-lm(Prior_purchases~-Warehouse_block-Mode_of_Shipment+Customer_care_calls-Customer_rating-Cost_of_the_Product-Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df)
summary(model5)
AIC(model5)
AIC(reg_multi)
par(mfrow=c(2,2))
plot(reg_multi)
plot(model5)
#using log 
df$log_Prior_purchases <- log(df$Prior_purchases)
missing_values <- is.na(df$log_Prior_purchases)
invalid_values <- !is.finite(df$log_Prior_purchases)
df <- df[complete.cases(df$log_Prior_purchases), ]
sum(is.na(df$log_Prior_purchases))
regression_model_log <- lm(log_Prior_purchases ~Warehouse_block-Mode_of_Shipment+Customer_care_calls-Customer_rating-Cost_of_the_Product-Gender+Discount_offered+Weight_in_gms+Reached.on.Time_Y.N,data=df)
summary(regression_model_log)
plot(regression_model_log)
#k-means
features_for_clustering <- df[, c("Cost_of_the_Product", "Discount_offered", "Prior_purchases")]
# Perform k-means clustering
kmeans_result <- kmeans(features_for_clustering, centers = 3)
# Visualize clustering results
plot(features_for_clustering, col = kmeans_result$cluster)
