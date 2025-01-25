#-----Section 01: Import Libraries-------------------------------------------

pacman::p_load(pacman, 
               dplyr, 
               tidyverse, 
               tidyr, 
               vtable, 
               ggplot2, 
               gridExtra, 
               ggthemes, 
               stats, 
               lmtest,
               rstatix,
               car,
               corrplot,
               ppcor,
               psych,
               caret,
               nFactors,
               GPArotation,
               relaimpo,
               RcmdrMisc,
               naniar,
               formattable)

#-----Section 02: Data loading and corrections-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
df <- read.csv("u2642080_DS7006_CW2_data.csv", stringsAsFactors = FALSE)
dim(df)
str(df)
head(df)

df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x)

df <- df %>% rename(Works_offshore = Works_mainly_at_an_offshore_installation._in_no_fixed_place._or_outside_the_UK)
df <- df %>% rename(EA_Ex_FTS = Economically_active_.excluding_full_time_students.)
df <- df %>% rename(EA_FTS = Economically_active_and_a_full_time_student)
df <- df %>% rename(Lives_in_CE = Lives_in_a_communal_establishment)

colnames(df) <- make.names(colnames(df))

str(df)

df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)

attach(df)

#-----Section 03: Basic Data Manipulation-------------------------------------------

# ------------//Check if there are any missing values //--------------------

apply(df, MARGIN = 2, FUN = function(x) sum(is.na(x))) 

#There are no missing values
vis_miss(df)

gg_miss_var(df)

na.omit(df)
dim(df)
# ------------// Check if there are any duplicate rows //------------------

sum(duplicated(df)) 

#0 duplicate rows

# duplicates <- df[duplicated(df), ]
# print(duplicates)

# percentage of duplicates with compete dataset

# round((nrow(duplicates)/nrow(df)) * 100, digits = 2) # 2.31% of total data safe to remove

# ------------// Summary statistics //------------------
summary(df)

summary(Total_deaths)

sumtable(df[, sapply(df, is.numeric)])

# Median - 2.070 and mode - 2.081 values are closely inter-related to each other.

# ------------// Normality test for Total_deaths //------------------

# graphically
qqnorm(Total_deaths, xlab = "Theoretical Quantiles: Total_deaths" )
qqline(Total_deaths, col = 2) ## red color

# K-S test
ks.test(Total_deaths, "pnorm", mean(Total_deaths), sd(Total_deaths))


# histogram
hist(Total_deaths, 
     main="Distribution of Total deaths",
     col = "#FF999F",
     border = "white",
     probability = TRUE,
     xlab = "Values")

curve(dnorm(x, mean = mean(Total_deaths), sd = sd(Total_deaths)), 
      col = "blue",
      lwd = 2,
      add = TRUE)

rug(Total_deaths)

legend("topright", legend = c("Density curve"),
       lty = c("solid"), col = "blue")

# Resonally normally sistributted, but with outlier

# population density vs deaths

ggplot(df, aes(x = Population_density, y = Total_deaths)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Population Density vs Total Deaths",
       x = "Population Density",
       y = "Total Deaths")


# ------------// Finding out the outliers //------------------

# Boxplot

boxdata <- boxplot(Total_deaths,
                   main = "Box Plot of Total Deaths",
                   xlab = "Total Deaths",
                   ylab="Count",
                   col = "#FF999F",
                   border = "darkblue")
# label outliers

#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], 
       boxdata$out[i],
       which(Total_deaths==boxdata$out[i]),
       pos=4, 
       cex=1,
       col = "darkblue")}


# inspect outliers
df[50,c("LA_name","LA_code","Total_deaths")]
df[93,c("LA_name","LA_code","Total_deaths")]
df[100,c("LA_name","LA_code","Total_deaths")]
df[183,c("LA_name","LA_code","Total_deaths")]

#-------- Alternate method Using IQR --------------------------------------------
# Calculate the IQR
IQR_value <- IQR(Total_deaths)

# Calculate the first and third quartiles
Q1 <- quantile(Total_deaths, 0.25)
Q3 <- quantile(Total_deaths, 0.75)

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Find the outliers
outliers <- Total_deaths[Total_deaths < lower_bound | Total_deaths > upper_bound]
print(outliers)

# 4 outliers -> 0.00 4.07 3.88 6.03


#-----Section 04: Normalization-------------------------------------------

# Normalization using min-max

mm <- apply(df["Total_deaths"], MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))
boxplot(mm, main = "Min-Max normalization" )

# Log transformation

logTranform <- apply(df["Total_deaths"], MARGIN = 2, FUN = function(x) log(x))
boxplot(logTranform, main = "Log Normalization" )

# Scale data

scaled_death <- scale(df["Total_deaths"])
boxplot(scaled_data, main = "Scaled data" )


#-----Section 05: Exploratory Data Analysis for all variables-------------------------------------------


# ------------// QQ -plot for all variables//------------------

plot_qq_plots <- function(df) {
  # Iterate through all columns in the dataframe
  for (col in names(df)) {
    # Check if the column is numeric
    if (col != "Total_deaths" && is.numeric(df[[col]])) {
      # Set up the Q-Q plot
      qqnorm(df[[col]], 
             main = paste("Q-Q Plot: ", col),  # Title of the plot
             col = "blue",                     # Color of the points
             pch = 16,                         # Shape of the points
             cex = 1.2)                        # Size of the points
      
      # Add a reference line (theoretical normal line)
      qqline(df[[col]], col = "red", lwd = 2)  # Red line with thickness 2
    }
  }
}

# Usage of the function
plot_qq_plots(df)


# ------------// Scatter plots with respect to Dependent variables //-----------

plot_scatter_with_total_deaths <- function(df) {
  # Iterate through all columns in the dataframe
  for (col in names(df)) {
    # Skip the 'Total_deaths' column, as it is the dependent variable
    if (col != "Total_deaths" && is.numeric(df[[col]])) {
      # Set up the plot
      plot(df$Total_deaths, df[[col]], 
           main = paste("Scatter plot: Total_deaths vs", col),
           xlab = "Total_deaths", 
           ylab = col,
           pch = 16,            # Filled circle marker
           col = rgb(0.2, 0.6, 1, 0.7),  # Semi-transparent blue
           cex = 1.2,           # Size of the points
           las = 1,             # Rotate axis labels to horizontal
           xlim = c(min(df$Total_deaths), max(df$Total_deaths) * 1.1),  # Adjust x-axis range
           ylim = c(min(df[[col]]), max(df[[col]]))  # Adjust y-axis range
      )
      
      # Add a regression line with a smooth curve
      abline(lm(df[[col]] ~ df$Total_deaths), col = "red")
      
      # Add grid lines to make the plot more readable
      grid(col = "gray", lty = "dotted", lwd = 0.5)
      
      # Add a legend in the top left corner
      legend("topright", 
             legend = c("Data Points", "Regression Line"),
             col = c(rgb(0.2, 0.6, 1, 0.7), "red"),
             pch = c(16, NA), 
             lwd = c(NA, 2), 
             bty = "n")  # No box around the legend
    }
  }
}

# Usage of the function
plot_scatter_with_total_deaths(df)

# ------------// Box plots //------------------

# Extract numeric column names
num_cols <- colnames(df[, sapply(df, is.numeric)])

# Create an empty list to store boxplots
boxplots <- list()

# Loop through numeric columns to create boxplots
for (i in 1:9) {
  feature_data <- df[, c(num_cols[i]), drop = FALSE]
  
  # Create boxplot for each numeric column
  p <- ggplot(df, aes(x = factor(1), y = .data[[num_cols[i]]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    ggtitle(paste("Boxplot of", num_cols[i])) +
    labs(x = " ", y = num_cols[i]) +
    theme_minimal()
  
  boxplots[[length(boxplots) + 1]] <- p # Append the boxplot to the list
}

# Dynamically calculate grid dimensions
num_plots <- length(boxplots)
nrow <- ceiling(sqrt(num_plots))
ncol <- ceiling(num_plots / nrow)

# Arrange boxplots in a grid
grid.arrange(grobs = boxplots, nrow = nrow, ncol = ncol)

rm(num_plots, nrow, ncol, feature_data, i, p, boxplots)

# ~~~~~~~~~~~~~~~~~~

# Create an empty list to store boxplots
boxplots <- list()

# Loop through numeric columns to create boxplots
for (i in 10:18) {
  feature_data <- df[, c(num_cols[i]), drop = FALSE]
  
  # Create boxplot for each numeric column
  p <- ggplot(df, aes(x = factor(1), y = .data[[num_cols[i]]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    ggtitle(paste("Boxplot of", num_cols[i])) +
    labs(x = " ", y = num_cols[i]) +
    theme_minimal()
  
  boxplots[[length(boxplots) + 1]] <- p # Append the boxplot to the list
}

# Dynamically calculate grid dimensions
num_plots <- length(boxplots)
nrow <- ceiling(sqrt(num_plots))
ncol <- ceiling(num_plots / nrow)

# Arrange boxplots in a grid
grid.arrange(grobs = boxplots, nrow = nrow, ncol = ncol)

rm(num_plots, nrow, ncol, feature_data, i, p, boxplots)

# ~~~~~~~~~~~~~~~~~~~~~~~~~

# Create an empty list to store boxplots
boxplots <- list()

# Loop through numeric columns to create boxplots
for (i in 19:length(num_cols)) {
  feature_data <- df[, c(num_cols[i]), drop = FALSE]
  
  # Create boxplot for each numeric column
  p <- ggplot(df, aes(x = factor(1), y = .data[[num_cols[i]]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    ggtitle(paste("Boxplot of", num_cols[i])) +
    labs(x = " ", y = num_cols[i]) +
    theme_minimal()
  
  boxplots[[length(boxplots) + 1]] <- p # Append the boxplot to the list
}

# Dynamically calculate grid dimensions
num_plots <- length(boxplots)
nrow <- ceiling(sqrt(num_plots))
ncol <- ceiling(num_plots / nrow)

# Arrange boxplots in a grid
grid.arrange(grobs = boxplots, nrow = nrow, ncol = ncol)

rm(num_plots, nrow, ncol, feature_data, i, p, boxplots)

# -------------------------// KS Test for all variables //----------------------

KS_tests <- function(df) {
  # Create an empty data frame to store the results
  ks_results <- data.frame(Column = character(), 
                           D_value = numeric(), 
                           p_value = numeric(), 
                           Normal_Distribution = character(), 
                           stringsAsFactors = FALSE)
  
  # Iterate through all columns in the dataframe
  for (col in names(df)) {
    # Skip the 'Total_deaths' column, as it is the dependent variable
    if (col != "Total_deaths" && is.numeric(df[[col]])) {
      
      # Perform K-S test and extract p-value and D statistic
      ks_test_result <- ks.test(df[[col]], "pnorm", mean(df[[col]]), sd(df[[col]]))
      
      # Check if the p-value is less than 0.05 and add "***" for significance
      significance <- ifelse(ks_test_result$p.value < 0.05, "No", "Yes")
      
      # Append results to the results table
      ks_results <- rbind(ks_results, 
                          data.frame(Column = col, 
                                     D_value = ks_test_result$statistic, 
                                     p_value = ks_test_result$p.value, 
                                     Normal_Distribution = significance))
    }
  }
  
  # Return the table with the K-S test results
  return(ks_results)
}
# Usage of the function
ks_test_results <- KS_tests(df)

# View the result table
print(ks_test_results)
ks_test_results_df = as.data.frame(ks_test_results)
formattable(ks_test_results_df)


#----------------- Section 06: Hypothesis testing ------------------------------

# Hypothesis: Population density impacts COVID-19 mortality -> accepted
# Pearson Correlation Test
cor_test <- cor.test(df$Population_density, df$Total_deaths, method = "pearson")
cor_test

# ~~~~~~~~~~~~~~~~~~~ Annova test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initialize an empty data frame to store results
anova_results <- data.frame(
  Hypothesis = character(),
  F_Statistic = numeric(),
  Degrees_of_Freedom = numeric(),
  P_Value = numeric(),
  Significant = logical(),
  stringsAsFactors = FALSE
)

# Add results to the data frame using the following ANOVA tests:

# Test 1: Distribution of deaths differs significantly across health status groups
anova_health <- aov(
  Total_deaths ~ Very_good_health + Good_health + Fair_health + Bad_health + Very_bad_health, 
  data = df
)
summary_health <- summary(anova_health)
anova_results <- rbind(
  anova_results,
  data.frame(
    Hypothesis = "Distribution of deaths differs significantly across health status groups",
    F_Statistic = summary_health[[1]][["F value"]][1],
    Degrees_of_Freedom = summary_health[[1]][["Df"]][1],
    P_Value = summary_health[[1]][["Pr(>F)"]][1],
    Significant = summary_health[[1]][["Pr(>F)"]][1] < 0.05
  )
)

# Test 2: Distribution of deaths differs significantly across household deprivation
anova_household <- aov(
  Total_deaths ~ Household_deprivation + Household_not_deprived, 
  data = df
)
summary_household <- summary(anova_household)
anova_results <- rbind(
  anova_results,
  data.frame(
    Hypothesis = "Distribution of deaths differs significantly across household deprivation",
    F_Statistic = summary_household[[1]][["F value"]][1],
    Degrees_of_Freedom = summary_household[[1]][["Df"]][1],
    P_Value = summary_household[[1]][["Pr(>F)"]][1],
    Significant = summary_household[[1]][["Pr(>F)"]][1] < 0.05
  )
)

# Test 3: Distribution of deaths differs significantly across travel modes
anova_travel <- aov(
  Total_deaths ~ On_foot + Public_transport + personal_vehivle, 
  data = df
)
summary_travel <- summary(anova_travel)
anova_results <- rbind(
  anova_results,
  data.frame(
    Hypothesis = "Distribution of deaths differs significantly across travel modes",
    F_Statistic = summary_travel[[1]][["F value"]][1],
    Degrees_of_Freedom = summary_travel[[1]][["Df"]][1],
    P_Value = summary_travel[[1]][["Pr(>F)"]][1],
    Significant = summary_travel[[1]][["Pr(>F)"]][1] < 0.05
  )
)

# Test 4: Distribution of deaths differs significantly across work modes
anova_work <- aov(
  Total_deaths ~ Travel_for_work + Works_mainly_from_home + Works_offshore, 
  data = df
)
summary_work <- summary(anova_work)
anova_results <- rbind(
  anova_results,
  data.frame(
    Hypothesis = "Distribution of deaths differs significantly across work modes",
    F_Statistic = summary_work[[1]][["F value"]][1],
    Degrees_of_Freedom = summary_work[[1]][["Df"]][1],
    P_Value = summary_work[[1]][["Pr(>F)"]][1],
    Significant = summary_work[[1]][["Pr(>F)"]][1] < 0.05
  )
)

# Test 5: Distribution of deaths differs significantly across economic activity
anova_economic <- aov(
  Total_deaths ~ EA_Ex_FTS + EA_FTS + Economically_inactive, 
  data = df
)
summary_economic <- summary(anova_economic)
anova_results <- rbind(
  anova_results,
  data.frame(
    Hypothesis = "Distribution of deaths differs significantly across economic activity",
    F_Statistic = summary_economic[[1]][["F value"]][1],
    Degrees_of_Freedom = summary_economic[[1]][["Df"]][1],
    P_Value = summary_economic[[1]][["Pr(>F)"]][1],
    Significant = summary_economic[[1]][["Pr(>F)"]][1] < 0.05
  )
)

# Print the results data frame
print(anova_results)
anova_results_df <- as.data.frame(anova_results)
formattable(anova_results_df)

# ~~~~~~~~~~~~~~~~~~~ Chi-Squared Tests ~~~~~~~~~~~~~~~~~~~

# Chi-Squared Tests for Hypotheses

# Initialize an empty data frame to store results
chi_results <- data.frame(
  Hypothesis = character(),
  Chi_Square_Statistic = numeric(),
  Degrees_of_Freedom = numeric(),
  P_Value = numeric(),
  Significant = logical(),
  stringsAsFactors = FALSE
)

# Add results to the data frame using the following Chi-Squared tests:

# Test 1: Household deprivation vs. Health status
chi_household_health <- chisq.test(table(df$Household_deprivation, df$Very_good_health))
chi_results <- rbind(
  chi_results,
  data.frame(
    Hypothesis = "Household deprivation vs Health status",
    Chi_Square_Statistic = chi_household_health$statistic,
    Degrees_of_Freedom = chi_household_health$parameter,
    P_Value = chi_household_health$p.value,
    Significant = chi_household_health$p.value < 0.05
  )
)

# Test 2: Travel mode vs. Economic activity
chi_travel_economy <- chisq.test(table(df$Public_transport, df$Economically_inactive))
chi_results <- rbind(
  chi_results,
  data.frame(
    Hypothesis = "Travel mode vs Economic activity",
    Chi_Square_Statistic = chi_travel_economy$statistic,
    Degrees_of_Freedom = chi_travel_economy$parameter,
    P_Value = chi_travel_economy$p.value,
    Significant = chi_travel_economy$p.value < 0.05
  )
)

# Test 3: Work location vs. Health status
chi_work_health <- chisq.test(table(df$Works_mainly_from_home, df$Fair_health))
chi_results <- rbind(
  chi_results,
  data.frame(
    Hypothesis = "Work location vs Health status",
    Chi_Square_Statistic = chi_work_health$statistic,
    Degrees_of_Freedom = chi_work_health$parameter,
    P_Value = chi_work_health$p.value,
    Significant = chi_work_health$p.value < 0.05
  )
)

# Test 4: Age group vs. Household type
chi_age_household <- chisq.test(table(df$Young, df$Lives_in_a_household))
chi_results <- rbind(
  chi_results,
  data.frame(
    Hypothesis = "Age group vs Household type",
    Chi_Square_Statistic = chi_age_household$statistic,
    Degrees_of_Freedom = chi_age_household$parameter,
    P_Value = chi_age_household$p.value,
    Significant = chi_age_household$p.value < 0.05
  )
)

# Test 5: Economic activity vs. Health categories
chi_economy_health <- chisq.test(table(df$Part_time, df$Very_bad_health))
chi_results <- rbind(
  chi_results,
  data.frame(
    Hypothesis = "Economic activity vs Health categories",
    Chi_Square_Statistic = chi_economy_health$statistic,
    Degrees_of_Freedom = chi_economy_health$parameter,
    P_Value = chi_economy_health$p.value,
    Significant = chi_economy_health$p.value < 0.05
  )
)

# Print the results data frame
print(chi_results)
chi_results_df <- as.data.frame(chi_results)
formattable(chi_results_df)

# ~~~~~~~~~~~~~~~~~~~ auto correlation ~~~~~~~~~~~~~~~~~~~~~~

# Hypothesis: Test for autocorrelation in deaths
# Durbin-Watson Test (checks for serial correlation in residuals of regression model)

dw_test <- dwtest(lm(Total_deaths ~ Population_density + Good_health + Bad_health, data = df))
dw_test

#----------------- Section 07: Correlation Ananlysis ------------------------------

df_numeric <- df[, sapply(df, is.numeric)]

# Correlation Matrix (Pearson and Spearman)
# Pearson Correlation

cor_matrix_pearson <- cor(df_numeric[-1], method = "pearson", use="pairwise.complete.obs")
cor_matrix_pearson

# Pearson Correlation Visualization
corrplot(cor_matrix_pearson, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.5,
         main="Pearson's Correlation without total deaths")

# Spearman Correlation
cor_matrix_spearman <- cor(df_numeric[-1], method = "spearman", use="pairwise.complete.obs")
cor_matrix_spearman

# Spearman Correlation Visualization
corrplot(cor_matrix_spearman, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.5,
         main="Spearmen's Correlation without total deaths")

# Identify Highly Correlated Variables
cor_threshold <- 0.4 # Set correlation threshold
high_cor_pairs <- findCorrelation(cor_matrix_pearson, cutoff = cor_threshold, names = TRUE)
high_cor_pairs

#------------Scaled data-------------------------------
  
scaled_df <- as.data.frame(scale(df_numeric))

# Pearson Correlation
cor_matrix_pearson_scaled <- cor(scaled_df[-1], method = "pearson", use="pairwise.complete.obs")
cor_matrix_pearson_scaled

# Pearson Correlation Visualization
corrplot(cor_matrix_pearson_scaled, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.5,
         main="Pearson's Correlation without total deaths for scaled data")

# Spearman Correlation
cor_matrix_spearman_scaled <- cor(scaled_df[-1], method = "spearman", use="pairwise.complete.obs")
cor_matrix_spearman_scaled

# Spearman Correlation Visualization
corrplot(cor_matrix_spearman_scaled, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.5,
         main="Spearmen's Correlation without total deaths for scaled data")

# Identify Highly Correlated Variables
cor_threshold <- 0.4 # Set correlation threshold
high_cor_pairs_scaled <- findCorrelation(cor_matrix_pearson_scaled, cutoff = cor_threshold, names = TRUE)
high_cor_pairs_scaled

# ---------------- compare both cor pairs -------------------------------------

# Find common elements
common_elements <- intersect(high_cor_pairs_scaled, high_cor_pairs)

# Find elements unique to each variable
unique_to_scaled <- setdiff(high_cor_pairs_scaled, high_cor_pairs)
unique_to_pairs <- setdiff(high_cor_pairs, high_cor_pairs_scaled)

# Output the results
cat("Common elements:\n", common_elements, "\n\n")
cat("Elements unique to high_cor_pairs_scaled:\n", unique_to_scaled, "\n\n")
cat("Elements unique to high_cor_pairs:\n", unique_to_pairs, "\n")

# All elements are common -> Mo difference if we take scalled data

# ~~~~~~~~~~~~~~~~ Partial Correlation Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compute Partial Correlation controlling for all other variables
partial_cor <- pcor(df_numeric[-1])
partial_cor$estimate

# Visualize Partial Correlation Matrix
corrplot(partial_cor$estimate, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.4,
         main = "Partial Correlation",
         cex.main = 1,  # Adjust title size for better fit
         mar = c(1, 1, 2, 1))

# Identify Highly Correlated Variables
cor_threshold <- 0.4 # Set correlation threshold
partial_cor_pairs <- findCorrelation(as.matrix(partial_cor$estimate), cutoff = cor_threshold, names = TRUE)
partial_cor_pairs

# Find elements unique to each variable
unique_to_partial <- setdiff(partial_cor_pairs, high_cor_pairs)
cat("Elements unique to partial_cor_pairs:\n", unique_to_partial, "\n")

# Elements unique to partial_cor_pairs : Lives_in_a_household Good_health Part_time On_foot

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate Correlations with the Dependent Variable
cor_with_dep <- sapply(df_numeric[-1], function(x) cor(df_numeric$Total_deaths, x,
                                                 method = "pearson", 
                                                 use = "pairwise.complete.obs"))
cor_with_dep

# Convert to a data frame for ggplot
cor_df <- data.frame(Variable = names(cor_with_dep), 
                     Correlation = cor_with_dep)

# Plot using ggplot
ggplot(cor_df, aes(x = Variable, y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(title = "Correlation with Total Deaths", y = "Correlation Coefficient", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))  # Adjusts label and title positioning

# Sort Correlations by Absolute Value (Descending)
cor_with_dep_sorted <- sort(abs(cor_with_dep), decreasing = TRUE)

# Visualize Correlation with Dependent Variable
barplot(cor_with_dep_sorted, main = "Correlation with Dependent Variable", 
        ylab = "Correlation Coefficient", 
        las = 2, 
        col = "purple",
        cex.names = 0.5)

# Print Variables with High Correlation
cor_threshold <- 0.3 # Define a threshold for high correlation
highly_correlated_vars <- cor_with_dep_sorted[cor_with_dep_sorted >= cor_threshold]

highly_correlated_vars

# Visualize Correlation with Dependent Variable
barplot(highly_correlated_vars, main = "Correlation with Dependent Variable(threshold>0.3)", 
        ylab = "Correlation Coefficient", 
        las = 2, 
        col = "purple",
        cex.names = 0.5)


# There is no difeerence in correlation analysis between normal and scaled datasets.

#-----Section 08 : Factor Analysis -------------------------------------------

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.5, proceed to factor analysis
KMO(cor(df_numeric[-1]))

# get eigenvalues
ev <- eigen(cor(df_numeric[-1]))
ev$values

# Create the scree plot with improved aesthetics
plot(ev$values, 
     type = "b",        # Both points and lines
     col = "blue",      # Color for points and line
     pch = 19,          # Use filled circles for points
     xlab = "Principal Components", 
     ylab = "Eigenvalue", 
     main = "Scree Plot of Eigenvalues",
     cex.lab = 1.2,     # Larger axis labels
     cex.main = 1.5,    # Larger title
     cex.axis = 0.9,    # Adjust axis label size
     col.axis = "black", # Color for axis labels
     lwd = 2,           # Line width
     xaxt = "n",        # Disable x-axis ticks to manually set them
     ylim = c(0, max(ev$values) * 1.1))  # Adjust y-axis range to leave space above points

# Manually add x-axis labels (variable numbers)
axis(1, at = 1:length(ev$values), labels = 1:length(ev$values))

# Add eigenvalue numbers to each point
text(x = 1:length(ev$values), 
     y = ev$values, 
     labels = round(1:length(ev$values), 2),  # Round eigenvalues to 2 decimal places
     pos = 3,  # Place labels above the points
     col = "black",  # Text color
     cex = 0.8)  # Text size


# calculate cumulative proportion of eigenvalue and plot
ev.sum <- 0
for(i in 1:length(ev$value)){
  ev.sum <- ev.sum+ev$value[i]
}
ev.list1 <- 1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i] <- ev$value[i]/ev.sum
}
ev.list2 <- 1:length(ev$value)
ev.list2[1] <- ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i] <- ev.list2[i-1]+ev.list1[i]
}

# Create the scree plot with improved aesthetics
plot(ev.list2, 
     type = "b",        # Both points and lines
     col = "red",      # Color for points and line
     pch = 19,          # Use filled circles for points
     xlab = "Principal Components", 
     ylab = "Eigenvalue", 
     main = "Cummulative Plot of Eigenvalues",
     cex.lab = 1.2,     # Larger axis labels
     cex.main = 1.5,    # Larger title
     cex.axis = 0.9,    # Adjust axis label size
     col.axis = "black", # Color for axis labels
     lwd = 2,           # Line width
     xaxt = "n",        # Disable x-axis ticks to manually set them
     ylim = c(0, max(ev.list2) * 1.1))  # Adjust y-axis range to leave space above points

# Manually add x-axis labels (variable numbers)
axis(1, at = 1:length(ev.list2), labels = 1:length(ev.list2))

# Add eigenvalue numbers to each point
text(x = 1:length(ev.list2), 
     y = ev.list2, 
     labels = round(1:length(ev.list2), 2),  # Round eigenvalues to 2 decimal places
     pos = 3,  # Place labels above the points
     col = "black",  # Text color
     cex = 0.8)  # Text size


# ---------------- Varimax Rotated Principal Components ------------------------
# retaining 'nFactors' components

fit <- principal(df_numeric, nfactors = 7, rotate = "varimax")
fit

# High Correlation (Multicollinearity):
#   Variables with high loadings on the same component might be redundant.
# For example, Household_deprivation and Household_not_deprived both have strong loadings on RC1.
# For example, Lives_in_a_household and Lives_in_CE bot have strong loadings on RC3
# Since they are inversely related, you can remove one.
# 
# Low Communality (h2 values):
#   Variables with low communalities contribute less overall to explaining variance. 
# Focus on variables with higher h2 values (closer to 1). 
# If u2 (uniqueness) is high, consider removing the variable.
# 
# Complex Variables (High com Values):
#   Variables with high com values contribute to multiple components, indicating they might confuse the interpretation. 
# Consider removing variables with high com values, such as EA_Ex_FTS (com = 3.6) and Full_time (com = 3.5).

# Variables to remove based on analysis
vars_to_remove <- c("Total_deaths", 
                    "Household_not_deprived", 
                    "Very_good_health", 
                    "EA_Ex_FTS", 
                    "Full_time", 
                    "Lives_in_a_household")

# Create a new dataframe excluding these variables
df_reduced <- df_numeric[, !(colnames(df_numeric) %in% vars_to_remove)]
str(df_reduced)


# get eigenvalues
ev <- eigen(cor(df_reduced))
ev$values
# Create the scree plot with improved aesthetics
plot(ev$values, 
     type = "b",        # Both points and lines
     col = "blue",      # Color for points and line
     pch = 19,          # Use filled circles for points
     xlab = "Principal Components", 
     ylab = "Eigenvalue", 
     main = "Scree Plot of Eigenvalues",
     cex.lab = 1.2,     # Larger axis labels
     cex.main = 1.5,    # Larger title
     cex.axis = 0.9,    # Adjust axis label size
     col.axis = "black", # Color for axis labels
     lwd = 2,           # Line width
     xaxt = "n",        # Disable x-axis ticks to manually set them
     ylim = c(0, max(ev$values) * 1.1))  # Adjust y-axis range to leave space above points

# Manually add x-axis labels (variable numbers)
axis(1, at = 1:length(ev$values), labels = 1:length(ev$values))

# Add eigenvalue numbers to each point
text(x = 1:length(ev$values), 
     y = ev$values, 
     labels = round(1:length(ev$values), 2),  # Round eigenvalues to 2 decimal places
     pos = 3,  # Place labels above the points
     col = "black",  # Text color
     cex = 0.8)  # Text size

fit <- principal(df_reduced, nfactors = 7, rotate = "varimax")
fit

# create four variables to represent the rorated components
fit$scores

RC_data <- data.frame(fit$scores)

# check new variables are uncorrelated
cor.matrix2 <-cor(RC_data, method = "spearman")
cor.matrix2

corrplot(cor.matrix2, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.5,
         main="Correlation for Rotated Components",
         cex.main = 1,  # Adjust title size for better fit
         mar = c(1, 1, 2, 1))

#-----Section 09 : Linear Regression modelling -------------------------------------------
set.seed(12345)
# Linear Regression
model <- lm(Total_deaths ~ Population_density)
summary(model)

# Create the scatter plot with improved aesthetics
plot(Population_density, Total_deaths, 
     main = "Linear Regression Model", 
     xlab = "Population Density (per km²)", 
     ylab = "COVID-19 Mortality (Total Deaths)", 
     pch = 19,  # Filled circles for points
     col = "blue",  # Blue points for the scatter plot
     cex = 0.6,  # Adjust size of points
     cex.main = 1.5,  # Larger title
     cex.lab = 1.2,   # Larger axis labels
     col.axis = "black",  # Color for axis labels
     bty = "l",    # Simple border around the plot
     xlim = range(Population_density, na.rm = TRUE), 
     ylim = range(Total_deaths, na.rm = TRUE)) 

# Add the regression line in red
abline(model, col = "red", lwd = 2)  # Red line with a larger width

# Add confidence interval shading to the regression line
# Using predict to get confidence intervals
predictions <- predict(model, interval = "confidence", level = 0.95)
lines(Population_density, predictions[, 2], col = "green", lty = 2)  # Lower CI bound
lines(Population_density, predictions[, 3], col = "green", lty = 2)  # Upper CI bound

# Add a legend
legend("topleft", legend = c("Regression Line", "Confidence Interval"), 
       col = c("red", "green"), lty = c(1, 2), lwd = c(2, 1), cex = 0.8)


hist(model$residuals, 
     main="Histogram for Resdual",
     col = "purple")
rug(model$residuals)

# consider normality of residuals
plot(model$residuals ~ model$fitted.values, 
     xlab = "fitted values", 
     ylab = "residuals", 
     main="Resdual Plot")

ks.test(model$residuals, "pnorm", mean(model$residuals), sd(model$residuals))

#-----Section 10 : Multivariate Linear Regression modelling -------------------------------------------

# Define the dependent variable
dependent_var <- "Total_deaths"

# Create the formula for the linear regression model
formula <- as.formula(paste(dependent_var, "~", paste(colnames(df_numeric[-1]), collapse = " + ")))
formula

# Fit the linear regression model with all variabels
model1 <- lm(formula, data = df)

summary(model1)

hist(model1$residuals, 
     main="Histogram for Resdual",
     col = "purple")
rug(model1$residuals)

# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, 
     xlab = "fitted values", 
     ylab = "residuals", 
     main="Resdual Plot")

ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))

vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high

# Fit the linear regression model with highly correlated variables
model1a <- lm(Total_deaths ~ Very_bad_health + Household_not_deprived + Bad_health + Household_deprivation + On_foot + Works_mainly_from_home + Fair_health, data = df)

summary(model1a)

hist(model1a$residuals, 
     main="Histogram for Resdual",
     col = "purple")
rug(model1a$residuals)

# consider normality of residuals
plot(model1a$residuals ~ model1$fitted.values, 
     xlab = "fitted values", 
     ylab = "residuals", 
     main="Resdual Plot for model1a")

ks.test(model1a$residuals, "pnorm", mean(model1a$residuals), sd(model1a$residuals))

vif(model1a)
sqrt(vif(model1a)) > 2  # if > 2 vif too high

anova(model1, model1a, test = "F")

# ~~~~~~~~~~~~~~~~~~~ model with selected variables ~~~~~~~~~~~~~~ 

# Household Deprivation + Young + Elderly + Young + On Foot + Part-time + Travel for Work

model2 <- lm(Total_deaths ~ Household_deprivation + Young + Elderly + On_foot + Part_time + Travel_for_work)
summary(model2)
sqrt(vif(model2)) > 2

# Create a data frame with selected variables for the partial correlation test
selected_data <- data.frame(
  Household_deprivation = Household_deprivation,
  Young = Young,
  Elderly = Elderly,
  On_foot = On_foot,
  Part_time = Part_time,
  Travel_for_work = Travel_for_work,
  Total_deaths = Total_deaths
)

pcor_result <- pcor(selected_data, method = "pearson")

# View the results
print(pcor_result)

pcor_df <- as.data.frame(pcor_result$estimate)

# Filter correlations based on the threshold
threshold <- 0.5
highly_correlated <- which(abs(pcor_df) > threshold & pcor_df != 1, arr.ind = TRUE)

# Create a data frame for plotting
high_cor_pairs <- data.frame(
  Variable1 = rownames(pcor_df)[highly_correlated[, 1]],
  Variable2 = colnames(pcor_df)[highly_correlated[, 2]],
  Correlation = pcor_df[highly_correlated]
)

# Remove duplicate pairs (since the matrix is symmetric)
high_cor_pairs <- high_cor_pairs[high_cor_pairs$Variable1 < high_cor_pairs$Variable2, ]

ggplot(high_cor_pairs, aes(x = interaction(Variable1, Variable2, sep = " & "), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Highly Correlated Variable Pairs",
       x = "Variable Pairs",
       y = "Correlation") +
  theme_minimal()


# This suggests that the variables under consideration 
# (such as Young, Elderly, Part_time, and Travel_for_work) 
# have important relationships with Total_deaths 
# and should be considered in any further analyses.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ selected variables after partial corelation ~~~~~~~~~~~~~~

model3 <- lm(Total_deaths ~ Young + Elderly + Part_time + Travel_for_work)
summary(model3)
sqrt(vif(model3)) > 2

# relative importance of variables

calc.relimp(model3, type = c("lmg"), rela = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ stepwise model  ~~~~~~~~~~~~~~

model4 <- stepwise(model1, direction = "forward")
summary(model4)
hist(model4$residuals)
rug(model4$residuals)
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))
sqrt(vif(model4)) > 2
calc.relimp(model4, type = c("lmg"), rela = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ model with rotated components ~~~~~~~~~~~~~~

model5 <- lm(Total_deaths ~ RC_data$RC2 + RC_data$RC1 + RC_data$RC3 + RC_data$RC4 + RC_data$RC5 + RC_data$RC6 + RC_data$RC7)
summary(model5)
sqrt(vif(model5)) > 2

# relative importance of variables

calc.relimp(model5, type = c("lmg"), rela = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ stepwise model : rotated components  ~~~~~~~~~~~~~~

model6 <- stepwise(model5, direction = "forward")
summary(model6)
hist(model6$residuals)
rug(model6$residuals)
plot(model6$residuals ~ model6$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model6$residuals, "pnorm", mean(model6$residuals), sd(model6$residuals))
sqrt(vif(model6)) > 2
calc.relimp(model6, type = c("lmg"), rela = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Section 11 : annova tests for best predictor comparison ~~~~~~~~~~~~~~

# test whether model1 and model4 are significantly different using F test
anova(model1, model4, test = "F")

#  --------->   model 4 is as good as model 1

# test whether model2 and model2 are significantly different using F test
anova(model2, model3, test = "F")

#  --------->  model 3 is bad than model 2 due to house deprivation removal

# test whether model2 and model2 are significantly different using F test
anova(model1, model2, test = "F")

#  --------->  model 1 is as good as model 2 

# test whether model2 and model2 are significantly different using F test
anova(model4, model2, test = "F")

#  --------->  model 4 bad than model 2 

anova(model1, model5, test = "F")

#  --------->  model 1 good 5

anova(model4, model5, test = "F")

# statistically equivalent

anova(model6, model5, test = "F")

anova(model4,model6, test="F")

#-----Section -------------------------------------------

detach(df)

# remove all variables from the environment
rm(list=ls())
dev.off()
# do ctrl + L to clear terminal





























