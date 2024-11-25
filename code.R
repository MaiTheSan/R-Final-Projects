# Install Pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Using Pacman to load all libraries
pacman::p_load(
  pacman, ggplot2, dplyr, distrEx, tidyverse, 
  ggridges, viridis, BSDA, readxl
)

# Load Dataset
library(readxl)
student_scores <- read_excel("final_student_scores_rounded.xlsx")
View(student_scores)

# Define variables
SelfStudyTime <- factor(student_scores$`Self Study Time`)
DisciplinaryScore <- factor(student_scores$`Disciplinary Score`)

# ------------------------------- Data Processing -------------------------------

# 1. Convert factor to numeric
SelfStudyTime_numeric <- as.numeric(as.character(SelfStudyTime))
DisciplinaryScore_numeric <- as.numeric(as.character(DisciplinaryScore))

# 2. Calculate mean
mean_SelfStudyTime <- mean(SelfStudyTime_numeric, na.rm = TRUE)
mean_DisciplinaryScore <- mean(DisciplinaryScore_numeric, na.rm = TRUE)

# 3. Print mean results
cat("Mean of Self Study Time:", mean_SelfStudyTime, "\n")
cat("Mean of Disciplinary Score:", mean_DisciplinaryScore, "\n")

# 4. Calculate standard deviation
sd_SelfStudyTime <- sd(SelfStudyTime_numeric, na.rm = TRUE)
sd_DisciplinaryScore <- sd(DisciplinaryScore_numeric, na.rm = TRUE)

# 5. Print standard deviation
cat("Standard Deviation of Self Study Time:", sd_SelfStudyTime, "\n")
cat("Standard Deviation of Disciplinary Score:", sd_DisciplinaryScore, "\n")

# 6. Calculate median
median_SelfStudyTime <- median(SelfStudyTime_numeric, na.rm = TRUE)
median_DisciplinaryScore <- median(DisciplinaryScore_numeric, na.rm = TRUE)

# 7. Print median
cat("Median of Self Study Time:", median_SelfStudyTime, "\n")
cat("Median of Disciplinary Score:", median_DisciplinaryScore, "\n")

# 8. Calculate quantiles
q1a <- quantile(SelfStudyTime_numeric, 0.25, na.rm = TRUE)
q2a <- quantile(SelfStudyTime_numeric, 0.50, na.rm = TRUE)
q3a <- quantile(SelfStudyTime_numeric, 0.75, na.rm = TRUE)
q4a <- quantile(SelfStudyTime_numeric, 1.00, na.rm = TRUE)

q1b <- quantile(DisciplinaryScore_numeric, 0.25, na.rm = TRUE)
q2b <- quantile(DisciplinaryScore_numeric, 0.50, na.rm = TRUE)
q3b <- quantile(DisciplinaryScore_numeric, 0.75, na.rm = TRUE)
q4b <- quantile(DisciplinaryScore_numeric, 1.00, na.rm = TRUE)

# 9. Print quantiles
cat("Quantiles for Self Study Time:\n")
cat("Q1a (25th percentile):", q1a, "\n")
cat("Q2a (Median):", q2a, "\n")
cat("Q3a (75th percentile):", q3a, "\n")
cat("Q4a (100th percentile):", q4a, "\n")

cat("\nQuantiles for Disciplinary Score:\n")
cat("Q1b (25th percentile):", q1b, "\n")
cat("Q2b (Median):", q2b, "\n")
cat("Q3b (75th percentile):", q3b, "\n")
cat("Q4b (100th percentile):", q4b, "\n")

# 10. Calculate IQR
IQR1 <- q3a - q1a
IQR2 <- q3b - q1b

# 11. Print IQR results
cat("IQR for Self Study Time:", IQR1, "\n")
cat("IQR for Disciplinary Score:", IQR2, "\n")

# 12. Find min and max of SelfStudyTime
minSelfStudyTime <- min(SelfStudyTime_numeric, na.rm = TRUE)
maxSelfStudyTime <- max(SelfStudyTime_numeric, na.rm = TRUE)

# 13. Find min and max of DisciplinaryScore
minDisciplinaryScore <- min(DisciplinaryScore_numeric, na.rm = TRUE)
maxDisciplinaryScore <- max(DisciplinaryScore_numeric, na.rm = TRUE)

# 14. Calculate range
rangeSelfStudyTime <- maxSelfStudyTime - minSelfStudyTime
rangeDisciplinaryScore <- maxDisciplinaryScore - minDisciplinaryScore

# 15. Print range
cat("Range of Self Study Time:", rangeSelfStudyTime, "\n")
cat("Range of Disciplinary Score:", rangeDisciplinaryScore, "\n")

# ------------------------------- Hypothesis Testing -------------------------------

# 1. Test if the mean of Self Study Time is greater than 190
mean_null <- 190  # Null hypothesis
n <- length(SelfStudyTime_numeric[!is.na(SelfStudyTime_numeric)])  # Sample size
alpha <- 0.05

# 2. Calculate test statistic
z_test <- (mean_SelfStudyTime - mean_null) / (sd_SelfStudyTime / sqrt(n))
p_value_z <- 1 - pnorm(z_test)

# 3. Print Z-test results
cat("Z-test statistic:", z_test, "\n")
cat("P-value for Z-test:", p_value_z, "\n")

# 4. Conclusion
if (p_value_z < alpha) {
  cat("Reject the null hypothesis: Self Study Time is significantly greater than 190.\n")
} else {
  cat("Fail to reject the null hypothesis: Insufficient evidence to conclude Self Study Time > 190.\n")
}

# ------------------------------- Correlation -------------------------------

# 1. Correlation between Self Study Time and Disciplinary Score
correlation <- cor(SelfStudyTime_numeric, DisciplinaryScore_numeric, use = "complete.obs", method = "pearson")
cat("Correlation between Self Study Time and Disciplinary Score:", correlation, "\n")

# 2. Test significance of correlation
cor_test <- cor.test(SelfStudyTime_numeric, DisciplinaryScore_numeric, method = "pearson")
cat("\nCorrelation Test Results:\n")
print(cor_test)

# ------------------------------- Error Test  -------------------------------

# 1. Given values
mu0 <- 190       # Null hypothesis mean
mua <- 195       # True mean for alternative hypothesis
sigma <- 28.01   # Standard deviation
n <- 50          # Sample size
alpha <- 0.05    # Significance level

# 2. Calculate the critical value for Type I error
z_alpha <- qnorm(1 - alpha)
critical_value <- mu0 + z_alpha * (sigma / sqrt(n))

# 3. Type II Error (Beta)
z_beta <- (critical_value - mua) / (sigma / sqrt(n))
beta <- pnorm(z_beta)

# 4. Power of the test
power <- 1 - beta

# 5. Print results
cat("Critical Value:", critical_value, "\n")
cat("Type I Error (Alpha):", alpha, "\n")
cat("Type II Error (Beta):", beta, "\n")
cat("Power of the Test:", power, "\n")

# ------------------------------- Plotting ------------------------------- 

# 1. Boxplot: Distribution of self-study time by gender
ggplot(student_scores, aes(x = Sex, y = `Self Study Time`, fill = Sex)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  labs(title = "Analysis of Self-Study Time by Gender (with Mean)",
       x = "Gender",
       y = "Self-Study Time") +
  theme_minimal()

# 2. Boxplot: Distribution of disciplinary score by gender
ggplot(student_scores, aes(x = Sex, y = `Disciplinary Score`, fill = Sex)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  labs(title = "Distribution of Disciplinary Scores by Gender (with Mean)",
       x = "Gender",
       y = "Disciplinary Score") +
  theme_minimal()
# ------------------------------- Updated Plotting ------------------------------- 

# Calculate statistics for Self Study Time
mean_self_study <- mean(SelfStudyTime_numeric, na.rm = TRUE)
std_self_study <- sd(SelfStudyTime_numeric, na.rm = TRUE)
lower_bound_self_study <- mean_self_study - 1.96 * std_self_study
upper_bound_self_study <- mean_self_study + 1.96 * std_self_study

# Calculate statistics for Disciplinary Score
mean_score <- mean(DisciplinaryScore_numeric, na.rm = TRUE)
std_score <- sd(DisciplinaryScore_numeric, na.rm = TRUE)
lower_bound_score <- mean_score - 1.96 * std_score
upper_bound_score <- mean_score + 1.96 * std_score

# Generate data for density plot (aligned)
x_self_study <- seq(mean_self_study - 4 * std_self_study, mean_self_study + 4 * std_self_study, length.out = 1000)
y_self_study <- dnorm(x_self_study, mean = mean_self_study, sd = std_self_study)

x_score <- seq(mean_score - 4 * std_score, mean_score + 4 * std_score, length.out = 1000)
y_score <- dnorm(x_score, mean = mean_score, sd = std_score)

# Combine and align datasets
x_self_study_shifted <- x_self_study - mean_self_study
x_score_shifted <- x_score - mean_score

data <- data.frame(
  x = c(x_self_study_shifted, x_score_shifted),
  y = c(y_self_study, y_score),
  group = rep(c("Self Study Time", "Disciplinary Score"), each = 1000)
)

# Plot combined distributions
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) + # Mean
  # Annotate Self Study Time bounds
  annotate("segment", x = lower_bound_self_study - mean_self_study, xend = lower_bound_self_study - mean_self_study,
           y = 0, yend = dnorm(lower_bound_self_study, mean = mean_self_study, sd = std_self_study),
           color = "blue", linetype = "solid") +
  annotate("segment", x = upper_bound_self_study - mean_self_study, xend = upper_bound_self_study - mean_self_study,
           y = 0, yend = dnorm(upper_bound_self_study, mean = mean_self_study, sd = std_self_study),
           color = "blue", linetype = "solid") +
  # Annotate Disciplinary Score bounds
  annotate("segment", x = lower_bound_score - mean_score, xend = lower_bound_score - mean_score,
           y = 0, yend = dnorm(lower_bound_score, mean = mean_score, sd = std_score),
           color = "green", linetype = "solid") +
  annotate("segment", x = upper_bound_score - mean_score, xend = upper_bound_score - mean_score,
           y = 0, yend = dnorm(upper_bound_score, mean = mean_score, sd = std_score),
           color = "green", linetype = "solid") +
  labs(
    title = "Aligned Distributions of Self Study Time and Disciplinary Score",
    x = "Aligned Values (Shifted to Mean 0)",
    y = "Density",
    color = "Group"
  ) +
  theme_minimal()


# ------------------------------- Search ------------------------------- 

# ------- displays the sequence of environments currently active -------
search()

