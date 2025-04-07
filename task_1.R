# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(gridExtra)

# Load datasets
time <- read_csv("time.csv", skip = 1, col_names = "time")
X <- read_csv("X.csv", skip = 1, col_names = c("x1", "x2"))
y <- read_csv("y.csv", skip = 1, col_names = "y")

# Combine into one dataframe
df <- bind_cols(time, X, y)

# Convert x2 to factor for plotting
df$x2 <- factor(df$x2, labels = c("Neutral", "Emotional"))

# --- 1. Time Series Plots ---
p1 <- ggplot(df, aes(x = time, y = x1)) +
  geom_line(color = "blue") +
  labs(title = "Sound Signal over Time", x = "Time (s)", y = "x1 (Sound)")

p2 <- ggplot(df, aes(x = time, y = y)) +
  geom_line(aes(color = x2)) +
  labs(title = "MEG Signal over Time", x = "Time (s)", y = "y (Brain Response)") +
  scale_color_manual(values = c("Neutral" = "gray40", "Emotional" = "darkred"))

grid.arrange(p1, p2, ncol = 1)

# --- 2. Distribution Plots ---
p3 <- ggplot(df, aes(x = x1)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Sound Input x1")

p4 <- ggplot(df, aes(x = y)) +
  geom_histogram(bins = 40, fill = "salmon", color = "black") +
  labs(title = "Distribution of MEG Output y")

grid.arrange(p3, p4, ncol = 2)

# --- 3. Correlation and Scatter Plot ---
correlation <- cor(df$x1, df$y)
cat(sprintf("Correlation between x1 and y: %.4f\n", correlation))

ggplot(df, aes(x = x1, y = y, color = x2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of x1 vs y by Category",
       x = "Sound Input (x1)",
       y = "MEG Signal (y)") +
  scale_color_manual(values = c("Neutral" = "blue", "Emotional" = "red"))

# --- 4. Boxplots by Sound Category ---
ggplot(df, aes(x = x2, y = y, fill = x2)) +
  geom_boxplot() +
  labs(title = "Boxplot of Brain Response by Sound Type", x = "Sound Type", y = "MEG Output (y)") +
  scale_fill_manual(values = c("Neutral" = "lightblue", "Emotional" = "orange"))

# --- 5. Separate EDA by Sound Type ---

# Subset data
df_neutral <- filter(df, x2 == "Neutral")
df_emotional <- filter(df, x2 == "Emotional")

# Density plots for MEG response
p5 <- ggplot(df_neutral, aes(x = y)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "MEG Response (Neutral Voice)", x = "y")

p6 <- ggplot(df_emotional, aes(x = y)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "MEG Response (Emotional Voice)", x = "y")

grid.arrange(p5, p6, ncol = 2)
