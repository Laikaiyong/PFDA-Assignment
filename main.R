install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")
install.packages("GGally")
install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(scales)
library(GGally)
library(tidyr)


## Data import
df <- read.csv(
  "C:/Users/USER/Downloads/PFDA-Assignment/Placement_Data_Full_Class.csv",
  header = TRUE,
  sep = ","
)
df


## Data exploration
# View Data
View(df)

# Total columns and rows
dim(df)
nrow(df)
ncol(df)

# Dataframe Property
str(df)
colnames(df)
head(df)
tail(df, n = 10)

# Describe dataframe
summary(df)
ggpairs(df)

# Identify NULL value
is.na(df)
colSums(is.na(df))

# Outlier for numeric columns
stripchart(
  select_if(df, is.numeric),
  horizontal = TRUE,
  method = "jitte",
  pch = 19,
  add = TRUE
)


## Data pre-processing
# Replace NA values
df$salary <- ifelse(
  is.na(df$salary),
  0, df$salary
)


 