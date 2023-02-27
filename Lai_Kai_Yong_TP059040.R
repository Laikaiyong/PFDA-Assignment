# LAI KAI YONG
# TP059040

## Packages installation + Import
install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")
install.packages("GGally")
install.packages("tidyr")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("dummies")
install.packages("ROCR")
install.packages("corrplot")
install.packages("treemapify")


library(ggplot2)
library(dplyr)
library(scales)
library(GGally)
library(tidyr)
library(funModeling)
library(Hmisc)
library(ROCR)
library(corrplot)
library(treemapify)


## Data import
df <- read.csv(
  "C:/Users/USER/Downloads/PFDA-Assignment/Placement_Data_Full_Class.csv",
  header = TRUE,
  sep = ","
)


## Data exploration
# View Data
class(df)
View(df)

# Total columns and rows
dim(df)
nrow(df)
ncol(df)

# Total levels of specified field
nlevels(factor(df$degree_t))
nlevels(factor(df$specialisation))

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
sapply(df, function(x) sum(is.na(x)))

# Identify unique value
sapply(df, function(x) length(unique(x)))

# Explore numerical values
plot_num(select_if(df, is.numeric), bins = 5, path_out = ".")

# Explore characteristic values
freq(
  df, input = colnames(select_if(df, is.character)),
  plot = FALSE
)

## Data transformation
# Column naming convention
colnames(df) <- c(
  "serial_number",
  "gender", "age",
  "address", "mother_education",
  "father_education", "mother_job",
  "father_job", "family_educational_support",
  "paid", "activities", "internet",
  "secondary_education_percentage",
  "board_of_education",
  "higher_secondary_education_percentage",
  "higher_board_of_education",
  "higher_secondary_education_specialization",
  "degree_percentage", "degree_type",
  "work_experience", "employability_test_percentage",
  "specialization", "post_grad_mba_percentage",
  "status_of_placement", "salary"
)

# Data Conversion "yes"/"no" -> TRUE/FALSE
df[, 9:12] <- ifelse(
  df[, 9:12] == "yes",
  TRUE, FALSE
)

# Swap data to understandable form
# Address "U" <- "Urban", "R" <- "Rural"
df <- df %>% mutate(
  address_area <- case_when(
    address == "U" ~ "Urban",
    address == "R" ~ "Rural"
  )
)
View(df)


# Remove unecessary columns and create new dataframe
df <- subset(
  df, select = -c(
    address, 
  )
)


## Data Visualization
## Question 1: How does family background affects job placement?

# Analysis 1-1: Does father's status affect job placement?

# Graph 1-1-1: Does father's education affect job placement?
df_graph111 <- df %>% group_by(father_education) %>% tally()
ggplot(
  df_graph111,
  aes(
    area = n,
    fill = father_education,
    label = n
  )
) +
geom_treemap() +
geom_treemap_text(
  colour = "white",
  place = "centre",
  size = 15
)


# Graph 1-1-2: Does father's job affect job placement?

# Analysis 1-2: Does mother's status affect job placement?

# Graph 1-2-1: Does mother's education affect job placement?

# Graph 1-2-2: Does mother's job affect job placement?


# Analysis 1-3: Does family educational support affect job placement?

# Graph 1-3-1: Does family educational support affect job placement?


## Question 2: How does personal attribute affects job placement?

# Analysis 2-1: Relationship of Age on job placement

# Graph 2-1-1 

df %>% ggplot(aes(x = gender, fill = gender)) +  geom_bar(aes(fill = status))
df %>% ggplot(aes(x = workex)) +  geom_bar(aes(fill = status))

# Analysis 2-2: Relationship of Age on job placement

ggplot(
  df, aes(
    x=age, y=Fare)) + 
  geom_point() + 
  labs(y="Fare", 
       x="Age", 
       title="Titanic - Age vs Fare")+
        theme_gray()+
       theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
                            axis.text.y= element_text(size=15), axis.title=element_text(size=18))


## Question 3: How does personal educational achievement affects job placement?

# Analysis 3-1: Educational achievement progression on job placement
# Graph 3-1-1 

# Analysis 3-2: Educational specialization on job placement
ggplot(
  data=df,
  aes(x = specialization, y = status_of_placement, fill = status_of_placement)
) +
theme_gray() +
geom_bar(
  stat="identity",
  position=position_dodge()
) + 
labs(
  x="Specialization", 
  y="Status of Placement", 
  title="Overview Status of Placement by Specialization"
)

# Graph 3-2-1 Success placement grouped by specialization
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(x = "", fill = factor(specialization))
) +
geom_bar(width = 1) +
theme_gray() + 
labs(
  fill = "Specialization",
  x = NULL,
  y = NULL,
  title = "Successful Placement by Specialization"
) +
coord_polar(
  theta = "y",
  start = 0
)

# Graph 3-2-2 Unsuccessful placement grouped by specialization
ggplot(
  df %>% filter(df$status_of_placement == "Not Placed"),
  aes(x = "", fill = factor(specialization))
) +
  geom_bar(width = 1) +
  theme_gray() + 
  labs(
    fill="Specialization", 
    x=NULL, 
    y=NULL, 
    title="Successful Placement by Specialization"
  ) +
  coord_polar(theta = "y", start=0)

# Analysis 3-3: Educational degree type on job placement

# Graph 3-3-1 Success placement grouped by degree type
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(x = "", fill = factor(specialization))
) + 
  geom_bar(width = 1) +
  theme_gray() + 
  labs(
    fill="Specialization", 
    x=NULL, 
    y=NULL, 
    title="Successful Placement by Specialization"
  ) +
  coord_polar(theta = "y", start=0)


## Question 4: How does job opportunities affects job placement
# Analysis 4-1: Hi
# Analysis 4-2:
# Analysis 4-3:
## Question 5: Does Job placement with preferably high salary require high qualification

## Extra Feature 1: Linear Regression of educational



## Extra Feature 2: Prediction on Placement



ggplot(
  df %>% filter(date > '2004-12-31'),
  aes(
    x = date,
    y = pop
  )
) +
  geom_line(col="cyan") +
  labs(
    title = "Population over months",
    x = "Date (Month)",
    y = "Population"
  ) +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("3 months"),
    labels = scales::label_date_short()
  ) +
  theme_gray()