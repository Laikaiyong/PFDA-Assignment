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
  "ori_gender", "age",
  "ori_address", "ori_mother_education",
  "ori_father_education", "mother_job",
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
df$work_experience <- ifelse(
  df$work_experience == "Yes",
  TRUE, FALSE
)

# Swap data to understandable form
# Address "U" <- "Urban", "R" <- "Rural"
df <- df %>% mutate(
  address <- case_when(
    ori_address == "U" ~ "Urban",
    ori_address == "R" ~ "Rural"
  )
)

# Gender "M" <- "Male", "F" <- "Female"
df <- df %>% mutate(
  gender <- case_when(
    ori_gender == "M" ~ "Male",
    ori_gender == "F" ~ "Female"
  )
)

# Mother education
# 0 - none
# 1 - primary (4th grade)
# 2 - primary (5th to 9th grade)
# 3 - secondary education
# 4 - higher education
df <- df %>% mutate(
  mother_education <- case_when(
    ori_mother_education == 0 ~ "none",
    ori_mother_education == 1 ~ "primary (4th grade)",
    ori_mother_education == 2 ~ "primary (5th to 9th grade)",
    ori_mother_education == 3 ~ "secondary education",
    ori_mother_education == 4 ~ "higher education"
  )
)

# Father education
# 0 - none
# 1 - primary (4th grade)
# 2 - primary (5th to 9th grade)
# 3 - secondary education
# 4 - higher education
df <- df %>% mutate(
  father_education <- case_when(
    ori_father_education == 0 ~ "none",
    ori_father_education == 1 ~ "primary (4th grade)",
    ori_father_education == 2 ~ "primary (5th to 9th grade)",
    ori_father_education == 3 ~ "secondary education",
    ori_father_education == 4 ~ "higher education"
  )
)


# Remove unecessary columns and create new dataframe
df <- subset(
  df, select = -c(
    ori_address, ori_gender,
    ori_mother_education,
    ori_father_education
  )
)

# Remove serial number column that is not useful for analysis
df$serial_number <- NULL


# Defaultize behaviour of mutate values
df$father_education <- df$father_education
df$mother_education <- df$mother_education
df$address <- df$address
df$gender <- df$gender

## Data Visualization
## Question 1: How does family background affects job placement?

# Analysis 1-1: Does father's status affect job placement?

# Graph 1-1-1: Total students grouped by Father's Education
df_graph111 <- df %>% group_by(father_education) %>% tally()
df_graph111
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
) +
labs(
  fill = "Father's education level",
  title = "Total students grouped by Father's Education"
)

# Graph 1-1-2: Placed student grouped by father's education
df_graph112 <- df  %>% 
                filter(df$status_of_placement == "Placed") %>%
                group_by(father_education) %>%
                tally()

df_graph112$fraction <- df_graph112$n / sum(df_graph112$n)
df_graph112$ymax <- cumsum(df_graph112$fraction)
df_graph112$ymin <- c(0, head(df_graph112$ymax, n = -1))
df_graph112$labelPosition <- (df_graph112$ymax + df_graph112$ymin) / 2
df_graph112$label <- paste0(
  df_graph112$father_education,
  "\n value: ",
  df_graph112$n
)

ggplot(
  df_graph112,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = father_education
  )
) +
geom_rect() +
geom_label(
  x = 3.5,
  aes(
    y = labelPosition,
    label = label
  ),
  size = 6
) +
scale_fill_brewer(
  palette = 4
) +
coord_polar(
  theta = "y"
) +
xlim(
  c(2, 4)
) +
theme_void() +
theme(legend.position = "none") +
labs(
  title = "Placed student grouped by father's education level"
)

# Graph 1-1-3: Unplaced student grouped by father's education
df_graph113 <- df  %>%
                filter(df$status_of_placement == "Not Placed") %>%
                group_by(father_education) %>%
                tally()

ggplot(
  df_graph113,
  aes(
    x = "",
    y = n,
    fill = father_education
  )
) +
geom_bar(
  stat = "identity",
  width = 1,
  color = "white"
) +
theme_void() +
coord_polar("y", start = 0) +
labs(
  title = "Unplaced students grouped by father's educational level",
) +
guides(fill = guide_legend(title = "Father's educational level"))


# Graph 1-1-4: Total students grouped by Father's Job
df_graph114 <- df %>%
                group_by(father_job) %>%
                tally()
ggplot(
  df_graph114,
  aes(
    area = n,
    fill = father_job,
    label = n
  )
) +
geom_treemap() +
geom_treemap_text(
  colour = "white",
  place = "centre",
  size = 15
) +
labs(
  fill = "Father's job",
  title = "Total students grouped by Father's Job"
)

# Graph 1-1-5: Placed student grouped by father's job
df_graph115 <- df  %>%
                filter(df$status_of_placement == "Placed") %>%
                group_by(father_job) %>%
                tally()

df_graph115$fraction <- df_graph115$n / sum(df_graph115$n)
df_graph115$ymax <- cumsum(df_graph115$fraction)
df_graph115$ymin <- c(0, head(df_graph115$ymax, n = -1))
df_graph115$labelPosition <- (df_graph115$ymax + df_graph115$ymin) / 2
df_graph115$label <- paste0(
  df_graph115$father_job,
  "\n value: ",
  df_graph112$n
)

ggplot(
  df_graph115,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = father_job
  )
) +
geom_rect() +
geom_label(
  x = 3.5,
  aes(
    y = labelPosition,
    label = label
  ),
  size = 6
) +
scale_fill_brewer(
  palette = 4
) +
coord_polar(
  theta = "y"
) +
xlim(
  c(2, 4)
) +
theme_void() +
theme(legend.position = "none") +
labs(
  title = "Placed student grouped by father's job"
)


# Graph 1-1-6: Unplaced student grouped by father's job
df_graph116 <- df  %>%
                filter(df$status_of_placement == "Not Placed") %>%
                group_by(father_job) %>%
                tally()

ggplot(
  df_graph116,
  aes(
    x = "",
    y = n,
    fill = father_job
  )
) +
geom_bar(
  stat = "identity",
  width = 1,
  color = "white"
) +
theme_void() +
coord_polar("y", start = 0) +
labs(
  title = "Unplaced students grouped by father's job",
) +
guides(fill = guide_legend(title = "Father's job"))


# Analysis 1-2: Does mother's status affect job placement?

# Graph 1-2-1: Total students grouped by Mother's Education
df_graph121 <- df %>% group_by(mother_education) %>% tally()
ggplot(
  df_graph121,
  aes(
    x = mother_education,
    y = n
  )
) +
geom_bar(
  stat = "identity"
) +
theme_gray() +
theme(
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position="none"
) +
title() +
xlab("Mother's Education Level") +
ylab("Count") +
coord_flip()

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
) +
labs(
  fill = "Father's education level",
  title = "Total students grouped by Father's Education"
)

# Graph 1-1-2: Placed student grouped by father's education
df_graph112 <- df  %>% 
                filter(df$status_of_placement == "Placed") %>%
                group_by(father_education) %>%
                tally()

df_graph112$fraction <- df_graph112$n / sum(df_graph112$n)
df_graph112$ymax <- cumsum(df_graph112$fraction)
df_graph112$ymin <- c(0, head(df_graph112$ymax, n = -1))
df_graph112$labelPosition <- (df_graph112$ymax + df_graph112$ymin) / 2
df_graph112$label <- paste0(
  df_graph112$father_education,
  "\n value: ",
  df_graph112$n
)

ggplot(
  df_graph112,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = father_education
  )
) +
geom_rect() +
geom_label(
  x = 3.5,
  aes(
    y = labelPosition,
    label = label
  ),
  size = 6
) +
scale_fill_brewer(
  palette = 4
) +
coord_polar(
  theta = "y"
) +
xlim(
  c(2, 4)
) +
theme_void() +
theme(legend.position = "none") +
labs(
  title = "Placed student grouped by father's education level"
)

# Graph 1-1-3: Unplaced student grouped by father's education
df_graph113 <- df  %>%
                filter(df$status_of_placement == "Not Placed") %>%
                group_by(father_education) %>%
                tally()

ggplot(
  df_graph113,
  aes(
    x = "",
    y = n,
    fill = father_education
  )
) +
geom_bar(
  stat = "identity",
  width = 1,
  color = "white"
) +
theme_void() +
coord_polar("y", start = 0) +
labs(
  title = "Unplaced students grouped by father's educational level",
) +
guides(fill = guide_legend(title = "Father's educational level"))


# Graph 1-1-4: Total students grouped by Father's Job
df_graph114 <- df %>%
                group_by(father_job) %>%
                tally()
ggplot(
  df_graph114,
  aes(
    area = n,
    fill = father_job,
    label = n
  )
) +
geom_treemap() +
geom_treemap_text(
  colour = "white",
  place = "centre",
  size = 15
) +
labs(
  fill = "Father's job",
  title = "Total students grouped by Father's Job"
)

# Graph 1-1-5: Placed student grouped by father's job
df_graph115 <- df  %>%
                filter(df$status_of_placement == "Placed") %>%
                group_by(father_job) %>%
                tally()

df_graph115$fraction <- df_graph115$n / sum(df_graph115$n)
df_graph115$ymax <- cumsum(df_graph115$fraction)
df_graph115$ymin <- c(0, head(df_graph115$ymax, n = -1))
df_graph115$labelPosition <- (df_graph115$ymax + df_graph115$ymin) / 2
df_graph115$label <- paste0(
  df_graph115$father_job,
  "\n value: ",
  df_graph112$n
)

ggplot(
  df_graph115,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = father_job
  )
) +
geom_rect() +
geom_label(
  x = 3.5,
  aes(
    y = labelPosition,
    label = label
  ),
  size = 6
) +
scale_fill_brewer(
  palette = 4
) +
coord_polar(
  theta = "y"
) +
xlim(
  c(2, 4)
) +
theme_void() +
theme(legend.position = "none") +
labs(
  title = "Placed student grouped by father's job"
)


# Graph 1-1-6: Unplaced student grouped by father's job
df_graph116 <- df  %>%
                filter(df$status_of_placement == "Not Placed") %>%
                group_by(father_job) %>%
                tally()

ggplot(
  df_graph116,
  aes(
    x = "",
    y = n,
    fill = father_job
  )
) +
geom_bar(
  stat = "identity",
  width = 1,
  color = "white"
) +
theme_void() +
coord_polar("y", start = 0) +
labs(
  title = "Unplaced students grouped by father's job",
) +
guides(fill = guide_legend(title = "Father's job"))


# Analysis 1-3: Does family educational support affect job placement?

# Graph 1-3-1: Does family educational support affect job placement?


## Question 2: How does personal attribute affects job placement?

# Analysis 2-1: Relationship of Age on job placement

# Graph 2-1-1 

df %>% ggplot(aes(x = gender, fill = gender)) +  geom_bar(aes(fill = status))
df %>% ggplot(aes(x = workex)) +  geom_bar(aes(fill = status))

# Analysis 2-2: Relationship of Address on job placement

# Analysis 2-2: Relationship of Gender on job placement

## Question 3: How does personal educational achievement affects job placement?

# Analysis 3-1: Educational achievement progression on job placement
# Graph 3-1-1 

# Analysis 3-2: Educational specialization on job placement
ggplot(
  data = df,
  aes(x = specialization, y = status_of_placement, fill = status_of_placement)
) +
theme_gray() +
geom_bar(
  stat = "identity",
  position = position_dodge()
) +
labs(
  x = "Specialization",
  y = "Status of Placement",
  title = "Overview Status of Placement by Specialization"
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
    fill = "Specialization",
    x = NULL,
    y = NULL,
    title =" Successful Placement by Specialization"
  ) +
  coord_polar(theta = "y", start = 0)

# Analysis 3-3: Does undergraduate educational degree type affects on job placement? # nolint: line_length_linter.

# Graph 3-3-1 Success placement grouped by degree type
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
coord_polar(theta = "y", start = 0)

ggplot(university, aes(x=quality_of_education, y=score)) + 
  geom_point(aes(color=country,size=citations)) + 
  labs(x="Quality Of Education", 
       y="Score",
       title="Quality of Education Vs Score against Country & Citations")+ 
        theme_linedraw()+
       theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
                            axis.text.y= element_text(size=15), axis.title=element_text(size=18))


# Analysis 3-4: Educational degree type on job placement

# Graph 3-3-1 Success placement grouped by degree type



## Question 4: How does work-related factors affects job placement

# Analysis 4-1: Does previous working experience helps in landing a job?


# Analysis 4-2: Does employability test score affects job placement?


## Question 5: Does current studies affect job placement

# Analysis 5-1: Does current post-graduate specialization affects job placement?


# Analysis 5-2: Does the result of the Post Graduation (MBA) benefits in job placement? # nolint: line_length_linter.


## Extra Feature 1: Linear Regression of education



## Extra Feature 2: Prediction on Placement


## Extra Feature 3: Salary Insights from student qualification

# Analysis E3-1: Distribution of Salary in mareketing financial specialization
# Analysis E3-2: Distribution of Salary in marketing human resource specialization # nolint: line_length_linter.

## Extra Feature 4: Transition of courses relating to Job Placement

# Analysis E4-1: Transition of courses: undergraduate -> graduate
as_tibble(df) %>%
arrange(
  degree_type, specialization
) %>%
group_by(
  degree_type, specialization
) %>%
summarise(
  num_pairs = n(), .groups = "drop"
) %>%
pivot_wider(
  names_from = specialization,
  values_from = num_pairs
)

# Analysis E4-2