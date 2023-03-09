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
ggplot(
  df[is.na(df$salary), ],
  aes(status)
) +
geom_bar(
  stat = "count",
  width = 0.5
) +
labs(
  x = "Placement",
  y = "Total",
  title = "Null value in salary on placement identification"
) +
theme_gray()

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
  "secondary_board_of_education",
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

drop_columns <- c(
    "address <- ...",
    "gender <- ...",
    "mother_education <- ...",
    "father_education <- ..."
)
df <- df[, !(names(df) %in% drop_columns)]

options(scipen = 999)
View(df)

## Data Visualization
## Question 1: How does family background affects placement and salary?

# Analysis 1-1: Does father's status affect placement?

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
  df_graph115$n
)
df_graph115
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


# Analysis 1-2 Does father's status affect salary?

# Graph 1-2-1: Placed student grouped by father's educational level
df121 <- df  %>%
          filter(df$status_of_placement == "Placed")
ggplot(
  df121,
  aes(
    x = factor(father_education),
    y = salary,
    fill = factor(father_education)
  )
) +
geom_boxplot() +
labs(
  x = "Father's education",
  y = "Salary",
  fill = "Father's education",
  title = "Salary Distribution according to father's educational level"
) +
theme_gray()
df121 %>%
  filter(df121$father_education == "none")

# Graph 1-2-2: Placed student grouped by father's job
df122 <- df  %>%
          filter(df$status_of_placement == "Placed")
ggplot(
  df122,
  aes(
    x = father_job,
    y = salary,
    fill = father_job
  )
) +
geom_violin() +
labs(
  x = "Father's Occupation",
  y = "Salary",
  title = "Distribution of  Salary compared by father's job"
) +
theme_gray()

# Analysis 1-3: Does mother's status affect placement?

# Graph 1-3-1: Total students grouped by Mother's Education
df131 <- df %>%
          group_by(mother_education) %>%
          tally()

ggplot(
  df131,
  aes(
    x = mother_education,
    y = n
  )
) +
geom_bar(
  stat = "identity",
  width = 0.5,
  aes(
    fill = n
  )
) +
scale_fill_gradient(
  low = "red",
  high = "blue"
) +
coord_flip() +
labs(
  x = "Mother's education",
  y = "Student Count", 
  title = "Overview of student counts grouped by mother's education"
) +
theme_gray()

# Graph 1-3-2: Placed student grouped by mother's education
df_graph132 <- df  %>% 
                filter(df$status_of_placement == "Placed") %>%
                group_by(mother_education) %>%
                tally()

ggplot(
  df_graph132,
  aes(
    mother_education,
    n,
    fill = n
  )
) +
geom_bar(
  stat = "identity",
  width = 0.5
) +
geom_text(
  aes(
    label = n
  ), vjust = 0
) +
scale_fill_gradient(
  low = "green",
  high = "red"
) +
labs(
  x = "Mother's education",
  y = "Total student",
  title = "Placed student grouped by Mother's education"
) +
theme_gray()

# Graph 1-3-3: Unplaced student grouped by mother's education
df_graph133 <- df  %>%
                filter(df$status_of_placement == "Not Placed") %>%
                group_by(mother_education) %>%
                tally()

ggplot(
  df_graph133,
  aes(
    x = mother_education,
    y = n
  )
) +
geom_point(
  size = 5
) +
geom_segment(
  aes(
    x = mother_education,
    xend = mother_education,
    y = 0,
    yend = n
  )
) +
labs(
  x = "Mother's educational level",
  y = "Student Count",
  title = "Lolipop chart unplaced students based on mother's educational leve;"
) +
theme_gray()

# Graph 1-3-4: Total students grouped by Mother's Job
df134 <- df %>%
          group_by(mother_job) %>%
          tally()

ggplot(
  df134,
  aes(
    x = mother_job,
    y = n
  )
) +
geom_bar(
  stat = "identity",
  width = 0.5,
  aes(
    fill = n
  )
) +
scale_fill_gradient(
  low = "red",
  high = "blue"
) +
coord_flip() +
labs(
  x = "Mother's job",
  y = "Student Count",
  title = "Overview of student counts grouped by mother's job"
) +
theme_gray()

# Graph 1-3-5: Placed student grouped by mother's job
df_graph135 <- df  %>%
                filter(df$status_of_placement == "Placed") %>%
                group_by(mother_job) %>%
                tally()

ggplot(
  df_graph135,
  aes(
    mother_job,
    n,
    fill = n
  )
) +
geom_bar(
  stat = "identity",
  width = 0.5
) +
geom_text(
  aes(
    label = n
  ), vjust = 0
) +
scale_fill_gradient(
  low = "green",
  high = "red"
) +
labs(
  x = "Mother's job",
  y = "Total student",
  title = "Placed student grouped by Mother's job"
) +
theme_gray()

# Graph 1-3-6: Unplaced student grouped by mother's job
df_graph136 <- df  %>%
                filter(df$status_of_placement == "Not Placed") %>%
                group_by(mother_job) %>%
                tally()

ggplot(
  df_graph136,
  aes(
    x = mother_job,
    y = n
  )
) +
geom_point(
  size = 5
) +
geom_segment(
  aes(
    x = mother_job,
    xend = mother_job,
    y = 0,
    yend = n
  )
) +
labs(
  x = "Mother's job",
  y = "Student Count",
  title = "Lolipop chart unplaced students based on mother's job"
) +
theme_gray()

# Analysis 1-4 Does mother's status affect salary?

# Graph 1-4-1: Placed student grouped by father's educational level
df141 <- df  %>%
          filter(df$status_of_placement == "Placed")
ggplot(
  df141,
  aes(
    x = factor(mother_education),
    y = salary,
    fill = factor(mother_education)
  )
) +
geom_boxplot() +
labs(
  x = "Mother's education",
  y = "Salary",
  fill = "Mother's education",
  title = "Salary Distribution according to mother's educational level"
) +
theme_gray()

# Graph 1-4-2: Placed student grouped by mother's job
df142 <- df  %>%
          filter(df$status_of_placement == "Placed")
ggplot(
  df142,
  aes(
    x = mother_job,
    y = salary,
    fill = mother_job
  )
) +
geom_violin() +
labs(
  x = "Mother's Occupation",
  y = "Salary",
  title = "Distribution of  Salary compared by mother's job"
) +
theme_gray()

# Analysis 1-5: Does family educational support affect placement and salary?

# Graph 1-5-1: Relationship between family educational support and placement
df151 <- df  %>%
          filter(df$status_of_placement == "Placed") %>%
          group_by(family_educational_support) %>%
          tally()
ggplot(
  df151,
  aes(
    x = "",
    y = n,
    fill = family_educational_support
  )
) +
geom_bar(
  width = 1,
  stat = "identity"
) +
coord_polar(
  theta = "y",
  start = 0
) +
scale_fill_brewer(
  palette = "Blues"
) +
labs(
  fill = "Family educational support",
  x = NULL,
  y = NULL,
  title = "Total placed student with family educational support"
) +
geom_text(
  aes(
    label = n
  ),
  size = 8,
  position = position_stack(vjust = 0.5)
) +
theme_void()

# Graph 1-5-2: Relationship between family educational support and salary
ggplot(
  df,
  aes(
    factor(family_educational_support),
    salary
  )
) +
geom_boxplot(
  aes(
    fill = factor(family_educational_support)
  )
) +
geom_dotplot(
  binaxis = "y",
  stackdir = "center",
  dotsize = .5,
  fill = "red"
) +
labs(
  x = "Family Educational Support",
  y = "Salary",
  fill = "Family Educational Support",
  title = "Distribution of Salary by family educational support"
) +
theme_gray()

## Question 2: How does personal demographic attributes affects placement and salary?

# Analysis 2-1: Does age affects placement and salary?

# Graph 2-1-1 Student status of placement relation to age
df211 <- df
ggplot(
  df211,
  aes(
    x = age
  )
) +
geom_density(
  aes(
    fill = status_of_placement
  ),
  alpha = 0.7
) +
labs(
  x = "Age",
  y = "Count",
  title = "Distribution of Students age for successful placement"
) +
theme_gray()

# Graph 2-1-2 Student status of placement relation to salary
df212 <- df  %>%
          filter(df$status_of_placement == "Placed")
ggplot(
  df212,
  aes(
    x = age,
    y = salary
  )
) +
geom_bin2d(bins = 10) +
scale_fill_continuous(type = "viridis") +
theme_gray() +
labs(
  x = "Age",
  y = "Salary",
  title = "Distribution of Age and Salary"
)

# Analysis 2-3: Does gender affects placement and salary?

# Graph 2-3-1 Placed student relation to gender
df_graph231 <- df  %>%
                filter(df$status_of_placement == "Placed") %>%
                group_by(gender) %>%
                tally()

df_graph231$fraction <- df_graph231$n / sum(df_graph231$n)
df_graph231$ymax <- cumsum(df_graph231$fraction)
df_graph231$ymin <- c(0, head(df_graph231$ymax, n = -1))
df_graph231$labelPosition <- (df_graph231$ymax + df_graph231$ymin) / 2
df_graph231$label <- paste0(
  df_graph231$gender,
  "\n value: ",
  df_graph231$n
)
df_graph231
ggplot(
  df_graph231,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = gender
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
  title = "Placed student grouped by gender"
)

# Graph 2-3-2 Student gender relation to salary
ggplot(
  df,
  aes(
    y = salary,
    x = gender,
    color = gender
  )
) +
geom_boxplot(varwidth = T)  +
labs(
  x = "Gender",
  y = "Salary",
  title = "Distribution of Salary by gender"
) +
theme_gray()

# Analysis 2-4: Does address affects placement and salary?

# Graph 2-4-1 Address on placement
df251 <- df  %>%
          filter(df$status_of_placement == "Placed") %>%
          group_by(address) %>%
          tally()
ggplot(
  df251,
  aes(
    x = "",
    y = n,
    fill = address
  )
) +
geom_bar(
  width = 1,
  stat = "identity"
) +
coord_polar(
  theta = "y",
  start = 0
) +
scale_fill_brewer(
  palette = "Blues"
) +
labs(
  fill = "Address",
  x = NULL,
  y = NULL,
  title = "Total placed student with address"
) +
geom_text(
  aes(
    label = n
  ),
  size = 8,
  position = position_stack(vjust = 0.5)
) +
theme_void()

# Graph 2-4-2 Address on salary
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = address,
    y = salary,
    fill = address
  )
) +
geom_violin() +
geom_jitter(
  shape = 16,
  position = position_jitter(0.2)
) +
labs(
  x = "Address",
  y = "Salary",
  title = "Distribution of Salary by address"
) +
theme_gray()

# Analysis 2-5: Does internet affects placement and salary?

# Graph 2-5-1 internet on placement
df_graph251 <- df %>%
                group_by(internet) %>%
                tally()
ggplot(
  df_graph251,
  aes(
    area = n,
    fill = internet,
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
  fill = "Internet",
  title = "Total students grouped by Internet accessibility"
)

# Graph 2-5-2 Address on salary
ggplot(
  df,
  aes(
    factor(internet),
    salary
  )
) +
geom_boxplot(
  aes(
    fill = factor(internet)
  )
) +
geom_dotplot(
  binaxis = "y",
  stackdir = "center",
  dotsize = .5,
  fill = "red"
) +
labs(
  x = "Internet Accessibility",
  y = "Salary",
  fill = "Internet Accessibility",
  title = "Distribution of Salary by Internet Accessibility"
) +
theme_gray()

## Question 3: How does personal educational achievement affects placement and salary?

# Analysis 3-1: Educational achievement progression on placement and salary

# Graph 3-1-1: Degree to master with placement
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = degree_percentage,
    y = post_grad_mba_percentage
  )
) +
scale_fill_gradient(
  low = "blue",
  high = "green",
  na.value = NA
) +
geom_bin2d(bins = 10) +
theme_gray() +
labs(
  x = "Degree Marks",
  y = "Master Marks",
  fill = "Status of Placement",
  title = "Degree to Master Progression on Placement Status"
)

# Graph 3-1-2: Higher secondary to degree with placement and placed student
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = higher_secondary_education_percentage,
    y = degree_percentage
  )
) +
scale_fill_gradient(
  low = "yellow",
  high = "red",
  na.value = NA
) +
geom_bin2d(bins = 10) +
theme_gray() +
labs(
  x = "Higher Secondary Education Grade",
  y = "Degree Percentage",
  title = "Distribution of Higher Secondary and Degree"
)

# Graph 3-1-3: seconday to higher secondary with placement and placed student
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = secondary_education_percentage,
    y = higher_secondary_education_percentage
  )
) +
scale_fill_gradient(
  low = "orange",
  high = "purple",
  na.value = NA
) +
geom_bin2d(bins = 10) +
theme_gray() +
labs(
  x = "Secondary Education Grade",
  y = "Higher Secondary Education Grade",
  title = "Distribution of Secondary and Higher Secondary"
)

# Graph 3-1-4: master grades vs salary
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = post_grad_mba_percentage,
    y = salary
  )
) +
scale_fill_gradient(
  low = "cyan",
  high = "red",
  na.value = NA
) +
geom_bin2d(bins = 10) +
theme_gray() +
labs(
  x = "Post Graduation MBA Grade",
  y = "Salary",
  title = "Distribution of Salary by MBA Grade"
)

# Analysis 3-1-5: master grade on placement
ggplot(
  df,
  aes(
     post_grad_mba_percentage
  )
) +
geom_histogram(
  aes(
    fill = status_of_placement
  ), bins = 10
) +
labs(
  title = "Histogram of MBA Grade against status of placement",
  x = " Post graduate MBA Percentage",
  y = "Number of students"
) +
geom_freqpoly(
  aes(color = status_of_placement),
  binwidth = 5, size = 1, alpha = 0.8
)

# Analysis 3-2: Educational specialization on placement
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
    title = "Unplaced students count by Specialization"
  ) +
  coord_polar(theta = "y", start = 0)

# Graph 3-2-3 Salary by master specialization
ggplot(
  df,
  aes(
    x = salary
  )
) +
geom_histogram(
  binwidth = 100000,
  aes(
    fill = specialization
  )
) +
labs(
  x = "Salary",
  y = "Count",
  title = "Distribution of Salaries for Specialization"
) +
facet_grid(specialization ~ .) +
theme_gray()

# Analysis 3-3: Does undergraduate educational degree type affects on placement? # nolint: line_length_linter.

# Graph 3-3-1 Success placement grouped by specialization
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

# Graph 3-3-2 Degree Type on salary
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = specialization,
    y = salary,
    fill = specialization
  )
) +
geom_violin() +
geom_jitter(
  shape = 16,
  position = position_jitter(0.2)
) +
labs(
  x = "Specialization",
  y = "Salary",
  title = "Distribution of Salary by specialization"
) +
theme_gray()

# Graph 3-3-3 Degree Type on success placement
df_graph333 <- df  %>%
                filter(df$status_of_placement == "Placed") %>%
                group_by(degree_type) %>%
                tally()

df_graph333$fraction <- df_graph333$n / sum(df_graph333$n)
df_graph333$ymax <- cumsum(df_graph333$fraction)
df_graph333$ymin <- c(0, head(df_graph333$ymax, n = -1))
df_graph333$labelPosition <- (df_graph333$ymax + df_graph333$ymin) / 2
df_graph333$label <- paste0(
  df_graph333$degree_type,
  "\n value: ",
  df_graph333$n
)
df_graph333
ggplot(
  df_graph333,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = degree_type
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
  title = "Placed student grouped by undergraduate degree type"
)


# Graph 3-3-4 Salaries grouped by degree type
ggplot(
  df,
  aes(
    x = degree_type,
    y = salary
  )
) +
geom_point(
  aes(
    color = degree_type
  )
) +
labs(
  x = "Under graduate degree type",
  y = "Score",
  title = "Quality of Education Vs Score against Country & Citations"
) +
theme_gray()

# Analysis 3-4: Educational Area during high school on job placement and salary

# Graph 3-4-1 Placed students grouped by educational area during high school
df_graph341 <- df %>%
                filter(df$status_of_placement == "Placed")
ggplot(
  df_graph341,
  aes(
    x = secondary_board_of_education,
    y = higher_board_of_education
  )
) +
scale_fill_gradient(
  low = "#5500ff",
  high = "#2c2b2b",
  na.value = NA
) +
geom_bin2d(bins = 10) +
theme_gray() +
labs(
  x = "Secondary board of education",
  y = "Higher secondary board of education",
  title = "Correlation on secondary school board of education"
)

# Graph 3-4-2 Salaries grouped by educational area during higher secondary
ggplot(
  df,
  aes(
    y = salary,
    fill = higher_board_of_education
  )
) +
geom_boxplot(varwidth = T)  +
labs(
  x = "",
  y = "Salary",
  title = "Distribution of Salary by higher secondary board of education"
) +
theme_gray()


# Analysis 3-5: Activities ecxtra class on job placement and salary

# Graph 3-5-1 Placement grouped by extra curricular
df_graph351 <- df  %>%
                filter(df$status_of_placement == "Placed") %>%
                group_by(activities) %>%
                tally()

df_graph351$fraction <- df_graph351$n / sum(df_graph351$n)
df_graph351$ymax <- cumsum(df_graph351$fraction)
df_graph351$ymin <- c(0, head(df_graph351$ymax, n = -1))
df_graph351$labelPosition <- (df_graph351$ymax + df_graph351$ymin) / 2
df_graph351$label <- paste0(
  df_graph351$activities,
  "\n value: ",
  df_graph351$n
)

ggplot(
  df_graph351,
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = activities
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
  title = "Placed student grouped by extra curricular activities"
)


# Graph 3-5-2 Salaries grouped by extra curricular
ggplot(
  df,
  aes(
    y = salary,
    fill = activities
  )
) +
geom_boxplot(varwidth = T)  +
labs(
  x = "",
  y = "Salary",
  title = "Distribution of Salary by extra curricular activities involvement"
) +
theme_gray()

# Analysis 3-6: Extra classes on job placement

# Graph 3-6-1 Placement grouped by extra classes
ggplot(
  data = df,
  aes(x = paid, y = status_of_placement, fill = status_of_placement)
) +
theme_gray() +
geom_bar(
  stat = "identity",
  position = position_dodge()
) +
labs(
  x = "Registered for extra classes",
  y = "Status of Placement",
  title = "Overview Status of Placement by paid extra classes"
)

# Graph 3-6-2 Salaries grouped by extra classes
df362 <- df  %>%
          filter(df$status_of_placement == "Placed")
ggplot(
  df362,
  aes(
    x = paid,
    y = salary,
    fill = paid
  )
) +
geom_violin() +
labs(
  x = "Registered extra classes",
  y = "Salary",
  title = "Distribution of  Salary compared by extra classes involvement"
) +
theme_gray()

## Question 4: How does work-related factors affects job placement and salary

# Analysis 4-1: Does previous working experience helps in landing a job?

# Graph 4-1-1 Work experience on placement
ggplot(
  df,
  aes(
    x = work_experience
  )
) +
geom_bar(
  aes(
    fill = status_of_placement
  )
) +
labs(
  title = "Stacked bar plot on work experience by status of placement",
  x = "Work Experience",
  y = "Total student",
  fill = "Status of placement"
)

# Graph 4-1-2 Work experience on salary
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = work_experience,
    y = salary,
    fill = work_experience
  )
) +
geom_violin() +
geom_jitter(
  shape = 16,
  position = position_jitter(0.2)
) +
labs(
  x = "Working experience",
  y = "Salary",
  title = "Distribution of Salary by working experience"
) +
theme_gray()

# Analysis 4-2: Does employability test score affects placement and salary?

# Graph 4-2-1 Employability test against placement
ggplot(
  df %>% filter(df$status_of_placement == "Placed"),
  aes(
    x = status_of_placement,
    y = employability_test_percentage,
    fill = status_of_placement
  )
) +
geom_violin() +
geom_jitter(
  shape = 16,
  position = position_jitter(0.2)
) +
labs(
  x = "Status of placement",
  y = "Employability test score",
  title = "Distribution of Employability test score by status of placement"
) +
theme_gray()

# Graph 4-2-2 Employability test against salary
ggplot(
  df,
  aes(
    employability_test_percentage,
    salary
  )
) +
geom_count(
  col = "tomato3",
  show.legend = TRUE
) +
labs(
  x = "Employability Test Score",
  y = "Salary",
  title = "Distribution of Salary and employability test score"
) +
theme_gray()

## Extra Feature 1: Parents' occupation

# Analysis E1-1: Mother's occupation X Father's occupation
ggplot(
  df,
  aes(
    x = mother_job,
    y = father_job
  )
) +
scale_fill_gradient(
  low = "pink",
  high = "black",
  na.value = NA
) +
geom_bin2d(bins = 10) +
theme_gray() +
labs(
  x = "Mother's job",
  y = "Father's job",
  title = "Correlation on parents' occupation"
)



## Extra Feature 2: Transition of courses

# Analysis E2-1: Transition of courses: higher -> degree
as_tibble(df) %>%
arrange(
  higher_secondary_education_specialization,
  degree_type
) %>%
group_by(
  higher_secondary_education_specialization,
  degree_type
) %>%
summarise(
  num_pairs = n(), .groups = "drop"
) %>%
pivot_wider(
  names_from = degree_type,
  values_from = num_pairs
)

# Analysis E2-2: Transition of courses: degree -> post-grad
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