# ============================
# 1. Load Libraries and Data
# ============================
library(tidyverse)
library(stargazer)
library(ggplot2)
library(ggeffects)
library(haven)

# Load dataset (update path as needed)
df <- read_sav("C:/Users/aaron/Downloads/NIJ Challenge2021_Full Dataset.sav")

# ============================
# 2. Data Preprocessing
# ============================
df <- df %>%
  mutate(
    # Program Attendances
    Program_Attendances = as.character(Program_Attendances),
    Program_Attendances = ifelse(Program_Attendances == "10 or more", "10", Program_Attendances),
    Program_Attendances = as.numeric(Program_Attendances)
  ) %>%
  mutate(
    # Education Ordinal
    Education_Ordinal = case_when(
      Education_Level %in% c(1, 2) ~ 0L,
      Education_Level == 3 ~ 1L,
      TRUE ~ NA_integer_
    ),
    Education_Ordinal = factor(Education_Ordinal, 
                               levels = c(0, 1),
                               labels = c("Without College", "Some College"),
                               ordered = TRUE)
  ) %>%
  mutate(
    # Prison Years Ordinal
    Prison_Years_Ordinal = case_when(
      Prison_Years == 0 ~ 1L,
      Prison_Years == 1 ~ 2L,
      Prison_Years == 2 ~ 3L,
      Prison_Years == 3 ~ 4L,
      TRUE ~ NA_integer_
    ),
    Prison_Years_Ordinal = factor(Prison_Years_Ordinal,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Less than 1 year", "1-2 years", "2-3 years", "More than 3 years"),
                                  ordered = TRUE)
  ) %>%
  mutate(
    # Employment Category
    Employment_Category = case_when(
      Percent_Days_Employed == 0 ~ "0% (Unemployed)",
      Percent_Days_Employed > 0 & Percent_Days_Employed <= 0.25 ~ "1-25%",
      Percent_Days_Employed > 0.25 & Percent_Days_Employed <= 0.50 ~ "26-50%",
      Percent_Days_Employed > 0.50 & Percent_Days_Employed <= 0.75 ~ "51-75%",
      Percent_Days_Employed > 0.75 ~ "76-100%",
      TRUE ~ NA_character_
    ),
    Employment_Category = factor(Employment_Category,
                                 levels = c("0% (Unemployed)", "1-25%", "26-50%", "51-75%", "76-100%"))
  ) %>%
  mutate(
    # Gang Affiliated
    Gang_Affiliated = factor(Gang_Affiliated,
                             levels = c(0, 1),
                             labels = c("Non-Gang", "Gang")),
    
    # Recidivism - keep as numeric since it's already 0/1
    Recidivism_Within_3years = as.numeric(Recidivism_Within_3years)
  )

# ============================
# 3. Summary Statistics
# ============================

# Helper function to summarize recidivism by a given grouping
summarize_recidivism <- function(data, group_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      Count = n(),
      Recidivism_Count = sum(Recidivism_Within_3years, na.rm = TRUE),
      Recidivism_Rate = mean(Recidivism_Within_3years, na.rm = TRUE) * 100,
      .groups = 'drop'
    ) %>%
    mutate(Percentage = Count / sum(Count) * 100)
}

# Generate summaries
summarize_recidivism(df, Prison_Years_Ordinal)
summarize_recidivism(df, Program_Attendances)
summarize_recidivism(df, Education_Ordinal)
summarize_recidivism(df, Employment_Category)
summarize_recidivism(df, Gang_Affiliated)

# ============================
# 4. Logistic Regressions and Plots
# ============================

# (1) Program Attendance Model
Rehab_Recid <- glm(Recidivism_Within_3years ~ Program_Attendances, 
                   data = df, family = binomial)
summary(Rehab_Recid)

# Create Probability table for recidivism with respect to rehabilitation attendance
pred_rehab_recid <- ggpredict(Rehab_Recid, "Program_Attendances") |> 
  as.data.frame() |> 
  select(x, predicted, conf.low, conf.high)
print(pred_rehab_recid)

# Generate plot for difference in recidivism caused by rehabilitation
plot2 <- ggplot(pred_rehab_recid, aes(x = x, y = predicted)) +
  geom_col(fill = "#FF7F0E", width = 0.6, alpha = 0.8) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.15, color = "black", linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(title = "Recidivism Probability by Program Attendance",
       x = "Number of Programs Attended",
       y = "Probability of Recidivism (3 Years)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))
print(plot2)

# (2) Length of Stay Model
df$Prison_Years_Ordinal <- factor(df$Prison_Years_Ordinal, ordered = FALSE)
df$Prison_Years_Ordinal <- relevel(df$Prison_Years_Ordinal, ref = "Less than 1 year")
Time_Recid <- glm(Recidivism_Within_3years ~ Prison_Years_Ordinal, 
                  data = df, family = binomial)
summary(Time_Recid)

# Create probability table for recidivism with respect to LOS
pred_severity_recid <- ggpredict(Time_Recid, "Prison_Years_Ordinal") |> 
  as.data.frame() |> 
  select(x, predicted, conf.low, conf.high)
print(pred_severity_recid)

# Generate plot to show difference in recidivism caused by prison LOS
plot3 <- ggplot(pred_severity_recid, aes(x = x, y = predicted)) +
  geom_col(fill = "#1F77B4", width = 0.6, alpha = 0.8) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.15, color = "black", linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(title = "Recidivism Probability by Length of Stay",
       x = "Prison Time Category",
       y = "Probability of Recidivism (3 Years)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))
print(plot3)

# (3) Education Model
df <- df %>%
  mutate(Education_Unordered = factor(Education_Ordinal, ordered = FALSE)) %>%
  mutate(Education_Unordered = relevel(Education_Unordered, ref = "Without College"))
Edu_Recid <- glm(Recidivism_Within_3years ~ Education_Unordered, 
                 data = df, family = binomial)
summary(Edu_Recid)

# Create probability table for recidivism with respect to levels of education 
pred_edu_recid <- ggpredict(Edu_Recid, "Education_Unordered") |> 
  as.data.frame() |> 
  select(x, predicted, conf.low, conf.high)
print(pred_edu_recid)

# Generate plot to show difference in recidivism caused by education
plot4 <- ggplot(pred_edu_recid, aes(x = x, y = predicted)) +
  geom_col(fill = "#2CA02C", width = 0.6, alpha = 0.8) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.15, color = "black", linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(title = "Recidivism Probability by Education Level",
       x = "Education Level",
       y = "Probability of Recidivism (3 Years)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))
print(plot4)

# (4) Employment Model
Emp_Recid <- glm(Recidivism_Within_3years ~ Percent_Days_Employed, 
                 data = df, family = binomial)
summary(Emp_Recid)

# Create probability table for recidivism with respect to percentage of time out of jail spent employed 
pred_emp_recid <- ggpredict(Emp_Recid, "Percent_Days_Employed [0, 0.25, 0.50, 0.75, 1]") |> 
  as.data.frame() |> 
  select(x, predicted, conf.low, conf.high)
print(pred_emp_recid)

# Generate plot to show difference in recidivism caused by employment
pred_emp_recid$x <- factor(pred_emp_recid$x, levels = c("0", "0.25", "0.5", "0.75", "1"))
plot5 <- ggplot(pred_emp_recid, aes(x = x, y = predicted)) +
  geom_col(fill = "#1F77B4", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.15, color = "black", linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(title = "Recidivism Probability by Employment Level",
       x = "Percent of Time Employed",
       y = "Probability of Recidivism (3 Years)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))
print(plot5)

# (5) Gang Affiliation Model
Gang_Recid <- glm(Recidivism_Within_3years ~ Gang_Affiliated, 
                  data = df, family = binomial)

# ============================
# 5. Full Logistic Regression Model
# ============================
full_model <- glm(
  Recidivism_Within_3years ~ Program_Attendances + Education_Unordered + 
  Percent_Days_Employed + Gang_Affiliated + Prison_Years_Ordinal,
  data = df,
  family = binomial
)
summary(Gang_Recid)

# Create probability table for recidivism with respect to gang affiliation
pred_gang_recid <- ggpredict(Gang_Recid, "Gang_Affiliated") |> 
  as.data.frame() |> 
  select(x, predicted, conf.low, conf.high)
print(pred_gang_recid)

# Generate Plot to show difference in recidivism from gang affiliation 
plot1 <- ggplot(pred_gang_recid, aes(x = x, y = predicted)) +
  geom_col(fill = "#1E88E5", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.15, color = "black", linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(title = "Recidivism Probability by Gang Affiliation",
       x = NULL,
       y = "Probability of Recidivism (3 Years)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))
print(plot1)

# ============================
# 6. Export LaTeX Table (Stepwise Models)
# ============================
model1 <- Rehab_Recid
model2 <- glm(Recidivism_Within_3years ~ Program_Attendances + Prison_Years_Ordinal, data = df, family = binomial)
model3 <- glm(Recidivism_Within_3years ~ Program_Attendances + Prison_Years_Ordinal + Education_Unordered, data = df, family = binomial)
model4 <- glm(Recidivism_Within_3years ~ Program_Attendances + Prison_Years_Ordinal + Education_Unordered + Percent_Days_Employed, data = df, family = binomial)
model5 <- full_model

stargazer(model1, model2, model3, model4, model5,
          title = "Stepwise Logistic Regression Predicting Recidivism",
          type = "latex",
          dep.var.labels = c("Recidivism within 3 Years"),
          column.labels = c("Rehab", "+ Sentence Length", "+ Education", "+ Employment", "+ Gang Affiliation"),
          covariate.labels = c("Program Attendances",
                               "Prison Years (1-2 vs <1)",
                               "Prison Years (2-3 vs <1)",
                               "Prison Years (>3 vs <1)",
                               "Education (Some College vs Without)",
                               "Percent Days Employed",
                               "Gang Affiliated"),
          omit.stat = c("ll", "aic"),
          no.space = TRUE,
          digits = 3)

