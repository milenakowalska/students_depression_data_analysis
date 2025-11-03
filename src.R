library(data.table)
library(tidyverse)
library(ggplot2)

# Load data
df <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/dataset/student_depression_dataset.csv"))

# Initial data exploration
head(df)
names(df)

dim(df)

unique(df$Profession)

dt <- as.data.table(df)

dt[Profession != "Student"]

dt[, .N, by="Profession"]

dt[, .N, by="City"][order(-N)]

unique(dt$Sleep.Duration)

unique(dt$Dietary.Habits)

unique(dt$Degree)

unique(dt$Have.you.ever.had.suicidal.thoughts..)

unique(dt$Financial.Stress)

unique(dt$Family.History.of.Mental.Illness)

age_distribution <- ggplot(dt, aes(x = Age)) +
  geom_histogram(fill="pink", color="grey") + 
  ylab("Number of students") + 
  xlab("Age") + 
  ggtitle("Distribution of Participant Ages")

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/age_distribution.png"))

gender_distribution <- ggplot(dt, aes(x = Gender)) +
  geom_bar(fill="lightblue", color="grey") + 
  ylab("Number of students") + 
  xlab("") +
  ggtitle("Distribution of gender among participants")

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/gender_distribution.png"))

depression_distribution <- ggplot(dt, aes(x = factor(Depression, levels=c(0, 1), labels=c("No", "Yes")))) +
  geom_bar(fill="lightgreen", color="grey") + 
  ylab("Number of students") + 
  xlab("") +
  ggtitle("Distribution of depressive vs. non-depressive participants")

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/depression_distribution.png"))

suicidal_thoughts <- ggplot(dt, aes(x = Have.you.ever.had.suicidal.thoughts..)) +
  geom_bar(fill="lightblue", color="grey") + 
  ylab("Number of students") + 
  xlab("") +
  ggtitle("Distribution of students with/without suicidal thoughts")

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/suicidal_thoughts.png"))

# Data preprocessing
head(dt)

dt <- dt[, .(id, Age, Gender, Academic.Pressure, CGPA, Study.Satisfaction, Sleep.Duration, Dietary.Habits, Have.you.ever.had.suicidal.thoughts.., Work.Study.Hours, Financial.Stress, Family.History.of.Mental.Illness, Depression)]

dt[, Have.you.ever.had.suicidal.thoughts.. := ifelse(Have.you.ever.had.suicidal.thoughts.. == "Yes", 1L, 0L)]

dt[, Family.History.of.Mental.Illness := ifelse(Family.History.of.Mental.Illness == "Yes", 1L, 0L)]

dt$Gender <- as.factor(dt$Gender)
dt$Sleep.Duration <- as.factor(dt$Sleep.Duration)
dt$Dietary.Habits <- as.factor(dt$Dietary.Habits)
dt$Financial.Stress <- as.numeric(dt$Financial.Stress)

setnames(dt, old="Family.History.of.Mental.Illness", new="Family.Mental.Illness")
setnames(dt, old="Have.you.ever.had.suicidal.thoughts..", new="Suicidal.thoughts")

head(dt)

colSums(is.na(df))
str(dt)
summary(dt)

# Answer research questions

cor(dt$CGPA, dt$Depression)

cor_cgpa_depression <- ggplot(dt, aes(x=factor(Depression), y=CGPA, fill=Depression)) + 
  geom_boxplot(alpha=0.7) + 
  labs(
    title="Correlation between the cumulative grade point average and depression",
    x="Depression",
    y="CGPA"
  )

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/cor_cgpa_depression.png"))

cor(dt$Age, dt$Depression)

cor_age_depression <- ggplot(dt, aes(x=factor(Depression), y=Age, fill=Depression)) + 
  geom_boxplot(alpha=0.7) + 
  labs(
    title="Correlation between age and depression",
    x="Depression",
    y="Age"
  )

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/cor_age_depression.png"))

cor(dt$Academic.Pressure, dt$Depression)

cor_academic_pressure_dep <- ggplot(dt, aes(x=factor(Depression), y=Academic.Pressure, fill=Depression)) + 
  geom_boxplot(alpha=0.7) + 
  labs(
    title="Correlation between academic pressure and depression",
    x="Depression",
    y="Academic Pressure"
  )

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/cor_academic_pressure_depression.png"))

cor(dt$Work.Study.Hours, dt$Depression)

cor_work_hours_depression <- ggplot(dt, aes(x=factor(Depression), y=Work.Study.Hours, fill=Depression)) + 
  geom_boxplot(alpha=0.7) + 
  labs(
    title="Correlation between work/study hours and depression",
    x="Depression",
    y="Work/Study hours"
  )

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/cor_work_hours_depression.png"))

anova_result <- aov(as.numeric(Depression) ~ as.factor(Dietary.Habits), data = dt)

summary(anova_result)

cor_diet_depression <- ggplot(dt, aes(x=Dietary.Habits, fill=as.factor(Depression))) + 
  geom_bar(position = "stack") + 
  labs(
    title="Correlation between dietary habits and depression",
    x="Depression",
    y="Dietary habits",
    fill="Depression"
  )

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/cor_diet_depression.png"))

cor(as.numeric(dt$Financial.Stress), as.numeric(dt$Depression))

dt <- dt[!is.na(Financial.Stress)]

cor(as.numeric(dt$Financial.Stress), as.numeric(dt$Depression))

cor_finance_depression <- ggplot(dt, aes(x=factor(Depression), y=Financial.Stress, fill=Depression)) + 
  geom_boxplot(alpha=0.7) + 
  labs(
    title="Correlation between financial stress and depression",
    x="Depression",
    y="Financial stress"
  )

ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/diagrams/cor_finance_depression.png"))

