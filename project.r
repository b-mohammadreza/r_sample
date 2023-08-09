library(dslabs)
library(tidyverse)
library(ggthemes)

#############################################################################################
lifestyle_ds <- read_csv("Downloads/florida_tech/data_analysis_methods/project/dataset/archive/Wellbeing_and_lifestyle_data_Kaggle.csv")
View(lifestyle_ds)

total_number_of_variables <- ncol(lifestyle_ds)
print(paste("Total number of variables:", total_number_of_variables))
total_number_of_entries <- nrow(lifestyle_ds)
print(paste("Total number of entries:", total_number_of_entries))


#############################################################################################
# To make sure there is no NA in the dataset
number_of_NAs_in_dataset <- sum(is.na(lifestyle_ds[,1:total_number_of_variables]))
print(paste("Number of total NAs in the dataset:",number_of_NAs_in_dataset))


#############################################################################################
# To check for any Outlier and correct inconsistent entries

outlier_entries <- c()
outlier_entries_index <- 1

# Finding any outlier in Timestamp. any timestamp needs to be in the following range:
# 7/6/15 23:59 < timestamp < 3/15/21 00:00
verify_timestamp <- function() {
  min_ts <- as.POSIXlt("7/6/15 23:59", tz="", format="%m/%d/%y %H:%M")
  max_ts <- as.POSIXlt("3/15/21 00:00", tz="", format="%m/%d/%y %H:%M")
  try_formats <- c("%m/%d/%y %H:%M", "%m/%d/%y")

  for(entry in 1:total_number_of_entries) {
    cur_ts_str <- lifestyle_ds$Timestamp[entry]
    cur_ts <- as.POSIXlt(cur_ts_str, tz="", tryFormats=try_formats)

    if (is.na(cur_ts) | cur_ts < min_ts | cur_ts > max_ts) {
      outlier_entries[outlier_entries_index] <<- entry
      outlier_entries_index <<- outlier_entries_index + 1
      print(paste("verify_timestamp() --- Outlier entry:", entry, "value:", cur_ts_str))
    }
  }
  print(paste("verify_timestamp() valid range: [7/6/15 23:59, 3/15/21 00:00] --- Total number of outliers:", outlier_entries_index-1))
}
verify_timestamp()

# Finding any outlier in 'numerical' variables
verify_numerical_variables <- function(variable, variable_name, min_val, max_val) {
  for (entry in 1:total_number_of_entries) {
    if (is.na(as.numeric(variable[entry])) | variable[entry] < min_val | variable[entry] > max_val) {
      outlier_entries[outlier_entries_index] <<- entry
      outlier_entries_index <<- outlier_entries_index + 1
      print(paste("verify_numerical_variables():", variable_name, "--- Outlier entry:", entry, ", value:", variable[entry]))
    }
  }
  print(paste("verify_numerical_variables():", variable_name, "valid range:[", min_val, ",", max_val, "] --- Total number of outliers:", outlier_entries_index-1))
}

# 'FRUITS_VEGGIES' valid range: 0 <= val <= 5
verify_numerical_variables(lifestyle_ds$FRUITS_VEGGIES, "FRUITS_VEGGIES", 0, 5)

# 'DAILY_STRESS' valid range: 0 <= val <= 5
verify_numerical_variables(lifestyle_ds$DAILY_STRESS, "DAILY_STRESS", 0, 5)

# 'PLACES_VISITED' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$PLACES_VISITED, "PLACES_VISITED", 0, 10)

# 'CORE_CIRCLE' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$CORE_CIRCLE, "CORE_CIRCLE", 0, 10)

# 'SUPPORTING_OTHERS' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$SUPPORTING_OTHERS, "SUPPORTING_OTHERS", 0, 10)

# 'SOCIAL_NETWORK' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$SOCIAL_NETWORK, "SOCIAL_NETWORK", 0, 10)

# 'ACHIEVEMENT' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$ACHIEVEMENT, "ACHIEVEMENT", 0, 10)

# 'DONATION' valid range: 0 <= val <= 5
verify_numerical_variables(lifestyle_ds$DONATION, "DONATION", 0, 5)

# 'BMI_RANGE' valid range: 1 <= val <= 2
verify_numerical_variables(lifestyle_ds$BMI_RANGE, "BMI_RANGE", 1, 2)

# 'TODO_COMPLETED' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$TODO_COMPLETED, "TODO_COMPLETED", 0, 10)

# 'FLOW' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$FLOW, "FLOW", 0, 10)

# 'DAILY_STEPS' valid range: 1 <= val <= 10
verify_numerical_variables(lifestyle_ds$DAILY_STEPS, "DAILY_STEPS", 1, 10)

# 'LIVE_VISION' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$LIVE_VISION, "LIVE_VISION", 0, 10)

# 'SLEEP_HOURS' valid range: 1 <= val <= 10
verify_numerical_variables(lifestyle_ds$SLEEP_HOURS, "SLEEP_HOURS", 1, 10)

# 'LOST_VACATION' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$LOST_VACATION, "LOST_VACATION", 0, 10)

# 'DAILY_SHOUTING' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$DAILY_SHOUTING, "DAILY_SHOUTING", 0, 10)

# 'SUFFICIENT_INCOME' valid range: 1 <= val <= 2
verify_numerical_variables(lifestyle_ds$SUFFICIENT_INCOME, "SUFFICIENT_INCOME", 1, 2)

# 'PERSONAL_AWARDS' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$PERSONAL_AWARDS, "PERSONAL_AWARDS", 0, 10)

# 'TIME_FOR_PASSION' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$TIME_FOR_PASSION, "TIME_FOR_PASSION", 0, 10)

# 'WEEKLY_MEDITATION' valid range: 0 <= val <= 10
verify_numerical_variables(lifestyle_ds$WEEKLY_MEDITATION, "WEEKLY_MEDITATION", 0, 10)

# 'WORK_LIFE_BALANCE_SCORE' valid range: 480 <= val < 821
verify_numerical_variables(lifestyle_ds$WORK_LIFE_BALANCE_SCORE, "WORK_LIFE_BALANCE_SCORE", 480, 821)

# 'AGE' range:
print(unique(lifestyle_ds$AGE))

# 'GENDER' values:
print(unique(lifestyle_ds$GENDER))

print(paste("Inconsistent(outlier) entries:", outlier_entries))

# The only inconsistent value is 'DAILY_STRESS[10006]', we simply change
# this value to default(0) and will update the variable type to 'numeric'
lifestyle_ds$DAILY_STRESS[10006] <- 0
class(lifestyle_ds$DAILY_STRESS) <- "numeric"

# 'lifestyle_ds$DAILY_STRESS[10006]' set to 0 and variable type changed from <chr> to <dbl>
tibble(lifestyle_ds[10006,])
#############################################################################################

# to find out the average for 
# 'PLACES_VISITED','CORE_CIRCLE','SUPPORTING_OTHERS','SOCIAL_NETWORK'
# for every age group. All values are in the range of [0,10]
lifestyle_ds %>% select(PLACES_VISITED,CORE_CIRCLE,
                        SUPPORTING_OTHERS,SOCIAL_NETWORK,AGE) %>% 
  filter(!is.na(PLACES_VISITED) & !is.na(CORE_CIRCLE) &
           !is.na(SUPPORTING_OTHERS) & !is.na(SOCIAL_NETWORK)) %>%
  group_by(AGE) %>% summarize(PLACES_VISITED_avg=mean(PLACES_VISITED),
                              CORE_CIRCLE_avg=mean(CORE_CIRCLE),
                              SUPPORTING_OTHERS_avg=mean(SUPPORTING_OTHERS),
                              SOCIAL_NETWORK_avg=mean(SOCIAL_NETWORK))

#############################################################################################

# to show the density for SLEEP_HOURS (range: [1,10])
lifestyle_ds %>% select(SLEEP_HOURS) %>% filter(!is.na(SLEEP_HOURS)) %>%
  ggplot(aes(SLEEP_HOURS)) + geom_density(fill="purple",adjust=3)

#############################################################################################

# to show WEEKLY_MEDITATION (range: [0,10]) per AGE
lifestyle_ds %>% select(WEEKLY_MEDITATION, AGE) %>% 
  filter(!is.na(WEEKLY_MEDITATION) & !is.na(AGE)) %>%
  ggplot(aes(AGE, WEEKLY_MEDITATION, col=AGE)) +
  geom_boxplot() + ggtitle("Number of meditations in a week")

#############################################################################################

# to show DAILY_STRESS (range: [0,10]) per AGE
lifestyle_ds %>% select(DAILY_STRESS, AGE) %>% 
  filter(!is.na(DAILY_STRESS) & !is.na(AGE)) %>%
  ggplot(aes(AGE, DAILY_STRESS, col=AGE)) +
  geom_boxplot() + ggtitle("Daily Stress")

#############################################################################################

wl_balance_1 <- lifestyle_ds %>% 
  select(WORK_LIFE_BALANCE_SCORE, DAILY_STRESS, AGE) %>%
  filter(!is.na(DAILY_STRESS) & !is.na(AGE) & 
         !is.na(WORK_LIFE_BALANCE_SCORE))
  
# to show 'WORK_LIFE_BALANCE_SCORE', 'DAILY_STRESS' correlation
mu_x <- mean(wl_balance_1$WORK_LIFE_BALANCE_SCORE)
mu_y <- mean(wl_balance_1$DAILY_STRESS)
s_x <- sd(wl_balance_1$WORK_LIFE_BALANCE_SCORE)
s_y <- sd(wl_balance_1$DAILY_STRESS)
r <- cor(wl_balance_1$WORK_LIFE_BALANCE_SCORE, wl_balance_1$DAILY_STRESS)

wl_balance_1 %>% ggplot(aes(WORK_LIFE_BALANCE_SCORE, DAILY_STRESS, col = AGE)) +
  geom_point(alpha = 0.5) + facet_wrap(~AGE,scales="free") +
  theme(legend.position = "none") +
  geom_abline(slope=r*s_y/s_x,intercept=mu_y-r*s_y/s_x*mu_x,col="maroon",size=1)+
  ggtitle("Wl-Balance & Daily Stress correlation")


# to compute 'WORK_LIFE_BALANCE_SCORE', 'DAILY_STRESS' correlation
# using Monte Carlo simulation. Visualize the result using a histogram and
# normal distribution plot

B <- 3000
N <- 25
R <- replicate(B, {
  sample_n(wl_balance_1, N, replace = TRUE) %>%
    summarize(r=cor(WORK_LIFE_BALANCE_SCORE, DAILY_STRESS)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
ggplot(aes(sample=R), data = data.frame(R)) + stat_qq(col="maroon") + 
  stat_qq_line(col="blue", size = 1) + xlab("Theorical") + ylab("Sample") + 
  ggtitle("Wl-Balance & Daily Stress correlation distribution")

#############################################################################################

wl_balance_1 <- lifestyle_ds %>% 
  select(WORK_LIFE_BALANCE_SCORE, SLEEP_HOURS, AGE) %>%
  filter(!is.na(SLEEP_HOURS) & !is.na(AGE) & 
           !is.na(WORK_LIFE_BALANCE_SCORE))

# to show 'WORK_LIFE_BALANCE_SCORE', 'SLEEP_HOURS' correlation
mu_x <- mean(wl_balance_1$WORK_LIFE_BALANCE_SCORE)
mu_y <- mean(wl_balance_1$SLEEP_HOURS)
s_x <- sd(wl_balance_1$WORK_LIFE_BALANCE_SCORE)
s_y <- sd(wl_balance_1$SLEEP_HOURS)
r <- cor(wl_balance_1$WORK_LIFE_BALANCE_SCORE, wl_balance_1$SLEEP_HOURS)

wl_balance_1 %>% ggplot(aes(WORK_LIFE_BALANCE_SCORE, SLEEP_HOURS, col=AGE)) +
  geom_point(alpha = 0.5) + facet_wrap(~AGE,scales="free") +
  theme(legend.position = "none") +
  geom_abline(slope=r*s_y/s_x,intercept=mu_y-r*s_y/s_x*mu_x,col="maroon",size=1)+
  ggtitle("Wl-Balance & Sleep Hours correlation")


# to compute 'WORK_LIFE_BALANCE_SCORE', 'SLEEP_HOURS' correlation
# using Monte Carlo simulation. Visualize the result using a histogram and
# normal distribution plot

B <- 3000
N <- 25
R <- replicate(B, {
  sample_n(wl_balance_1, N, replace = TRUE) %>%
    summarize(r=cor(WORK_LIFE_BALANCE_SCORE, SLEEP_HOURS)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
ggplot(aes(sample=R), data = data.frame(R)) + stat_qq(col="maroon") + 
  stat_qq_line(col="black", size=0.7)+xlab("Theorical")+ylab("Sample")+ 
  ggtitle("Wl-Balance & Sleep Hours correlation distribution")

#############################################################################################

wl_balance_1 <- lifestyle_ds %>% 
  select(WORK_LIFE_BALANCE_SCORE, WEEKLY_MEDITATION, AGE) %>%
  filter(!is.na(WEEKLY_MEDITATION) & !is.na(AGE) & 
           !is.na(WORK_LIFE_BALANCE_SCORE))

# to show 'WORK_LIFE_BALANCE_SCORE', 'WEEKLY_MEDITATION' correlation
mu_x <- mean(wl_balance_1$WORK_LIFE_BALANCE_SCORE)
mu_y <- mean(wl_balance_1$WEEKLY_MEDITATION)
s_x <- sd(wl_balance_1$WORK_LIFE_BALANCE_SCORE)
s_y <- sd(wl_balance_1$WEEKLY_MEDITATION)
r <- cor(wl_balance_1$WORK_LIFE_BALANCE_SCORE, wl_balance_1$WEEKLY_MEDITATION)

wl_balance_1 %>% 
  ggplot(aes(WORK_LIFE_BALANCE_SCORE, WEEKLY_MEDITATION, col=AGE)) +
  geom_point(alpha = 0.5) + facet_wrap(~AGE,scales="free") +
  theme(legend.position = "none") +
  geom_abline(slope=r*s_y/s_x,intercept=mu_y-r*s_y/s_x*mu_x,col="maroon",size=1)+
  ggtitle("Wl-Balance & Weekly Meditation correlation")


# to compute 'WORK_LIFE_BALANCE_SCORE', 'WEEKLY_MEDITATION' correlation
# using Monte Carlo simulation. Visualize the result using a histogram and
# normal distribution plot

B <- 3000
N <- 25
R <- replicate(B, {
  sample_n(wl_balance_1, N, replace = TRUE) %>%
    summarize(r=cor(WORK_LIFE_BALANCE_SCORE, WEEKLY_MEDITATION)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
ggplot(aes(sample=R), data = data.frame(R)) + stat_qq(col="maroon") + 
  stat_qq_line(col="black", size=0.7)+xlab("Theorical")+ylab("Sample")+ 
  ggtitle("Wl-Balance & Weekly Meditation correlation distribution")

#############################################################################################
