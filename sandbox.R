if (!dir.exists("data")) {
    dir.create("data")
}
unzip("activity.zip", exdir = "data")

activity_data <- read.csv("data/activity.csv")

library(dplyr)
library(ggplot2)
library(xtable)

# mean of the total steps per day
activity_summary <- activity_data %>%
    mutate_if(is.factor, as.Date) %>%
    group_by(date) %>%
    filter(!is.na(steps)) %>%
    summarise(total_steps = sum(steps))

print(qplot(activity_summary$total_steps,
    geom = "histogram",
    bins = nrow(activity_summary),
    xlab = "Total Steps",
    ylab = "Count",
    col = I("red"),
    fill = I("green")))

steps_table <- as.data.frame(activity_data %>%
    mutate_if(is.factor, as.character) %>%
    group_by(date) %>%
    filter(!is.na(steps)) %>%
    summarise(mean_steps = mean(steps),
        median_steps = median(steps)) %>%
    select(Date = date, Mean = mean_steps, Median = median_steps))

xt <- xtable(steps_table,
    align = "llrr",
    digits = xdigits(steps_table),
    display = xdisplay(steps_table))

print(xt,
    include.rownames = FALSE)

# activity pattern
activity_pattern <- as.data.frame(activity_data %>%
    mutate_if(is.factor, as.character) %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps)) %>%
    select(Interval = interval, Steps = mean_steps))

