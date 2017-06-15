if (!dir.exists("data")) {
    dir.create("data")
}
unzip("activity.zip", exdir = "data")

activity_data <- read.csv("data/activity.csv")

library(dplyr)
library(ggplot2)
library(huxtable)
library(chron)

# mean of the total steps per day
#activity_summary <- activity_data %>%
#mutate_if(is.factor, as.Date) %>%
#group_by(date) %>%
#filter(!is.na(steps)) %>%
#summarise(total_steps = sum(steps))

#print(qplot(activity_summary$total_steps,
#geom = "histogram",
#bins = nrow(activity_summary),
#xlab = "Total Steps",
#ylab = "Count",
#col = I("red"),
#fill = I("green")))

#steps_table <- as.data.frame(activity_data %>%
#mutate_if(is.factor, as.character) %>%
#group_by(date) %>%
#filter(!is.na(steps)) %>%
#summarise(mean_steps = mean(steps),
#median_steps = median(steps)) %>%
#select(Date = date, Mean = mean_steps, Median = median_steps))

#xt <- xtable(steps_table,
#align = "llrr",
#digits = xdigits(steps_table),
#display = xdisplay(steps_table))

#print(xt,
#include.rownames = FALSE)

# activity pattern
#activity_pattern <- activity_data %>%
#mutate_if(is.factor, as.character) %>%
#group_by(interval) %>%
#summarise(average_steps = mean(steps, na.rm = TRUE))


#pattern_plot <- ggplot(data = activity_pattern, mapping = aes(interval, average_steps)) +
#geom_smooth(method = "lm", se = FALSE)
#print(pattern_plot)

missing_values <- count(activity_data[is.na(activity_data$steps),])

imputed_values <- activity_data %>%
    group_by(date) %>%
    summarise(average_steps = ceiling(mean(steps, na.rm = TRUE))) %>%
    mutate_if(is.numeric, funs(if_else(is.nan(.), 0.0, .))) %>%
    mutate_if(is.numeric, as.integer)

activity_tidy <- activity_data %>%
    inner_join(imputed_values, by = c("date")) %>%
    mutate(actual_steps = if_else(is.na(steps), average_steps, steps)) %>%
    select(date, interval, steps = actual_steps)

activity_split_pattern <- activity_tidy %>%
    mutate(weekday_weekend = if_else(is.weekend(date), "weekend", "weekday")) %>%
    mutate_at(vars(weekday_weekend), as.factor) %>%
    group_by(interval, weekday_weekend) %>%
    summarise(average_steps = mean(steps))

split_pattern_plot <- ggplot(activity_split_pattern) +
    geom_line(aes(x = interval, y = average_steps, color = weekday_weekend)) +
    facet_grid(weekday_weekend ~ .,
        labeller = as_labeller(c("weekday" = "Weekday", "weekend" = "Weekend"))) +
    theme_linedraw(base_size = 14) +
    theme(legend.position = "none") +
    xlab("Interval (5 min)") +
    ylab("Average Steps")
    

print(split_pattern_plot)