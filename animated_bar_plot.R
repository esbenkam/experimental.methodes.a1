library(animation)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(tweenr)

# Read in the data set.
#data = read_csv(input_file,
#                col_names = TRUE,
#                col_types = cols(.default = col_character(),
#                                 Cost = col_number()))

data_year <- c(2007, 2007, 2007, 2007, 2007, 2007, 2012, 2012, 2012, 2012, 2012, 2012, 2017, 2017, 2017, 2017, 2017, 2017)
data <- data.frame(Year = as.character(data_year))

data_source <- c("Inpatient Care", "Outpatient Care", "Medication and Supplies", "Reduced Productivity", "Reduced Labor Force", "Early Mortality", "Inpatient Care", "Outpatient Care", "Medication and Supplies", "Reduced Productivity", "Reduced Labor Force", "Early Mortality", "Inpatient Care", "Outpatient Care", "Medication and Supplies", "Reduced Productivity", "Reduced Labor Force", "Early Mortality")
data$Source <- as.character(data_source)

data_cost <- c(65830, 22742, 27684, 23400, 7900, 26900, 90652, 31798, 52306, 28500, 21600, 18500, 76164, 54001, 107104, 32500, 37500, 19900)
data$Cost <- as.numeric(data_cost)

# Explicitly set the ordering of the factors for cost source.
source_levels = c("Inpatient Care", 
                  "Outpatient Care",
                  "Medication and Supplies",
                  "Reduced Productivity",
                  "Reduced Labor Force",
                  "Early Mortality")

# Clean the data, applying factors to columns.
data = data %>%
  mutate(Year = factor(Year),
         Source = factor(Source, levels = source_levels, ordered = TRUE))

# Compute the y labels.
max_cost = max(data$Cost)
max_cost_limit = ceiling(max_cost / 20000) * 20000
y_breaks = seq(0, max_cost_limit, 20000)
y_labels = format(y_breaks, big.mark = ",")

# Set the x axis limits.
x_limits = rev(levels(data$Source))

# Create a data list, with a data frame per year.
data_list = list()
index = 1
for (year in unique(data$Year)) {
  data_list[[index]] = data %>% filter(Year == year)
  index = index + 1
}

# Create our "tween" data set, based on the data list we just created.
tween_data = tween_states(data_list, 1, 2, "cubic-in-out", 120)

frames = sort(unique(tween_data$.frame))

saveGIF({
  for (frame in frames) {
    # Get the data specific to this frame.
    frame_data = tween_data %>% filter(.frame == frame)
    
    # Compute the title of the graph.
    year = frame_data$Year[[1]]
    sum_cost = data %>% filter(Year == year) %>% group_by(Year) %>% summarise(Sum = sum(Cost))
    sum_cost_fmt = format(sum_cost$Sum[[1]], big.mark = ",")
    title = paste("Cost of Diabetes", year, "Total US$", sum_cost_fmt, "million")
    cat(title, "\n")
    
    p = ggplot(frame_data, aes(Source, Cost, fill = Source)) +
      geom_bar(stat = "identity") + 
      scale_y_continuous(breaks = y_breaks,
                         expand = c(0, 0),
                         labels = y_labels,
                         limits = c(0, max_cost_limit)) +
      scale_fill_brewer(palette = "Pastel2", guide = FALSE) +
      scale_x_discrete(limits = x_limits) +
      ggtitle(title) + 
      xlab("") + 
      ylab("Cost (millions US$)") +
      coord_flip() + 
      theme_light() +
      theme(plot.margin = unit(c(0.2, 1, 0.2, 0.2), "cm"))
    print(p)
  }
}, movie.name = "~/Desktop/output_file.gif", interval = 0.01, ani.width = 720, ani.height = 480)




