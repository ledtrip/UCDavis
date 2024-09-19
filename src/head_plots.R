install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

data <- read_csv("/data/temp_Head.csv")

head(data)
str(data)

# Create a function to generate the plots
plot <- ggplot(data, aes(x = days_to_head, y = cumul_head, color = Location, shape = source, label = variety)) +
  geom_point() +
  geom_line() +
  geom_text(vjust = -0.5, hjust = -0.5) +  # Add variety labels to the points
  facet_wrap(~ Year, scales = "free") +
  labs(title = "Cumulative Head vs Days to Head by Location for Each Year",
       x = "Days to Head",
       y = "Cumulative Head") +
  theme_minimal()

  
  
  pdf("plots.pdf")
  for(i in unique(data$Year)) {
    year_data <- data %>% filter(Year == i)
    print(plot_function(year_data) + ggtitle(paste("Year:", i)))
  }
  dev.off()
