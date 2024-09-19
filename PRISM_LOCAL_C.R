library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Load the dataset
data <- read.csv("data/PRISM_LOCAL_C.csv")

# Convert temperatures from Fahrenheit to Celsius
data <- data %>%
  mutate(
    LocMinTempC = (LocMinTempF - 32) * 5/9,
    LocMaxTempC = (LocMaxTempF - 32) * 5/9,
    PRISMMinTempC = (PRISMMinTempF - 32) * 5/9,
    PRISMMaxTempC = (PRISMMaxTempF - 32) * 5/9
  )

# Create new columns for temperature differences
data <- data %>%
  mutate(
    MinDif_PRISMLOC_C = PRISMMinTempC - LocMinTempC,
    MaxDif_PRISMLOC_C = PRISMMaxTempC - LocMaxTempC,
    AvgDif_PRISMLOC_C = (MinDif_PRISMLOC_C + MaxDif_PRISMLOC_C) / 2
  )

# Filter out points where MINDif_OUT or MAXDif_OUT is 1
filtered_data <- data %>%
  filter(MINDif_OUT == 0 & MAXDif_OUT == 0)

# Convert Date column to Date class
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")  # Adjust format if necessary
filtered_data$Date <- as.Date(filtered_data$Date, format = "%Y-%m-%d")  # Adjust format if necessary

# Define plotting functions for min, max, and avg temperature differences
plot_MINdif <- function(data, year, location) {
  ggplot(data, aes(x = Date, y = MinDif_PRISMLOC_C, group = 1)) + 
    geom_line() + 
    geom_point() +
    labs(
      title = paste("PRISM minus Local MIN temperatures for Year", year, 
                    "; Location:", location, "-", unique(data$County)),
      x = "Date",
      y = "Temperature Difference (C)"
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_minimal()
}

plot_MAXdif <- function(data, year, location) {
  ggplot(data, aes(x = Date, y = MaxDif_PRISMLOC_C, group = 1)) + 
    geom_line() + 
    geom_point() +
    labs(
      title = paste("PRISM minus Local MAX temperatures for Year", year, 
                    "; Location:", location, "-", unique(data$County)),
      x = "Date",
      y = "Temperature Difference (C)"
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_minimal()
}

plot_AVGdif <- function(data, year, location) {
  ggplot(data, aes(x = Date, y = AvgDif_PRISMLOC_C, group = 1)) + 
    geom_line() + 
    geom_point() +
    labs(
      title = paste("PRISM minus Local AVG temperatures for Year", year, 
                    "; Location:", location, "-", unique(data$County)),
      x = "Date",
      y = "Average Temperature Difference (C)"
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_minimal()
}

# Original Plots (no filtering)
pdf("outputs/PRISM_LOCAL_MIN_Differences_Original_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotMIN_C <- data %>% 
      select(Location, Date, County, Year, MinDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMIN_C) > 0) {  
      plot2 <- plot_MINdif(med_plotMIN_C, a, b)  
      print(plot2)  
    }
  }
}
dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences_Original_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotMAX_C <- data %>% 
      select(Location, Date, County, Year, MaxDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMAX_C) > 0) {  
      plot3 <- plot_MAXdif(med_plotMAX_C, a, b)  
      print(plot3)  
    }
  }
}
dev.off()

pdf("outputs/PRISM_LOCAL_AVG_Differences_Original_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotAVG_C <- data %>% 
      select(Location, Date, County, Year, AvgDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotAVG_C) > 0) {  
      plot4 <- plot_AVGdif(med_plotAVG_C, a, b)  
      print(plot4)  
    }
  }
}
dev.off()

# Filtered Plots (MINDif_OUT and MAXDif_OUT == 0)
pdf("outputs/PRISM_LOCAL_MIN_Differences_Filtered_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotMIN_C <- filtered_data %>% 
      select(Location, Date, County, Year, MinDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMIN_C) > 0) {  
      plot2 <- plot_MINdif(med_plotMIN_C, a, b)  
      print(plot2)  
    }
  }
}
dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences_Filtered_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotMAX_C <- filtered_data %>% 
      select(Location, Date, County, Year, MaxDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMAX_C) > 0) {  
      plot3 <- plot_MAXdif(med_plotMAX_C, a, b)  
      print(plot3)  
    }
  }
}
dev.off()

pdf("outputs/PRISM_LOCAL_AVG_Differences_Filtered_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotAVG_C <- filtered_data %>% 
      select(Location, Date, County, Year, AvgDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotAVG_C) > 0) {  
      plot4 <- plot_AVGdif(med_plotAVG_C, a, b)  
      print(plot4)  
    }
  }
}
dev.off()

# Define plotting functions with solid quadratic fit for average temperature differences
plot_AVGdif_with_fit <- function(data, year, location) {
  ggplot(data, aes(x = Date, y = AvgDif_PRISMLOC_C, group = 1)) + 
    geom_line() + 
    geom_point() +
    labs(
      title = paste("PRISM minus Local AVG temperatures for Year", year, 
                    "; Location:", location, "-", unique(data$County)),
      x = "Date",
      y = "Average Temperature Difference (C)"
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(breaks = seq(floor(min(data$AvgDif_PRISMLOC_C, na.rm = TRUE)), 
                                    ceiling(max(data$AvgDif_PRISMLOC_C, na.rm = TRUE)), 
                                    by = 1)) +  # Whole number increments
    theme_minimal() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue", linetype = "solid")  # Solid fit line
}

# Create a new PDF for average temperature differences with solid quadratic fit
pdf("outputs/PRISM_LOCAL_AVG_Differences_Quadratic_Fit_C.pdf", width = 10, height = 6)
for (b in location2) {   
  for (a in years2) {   
    med_plotAVG_C <- data %>% 
      select(Location, Date, County, Year, AvgDif_PRISMLOC_C) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotAVG_C) > 0) {  
      plot4 <- plot_AVGdif_with_fit(med_plotAVG_C, a, b)  
      print(plot4)  
    }
  }
}
dev.off()
