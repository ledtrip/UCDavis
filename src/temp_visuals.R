
library(ggplot2)

pdf("outputs/temp_plots.pdf")
# par(mfrow = c(9, 9))

for (a in unique(temp$Year)) {
  year_a <- temp %>% filter(Year == a)
  
  for (b in unique(year_a$Location)){
   loc_b <- year_a %>% filter(Location == b)
   
   temp_plot_a_b <- ggplot(data = loc_b, aes(x = Date)) +
             geom_line(aes(y = LocMinTempF, color = "Local", linetype = "Local")) +
             geom_line(aes(y = LocMaxTempF, color = "Local", linetype = "Local")) +
             geom_line(aes(y = StatMinTempF, color = "CIMIS", linetype = "CIMIS")) +
             geom_line(aes(y = StatMaxTempF, color = "CIMIS", linetype = "CIMIS")) +
             geom_line(aes(y = PRISMMinTempF, color = "PRISM", linetype = "PRISM")) +
             geom_line(aes(y = PRISMaxTempF, color = "PRISM", linetype = "PRISM")) +
             scale_color_manual(values = c("Local" = "#4793AF", "Local" = "#4793AF", "CIMIS" = "#FFB000", 
                                           "CIMIS" = "#FFB000", "PRISM" = "#DD5746", "PRISM" = "#DD5746")) +
             scale_linetype_manual(values = c("Local" = "solid", "Local" = "solid", "CIMIS" = "solid",
                                              "CIMIS" = "solid", "PRISM" = "solid", "PRISM" = "solid")) +
             guides(linetype = guide_none()) +
             labs(title = paste("Year:", a, "- Location:", b),
                  x = "Date",
                  y = "Temperature (F)") +
             theme_minimal() +
             scale_y_continuous(
               limits = c(0, 120),  # Set the limits of the y-axis
               breaks = seq(-0, 120, by = 20))  # Define the tick marks
   
   print(temp_plot_a_b)
  }
} 

# par(mfrow = c(1,1))
dev.off()