library(ggplot2)
library(readxl)
library(degday)
library(dplyr)
library(tidyr)
library(writexl)
library(lubridate)
library(stringr)  # For string extraction functions

# 1. Value definitions ####
temp <- read_xlsx("data/Local Temperatures Datasheet.xlsx")
temp$`Plant_Date` <- as.Date(temp$`Plant_Date`)
temp$Date <- as.Date(temp$Date)

temp <- merge(temp, PRISMmerge, by = c("Location", "Date"), all = TRUE)

## 1.1. Temp thresholds ####
thresh_low <- 50
thresh_up <- 95

temp <- temp %>%
          mutate(sng_tri_loc = dd_sng_tri(daily_min = LocMinTempF, daily_max = LocMaxTempF, 
                                          thresh_low = thresh_low, thresh_up = thresh_up),
                 sng_sine_loc = dd_sng_sine(daily_min = LocMinTempF, daily_max = LocMaxTempF, 
                                          thresh_low = thresh_low, thresh_up = thresh_up),
                 sng_tri_stat = dd_sng_tri(daily_min = StatMinTempF, daily_max = StatMaxTempF, 
                                          thresh_low = thresh_low, thresh_up = thresh_up),
                 sng_sine_stat = dd_sng_sine(daily_min = StatMinTempF, daily_max = StatMaxTempF, 
                                          thresh_low = thresh_low, thresh_up = thresh_up),
                 sng_tri_prism = dd_sng_tri(daily_min = PRISMMinTempF, daily_max = PRISMaxTempF, 
                                           thresh_low = thresh_low, thresh_up = thresh_up),
                 sng_sine_prism = dd_sng_sine(daily_min = PRISMMinTempF, daily_max = PRISMaxTempF, 
                                             thresh_low = thresh_low, thresh_up = thresh_up))

temp$sng_tri_loc <- ifelse(is.na(temp$sng_tri_loc), 0, temp$sng_tri_loc) # skipping over input data NAs to continue cumulative sums
temp$sng_sine_loc <- ifelse(is.na(temp$sng_sine_loc), 0, temp$sng_sine_loc)
temp$sng_tri_stat <- ifelse(is.na(temp$sng_tri_stat), 0, temp$sng_tri_stat)
temp$sng_sine_stat <- ifelse(is.na(temp$sng_sine_stat), 0, temp$sng_sine_stat)
temp$sng_tri_prism <- ifelse(is.na(temp$sng_tri_prism), 0, temp$sng_tri_prism)
temp$sng_sine_prism <- ifelse(is.na(temp$sng_sine_prism), 0, temp$sng_sine_prism)

## 1.2. Data checks ####
# temp2 <- temp %>%
#   mutate(tempdif_loc = LocMaxTempF - LocMinTempF,
#          tempdif_stat = StatMaxTempF - StatMinTempF,
#          tempdif_prism = PRISMaxTempF - PRISMMinTempF)
# 
# temp3 <- temp2 %>%              # backwards local temps
#   filter(temp2$tempdif_loc < 0)
# 
# temp4 <- temp2 %>%              # backwards stat temps
#   filter(temp2$tempdif_stat < 0)
# 
# temp5 <- temp2 %>%              # backwards stat temps
#   filter(temp2$tempdif_prism < 0)

## 1.3. Cumulative deg days per date #### 
temp <- temp %>%
          group_by(Location, Year) %>% 
          mutate(cum_sng_tri_loc = cumsum(sng_tri_loc),
                 cum_sng_sine_loc = cumsum(sng_sine_loc),
                 cum_sng_tri_stat = cumsum(sng_tri_stat),
                 cum_sng_sine_stat = cumsum(sng_sine_stat),
                 cum_sng_tri_prism = cumsum(sng_tri_prism),
                 cum_sng_sine_prism = cumsum(sng_sine_prism))

## 1.4. Summarizing total deg days by location #### 
dd_loc <- temp %>% 
          group_by(Location) %>% 
          summarize(totdd_tri_loc = sum(sng_tri_loc, na.rm = TRUE),
                    totdd_sine_loc = sum(sng_sine_loc, na.rm = TRUE),
                    totdd_tri_stat = sum(sng_tri_stat, na.rm = TRUE),
                    totdd_sine_stat = sum(sng_sine_stat, na.rm = TRUE),
                    totdd_tri_prism = sum(sng_tri_prism, na.rm = TRUE),
                    totdd_sine_prism = sum(sng_sine_prism, na.rm = TRUE))

## 1.5. Summarizing total deg days by growth stage change #### 

# Reshape the data to long format
dd_stage <- temp %>%
            pivot_longer(cols = starts_with("H"), names_to = "Var", values_to = "Stage") %>%
            arrange(Year, Location, Var, cum_sng_tri_loc, cum_sng_sine_loc, cum_sng_tri_stat, cum_sng_sine_stat, cum_sng_tri_prism, cum_sng_sine_prism)

# Find the cumulative value where "preHead" changes to "Head"
dd_stage1 <- dd_stage %>%
          group_by(Year, Location, Var) %>%
          filter(Stage == "Heading" & lag(Stage) == "preHeading") %>%
          summarize(start_Head_sng_tri_loc = first(cum_sng_tri_loc), .groups = 'drop')

dd_stage2 <- dd_stage %>%
          group_by(Year, Location, Var) %>%
          filter(Stage == "Heading" & lag(Stage) == "preHeading") %>%
          summarize(start_Head_sng_sine_loc = first(cum_sng_sine_loc), .groups = 'drop')

dd_stage3 <- dd_stage %>%
          group_by(Year, Location, Var) %>%
          filter(Stage == "Heading" & lag(Stage) == "preHeading") %>%
          summarize(start_Head_sng_tri_stat = first(cum_sng_tri_stat), .groups = 'drop')
  
dd_stage4 <- dd_stage %>%
          group_by(Year, Location, Var) %>%
          filter(Stage == "Heading" & lag(Stage) == "preHeading") %>%
          summarize(start_Head_sng_sine_stat = first(cum_sng_sine_stat), .groups = 'drop')

dd_stage5 <- dd_stage %>%
          group_by(Year, Location, Var) %>%
          filter(Stage == "Heading" & lag(Stage) == "preHeading") %>%
          summarize(start_Head_sng_tri_prism = first(cum_sng_tri_prism), .groups = 'drop')

dd_stage6 <- dd_stage %>%
          group_by(Year, Location, Var) %>%
          filter(Stage == "Heading" & lag(Stage) == "preHeading") %>%
          summarize(start_Head_sng_sine_prism = first(cum_sng_sine_prism), .groups = 'drop')

# Merge data frames 
dd_Heading <- dd_stage1 %>% 
          left_join(dd_stage2, by = c("Year", "Location", "Var")) %>% 
          left_join(dd_stage3, by = c("Year", "Location", "Var")) %>%
          left_join(dd_stage4, by = c("Year", "Location", "Var")) %>% 
          left_join(dd_stage5, by = c("Year", "Location", "Var")) %>%
          left_join(dd_stage6, by = c("Year", "Location", "Var"))

temp <- temp %>% filter(!is.na(Year) & Year != "2024")

# write_xlsx(dd_Heading, "outputs/dd_Heading.xlsx")  
# write_xlsx(temp, "outputs/temp2.xlsx")   

# 2. Heading Deg.days results comparison ####

## 2.1. Data frame for plots ####

calculate_days_to_head <- function(data) {
  result <- data.frame()
  
  # Get the list of cumulative columns
  cumul_cols <- grep("^cum_", names(data), value = TRUE)
  
  for (var in c("HeadM105", "HeadM206", "HeadM209", "HeadM210", "HeadM211", "HeadM401")) {
    for (cumul_col in cumul_cols) {
      # Identify the first "pre" and "head" dates and corresponding cumulative values
      head_dates <- data %>%
        filter(!!sym(var) == "Heading") %>%
        group_by(Location, Year) %>%
        summarize(
          first_head_date = min(Date),
          cumul_head = get(cumul_col)[which.min(Date)],
          .groups = 'drop'
        )
      
      pre_dates <- data %>%
        filter(!!sym(var) == "preHeading") %>%
        group_by(Location, Year) %>%
        summarize(
          first_pre_date = min(Date),
          .groups = 'drop'
        )
      
      # Join the head_dates and pre_dates
      combined <- left_join(head_dates, pre_dates, by = c("Location", "Year"))
      
      # Split the cumulative column name
      split_col <- str_split(cumul_col, "_", simplify = TRUE)
      
      # Extract `source` and `method` from the cumulative column name
      combined <- combined %>%
        mutate(days_to_head = as.numeric(difftime(first_head_date, first_pre_date, units = "days")),
               Variety = str_extract(var, "(?<=HeadM)\\d+"),
               source = split_col[, 4],  # Extract source part from the split column name
               method = split_col[, 3])  # Extract method part from the split column name
      
      result <- rbind(result, combined)
    }
  }
  
  result <- result %>%
    select(Location, Year, Variety, source, method, days_to_head, cumul_head)
  
  return(result)
}

temp_Head <- calculate_days_to_head(temp) # Apply the function to the data frame
write_xlsx(temp_Head, "outputs/temp_Head.xlsx")

temp_Head <- temp_Head %>% 
  mutate(County = case_when(Location == "BosworthRue" ~ "Yuba",
                             Location == "Canal" ~ "Colusa",
                             Location == "DelRio" ~ "San Joaquin", 
                             Location == "ErdmanGallagher" ~ "North Yolo", 
                             Location == "LarrabeeSheppard" ~ "North Butte", 
                             Location == "Lauppe" ~ "Sutter", 
                             Location == "Rehmann" ~ "South Yolo", 
                             Location == "Schohr" ~ "South Butte", 
                             Location == "Wylie" ~ "Glenn"))

## 2.2. Heading deg days plots ####

pdf("outputs/Head_cumdegs_plot2.pdf", height = 20, width = 20)

Head_cumdegs_plot2 <- ggplot(temp_Head, aes(x = interaction(source, method, Variety), y = cumul_head, color = Variety)) +
                      facet_wrap(~ County, scales = "free_x") +
                      geom_jitter(alpha = 0.5, width = 0.2, height = 0) +
                      stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
                      stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
                      labs(
                        x = "Grouped Factors (source-method-variety)",
                        y = "Cumulative Degree Days at Heading",
                        title = "Cumulative Degree Days at Heading by County"
                      ) +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Head_cumdegs_plot2)

dev.off()

## 2.3. Heading deg days plots ####

pdf("outputs/Head_days_plot2.pdf", height = 20, width = 20)

Head_days_plot2 <- ggplot(temp_Head, aes(x = Variety, y = days_to_head, color = Variety)) +
                    facet_wrap(~ County, scales = "free_x") +
                    geom_jitter(alpha = 0.5, width = 0.05, height = 0) +
                    stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
                    stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
                    labs(
                      x = "Variety",
                      y = "Days to Heading",
                      title = "Days to Heading by County"
                    ) +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Head_days_plot2)

dev.off()

## 2.4. Writing xlsx file ####

write.xlsx(temp, file = "outputs/temp_headdd.xlsx")

## 2.5. Heading deg days plots filtering Canal 2021 ####

temp_Head_filtered <- temp_Head %>%
  filter(!(Location == "Canal" & Year == 2021))

pdf("outputs/Head_cumdegs_plot2.pdf", height = 20, width = 20)

Head_cumdegs_plot2 <- ggplot(temp_Head_filtered, aes(x = interaction(source, method, Variety), y = cumul_head, color = Variety)) +
  facet_wrap(~ County, scales = "free_x") +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Grouped Factors (source-method-variety)",
    y = "Cumulative Degree Days at Heading",
    title = "Cumulative Degree Days at Heading by County"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Head_cumdegs_plot2)

dev.off()

## 2.6. Heading deg days plots filtering Canal 2021 ####

pdf("outputs/Head_days_plot2.pdf", height = 20, width = 20)

Head_days_plot2 <- ggplot(temp_Head_filtered, aes(x = Variety, y = days_to_head, color = Variety)) +
  facet_wrap(~ County, scales = "free_x") +
  geom_jitter(alpha = 0.5, width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Variety",
    y = "Days to Heading",
    title = "Days to Heading by County"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Head_days_plot2)

dev.off()

## 2.7. Heading deg days plots modifying size ####

# pdf("outputs/Head_days_plot3.pdf", height = 20, width = 20)

# Create a new dataset without Glenn County's Variety 401 for the error bars
temp_Head_filtered_no_errorbars <- temp_Head_filtered[!(temp_Head_filtered$County == "Glenn" & temp_Head_filtered$Variety == "401"), ]

Head_days_plot3 <- ggplot(temp_Head_filtered, aes(x = Variety, y = days_to_head, color = Variety)) +
  facet_wrap(~ County, scales = "free_x") +
  geom_jitter(alpha = 0.5, width = 0.05, height = 0) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    size = 3, 
    color = "black"
  ) +
  stat_summary(
    data = temp_Head_filtered_no_errorbars,  # Use the filtered dataset here
    fun.data = mean_sdl, 
    geom = "errorbar", 
    width = 0.2, 
    color = "black"
  ) +
  labs(
    x = "Variety",
    y = "Days to Heading",
    title = "Days to Heading by County"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 28),  
    axis.text.y = element_text(size = 28),                         
    axis.title.x = element_text(size = 28, face = "bold"),         
    axis.title.y = element_text(size = 28, face = "bold"),         
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    strip.text = element_text(size = 28, face = "bold"),           
    legend.text = element_text(size = 28),                         
    legend.title = element_text(size = 28, face = "bold"),         
    panel.grid.major = element_line(size = 1),                     
    panel.grid.minor = element_line(size = 0.5)                    
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4))  # Increase size of the points in the legend
  )

print(Head_days_plot3)

# dev.off()

## 2.8. Heading deg days plots modifying M removing legend and order ####

pdf("outputs/Head_days_plot4.pdf", height = 20, width = 20)

temp_Head_filtered <- temp_Head %>%
  filter(!(Location == "Canal" & Year == 2021))

# Create a new dataset without Glenn County's Variety 401 for the error bars
temp_Head_filtered_no_errorbars <- temp_Head_filtered[!(temp_Head_filtered$County == "Glenn" & temp_Head_filtered$Variety == "401"), ]

# Create a new column with "M" in front of each variety name
temp_Head_filtered$Variety_M <- paste0("M", temp_Head_filtered$Variety)

# Specify the correct desired order of counties
desired_order <- c("North Butte", "Glenn", "South Butte", "Colusa", "Yuba", "North Yolo", "Sutter", "South Yolo", "San Joaquin")

# Convert County to a factor with the specified levels
temp_Head_filtered$County <- factor(temp_Head_filtered$County, levels = desired_order)

# Create a new dataset without Glenn County's Variety 401 for the error bars
temp_Head_filtered_no_errorbars <- temp_Head_filtered[!(temp_Head_filtered$County == "Glenn" & temp_Head_filtered$Variety == "401"), ]

# Plot
Head_days_plot4 <- ggplot(temp_Head_filtered, aes(x = Variety_M, y = days_to_head)) +
  facet_wrap(~ County, scales = "free_x") +
  geom_jitter(alpha = 0.5, width = 0.05, height = 0) +  
  stat_summary(
    fun = mean, 
    geom = "point", 
    size = 3, 
    color = "black"
  ) +
  stat_summary(
    data = temp_Head_filtered_no_errorbars,  # Use the filtered dataset here
    fun.data = mean_sdl, 
    geom = "errorbar", 
    width = 0.2, 
    color = "black"
  ) +
  labs(
    x = "Variety",
    y = "Days to Heading",
    title = "Days to Heading by County"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 28),  
    axis.text.y = element_text(size = 28),                         
    axis.title.x = element_text(size = 28, face = "bold"),         
    axis.title.y = element_text(size = 28, face = "bold"),         
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    strip.text = element_text(size = 28, face = "bold"),           
    legend.position = "none",  # Remove legend
    panel.grid.major = element_line(size = 1),                     
    panel.grid.minor = element_line(size = 0.5)                    
  )

print(Head_days_plot4)

dev.off()