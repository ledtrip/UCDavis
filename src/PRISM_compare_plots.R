library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

data <- read.csv("outputs/PRISM_CIMIS.csv")

# 1. Plots per year and Location ####

data_long <- data %>%
  pivot_longer(cols = c(PRISMAVGTempF, PRISMMaxTempF, CIMISAVGTempF, CIMISMaxTempF), 
               names_to = "Temperature_Type", 
               values_to = "Temperature") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

plot_for_year_combined <- function(data) {
                  ggplot(data, aes(x=date, y=Temperature, color=Temperature_Type, shape=Temperature_Type)) +
                    geom_point() +
                    geom_line(aes(group=Temperature_Type)) +
                    labs(title=paste("PRISM vs CIMIS Temperatures for Year", a, "; Location: ", b),
                         x="Date",
                         y="Temperature",
                         color="Temperature Type",
                         shape="Temperature Type") +
                    scale_color_manual(values = c("PRISMAVGTempF" = "#4793AF", "PRISMMaxTempF" = "#4793AF", 
                                                  "CIMISAVGTempF" = "#DD5746", "CIMISMaxTempF" = "#DD5746")) +
                    theme_AVGimal() +
                    theme(legend.position="bottom")
}

years <- unique(data$year)
location <- unique (data$Location)

pdf("outputs/PRISM_CIMIS_Temperatures.pdf", width = 10, height = 6)

for (b in location) {
for (a in years) {
  data_year2 <- data_long %>% filter(year == a, Location == b)
  plot <- plot_for_year_combined(data_year2)
  print(plot)
}
}
  
dev.off()

# 2. Plotting difference in between Local us PRISM #### 

PRISM_LOCAL <- temp %>% 
  select("Location", "Date", "Year", "County", "LocMinTempF", "LocMaxTempF", "PRISMMinTempF", "PRISMaxTempF") %>% 
  mutate(MINDif_PRISMLOC = LocMinTempF - PRISMMinTempF,
         MAXDif_PRISMLOC = LocMaxTempF - PRISMaxTempF)

# plot_AVGdif <- function(data) {
#                       ggplot(data, aes(x = Date, y = AVGDif_PRISMLOC)) +
#                         geom_point() +
#                         geom_line() +
#                         labs(title=paste("Local minus PRISM AVG temperatures for Year ", a, "; Location: ", b),
#                              x="Date",
#                              y="Temperature") +
#                         # scale_color_manual(values = c("PRISMAVGTempF" = "#4793AF", "PRISMMaxTempF" = "#4793AF", 
#                         #                               "CIMISAVGTempF" = "#DD5746", "CIMISMaxTempF" = "#DD5746")) +
#                         theme_AVGimal() +
#                         theme(legend.position="bottom")
# }
# 
# plot_MAXdif <- function(data) {
#   ggplot(data, aes(x = Date, y = MAXDif_PRISMLOC)) +
#     geom_point() +
#     geom_line() +
#     labs(title=paste("Local AVGus PRISM MAX temperatures for Year ", a, "; Location: ", b),
#          x="Date",
#          y="Temperature") +
#     # scale_color_manual(values = c("PRISMAVGTempF" = "#4793AF", "PRISMMaxTempF" = "#4793AF", 
#     #                               "CIMISAVGTempF" = "#DD5746", "CIMISMaxTempF" = "#DD5746")) +
#     theme_AVGimal() +
#     theme(legend.position="bottom")
# }

years2 <- unique(PRISM_LOCAL$Year)
location2 <- unique(PRISM_LOCAL$Location)
# dif <- c("AVGDif_PRISMLOC", "MAXDif_PRISMLOC")

pdf("outputs/PRISM_LOCAL_AVG_Differences.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
            med_plotAVG <- PRISM_LOCAL %>% select(Location, Date, Year, AVGDif_PRISMLOC) %>% filter(Year == a, Location == b)
            plot2 <- plot_AVGdif(med_plotAVG)
            print(plot2)
  }}

dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
      med_plotMAX <- PRISM_LOCAL %>% select(Location, Date, Year, MAXDif_PRISMLOC) %>% filter(Year == a, Location == b)
      plot3 <- plot_MAXdif(med_plotMAX)
      print(plot3)
    }}

dev.off()

# 3. Plotting difference in between PRISM minus Local #### 

library(dplyr)
library(ggplot2)

## 3.1 Analysis with Outliers ####

PRISM_LOCAL <- temp %>% 
  select("Location", "Date", "Year", "County", "LocAVGTempF", "LocMaxTempF", "PRISMAVGTempF", "PRISMaxTempF") %>% 
  mutate(MINDif_PRISMLOC = PRISMMINTempF - LocMINTempF,
         MAXDif_PRISMLOC = PRISMaxTempF - LocMaxTempF,
         AVGDif_PRISMLOC = (AVGDif_PRISMLOC + MAXDif_PRISMLOC)/2)

plot_MINdif <- function(data, year, location) {
  ggplot(data, aes(x = Date, y = MINDif_PRISMLOC)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, color = "black") +
    labs(title=paste("PRISM minus Local MIN temperatures for Year", year, "; Location:", location, " - ", data$County, " County"),
         x="Date",
         y="Temperature Difference (F)") +
    theme_MINimal() +
    theme(legend.position="bottom")
}

plot_MAXdif <- function(data, year, location) {
  ggplot(data, aes(x = Date, y = MAXDif_PRISMLOC)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, color = "black") +
    labs(title=paste("PRISM AVGus Local MAX temperatures for Year", year, "; Location:", location, " - ", data$County, " County"),
         x="Date",
         y="Temperature Difference (F)") +
    theme_AVGimal() +
    theme(legend.position="bottom")
}

plot_AVGdif <- function(data, year, location) {
                mod_quadAVG <- lm(AVGDif_PRISMLOC ~ poly(Date, 2, raw = TRUE), data = data)
                summary_quad <- summary(mod_quadAVG)
                coeff_quad <- coef(mod_quadAVG)
                r_sqr_quad <- summary_quad$r.squared
                
                eq_quad <- paste("y = ",
                                 round(coeff_quad[1], 2), "+",
                                 round(coeff_quad[2], 2), "x +",
                                 format(coeff_quad[3], scientific = TRUE, digits = 2), "x^2",
                                 ", R2 =", round(r_sqr_quad, 3))
                
                x_pos <- AVG(data$Date) + 0.5 * (max(data$Date) - AVG(data$Date))
                
                ggplot(data, aes(x = Date, y = AVGDif_PRISMLOC)) +
                  geom_point() +
                  geom_line() +
                  geom_hline(yintercept = 0, color = "black") +
                  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "blue", se = FALSE) +
                  annotate("text", x = x_pos, y = -6, label = eq_quad) + #, hjust = 1, vjust = 1, color = "blue", size = 4) +
                  labs(title=paste("PRISM minus Local AVG temperatures for Year", year, "; Location:", location, " - ", data$County, " County"),
                       x="Date",
                       y="Temperature Difference (F)") +
                  theme_AVGimal() +
                  theme(legend.position="bottom")
}

years2 <- unique(PRISM_LOCAL$Year)
location2 <- unique(PRISM_LOCAL$Location)

pdf("outputs/PRISM_LOCAL_MIN_Differences2.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotMIN <- PRISM_LOCAL %>% 
      select(Location, Date, County, Year, MINDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMIN) > 0) {
      plot2 <- plot_MINdif(med_plotMIN, a, b)
      print(plot2)
    }
  }
}

dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences2.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotMAX <- PRISM_LOCAL %>% 
      select(Location, Date, County, Year, MAXDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMAX) > 0) {
      plot3 <- plot_MAXdif(med_plotMAX, a, b)
      print(plot3)
    }
  }
}

dev.off()

pdf("outputs/PRISM_LOCAL_AVG_Differences2.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotAVG <- PRISM_LOCAL %>% 
      select(Location, Date, County, Year, AVGDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotAVG) > 0) {
      plot4 <- plot_AVGdif(med_plotAVG, a, b)
      print(plot4)
    }
  }
}

dev.off()

## 3.2 Analysis without Outliers ####

PRISM_LOCAL_nOut <- read.csv("data/PRISM_LOCAL.csv") %>% 
                    filter(MINDif_OUT == 0,
                           MAXDif_OUT == 0)

PRISM_LOCAL_nOut <- PRISM_LOCAL_nOut %>% 
  select("Location", "Date", "Year", "County", "LocMinTempF", "LocMaxTempF", "PRISMMinTempF", "PRISMaxTempF", "MINDif_PRISMLOC", "MAXDif_PRISMLOC") %>% 
  mutate(AVGDif_PRISMLOC = (MINDif_PRISMLOC + MAXDif_PRISMLOC)/2) 

PRISM_LOCAL_nOut$Date <- as.Date(PRISM_LOCAL_nOut$Date, format = "%Y-%m-%d")

temp_sel <- temp %>% 
  select(Location, Plant_Date) %>% 
  mutate(LocYear = paste0(Location, Year))
temp_sel$Year <- NULL
temp_sel$Location <- NULL

PRISM_LOCAL_nOut <- PRISM_LOCAL_nOut %>% 
  mutate(LocYear = paste0(Location, Year)) 
  
temp_sel <- temp_sel %>% distinct(LocYear, Plant_Date)

PRISM_LOCAL_nOut <- PRISM_LOCAL_nOut %>% 
                    left_join(temp_sel, by = "LocYear")
# 
# plot_MINdif <- function(data, year, location) {
#   ggplot(data, aes(x = Date, y = MINDif_PRISMLOC)) +
#     geom_point() +
#     geom_line() +
#     geom_hline(yintercept = 0, color = "black") +
#     labs(title=paste("PRISM minus Local MIN temperatures for Year", year, "; Location:", location, " - ", data$County, " County"),
#          x="Date",
#          y="Temperature Difference (F)") +
#     theme_MINimal() +
#     theme(legend.position="bottom")
# }
# 
# plot_MAXdif <- function(data, year, location) {
#   ggplot(data, aes(x = Date, y = MAXDif_PRISMLOC)) +
#     geom_point() +
#     geom_line() +
#     geom_hline(yintercept = 0, color = "black") +
#     labs(title=paste("PRISM minus Local MAX temperatures for Year", year, "; Location:", location, " - ", data$County, " County"),
#          x="Date",
#          y="Temperature Difference (F)") +
#     theme_AVGimal() +
#     theme(legend.position="bottom")
# }
# 
# plot_AVGdif <- function(data, year, location) {
#   mod_quadAVG <- lm(AVGDif_PRISMLOC ~ poly(Date, 2, raw = TRUE), data = data)
#   summary_quad <- summary(mod_quadAVG)
#   coeff_quad <- coef(mod_quadAVG)
#   r_sqr_quad <- summary_quad$r.squared
#   
#   eq_quad <- paste("y = ",
#                    round(coeff_quad[1], 2), "+",
#                    round(coeff_quad[2], 2), "x +",
#                    format(coeff_quad[3], scientific = TRUE, digits = 2), "x^2",
#                    ", R2 =", round(r_sqr_quad, 3))
#   
#   x_pos <- AVG(data$Date) + 0.5 * (max(data$Date) - AVG(data$Date))
#   
#   ggplot(data, aes(x = Date, y = AVGDif_PRISMLOC)) +
#     geom_point() +
#     geom_line() +
#     geom_hline(yintercept = 0, color = "black") +
#     stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "blue", se = FALSE) +
#     annotate("text", x = x_pos, y = -6, label = eq_quad) + #, hjust = 1, vjust = 1, color = "blue", size = 4) +
#     labs(title=paste("PRISM minus Local AVG temperatures for Year", year, "; Location:", location, " - ", data$County, " County"),
#          x="Date",
#          y="Temperature Difference (F)") +
#     theme_AVGimal() +
#     theme(legend.position="bottom")
# }
# 
# years2 <- unique(PRISM_LOCAL_nOut$Year)
# location2 <- unique(PRISM_LOCAL_nOut$Location)

pdf("outputs/PRISM_LOCAL_MIN_Differences_nOut.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotMIN_nOut <- PRISM_LOCAL_nOut %>% 
      select(Location, Date, County, Year, MINDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMIN) > 0) {
      plot2 <- plot_MINdif(med_plotMIN_nOut, a, b)
      print(plot2)
    }
  }
}

dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences_nOut.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotMAX <- PRISM_LOCAL_nOut %>% 
      select(Location, Date, County, Year, MAXDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMAX) > 0) {
      plot3 <- plot_MAXdif(med_plotMAX, a, b)
      print(plot3)
    }
  }
}

dev.off()

pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotAVG <- PRISM_LOCAL_nOut %>% 
      select(Location, Date, County, Year, AVGDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotAVG) > 0) {
      plot4 <- plot_AVGdif(med_plotAVG, a, b)
      print(plot4)
    }
  }
}

dev.off()

## 3.3 Analysis without Outliers and without Blanks ####

pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotMIN_nOut <- PRISM_LOCAL_nOut %>% 
      select(Location, Date, County, Year, MINDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMIN_nOut) > 0) {
      plot2 <- plot_MINdif(med_plotMIN_nOut, a, b)
      print(plot2)
    }
  }
}

dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences_nOut.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotMAX <- PRISM_LOCAL_nOut %>% 
      select(Location, Date, County, Year, MAXDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotMAX) > 0) {
      plot3 <- plot_MAXdif(med_plotMAX, a, b)
      print(plot3)
    }
  }
}

dev.off()

pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut.pdf", width = 10, height = 6)

for (b in location2) {
  for (a in years2) {
    med_plotAVG <- PRISM_LOCAL_nOut %>% 
      select(Location, Date, County, Year, AVGDif_PRISMLOC) %>% 
      filter(Year == a, Location == b)
    
    if (nrow(med_plotAVG) > 0) {
      plot4 <- plot_AVGdif(med_plotAVG, a, b)
      print(plot4)
    }
  }
}

dev.off()

## 3.4 All in one plot - Analysis without Outliers and without Blanks ####

PRISM_LOCAL_nOut <- PRISM_LOCAL_nOut %>% 
  mutate(Date_ddmm = format(PRISM_LOCAL_nOut$Date, "%m-%d"))

all_dates <- format(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), by = "day"), "%m-%d")

PRISM_LOCAL_nOut$Date_ddmm <- format(PRISM_LOCAL_nOut$Date_ddmm, levels = all_dates)

## 3.4.1. Plots without quadratic model #### 

pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut_all.pdf", width = 10, height = 6)

plot_all_AVGdif <- ggplot(PRISM_LOCAL_nOut, aes(x = Date_ddmm, y = AVGDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                      geom_point() +
                      geom_line() +
                      geom_hline(yintercept = 0, color = "black") +
                      labs(title=paste("PRISM AVGus Local AVG temperatures for all years and locations"),
                           x="Date (month - day)",
                           y="Temperature Difference (F)",
                           color = "Year") +
                      theme_AVGimal() +
                      theme(legend.position="bottom") +
                      scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                            to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_AVGdif)                  

dev.off()

pdf("outputs/PRISM_LOCAL_MAX_Differences_nOut_all.pdf", width = 10, height = 6)

plot_all_MAXdif <- ggplot(PRISM_LOCAL_nOut, aes(x = Date_ddmm, y = MAXDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                      geom_point() +
                      geom_line() +
                      geom_hline(yintercept = 0, color = "black") +
                      labs(title=paste("PRISM AVGus Local MAX temperatures for all years and locations"),
                           x="Date (month - day)",
                           y="Temperature Difference (F)",
                           color = "Year") +
                      theme_AVGimal() +
                      theme(legend.position="bottom") +
                      scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                                to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_MAXdif)                  

dev.off()

pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut_all.pdf", width = 10, height = 6)

plot_all_AVGdif <- ggplot(PRISM_LOCAL_nOut, aes(x = Date_ddmm, y = AVGDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                      geom_point() +
                      geom_line() +
                      geom_hline(yintercept = 0, color = "black") +
                      labs(title=paste("PRISM AVGus Local AVG temperatures for all years and locations"),
                           x="Date (month - day)",
                           y="Temperature Difference (F)",
                           color = "Year") +
                      theme_AVGimal() +
                      theme(legend.position="bottom") +
                      scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                                to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_AVGdif)                  

dev.off()

## 3.4.1. Plots with quadratic model #### 

PRISM_LOCAL_nOut <- PRISM_LOCAL_nOut %>% 
  mutate(LocYear = paste0(Location, Year))


PRISM_LOCAL_nOut_filter <- PRISM_LOCAL_nOut %>% 
  filter(Location != "DelRio",
         LocYear != "Canal2021")

### wo/filtering DelRion and Canal2021 ####

### MAX 

PRISM_LOCAL_nOut$DayOfYear <- as.numeric(format(as.Date(paste0("2000-", PRISM_LOCAL_nOut$Date_ddmm), format="%Y-%m-%d"), "%j")) # new numeric element to use in quad mod

# Calculating the model:
mod_quadMAX_all <- lm(MAXDif_PRISMLOC ~ poly(DayOfYear, 2, raw = TRUE), data = PRISM_LOCAL_nOut)
summary_quadMAX_all <- summary(mod_quadMAX_all)
coeff_quadMAX_all <- coef(mod_quadMAX_all)
r_sqr_quadMAX_all <- summary_quadMAX_all$r.squared

eq_quadMAX_all <- paste("y = ",
                 round(coeff_quadMAX_all[1], 2), "+",
                 round(coeff_quadMAX_all[2], 2), "x +",
                 format(coeff_quadMAX_all[3], scientific = TRUE, digits = 2), "x^2",
                 ", R2 =", round(r_sqr_quadMAX_all, 3))

PRISM_LOCAL_nOut$Predicted_quadMAX <- predict(mod_quadMAX_all, 
                                              newdata = PRISM_LOCAL_nOut) # Creates a column with predicted values from the quadratic model to plot quad line

# restricting period to place quadratic model within the output plot:
start_day <- AVG(PRISM_LOCAL_nOut$DayOfYear) # = 114, we use this to correct quadratic line being ploted 114 days to the right in the output plot
end_day <- max(PRISM_LOCAL_nOut$DayOfYear)

# Plot:
pdf("outputs/PRISM_LOCAL_MAX_Differences_nOut_all_quad.pdf", width = 10, height = 6)

plot_all_quad_MAXdif <- ggplot(PRISM_LOCAL_nOut, aes(x = Date_ddmm, y = MAXDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                      geom_point() +
                      geom_line() +
                      geom_hline(yintercept = 0, color = "black") +
                      geom_line(aes(x = DayOfYear-114, y = Predicted_quadMAX), color = "black", linewidth = 1, 
                                data = subset(PRISM_LOCAL_nOut, DayOfYear >= start_day-114 & DayOfYear <= end_day)) +
                      annotate("text", x =  "09-01", y = -25, label = eq_quadMAX_all) +
                      labs(title=paste("PRISM minus Local MAX temperatures for all years and locations"),
                           x="Date (month - day)",
                           y="Temperature Difference (F)",
                           color = "Year") +
                      theme_AVGimal() +
                      theme(legend.position="bottom") +
                      scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                                to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_quad_MAXdif)                  

dev.off()

### MIN 

# Calculating the model:
mod_quadMIN_all <- lm(MINDif_PRISMLOC ~ poly(DayOfYear, 2, raw = TRUE), data = PRISM_LOCAL_nOut)
summary_quadMIN_all <- summary(mod_quadMIN_all)
coeff_quadMIN_all <- coef(mod_quadMIN_all)
r_sqr_quadMIN_all <- summary_quadMIN_all$r.squared

eq_quadMIN_all <- paste("y = ",
                        round(coeff_quadMIN_all[1], 2), "+",
                        round(coeff_quadMIN_all[2], 2), "x +",
                        format(coeff_quadMIN_all[3], scientific = TRUE, digits = 2), "x^2",
                        ", R2 =", round(r_sqr_quadMIN_all, 3))

PRISM_LOCAL_nOut$Predicted_quadMIN <- predict(mod_quadMIN_all, 
                                              newdata = PRISM_LOCAL_nOut) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_MIN_Differences_nOut_all_quad.pdf", width = 10, height = 6)

plot_all_quad_MINdif <- ggplot(PRISM_LOCAL_nOut, aes(x = Date_ddmm, y = MINDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                geom_point() +
                geom_line() +
                geom_hline(yintercept = 0, color = "black") +
                geom_line(aes(x = DayOfYear-114, y = Predicted_quadMIN), color = "black", linewidth = 1, 
                          data = subset(PRISM_LOCAL_nOut, DayOfYear >= start_day-114 & DayOfYear <= end_day)) +
                annotate("text", x =  "09-01", y = -17.5, label = eq_quadMIN_all) +
                labs(title=paste("PRISM minus Local MIN temperatures for all years and locations"),
                     x="Date (month - day)",
                     y="Temperature Difference (F)",
                     color = "Year") +
                theme_minimal() +
                theme(legend.position="bottom") +
                scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                          to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_quad_MINdif)                  

dev.off()

### AVG 

# Calculating the model:
mod_quadAVG_all <- lm(AVGDif_PRISMLOC ~ poly(DayOfYear, 2, raw = TRUE), data = PRISM_LOCAL_nOut)
summary_quadAVG_all <- summary(mod_quadAVG_all)
coeff_quadAVG_all <- coef(mod_quadAVG_all)
r_sqr_quadAVG_all <- summary_quadAVG_all$r.squared

eq_quadAVG_all <- paste("y = ",
                        round(coeff_quadAVG_all[1], 2), "+",
                        round(coeff_quadAVG_all[2], 2), "x +",
                        format(coeff_quadAVG_all[3], scientific = TRUE, digits = 2), "x^2",
                        ", R2 =", round(r_sqr_quadAVG_all, 3))

PRISM_LOCAL_nOut$Predicted_quadAVG <- predict(mod_quadAVG_all, 
                                              newdata = PRISM_LOCAL_nOut) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut_all_quad.pdf", width = 10, height = 6)

plot_all_quad_AVGdif <- ggplot(PRISM_LOCAL_nOut, aes(x = Date_ddmm, y = AVGDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                geom_point() +
                geom_line() +
                geom_hline(yintercept = 0, color = "black") +
                geom_line(aes(x = DayOfYear-114, y = Predicted_quadAVG), color = "black", linewidth = 1, 
                          data = subset(PRISM_LOCAL_nOut, DayOfYear >= start_day-114 & DayOfYear <= end_day)) +
                annotate("text", x =  "09-01", y = -17.5, label = eq_quadAVG_all) +
                labs(title=paste("PRISM minus Local AVG temperatures for all years and locations"),
                     x="Date (month - day)",
                     y="Temperature Difference (F)",
                     color = "Year") +
                theme_minimal() +
                theme(legend.position="bottom") +
                scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                          to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_quad_AVGdif)                  

dev.off()

### filtering DelRion and Canal2021 ####

PRISM_LOCAL_nOut_filter <- PRISM_LOCAL_nOut_filter %>% 
  mutate(Date_ddmm = format(PRISM_LOCAL_nOut_filter$Date, "%m-%d"))

all_dates <- format(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), by = "day"), "%m-%d")

PRISM_LOCAL_nOut_filter$Date_ddmm <- format(PRISM_LOCAL_nOut_filter$Date_ddmm, levels = all_dates)

### MAX 

PRISM_LOCAL_nOut_filter$DayOfYear <- as.numeric(format(as.Date(paste0("2000-", PRISM_LOCAL_nOut_filter$Date_ddmm), format="%Y-%m-%d"), "%j")) # new numeric element to use in quad mod

# Calculating the model:
mod_quadMAX_all <- lm(MAXDif_PRISMLOC ~ poly(DayOfYear, 2, raw = TRUE), data = PRISM_LOCAL_nOut_filter)
summary_quadMAX_all <- summary(mod_quadMAX_all)
coeff_quadMAX_all <- coef(mod_quadMAX_all)
r_sqr_quadMAX_all <- summary_quadMAX_all$r.squared

eq_quadMAX_all <- paste("y = ",
                        round(coeff_quadMAX_all[1], 2), "+",
                        round(coeff_quadMAX_all[2], 2), "x +",
                        format(coeff_quadMAX_all[3], scientific = TRUE, digits = 2), "x^2",
                        ", R2 =", round(r_sqr_quadMAX_all, 3))

PRISM_LOCAL_nOut_filter$Predicted_quadMAX <- predict(mod_quadMAX_all, 
                                              newdata = PRISM_LOCAL_nOut_filter) # Creates a column with predicted values from the quadratic model to plot quad line

# restricting period to place quadratic model within the output plot:
start_day <- min(PRISM_LOCAL_nOut_filter$DayOfYear) # = 114, we use this to correct quadratic line being ploted 114 days to the right in the output plot
end_day <- max(PRISM_LOCAL_nOut_filter$DayOfYear)

# Plot:
pdf("outputs/PRISM_LOCAL_MAX_Differences_nOut_all_quad_filtered.pdf", width = 10, height = 6)

plot_all_quad_MAXdif <- ggplot(PRISM_LOCAL_nOut_filter, aes(x = Date_ddmm, y = MAXDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                          geom_point() +
                          geom_line() +
                          geom_hline(yintercept = 0, color = "black") +
                          geom_line(aes(x = DayOfYear-114, y = Predicted_quadMAX), color = "black", linewidth = 1, 
                                    data = subset(PRISM_LOCAL_nOut_filter, DayOfYear >= start_day-114 & DayOfYear <= end_day)) +
                          annotate("text", x =  "09-01", y = -25, label = eq_quadMAX_all) +
                          labs(title=paste("PRISM minus Local MAX temperatures for all years and locations"),
                               x="Date (month - day)",
                               y="Temperature Difference (F)",
                               color = "Year") +
                          theme_minimal() +
                          theme(legend.position="bottom") +
                          scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                                    to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_quad_MAXdif)                  

dev.off()

### MIN 

# Calculating the model:
mod_quadMIN_all <- lm(MINDif_PRISMLOC ~ poly(DayOfYear, 2, raw = TRUE), data = PRISM_LOCAL_nOut_filter)
summary_quadMIN_all <- summary(mod_quadMIN_all)
coeff_quadMIN_all <- coef(mod_quadMIN_all)
r_sqr_quadMIN_all <- summary_quadMIN_all$r.squared

eq_quadMIN_all <- paste("y = ",
                        round(coeff_quadMIN_all[1], 2), "+",
                        round(coeff_quadMIN_all[2], 2), "x +",
                        format(coeff_quadMIN_all[3], scientific = TRUE, digits = 2), "x^2",
                        ", R2 =", round(r_sqr_quadMIN_all, 3))

PRISM_LOCAL_nOut_filter$Predicted_quadMIN <- predict(mod_quadMIN_all, 
                                              newdata = PRISM_LOCAL_nOut_filter) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_MIN_Differences_nOut_all_quad_filtered.pdf", width = 10, height = 6)

plot_all_quad_MINdif <- ggplot(PRISM_LOCAL_nOut_filter, aes(x = Date_ddmm, y = MINDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                        geom_point() +
                        geom_line() +
                        geom_hline(yintercept = 0, color = "black") +
                        geom_line(aes(x = DayOfYear-114, y = Predicted_quadMIN), color = "black", linewidth = 1, 
                                  data = subset(PRISM_LOCAL_nOut_filter, DayOfYear >= start_day-114 & DayOfYear <= end_day)) +
                        annotate("text", x =  "09-01", y = -17.5, label = eq_quadMIN_all) +
                        labs(title=paste("PRISM minus Local MIN temperatures for all years and locations"),
                             x="Date (month - day)",
                             y="Temperature Difference (F)",
                             color = "Year") +
                        theme_minimal() +
                        theme(legend.position="bottom") +
                        scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                                  to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_quad_MINdif)                  

dev.off()

### AVG 

# Calculating the model:
mod_quadAVG_all <- lm(AVGDif_PRISMLOC ~ poly(DayOfYear, 2, raw = TRUE), data = PRISM_LOCAL_nOut_filter)
summary_quadAVG_all <- summary(mod_quadAVG_all)
coeff_quadAVG_all <- coef(mod_quadAVG_all)
r_sqr_quadAVG_all <- summary_quadAVG_all$r.squared

eq_quadAVG_all <- paste("y = ",
                        round(coeff_quadAVG_all[1], 2), "+",
                        round(coeff_quadAVG_all[2], 2), "x +",
                        format(coeff_quadAVG_all[3], scientific = TRUE, digits = 2), "x^2",
                        ", R2 =", round(r_sqr_quadAVG_all, 3))

PRISM_LOCAL_nOut_filter$Predicted_quadAVG <- predict(mod_quadAVG_all, 
                                              newdata = PRISM_LOCAL_nOut_filter) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut_all_quad_filtered.pdf", width = 10, height = 6)

plot_all_quad_AVGdif <- ggplot(PRISM_LOCAL_nOut_filter, aes(x = Date_ddmm, y = AVGDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                          geom_point() +
                          geom_line() +
                          geom_hline(yintercept = 0, color = "black") +
                          geom_line(aes(x = DayOfYear-114, y = Predicted_quadAVG), color = "black", linewidth = 1, 
                                    data = subset(PRISM_LOCAL_nOut_filter, DayOfYear >= start_day-114 & DayOfYear <= end_day)) +
                          annotate("text", x =  "09-01", y = -17.5, label = eq_quadAVG_all) +
                          labs(title=paste("PRISM minus Local AVG temperatures for all years and locations"),
                               x="Date (month - day)",
                               y="Temperature Difference (F)",
                               color = "Year") +
                          theme_minimal() +
                          theme(legend.position="bottom") +
                          scale_x_discrete(breaks = format(seq.Date(from = as.Date("2000-01-01"),
                                                                    to = as.Date("2000-12-31"), by = "month"), "%m-%d"))
print(plot_all_quad_AVGdif)                  

dev.off()

## 3.4.2. Plotting temp diff vs Days after Planting #### 

PRISM_LOCAL_nOut_filter <- PRISM_LOCAL_nOut_filter %>% 
  mutate(DAP = difftime(Date, Plant_Date, units = "days"))

PRISM_LOCAL_nOut_filter$DAP <- as.factor(PRISM_LOCAL_nOut_filter$DAP)

### MAX 

# Calculating the model:
mod_quadMAX_alldap <- lm(MAXDif_PRISMLOC ~ poly(DAP, 2, raw = TRUE), data = PRISM_LOCAL_nOut_filter)
summary_quadMAX_alldap <- summary(mod_quadMAX_alldap)
coeff_quadMAX_alldap <- coef(mod_quadMAX_alldap)
r_sqr_quadMAX_alldap <- summary_quadMAX_alldap$r.squared

eq_quadMAX_alldap <- paste("y = ",
                        round(coeff_quadMAX_alldap[1], 2), "+",
                        round(coeff_quadMAX_alldap[2], 2), "x +",
                        format(coeff_quadMAX_alldap[3], scientific = TRUE, digits = 2), "x^2",
                        ", R2 =", round(r_sqr_quadMAX_alldap, 3))

PRISM_LOCAL_nOut_filter$Predicted_quadMAXdap <- predict(mod_quadMAX_alldap,
                                                     newdata = PRISM_LOCAL_nOut_filter) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_MAX_Differences_nOut_all_quad_filtereddap.pdf", width = 10, height = 6)

plot_all_quad_MAXdifdap <- ggplot(PRISM_LOCAL_nOut_filter, aes(x = DAP, y = MAXDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                geom_point() +
                geom_line() +
                geom_hline(yintercept = 0, color = "black") +
                geom_point(aes(x = DAP, y = Predicted_quadMAXdap), color = "black", size = 0.3) +
                annotate("text", x =  80, y = -25, label = eq_quadMAX_alldap) +
                labs(title=paste("PRISM minus Local MAX temperatures for all years and locations"),
                     x="Days after planting",
                     y="Temperature Difference (F)",
                     color = "Year") +
                theme_minimal() +
                theme(legend.position="bottom") +
                scale_x_discrete(breaks = seq(0, 180, by = 10)) 

print(plot_all_quad_MAXdifdap)                  

dev.off()

### MIN 

# Calculating the model:
mod_quadMIN_alldap <- lm(MINDif_PRISMLOC ~ poly(DAP, 2, raw = TRUE), data = PRISM_LOCAL_nOut_filter)
summary_quadMIN_alldap <- summary(mod_quadMIN_alldap)
coeff_quadMIN_alldap <- coef(mod_quadMIN_alldap)
r_sqr_quadMIN_alldap <- summary_quadMIN_alldap$r.squared

eq_quadMIN_alldap <- paste("y = ",
                           round(coeff_quadMIN_alldap[1], 2), "+",
                           round(coeff_quadMIN_alldap[2], 2), "x +",
                           format(coeff_quadMIN_alldap[3], scientific = TRUE, digits = 2), "x^2",
                           ", R2 =", round(r_sqr_quadMIN_alldap, 3))

PRISM_LOCAL_nOut_filter$Predicted_quadMINdap <- predict(mod_quadMIN_alldap,
                                                        newdata = PRISM_LOCAL_nOut_filter) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_MIN_Differences_nOut_all_quad_filtereddap.pdf", width = 10, height = 6)

plot_all_quad_MINdifdap <- ggplot(PRISM_LOCAL_nOut_filter, aes(x = DAP, y = MINDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                geom_point() +
                geom_line() +
                geom_hline(yintercept = 0, color = "black") +
                geom_point(aes(x = DAP, y = Predicted_quadMINdap), color = "black", size = 0.3) +
                annotate("text", x =  80, y = -15, label = eq_quadMIN_alldap) +
                labs(title=paste("PRISM minus Local MIN temperatures for all years and locations"),
                     x="Days after planting",
                     y="Temperature Difference (F)",
                     color = "Year") +
                theme_minimal() +
                theme(legend.position="bottom") +
                scale_x_discrete(breaks = seq(0, 180, by = 10)) 

print(plot_all_quad_MINdifdap)                  

dev.off()

### AVG

# Calculating the model:
mod_quadAVG_alldap <- lm(AVGDif_PRISMLOC ~ poly(DAP, 2, raw = TRUE), data = PRISM_LOCAL_nOut_filter)
summary_quadAVG_alldap <- summary(mod_quadAVG_alldap)
coeff_quadAVG_alldap <- coef(mod_quadAVG_alldap)
r_sqr_quadAVG_alldap <- summary_quadAVG_alldap$r.squared

eq_quadAVG_alldap <- paste("y = ",
                           round(coeff_quadAVG_alldap[1], 2), "+",
                           round(coeff_quadAVG_alldap[2], 2), "x +",
                           format(coeff_quadAVG_alldap[3], scientific = TRUE, digits = 2), "x^2",
                           ", R2 =", round(r_sqr_quadAVG_alldap, 3))

PRISM_LOCAL_nOut_filter$Predicted_quadAVGdap <- predict(mod_quadAVG_alldap,
                                                        newdata = PRISM_LOCAL_nOut_filter) # Creates a column with predicted values from the quadratic model to plot quad line

# Plot:
pdf("outputs/PRISM_LOCAL_AVG_Differences_nOut_all_quad_filtereddap.pdf", width = 10, height = 6)

plot_all_quad_AVGdifdap <- ggplot(PRISM_LOCAL_nOut_filter, aes(x = DAP, y = AVGDif_PRISMLOC, color = as.factor(format(Date, "%Y")))) +
                geom_point() +
                geom_line() +
                geom_hline(yintercept = 0, color = "black") +
                geom_point(aes(x = DAP, y = Predicted_quadAVGdap), color = "black", size = 0.3) +
                annotate("text", x =  80, y = -15, label = eq_quadAVG_alldap) +
                labs(title=paste("PRISM minus Local AVG temperatures for all years and locations"),
                     x="Days after planting",
                     y="Temperature Difference (F)",
                     color = "Year") +
                theme_minimal() +
                theme(legend.position="bottom") +
                scale_x_discrete(breaks = seq(0, 180, by = 10)) 

print(plot_all_quad_AVGdifdap)                  

dev.off()

# PRISM_LOCAL_nOut <- df %>%
#   group_by(Location, Year) %>%
#   mutate(days_after_planting = as.numeric(Date - min(Date))) %>%
#   ungroup()