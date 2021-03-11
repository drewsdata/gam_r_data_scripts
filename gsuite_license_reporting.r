library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(plotly)

# This script leverages data extracted by GAM
# from GSuite / Google Workspace to show daily trends
# of GSuite Business License sku net additions - subtractions and
# cumulative trends.  It starts with a seed
# base_log file of initial six month gsuite log license assignment events
# then this 'last 30 day' event report extraction script runs daily to append 
# and dedupe the base log:
#   gam.exe report admin event "USER_LICENSE_ASSIGNMENT" 
#      start -30d > "license_assignment_30d.csv"
# The data is used to build a ggplotly plot for a
# Shiny reporting app

base_log <- read_csv("license_assignment_base.csv", 
                     col_types = cols(.default = "c"))

last_30_log  <- read_csv("license_assignment_30d.csv", 
                         col_types = cols(.default = "c"))

admin_license_log <- bind_rows(base_log, last_30_log) %>% 
  distinct(id.uniqueQualifier, .keep_all = TRUE) 

write_csv(admin_license_log, "license_assignment_base.csv")

admin_license_log$id.time <- admin_license_log$id.time %>%
  strftime(format = "%Y-%m-%d") 

admin_license_log <- admin_license_log %>%  
  distinct(USER_EMAIL, .keep_all = TRUE) %>% 
  # trim back to start week of oldest log data
  filter(id.time >= "2020-08-30") %>% 
  select(NEW_VALUE, USER_EMAIL, id.time) %>%
  mutate(
    license_type = case_when(
      NEW_VALUE == "G Suite Business - Archived User" ~ "archive",
      NEW_VALUE == "G Suite Business" ~ "gsuite")) %>% 
  clean_names()

daily_net <- admin_license_log %>% 
  group_by(theday = id_time) %>%
  count(license_type) %>% 
  pivot_wider(names_from = license_type, values_from = n) %>% 
  mutate(archive = replace_na(archive, 0)) %>% 
  mutate(gsuite = replace_na(gsuite, 0)) %>% 
  mutate(net_change = gsuite - archive) 

daily_net$cumulative <- cumsum(daily_net$net_change)

daily_net$past_days <- as.integer(rownames(daily_net))

fig <- 
  daily_net %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = past_days, y = net_change), 
           stat = "identity", fill = ifelse(daily_net$net_change <0, "red","green")) + 
  geom_line(mapping = aes(x = past_days, y = cumulative), colour = "skyblue", size = 1.1) + 
  geom_point(aes(x = past_days, y = cumulative), 
             shape = 21, size = 1.75, stroke = 1.0, color = "skyblue", fill = "white") +
  scale_y_continuous(name = "Net Daily Change", breaks = breaks_width(50)) +
  scale_x_continuous(name = "Days", breaks = breaks_width(30)) +
  theme_bw() + 
  ggtitle("GSuite Business License SKU Trend") +
  annotate(geom = "text", x = 0.5*max(daily_net$past_days), 
           y = 0.52*max(daily_net$cumulative), 
           label = "Cumulative Change", hjust = 0, colour = "skyblue", size = 5) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5) 
    # plotly ignores vertical title adjustment: , vjust = -10)
  )

daily_trend_plotly <- 
  ggplotly(fig) %>% 
  plotly::config(displayModeBar = FALSE)

saveRDS(daily_trend_plotly,"daily_trend_plotly.rds")

# daily_trend_plotly Plotly plot can be loaded into Shiny
