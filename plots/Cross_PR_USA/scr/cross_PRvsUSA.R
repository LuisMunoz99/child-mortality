#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, tidyverse, ggplot2, data.table, here, showtext, stringr, gridExtra)


files <- list(input = here::here("plots/Cross_PR_USA/input/cross_minors.csv"), 
              output = here::here("plots/Cross_PR_USA/output/cross_minors.png"))


# Import data ------------------------------------------------------------------
df_orig <- fread(files$input, skip = 1, header = "auto") 

df <- df_orig %>% rename(year = 1,
                         firearm_PR = 2,
                          vehicle_PR = 3,
                          firearm_USA = 4,
                          vehicle_USA = 5) %>%
  mutate(firearm_PR = as.numeric(firearm_PR),
         vehicle_PR = as.numeric(vehicle_PR),
         firearm_USA = as.numeric(firearm_USA),
         vehicle_USA = as.numeric(vehicle_USA),
         year = as.character(year))

# changing year to date format
df <- df %>% mutate(year = paste0(df$year, "-01-01")) 
df <- df %>% mutate(year = as.Date(year, format="%Y-%d-%m"))
#df <- df %>% mutate(year = format(as.Date(year), "%Y"))


str(df$year)



# Plot for Puerto Rico  firearm  vs motor vehicle (2007-2021) 


# Create the line plots
PR <- ggplot(df) +
  geom_line(aes(x = year, y = firearm_PR), size = 1.5, color = "#Ffaf3a") +
  geom_line(aes(x = year, y = vehicle_PR), size = 1.5, color = "#aaaaaa") +
  labs(y = "Tasa por millón") + 
  theme_minimal() 
  
  
# axis 
PR <- PR +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = c(seq(from = 0, to = 12, by = 2))) +
  
  scale_x_date(date_labels = "%Y", # %d day, %b month, %Y year
               date_breaks = "1 year",
               limit = c(as.Date("2007-01-01"), as.Date("2021-01-01"))) #limiting axis to dates of interest 


PR


# Gridlines
col_grid <- rgb(235, 235, 235, 235, maxColorValue = 255)

PR <- PR + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = col_grid),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(vjust = 4))

# Text of plot
PR <- PR +
  theme(
    text = element_text(color = "black",
      size = 22))

# Labels
PR <- PR + labs(subtitle = "Puerto Rico") +
  annotate(
    x = as.Date("2018-5-01"), 
    y = 7.1, 
    label = "Arma de fuego", 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    fontface = "bold",
    size = 6)  +
  annotate(
    x = as.Date("2019-5-01"), 
    y = 3.5, 
    label = "Vehículo", 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    fontface = "bold",
    size = 6) 
PR

# Font
# PAUSE fonts <- font_files() %>% tibble
#Universe_fonts <- fonts %>% filter(str_detect(family, "Universe"))
#font_add(family = "Universe", regular = "Universe-Regular.otf")


# Plot for USA firearm  vs motor vehicle (2007-2021)
USA <- ggplot(df) +
  geom_line(aes(x = year, y = firearm_USA), size = 1.5, color = "#Ffaf3a") +
  geom_line(aes(x = year, y = vehicle_USA), size = 1.5, color = "#aaaaaa") +
  labs(y = "Tasa por millón") + 
  theme_minimal() 


# axis 
USA <- USA +
  scale_y_continuous(
    limits = c(0, 8),
    breaks = c(seq(from = 0, to = 8, by = 2))) +

  
  scale_x_date(date_labels = "%Y", # %d day, %b month, %Y year
               date_breaks = "1 year",
               limit = c(as.Date("2007-01-01"), as.Date("2021-01-01"))) #limiting axis to dates of interest 





# Gridlines
col_grid <- rgb(235, 235, 235, 235, maxColorValue = 255)

USA <- USA + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = col_grid),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10)),
        axis.text.x = element_text(vjust = 4))
USA

# Text of plot
USA <- USA +
  theme(text = element_text( color = "black",size = 22),
        axis.title.y = element_text(size = 23)) 

# Labels
USA <- USA + labs(subtitle = "Estados Unidos") +
  annotate(
    x = as.Date("2018-04-01"), 
    y = 6.4, 
    label = "Arma de fuego", 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    fontface = "bold",
    size = 6)  +
  annotate(
    x = as.Date("2019-9-01"), 
    y = 4.8, 
    label = "Vehículo", 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    fontface = "bold",
    size = 6) 

USA

# Arrange the plots side by side
cross <- grid.arrange(PR,USA, ncol = 2, heights = c(4, 4), widths = c(6.4, 6.4))


# Export
ggsave(files$output, plot = cross,  width = 10, height = 6, dpi = 300, scale = 2)
