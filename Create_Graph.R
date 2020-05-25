library(jsonlite)
library(tidyverse)
library(RColorBrewer)
library(ggiraph)
library(htmlwidgets)

rm(list = ls())

### Download lates File -------------------

download.file(url = "https://raw.githubusercontent.com/datameet/covid19/master/data/non_virus_deaths.json",
              destfile = "Data/non_virus_deaths.json")


### Import ------------
data = fromJSON(txt = "Data/non_virus_deaths.json") %>%
  pluck("rows") %>%
  pluck("value") %>%
  mutate(incident_date = as.Date(incident_date))

data$cause = as.character(NA)
for (i in 1:dim(data)[1]){
  data$cause[i] = data$reason[i] %>%
    pluck(1) %>%
    reduce(~str_c(.x, .y, sep = ", "))
}


### Clean ---------------
nonCovidDeaths = data %>%
  arrange(incident_date) %>%
  group_by(incident_date) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(non_covid_deaths = cumsum(deaths))



### Graph Cumulative --------------
max_y = max(nonCovidDeaths$non_covid_deaths)
min_x = as.Date("2020-03-15")
max_x = max(nonCovidDeaths$incident_date)

p1 = ggplot(nonCovidDeaths,
            mapping = aes(x = incident_date, y = non_covid_deaths)) +
  geom_area(fill = "steelblue", alpha = 0.7) +
  geom_line(color = "steelblue4", size = 1) +
  geom_point(color = "steelblue4", size = 1.5) +
  scale_y_continuous(breaks = seq(0,max_y + 200,100),
                     expand = c(0,0,0,50),
                     name = "Total Deaths") +
  scale_x_date(limits = c(min_x, max_x),
               expand = expansion(mult = 0,
                                 add = c(0,0)),
               name = "")

p2 = p1 + theme_light() +
  theme(panel.border = element_rect(color = "black"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.margin = margin(10,15,5,10, unit = "pt"))
p2

ggsave(filename = "Figures/nonCovidDeaths.svg", plot = p2,
       width = 17, height = 12, units = "cm")



# Interactive Graph ===================

# Function to Create Data for Scatter

data = data %>%
  mutate(tooltip = paste0("<b>Names (Age):</b> ", name_age,
                          "\n<b>Deaths:</b> ", deaths,
                          "\n<b>Death Cause:</b> ", cause,
                          "\n<b>Source: </b>", source))

data_interactive = data[rep(row.names(data), data$deaths), c("_id", "incident_date", "tooltip", "source_link")] %>%
  arrange(incident_date) %>%
  group_by(incident_date) %>%
  mutate(x = 1) %>%
  mutate(number = cumsum(x)) %>%
  rename(id = `_id`)

data_interactive$onClick = paste0('window.open(\"',
                                   data_interactive$source_link,
                                  '\")')

plot = data_interactive %>%
  mutate(number = number - 0.5) %>%
  ggplot(aes(x = incident_date, y = number)) +
  geom_point_interactive(aes(data_id = id,
                             tooltip = tooltip,
                             onclick = onClick),
                         color = "steelblue",
                         size = 2.2) +
  scale_y_continuous(expand = c(0,0.7,0.1,0),
                     name = "Deaths") +
  scale_x_date(name = "Date of Incident") +
  theme_classic()

plot_mobile = data_interactive %>%
  mutate(number = number - 0.5) %>%
  ggplot(aes(x = incident_date, y = number)) +
  geom_point_interactive(aes(data_id = id,
                             tooltip = tooltip),
                         color = "steelblue",
                         size = 2.2) +
  scale_y_continuous(expand = c(0,0.7,0.1,0),
                     name = "Deaths") +
  scale_x_date(name = "Date of Incident") +
  theme_classic()

plot_mobile

tooltip_css = "background-color:white;color:black;font-style:italic;padding:10px;border-radius:5px;border-style:solid;"

plot_girafe = girafe(ggobj = plot,
                     options = list(opts_tooltip(css = tooltip_css,
                                                 delay_mouseout = 1500,
                                                 offx = 5,
                                                 use_cursor_pos = TRUE) ))

saveWidget(plot_girafe, file = "covid_deaths_interactive.html")


plot_girafe_mobile = girafe(ggobj = plot_mobile,
                            options = list(opts_tooltip(css = tooltip_css,
                                                        delay_mouseout = 1500,
                                                        offx = 5,
                                                        use_cursor_pos = FALSE) ))
saveWidget(plot_girafe_mobile, file = "covid_deaths_interactive_mobile.html")
