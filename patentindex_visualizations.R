
library(tidyverse)
library(countrycode)
library(cowplot)
library(rio)
library(hrbrthemes)

# load dataset
download.file("http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx", 
              "Patent%20index1960%20-%202015.xlsx")

# function to clean the xlsx sheets
index_function <- function(data,index = TRUE){
  df <- data %>%
    dplyr::mutate(Country = case_when(
      Country == "Banglad." ~ "Bangladesh",
      Country == "Cent. Afr." ~ "Central African Republic",
      Country == "Cost. Rica" ~ "Costa Rica",
      Country == "Dom. Rep." ~ "Dominican Republic",
      Country == "El Salv." ~ "El Salvador",
      Country == "H. Kong" ~ "Hong Kong",
      Country == "Madagas." ~ "Madagascar",
      Country == "Mauritan." ~ "Mauritania",
      Country == "Mozamb." ~ "Mozambique",
      Country == "N. Zealand" ~ "New Zealand",
      Country == "Netherl." ~ "Netherlands",
      Country == "P.N.Guin." ~ "Papua New Guinea",
      Country == "Philipp." ~ "Philippines",
      Country == "S. Africa" ~ "South Africa",
      Country == "S. Leone" ~ "Sierra Leone",
      Country == "Saudi Ar." ~ "Saudi Arabia",
      Country == "Sri. Lanka" ~ "Sri Lanka",
      Country == "Swazil." ~ "Swaziland",
      Country == "Trin.& Tob." ~ "Trinidad and Tobago",
      TRUE ~ as.character(Country)
    )) %>%
    dplyr::mutate(iso3c = countrycode(Country,"country.name","iso3c")) %>%
    dplyr::mutate(country = countrycode(iso3c,"iso3c","country.name")) %>%
    dplyr::select(iso3c,country,`1960`:`2015`) %>%
    tidyr::pivot_longer(`1960`:`2015`,names_to = "year",values_to = index) %>%
    dplyr::mutate(year = as.numeric(year))
}

# coverage subindex
coverage <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "coverage")
coverage <- index_function(data=coverage,index="coverage")

coverage <- 
 coverage %>%
  dplyr::mutate(coverage = ifelse(is.na(coverage),0.000,coverage)) %>%
  ggplot(aes(x = year,y = reorder(country,coverage))) + 
  geom_tile(aes(fill = coverage), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "inferno", begin = 0.05, end = 0.98,
    limits = c(0, 1),
    name = "Coverage scores, 0-1",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0,
      ticks = FALSE,
      barwidth = grid::unit(3.5, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = NULL,
                     position = "top",
                     breaks = seq(1960,2015,5)) +
  scale_y_discrete(name = NULL, position = "left") +
  theme_ipsum() +
  labs(
    title = "Patent Rights Index (Category: Coverage), 1960-2015",
    caption = "Data: Park (2020), Graph: Markus Lang / @markushlang\n  http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 12*12/14)
  )

coverage

png("coverage.png", height = 2400, width = 1800, res = 100)
print(coverage)
dev.off()

# membership subindex
membership <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "membership")
membership <- index_function(data=membership,index="membership")

membership <- 
  membership %>%
  dplyr::mutate(membership = ifelse(is.na(membership),0.000,membership)) %>%
  ggplot(aes(x = year,y = reorder(country,membership))) + 
  geom_tile(aes(fill = membership), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "plasma", begin = 0.05, end = 0.98,
    limits = c(0, 1),
    name = "Membership scores, 0-1",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0,
      ticks = FALSE,
      barwidth = grid::unit(3.5, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = NULL,
                     position = "top",
                     breaks = seq(1960,2015,5)) +
  scale_y_discrete(name = NULL, position = "left") +
  theme_ipsum() +
  labs(
    title = "Patent Rights Index (Category: Membership), 1960-2015",
    caption = "Data: Park (2020), Graph: Markus Lang / @markushlang\n  http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 12*12/14)
  )

membership

png("membership.png", height = 2400, width = 1800, res = 100)
print(membership)
dev.off()

# loss of rights subindex
loss_of_rights <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "loss of rights")
loss_of_rights <- index_function(data=loss_of_rights,index = "loss_of_rights")

loss_of_rights <- 
  loss_of_rights %>%
  dplyr::mutate(loss_of_rights = ifelse(is.na(loss_of_rights),0.000,loss_of_rights)) %>%
  ggplot(aes(x = year,y = reorder(country,loss_of_rights))) + 
  geom_tile(aes(fill = loss_of_rights), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "cividis", begin = 0.05, end = 0.98,
    limits = c(0, 1),
    name = "Loss of rights scores, 0-1",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0,
      ticks = FALSE,
      barwidth = grid::unit(3.5, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = NULL,
                     position = "top",
                     breaks = seq(1960,2015,5)) +
  scale_y_discrete(name = NULL, position = "left") +
  theme_ipsum() +
  labs(
    title = "Patent Rights Index (Category: Loss of Rights), 1960-2015",
    caption = "Data: Park (2020), Graph: Markus Lang / @markushlang\n  http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 12*12/14)
  )

loss_of_rights

png("loss_of_rights.png", height = 2400, width = 1800, res = 100)
print(loss_of_rights)
dev.off()

# duration subindex
duration <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "duration")
duration <- index_function(data=duration,index = "duration")

duration <- 
  duration %>%
  dplyr::mutate(duration = ifelse(is.na(duration),0.000,duration)) %>%
  ggplot(aes(x = year,y = reorder(country,duration))) + 
  geom_tile(aes(fill = duration), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "mako", begin = 0.05, end = 0.98,
    limits = c(0, 1),
    name = "Duration scores, 0-1",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0,
      ticks = FALSE,
      barwidth = grid::unit(3.5, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = NULL,
                     position = "top",
                     breaks = seq(1960,2015,5)) +
  scale_y_discrete(name = NULL, position = "left") +
  theme_ipsum() +
  labs(
    title = "Patent Rights Index (Category: Duration), 1960-2015",
    caption = "Data: Park (2020), Graph: Markus Lang / @markushlang\n  http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 12*12/14)
  )

duration

png("duration.png", height = 2400, width = 1800, res = 100)
print(duration)
dev.off()

# enforcement subindex
enforcement <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "enforcement")
enforcement <- index_function(data=enforcement,index = "enforcement")

enforcement <- 
  enforcement %>%
  dplyr::mutate(enforcement = ifelse(is.na(enforcement),0.000,enforcement)) %>%
  ggplot(aes(x = year,y = reorder(country,enforcement))) + 
  geom_tile(aes(fill = enforcement), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "rocket", begin = 0.05, end = 0.98,
    limits = c(0, 1),
    name = "Enforcement scores, 0-1",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0,
      ticks = FALSE,
      barwidth = grid::unit(3.5, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = NULL,
                     position = "top",
                     breaks = seq(1960,2015,5)) +
  scale_y_discrete(name = NULL, position = "left") +
  theme_ipsum() +
  labs(
    title = "Patent Rights Index (Category: Enforcement), 1960-2015",
    caption = "Data: Park (2020), Graph: Markus Lang / @markushlang\n  http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 12*12/14)
  )

enforcement

png("enforcement.png", height = 2400, width = 1800, res = 100)
print(enforcement)
dev.off()

# overall index
overall <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "overall")
overall <- index_function(data=overall,index = "overall")

overall <- 
  overall %>%
  dplyr::mutate(overall = ifelse(is.na(overall),0.000,overall)) %>%
  ggplot(aes(x = year,y = reorder(country,overall))) + 
  geom_tile(aes(fill = overall), color = "white", size = 0.25) +
  scale_fill_viridis_c(
    option = "magma", begin = 0.05, end = 0.98,
    limits = c(0, 5),
    name = "Overall scores, 0-5",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0,
      ticks = FALSE,
      barwidth = grid::unit(3.5, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     name = NULL,
                     position = "top",
                     breaks = seq(1960,2015,5)) +
  scale_y_discrete(name = NULL, position = "left") +
  theme_ipsum() +
  labs(
    title = "Patent Rights Index (Overall), 1960-2015",
    caption = "Data: Park (2020), Graph: Markus Lang / @markushlang\n  http://fs2.american.edu/wgp/www/Patent%20index1960%20-%202015.xlsx"
  ) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 12*12/14)
  )

overall

png("overall.png", height = 2400, width = 1800, res = 100)
print(overall)
dev.off()

# average coverage
coverage <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "coverage")
coverage <- index_function(data=coverage,index="coverage")

figure1 <- 
  coverage %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(coverage = mean(coverage,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year,y = coverage)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0, 1, 1/7),
                     labels = c("0","1","2",
                                "3","4","5",
                                "6","7")) +
  scale_x_continuous(limits = c(1965,2015),
                     breaks = seq(1965,2015,10),
  ) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    title = "Number of patentable technologies (average)"
  ) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

figure1

# average membership
membership <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "membership")
membership <- index_function(data=membership,index="membership")

figure2 <- 
  membership %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(membership = mean(membership,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year,y = membership)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0, 1, 1/4),
                     labels = c("0","1","2","3","4")) +
  scale_x_continuous(limits = c(1965,2015),
                     breaks = seq(1965,2015,10)) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    title = "Number of IGO memberships (average)"
  ) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

figure2

# average loss of rights
loss_of_rights <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "loss of rights")
loss_of_rights <- index_function(data=loss_of_rights,index = "loss_of_rights")

figure3 <- 
  loss_of_rights %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(loss_of_rights = mean(loss_of_rights,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year,y = loss_of_rights)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0, 1, 1/3),
                     labels = c("0","1","2","3")) +
  scale_x_continuous(limits = c(1965,2015),
                     breaks = seq(1965,2015,10)) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    title = "Number of removed restrictions (average)"
  ) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

figure3

# average duration
duration <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "duration")
duration <- index_function(data=duration,index = "duration")

figure4 <- 
  duration %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(duration = mean(duration,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year,y = duration)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1)) +
  scale_x_continuous(limits = c(1965,2015),
                     breaks = seq(1965,2015,10)) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    title = "Patent Term Length (average)"
  ) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

figure4

# average enforcement 
enforcement <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "enforcement")
enforcement <- index_function(data=enforcement,index = "enforcement")

figure5 <- 
  enforcement %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(enforcement = mean(enforcement,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year,y = enforcement)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0, 1, 1/3),
                     labels = c("0","1","2","3")) +
  scale_x_continuous(limits = c(1965,2015),
                     breaks = seq(1965,2015,10)) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    title = "Number of enforcement mechanisms (average)"
  ) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

figure5

# average overall
overall <- rio::import("Patent%20index1960%20-%202015.xlsx",sheet = "overall")
overall <- index_function(data=overall,index = "overall")

figure6 <- 
  overall %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(overall = mean(overall,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year,y = overall)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,5)) +
  scale_x_continuous(limits = c(1965,2015),
                     breaks = seq(1965,2015,10)) +
  geom_line(size = 1.5, color = "#e64b4b") + 
  labs(
    x = NULL,
    y = NULL,
    title = "Patent Rights Index (average)"
  ) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

figure6

# combine figures into a grid
averages <- cowplot::plot_grid(figure1,figure2,figure3,figure4,figure5,figure6,
                               ncol = 1, align="hv")

png("averages.png", height = 2400, width = 1800, res = 100)
print(averages)
dev.off()
