
# If doing on my old Windows PC change location of R packages
.libPaths(c("E:/Luke/R_lib/4.2.2","E:/Program Files/R-4.2.2/library"))

## https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(tidyverse)
library(ggplot2)
library(ggridges)

##
##
## load & format temps data
temps=read_tsv("temps.txt")
temps

## inspect
temps |> filter(id_probe %in% c("28-0516804150ff","28-0516803ebfff")) |> group_by(id_probe) |> summarize(max(temp))
temps |> filter(id_probe %in% c("28-0516804150ff","28-0516803ebfff")) |> filter(temp>40)

## just keep useful probe and columns
temps = temps |>
  filter(id_probe=="28-0516804150ff") |>
  select(temp, date, name) |>
  rename(hour=name)

## format dates (annoying because unambiguous)
temps = temps |>
  mutate(date=paste(substr(date,1,4), substr(date,5,6), substr(date,7,8), sep="-")) |>
  mutate(date=as_date(date, format="%Y-%m-%d"))

# exclude before 2017 & after 2023
temps = filter(temps, date >= as_date("2017-01-01") & date < as_date("2025-01-01"))

# exclude unrealistic temps
temps = filter(temps, temp > 1 & temp < 40)

## some hours have multiple temp readings if pi rebooted - get average
keep=rep(TRUE, nrow(temps))
for (i in nrow(temps):2)  {
	if (temps[i,"date"]==temps[i-1,"date"]  &  temps[i,"hour"]==temps[i-1,"hour"])  {
		temps[i-1,"temp"]=(temps[i,"temp"]+temps[i-1,"temp"])/2
		keep[i]=FALSE
	}
}
temps=temps[keep,]

## create year and month variables
temps = temps |>
  mutate(year = year(date),
         month = month(date, label=TRUE))

## create datetime variable
temps = mutate(temps, datetime = ymd(date) + hours(hour))


##
##
## get density plot of temps in each year...
p = ggplot(temps, aes(x = temp, y = as.factor(year), fill = stat(x))) +
	geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
	scale_x_continuous(expand = c(0.01, 0)) +
	scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
	labs(title = 'Temperatures recorded @ Pilling HQ, Exeter') +
	xlab("Temperature [C]") + ylab("Year") +
	theme_ridges() + theme(legend.position = "none")
p

png("Pilling_house_temps_by_year_2017_2024.png",width=1800,height=1600,res=300)
p
#p + facet_grid(year ~ .)
dev.off()



##
##
## get density plot of temps in each month...
p = ggplot(temps, aes(x = temp, y = fct_rev(month), fill = stat(x))) +
	geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
	scale_x_continuous(expand = c(0.01, 0)) +
	scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
	labs(title = 'Temperatures recorded @ Pilling HQ, Exeter') +
	xlab("Temperature [C]") + ylab("Month") +
	theme_ridges() + theme(legend.position = "none")
p


##
##
## facet by year
p + facet_grid(year ~ .)

## save
png("Pilling_house_temps_by_month_2017_2024.png",width=1800,height=3500,res=300)
p + facet_grid(year ~ .)
dev.off()



##
##
## compare July and Augusts...

p = ggplot(temps |> filter(month == "Aug"), aes(x = temp, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
  labs(title = 'August temperatures recorded @ Pilling HQ, Exeter') +
  xlab("Temperature [C]") + ylab("Year") +
  theme_ridges() + theme(legend.position = "none")
p

png("Pilling_house_temps_by_August_2017_2024.png",width=1800,height=1400,res=300)
p
dev.off()




##
##
## get density plot of temps in each month... facetted together!

p = ggplot(temps, aes(x = temp, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
  labs(title = 'Temperatures recorded @ Pilling HQ, Exeter') +
  xlab("Temperature [C]") + ylab("Year") +
  theme_ridges() + theme(legend.position = "none")
p


##
##
## facet by month
p + facet_wrap(month ~ .)

## save
png("Pilling_house_temps_by_year-month_2017_2024.png",width=2800,height=2000,res=300)
p + facet_wrap(month ~ .)
dev.off()



##
## heatmap of average temp per week, per year
temp_week = temps |>
  mutate(week = week(date)) |>
  group_by(year, week) |>
  summarize(temp_avg = mean(temp, na.rm=TRUE)) |>
  ungroup()

library(ComplexHeatmap)

temp_week_M = temp_week |>
  pivot_wider(names_from = year, values_from = temp_avg) |>
  select(-week) |>
  as.matrix()
rownames(temp_week_M) = c(rep("",9), 10,
                          rep("",9), 20,
                          rep("",9), 30,
                          rep("",9), 40,
                          rep("",9), 50,
                          "", "", "")

cols <- rev(c('#370617', '#6a040f', '#9d0208', '#d00000', '#dc2f02', '#e85d04', '#f48c06', '#faa307', '#ffba08'))
min <- min(temp_week_M)
max <- max(temp_week_M)
col_fun <- circlize::colorRamp2(seq(min,max,by=(max-min)/(length(cols)-1)), cols)

Heatmap(temp_week_M,
        cluster_rows = FALSE, cluster_columns = FALSE,
        row_names_side = "left", column_names_side = "top", column_names_rot = 45,
        row_title = "Week",
        col = col_fun,
        heatmap_legend_param = list(title = "Temp [C]", legend_height = unit(4, "cm")))

png("Pilling_house_temps_by_week_2017_2024.png",width=1600,height=1000,res=300)
dev.off()

##
## line plot over time
p = ggplot(temps, aes(x = yday(date), y = temp)) +
  geom_line() +
  labs(title = 'Temperatures recorded @ Pilling HQ, Exeter') +
  xlab("Day of the year") + ylab("Temp. [C]") +
  scale_x_continuous(expand = c(0.01, 0),
                     breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     labels = unique(temps$month)) +
  scale_y_continuous(limits=c(10,30)) +
  theme_bw() +
  facet_grid(year ~ .)
p

png("Pilling_house_temps_over_day_2017_2024.png",width=2800,height=2000,res=300)
p
dev.off()


#
# loading heating data
heating_data = read_tsv("action_events4.txt")

# exclude before 2017 and after 2024
heating_data = filter(heating_data, date >= as_date("2017-01-01") & date < as_date("2025-01-01"))

## can get multiple "ons" or "offs" in a row as modes change etc. so go down and only keep first
keep = rep(TRUE, nrow(heating_data))
for (i in 2:nrow(heating_data))  if (heating_data[i,"action"]=="Off"  &  heating_data[i-1,"action"]=="Off")  keep[i] = FALSE
heating_data = heating_data[keep,]
keep = rep(TRUE, nrow(heating_data))
for (i in 2:nrow(heating_data))  if (heating_data[i,"action"]=="On"  &  heating_data[i-1,"action"]=="On")  keep[i] = FALSE
heating_data = heating_data[keep,]

# Identify 'Heating On' and 'Heating Off' events and pair them
heating_on  = heating_data |> filter(action == "On")
heating_off = heating_data |> filter(action == "Off")

# Assuming sequential 'On' and 'Off' events, pair them up
heating_periods = tibble(start = heating_on$date, end = heating_off$date)
heating_periods = heating_periods |> mutate(year = year(start))

#
# new plot with heating periods highlighted
p = ggplot(temps, aes(x = datetime, y = temp)) +
  geom_rect(data = heating_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, group = year), fill = "#dc2f02", alpha = 1, inherit.aes = FALSE) +
  geom_line() +
  labs(title = 'Temperatures recorded @ Pilling HQ, Exeter') +
  xlab("Day of the year") + ylab("Temp. [C]") +
  scale_x_continuous(expand = c(0.01, 0), breaks = NULL) +
  scale_y_continuous(limits=c(10,30)) +
  facet_wrap(~year, scales = "free_x", ncol=1, strip.position="right") +
  theme_bw()

p

#png("Pilling_house_temps_over_day_with_heating_2017_2023.png",width=8750,height=5000,res=300)
#p
#dev.off()

ggsave("Pilling_house_temps_over_day_with_heating_2017_2024.svg", width=10, height=8)
ggsave("Pilling_house_temps_over_day_with_heating_2017_2024.HD.svg", width=16, height=9)


