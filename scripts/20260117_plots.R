
# If doing on my old Windows PC change location of R packages
.libPaths(c("E:/R/lib-4.5.2","E:/Program Files/R-4.5.2/library"))

## https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(tidyverse)
library(ggridges)

##
## On 5th Jan 2025 had to swap to probe two ~9am. Some temps on this day are missing or too high.

##
## load & format temps data
temps=read_tsv("data/2026-01-17/temps.txt.gz")
temps

## inspect
temps |> filter(id_probe %in% c("28-0516804150ff","28-0516803ebfff")) |> group_by(id_probe) |> summarize(max(temp))
temps |> filter(id_probe %in% c("28-0516804150ff","28-0516803ebfff")) |> filter(temp>40)

## format dates
temps = temps |>
	mutate(date=paste(substr(date,1,4), substr(date,5,6), substr(date,7,8), sep="-")) |>
	mutate(date=as_date(date, format="%Y-%m-%d"))

# exclude before 2017 & after 2025
temps = filter(temps, date >= as_date("2017-01-01") & date < as_date("2026-01-01"))

# check out data around 5th Jan 2025
temps |> filter(date >= ymd("2025-01-04") & date < ymd("2025-01-06")) |> print(n=100)

## get temps for probe used until problems on 5th Jan 2025
temps = temps |>
	filter(
		(id_probe=="28-0516804150ff" & id <= 142211) |  # old primary
		(id_probe=="28-0516803ebfff" & id >= 142213)    # new primary
	) |>
	select(temp, date, name) |>
	rename(hour=name)

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
p = ggplot(temps, aes(x = temp, y = fct_rev(factor(year)), fill = stat(x))) +
	geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
	scale_x_continuous(expand = c(0.01, 0)) +
	scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
	labs(title = "Temperature distribution by year") +
	xlab("Temperature [C] recorded @ Pilling HQ, Exeter") +
	ylab("Year") +
	theme_ridges() + theme(legend.position = "none")
p

ggsave("outputs/2026-01-17/01.temps_by_year.png", width=14, height=11, units="cm", dpi=150, bg="white")



##
##
## get density plot of temps in each month...
p = ggplot(temps, aes(x = temp, y = fct_rev(month), fill = stat(x))) +
	geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
	scale_x_continuous(expand = c(0.01, 0)) +
	scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
	labs(title = "Temperature distribution by month by year") +
	xlab("Temperature [C] recorded @ Pilling HQ, Exeter") +
	ylab("Month") +
	theme_ridges() + theme(legend.position = "none")
p

ggsave("outputs/2026-01-17/02.temps_by_month.png", width=14, height=12, units="cm", dpi=150, bg="white")


##
##
## facet by year
p + facet_grid(year ~ .)

## save
ggsave("outputs/2026-01-17/03.temps_by_month_by_year.png", width=14, height=30, units="cm", dpi=150, bg="white")




##
##
## get density plot of temps in each month... facetted together!

p = ggplot(temps, aes(x = temp, y = fct_rev(factor(year)), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
	labs(title = "Temperature distribution by month") +
	xlab("Temperature [C] recorded @ Pilling HQ, Exeter") +
	ylab("Year") +
	theme_ridges() + theme(legend.position = "none") +
	facet_wrap(month ~ .)
p


## save
ggsave("outputs/2026-01-17/04.temps_by_year_by_month.png", width=22, height=20, units="cm", dpi=150, bg="white")





##
## line plot over time
p = ggplot(temps, aes(x = yday(date), y = temp)) +
  geom_line() +
	labs(title = "Temperature over time, by year") +
	xlab("Day of the year") + ylab("Temp. [C]") +
  scale_x_continuous(expand = c(0.01, 0),
                     breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     labels = unique(temps$month)) +
  scale_y_continuous(limits=c(10,30)) +
  theme_bw() +
  facet_grid(year ~ .)
p

#ggsave("outputs/2026-01-17/05.temps_by_day.png", width=16, height=10, units="cm", dpi=1000, bg="white")



#
# loading heating data
heating_data = read_tsv("data/2026-01-17/action_events4.txt.gz")

# exclude before 2017 and after 2024
heating_data = filter(heating_data, date >= as_date("2017-01-01") & date < as_date("2026-01-01"))

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

# If heating is "on" over midnight on last day this will error
# add a line to heating off with date set to 1 second before midnight
if (nrow(heating_off) == (nrow(heating_on)-1))  {

	# get end date dynamically
	max_date <- max(heating_on$date)
	dynamic_end_date <- lubridate::ceiling_date(max_date, unit = "year") - lubridate::seconds(1)

	# append
	heating_off <- rbind(heating_off, tibble(action = "Off", date = dynamic_end_date))
}

# Assuming sequential 'On' and 'Off' events, pair them up
heating_periods = tibble(start = heating_on$date, end = heating_off$date)
heating_periods = heating_periods |> mutate(year = year(start))

#
# new plot with heating periods highlighted
p = ggplot(temps, aes(x = datetime, y = temp)) +
  geom_rect(data = heating_periods,
  					aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, group = year),
  					fill = "#dc2f02", alpha = 1, inherit.aes = FALSE) +
  geom_line() +
  labs(title = 'Temperatures recorded @ Pilling HQ, Exeter') +
  xlab("Day of the year") + ylab("Temp. [C]") +
  scale_x_continuous(expand = c(0.01, 0), breaks = NULL) +
  scale_y_continuous(limits=c(10,30)) +
  facet_wrap(~year,
  					 ncol=1,
  					 scales = "free_x",
  					 axes = "margins",
  					 strip.position="right"
  					 ) +
  theme_bw() +
	theme(
		axis.text.y = element_text(size = 6),
		# Reduce vertical space between facet rows
		panel.spacing.y = unit(0.1, "lines")
	)

p

#ggsave("Pilling_house_temps_over_day_with_heating_2017_2024.HD.svg", width=16, height=9)

ggsave("outputs/2026-01-17/06.temps_by_day.with_heating.png", width=12, height=10, units="cm", dpi=800, bg="white")
ggsave("outputs/2026-01-17/07.temps_by_day.with_heating.HD.png", width=20, height=12, units="cm", dpi=1200, bg="white")

