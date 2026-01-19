
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


##
##
## STATISTICAL ANALYSIS
##
##

# Calculate duration of each heating period in hours
heating_periods = heating_periods |>
  mutate(duration_hours = as.numeric(difftime(end, start, units = "hours")))

# Add month and year information for each period
heating_periods = heating_periods |>
  mutate(month = month(start, label = TRUE),
         month_num = month(start))

# Total heating hours by year
heating_by_year = heating_periods |>
  group_by(year) |>
  summarise(total_hours = sum(duration_hours),
            n_periods = n()) |>
  arrange(year)

cat("\n\n=== HEATING HOURS BY YEAR ===\n")
print(heating_by_year)

# Total heating hours by month (across all years)
heating_by_month = heating_periods |>
  group_by(month) |>
  summarise(total_hours = sum(duration_hours),
            n_periods = n()) |>
  arrange(month)

cat("\n\n=== HEATING HOURS BY MONTH (all years) ===\n")
print(heating_by_month)

# Total heating hours by month and year
heating_by_month_year = heating_periods |>
  group_by(year, month, month_num) |>
  summarise(total_hours = sum(duration_hours),
            n_periods = n(),
            .groups = 'drop') |>
  arrange(year, month)

cat("\n\n=== HEATING HOURS BY MONTH AND YEAR ===\n")
print(heating_by_month_year, n = Inf)

# Find month with most heating hours
max_month_overall = heating_by_month |>
  filter(total_hours == max(total_hours))

cat("\n\n=== MONTH WITH MOST HEATING (overall) ===\n")
cat(sprintf("Month: %s\nTotal hours: %.1f\nNumber of periods: %d\n",
            max_month_overall$month, max_month_overall$total_hours, max_month_overall$n_periods))

# Find month-year combination with most heating hours
max_month_year = heating_by_month_year |>
  filter(total_hours == max(total_hours))

cat("\n\n=== MONTH-YEAR WITH MOST HEATING ===\n")
cat(sprintf("Month-Year: %s %d\nTotal hours: %.1f\nNumber of periods: %d\n",
            max_month_year$month, max_month_year$year, max_month_year$total_hours, max_month_year$n_periods))

# Find year with most heating hours
max_year = heating_by_year |>
  filter(total_hours == max(total_hours))

cat("\n\n=== YEAR WITH MOST HEATING ===\n")
cat(sprintf("Year: %d\nTotal hours: %.1f\nNumber of periods: %d\n",
            max_year$year, max_year$total_hours, max_year$n_periods))

# Additional analysis: average heating hours per day by year
heating_by_year = heating_by_year |>
  mutate(days_in_year = if_else(leap_year(year), 366, 365),
         avg_hours_per_day = total_hours / days_in_year)

cat("\n\n=== AVERAGE HEATING HOURS PER DAY BY YEAR ===\n")
print(heating_by_year |> select(year, avg_hours_per_day))

# Seasonal analysis: group months into seasons
heating_by_month_year = heating_by_month_year |>
  mutate(season = case_when(
    month_num %in% c(12, 1, 2) ~ "Winter",
    month_num %in% c(3, 4, 5) ~ "Spring",
    month_num %in% c(6, 7, 8) ~ "Summer",
    month_num %in% c(9, 10, 11) ~ "Autumn"
  ))

heating_by_season_year = heating_by_month_year |>
  group_by(year, season) |>
  summarise(total_hours = sum(total_hours),
            .groups = 'drop') |>
  arrange(year, factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))

cat("\n\n=== HEATING HOURS BY SEASON AND YEAR ===\n")
print(heating_by_season_year)

# Overall summary statistics
cat("\n\n=== OVERALL SUMMARY STATISTICS ===\n")
cat(sprintf("Total heating hours (all years): %.1f\n", sum(heating_periods$duration_hours)))
cat(sprintf("Average heating period duration: %.2f hours\n", mean(heating_periods$duration_hours)))
cat(sprintf("Median heating period duration: %.2f hours\n", median(heating_periods$duration_hours)))
cat(sprintf("Shortest heating period: %.2f hours\n", min(heating_periods$duration_hours)))
cat(sprintf("Longest heating period: %.2f hours\n", max(heating_periods$duration_hours)))
cat(sprintf("Total number of heating periods: %d\n", nrow(heating_periods)))

# Years comparison
cat("\n\n=== YEAR-OVER-YEAR COMPARISON ===\n")
heating_by_year = heating_by_year |>
  mutate(pct_change = (total_hours - lag(total_hours)) / lag(total_hours) * 100)
print(heating_by_year |> select(year, total_hours, pct_change))

