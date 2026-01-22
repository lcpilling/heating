
# If doing on my old Windows PC change location of R packages
.libPaths(c("E:/R/lib-4.5.2","E:/Program Files/R-4.5.2/library"))

library(tidyverse)
library(ggridges)

##
## Load & format internal temps data
temps=read_tsv("data/2026-01-17/temps.txt.gz")

## format dates
temps = temps |>
	mutate(date=paste(substr(date,1,4), substr(date,5,6), substr(date,7,8), sep="-")) |>
	mutate(date=as_date(date, format="%Y-%m-%d"))

# exclude before 2017 & after 2025
temps = filter(temps, date >= as_date("2017-01-01") & date < as_date("2026-01-01"))

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
## Load heating data to create heating periods
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


##
## Load external temperature data (Exeter Airport)
external_temps = read_csv("data/meteostat/meteostat_exeterairport_2017-01-01_2025-12-31.csv")

## format date
external_temps = external_temps |>
	mutate(date = as_date(date))

## check completeness of tavg (average temperature)
cat("\n\n=== EXTERNAL TEMPERATURE DATA COMPLETENESS ===\n")
cat(sprintf("Total rows: %d\n", nrow(external_temps)))
cat(sprintf("Missing tavg values: %d (%.1f%%)\n", 
            sum(is.na(external_temps$tavg)), 
            100 * sum(is.na(external_temps$tavg)) / nrow(external_temps)))

## Simple imputation for missing tavg values
## Fill gaps using average of nearest rows above and below
external_temps_imputed = external_temps

for (i in 1:nrow(external_temps_imputed)) {
	if (is.na(external_temps_imputed$tavg[i])) {
		# Find nearest non-NA value above
		above_idx = NA
		for (j in (i-1):1) {
			if (!is.na(external_temps_imputed$tavg[j])) {
				above_idx = j
				break
			}
		}
		
		# Find nearest non-NA value below
		below_idx = NA
		for (j in (i+1):nrow(external_temps_imputed)) {
			if (!is.na(external_temps_imputed$tavg[j])) {
				below_idx = j
				break
			}
		}
		
		# Impute based on what we found
		if (!is.na(above_idx) && !is.na(below_idx)) {
			external_temps_imputed$tavg[i] = mean(c(external_temps_imputed$tavg[above_idx], 
			                                         external_temps_imputed$tavg[below_idx]))
		} else if (!is.na(above_idx)) {
			external_temps_imputed$tavg[i] = external_temps_imputed$tavg[above_idx]
		} else if (!is.na(below_idx)) {
			external_temps_imputed$tavg[i] = external_temps_imputed$tavg[below_idx]
		}
	}
}

cat(sprintf("\nAfter imputation, missing tavg values: %d\n", sum(is.na(external_temps_imputed$tavg))))

## Calculate daily average internal temperature for merging
temps_daily = temps |>
	group_by(date, year, month) |>
	summarise(internal_temp = mean(temp, na.rm = TRUE),
	          .groups = 'drop')

## Merge internal and external temperatures
merged_temps = temps_daily |>
	left_join(external_temps_imputed |> select(date, tavg) |> rename(external_temp = tavg), 
	          by = "date")

## check merge
cat("\n\n=== MERGED TEMPERATURE DATA ===\n")
cat(sprintf("Total days: %d\n", nrow(merged_temps)))
cat(sprintf("Days with both internal and external temps: %d\n", 
            sum(!is.na(merged_temps$internal_temp) & !is.na(merged_temps$external_temp))))

##
## Add heating status to merged data
## For each day, check if heating was on at any point
merged_temps = merged_temps |>
	mutate(heating_on = FALSE)

for (i in 1:nrow(heating_periods)) {
	period_dates = seq.Date(as_date(heating_periods$start[i]), 
	                        as_date(heating_periods$end[i]), 
	                        by = "day")
	merged_temps$heating_on[merged_temps$date %in% period_dates] = TRUE
}

##
## STATISTICAL ANALYSIS
##

cat("\n\n=== TEMPERATURE STATISTICS ===\n")

# Overall temperature statistics
cat("\n--- Overall Statistics ---\n")
cat(sprintf("Internal temp - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(merged_temps$internal_temp, na.rm = TRUE),
            sd(merged_temps$internal_temp, na.rm = TRUE)))
cat(sprintf("External temp - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(merged_temps$external_temp, na.rm = TRUE),
            sd(merged_temps$external_temp, na.rm = TRUE)))

# Temperature difference
merged_temps = merged_temps |>
	mutate(temp_difference = internal_temp - external_temp)

cat(sprintf("Temp difference (internal - external) - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(merged_temps$temp_difference, na.rm = TRUE),
            sd(merged_temps$temp_difference, na.rm = TRUE)))

# Statistics when heating is on vs off
heating_on_temps = merged_temps |> filter(heating_on == TRUE)
heating_off_temps = merged_temps |> filter(heating_on == FALSE)

cat("\n--- When Heating is ON ---\n")
cat(sprintf("Days with heating on: %d\n", nrow(heating_on_temps)))
cat(sprintf("Internal temp - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(heating_on_temps$internal_temp, na.rm = TRUE),
            sd(heating_on_temps$internal_temp, na.rm = TRUE)))
cat(sprintf("External temp - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(heating_on_temps$external_temp, na.rm = TRUE),
            sd(heating_on_temps$external_temp, na.rm = TRUE)))
cat(sprintf("Temp difference - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(heating_on_temps$temp_difference, na.rm = TRUE),
            sd(heating_on_temps$temp_difference, na.rm = TRUE)))

cat("\n--- When Heating is OFF ---\n")
cat(sprintf("Days with heating off: %d\n", nrow(heating_off_temps)))
cat(sprintf("Internal temp - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(heating_off_temps$internal_temp, na.rm = TRUE),
            sd(heating_off_temps$internal_temp, na.rm = TRUE)))
cat(sprintf("External temp - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(heating_off_temps$external_temp, na.rm = TRUE),
            sd(heating_off_temps$external_temp, na.rm = TRUE)))
cat(sprintf("Temp difference - Mean: %.2f°C, SD: %.2f°C\n", 
            mean(heating_off_temps$temp_difference, na.rm = TRUE),
            sd(heating_off_temps$temp_difference, na.rm = TRUE)))

## Analysis of heating onset after summer
## Find first day of heating in autumn/winter (after Aug 31) for each year
heating_onset = tibble()

for (yr in unique(merged_temps$year)) {
	# Get heating periods starting after Aug 31 of this year
	autumn_heating = heating_periods |> 
		filter(year == yr, month(start) >= 9) |>
		arrange(start) |>
		slice(1)
	
	if (nrow(autumn_heating) > 0) {
		onset_date = as_date(autumn_heating$start[1])
		
		# Get external temp for that day and few days before
		onset_temps = merged_temps |>
			filter(date >= (onset_date - 7), date <= onset_date) |>
			select(date, external_temp)
		
		heating_onset = bind_rows(heating_onset, 
		                          tibble(year = yr, 
		                                 onset_date = onset_date,
		                                 external_temp_onset = onset_temps$external_temp[onset_temps$date == onset_date],
		                                 external_temp_week_avg = mean(onset_temps$external_temp, na.rm = TRUE)))
	}
}

cat("\n\n=== HEATING ONSET AFTER SUMMER ===\n")
print(heating_onset)
cat(sprintf("\nAverage external temperature at heating onset: %.2f°C (SD: %.2f°C)\n",
            mean(heating_onset$external_temp_onset, na.rm = TRUE),
            sd(heating_onset$external_temp_onset, na.rm = TRUE)))
cat(sprintf("Average external temperature (week before onset): %.2f°C (SD: %.2f°C)\n",
            mean(heating_onset$external_temp_week_avg, na.rm = TRUE),
            sd(heating_onset$external_temp_week_avg, na.rm = TRUE)))

## Save summary statistics
write_tsv(merged_temps, "outputs/2026-01-17/10.merged_temps.txt")
write_tsv(heating_onset, "outputs/2026-01-17/11.heating_onset.txt")


##
## PLOTS
##

## 1. Line plot faceted by year showing internal and external temperatures
merged_temps_long = merged_temps |>
	select(date, year, month, internal_temp, external_temp, heating_on) |>
	pivot_longer(cols = c(internal_temp, external_temp), 
	             names_to = "temp_type", 
	             values_to = "temperature") |>
	mutate(temp_type = case_when(
		temp_type == "internal_temp" ~ "Internal (Pilling HQ)",
		temp_type == "external_temp" ~ "External (Exeter Airport)"
	))

p = ggplot(merged_temps_long, aes(x = yday(date), y = temperature, color = temp_type)) +
	geom_line(linewidth = 0.4) +
	scale_color_manual(values = c("Internal (Pilling HQ)" = "#e63946", 
	                               "External (Exeter Airport)" = "#457b9d")) +
	labs(title = "Internal vs External Temperatures by Year",
	     color = "Temperature") +
	xlab("Day of the year") + ylab("Temp. [°C]") +
	scale_x_continuous(expand = c(0.01, 0),
	                   breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
	                   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
	theme_bw() +
	theme(legend.position = "bottom") +
	facet_grid(year ~ .)

p

ggsave("outputs/2026-01-17/12.internal_vs_external_by_year.png", 
       width=16, height=20, units="cm", dpi=150, bg="white")


## 2. Temperature difference over time with heating periods
p = ggplot(merged_temps, aes(x = date, y = temp_difference)) +
	geom_rect(data = heating_periods,
						aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, group = year),
						fill = "#dc2f02", alpha = 0.3, inherit.aes = FALSE) +
	geom_line(color = "#2a9d8f", linewidth = 0.5) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
	labs(title = "Temperature Difference (Internal - External) over Time",
	     subtitle = "Red shading indicates heating periods") +
	xlab("Date") + ylab("Temp. Difference [°C]") +
	scale_x_continuous(expand = c(0.01, 0), breaks = NULL) +
	facet_wrap(~year, 
	           ncol=1, 
	           scales = "free_x",
	           axes = "margins",
	           strip.position="right") +
	theme_bw() +
	theme(panel.spacing.y = unit(0.1, "lines"))

p

ggsave("outputs/2026-01-17/13.temp_difference_over_time.png", 
       width=16, height=20, units="cm", dpi=150, bg="white")


## 3. Scatter plot of internal vs external temperature
p = ggplot(merged_temps, aes(x = external_temp, y = internal_temp, color = heating_on)) +
	geom_point(alpha = 0.4, size = 1) +
	geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
	scale_color_manual(values = c("TRUE" = "#dc2f02", "FALSE" = "#457b9d"),
	                   labels = c("TRUE" = "Heating On", "FALSE" = "Heating Off")) +
	labs(title = "Internal vs External Temperature",
	     subtitle = "Dashed line shows y=x",
	     color = "Heating Status") +
	xlab("External Temp. [°C]") + ylab("Internal Temp. [°C]") +
	theme_bw() +
	theme(legend.position = "bottom")

p

ggsave("outputs/2026-01-17/14.internal_vs_external_scatter.png", 
       width=14, height=12, units="cm", dpi=150, bg="white")


## 4. Ridge plot of temperature difference by month
merged_temps_month = merged_temps |>
	filter(!is.na(temp_difference))

p = ggplot(merged_temps_month, aes(x = temp_difference, y = fct_rev(month), fill = stat(x))) +
	geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
	scale_x_continuous(expand = c(0.01, 0)) +
	scale_fill_viridis_c(name = "Temp. Diff. [°C]", option = "C") +
	labs(title = "Distribution of Temperature Difference by Month",
	     subtitle = "Temperature difference = Internal - External") +
	xlab("Temperature Difference [°C]") +
	ylab("Month") +
	theme_ridges() + theme(legend.position = "none")

p

ggsave("outputs/2026-01-17/15.temp_difference_by_month.png", 
       width=14, height=12, units="cm", dpi=150, bg="white")


## 5. Box plot showing temperature distributions when heating on vs off
merged_temps_long2 = merged_temps |>
	filter(!is.na(internal_temp), !is.na(external_temp)) |>
	select(date, heating_on, internal_temp, external_temp) |>
	pivot_longer(cols = c(internal_temp, external_temp), 
	             names_to = "temp_type", 
	             values_to = "temperature") |>
	mutate(temp_type = case_when(
		temp_type == "internal_temp" ~ "Internal",
		temp_type == "external_temp" ~ "External"
	),
	heating_status = if_else(heating_on, "Heating On", "Heating Off"))

p = ggplot(merged_temps_long2, aes(x = heating_status, y = temperature, fill = temp_type)) +
	geom_boxplot() +
	scale_fill_manual(values = c("Internal" = "#e63946", "External" = "#457b9d")) +
	labs(title = "Temperature Distributions: Heating On vs Off",
	     fill = "Temperature Type") +
	xlab("Heating Status") + ylab("Temp. [°C]") +
	theme_bw() +
	theme(legend.position = "bottom")

p

ggsave("outputs/2026-01-17/16.heating_on_off_comparison.png", 
       width=14, height=12, units="cm", dpi=150, bg="white")


cat("\n\n=== ANALYSIS COMPLETE ===\n")
cat("Plots and summary files saved to outputs/2026-01-17/\n\n")
