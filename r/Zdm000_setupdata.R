## This script is unlikely to go online. 

## Packages
library(data.table)
library(lubridate)

# Load participant data ---------------------------------------------------
part <- qs::qread("../comix/data/part.qs")
contacts <- qs::qread("../comix/data/contacts.qs")

## Reduce to latest data
part <- part[survey_round == 1000]
contacts <- contacts[survey_round == 1000]

# Edit part chracteristics ------------------------------------------------

## Turn age group in factor
part[part_age_group == "Under 1", part_age_group := "0-4"]
age_levs <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-120", "Unknown")
age_labs <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+", "Unknown")

part[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]


## Clean up age so adults aren't children and otherwise
part[sample_type == "child" & !is.na(part_age), part_age_group := NA]
part[is.na(part_age_group), part_age_group := "Unknown"]

part[sample_type == "adult" & part_age_group %in% c("0-4","5-11","12-17"), part_age_group := "Unknown"]

part[, table(part_age_group, country)]

## Gender
gen_levs <- c("female", "male", "other")
gen_labs <- c("Female", "Male", "Other")

part[, part_gender := factor(part_gender, levels = gen_levs, labels = gen_labs)]
part[is.na(part_gender), part_gender := "Other"]

## Weekday
day_levs <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
day_labs <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fr", "Sat")

part[, weekday := factor(weekday, levels = day_levs, labels = day_labs)]

## Household size
part[, hh_size_group := factor(hh_size_group)]

## Country
country_levs <- c("all", "uk", "be", "nl", "ch")
country_labs <- c("All", "UK", "BE", "NL", "CH")

part[, country := factor(country, levels = country_levs, labels = country_labs)]

part[, table(country)]

# Merge on total contacts ------------------------------------------------------
p_cnts <- qs::qread('../comix/data/part_cnts.qs')

# Add on contacts ---------------------------------------------------------
dt <- merge(part, p_cnts, by = "part_wave_uid", all.x = TRUE)

dt <- dt[survey_round == 1000]

## Cut contacts at 50
cnt_names <- grep("n_cnt", names(dt), value = TRUE)

pminv <- function(x) pmin(x,50)
dt_nocap <- copy(dt)
dt[, (cnt_names) := lapply(.SD, pminv), .SDcols= cnt_names] 
dt[, n_cnt_workschool := pmin(n_cnt_work + n_cnt_school, 50)]


## Save locally
qs::qsave(part, file = "data/wrapup_part.qs")
qs::qsave(contacts, file = "data/wrapup_part.qs")
qs::qsave(dt, file = "data/wrapup_part_cnts.qs")
qs::qsave(dt_nocap, file = "data/wrapup_part_cnts_nocap.qs")


