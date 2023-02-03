

## Packages
library(data.table)
library(lubridate)
library(flextable)
library(gtsummary)
library(magrittr)



# Load data ---------------------------------------------------------------
dt <- qs::qread('data/wrapup_part_cnts.qs')


# Filter to needed vars ---------------------------------------------------
## At the end put all these at the begining

## Age, Gender, Household size, day of week, country, High risk, Face mask wearing
## Employed, attending work


dt <- dt[, .(
  part_age_group, 
  sample_type,
  part_gender, 
  hh_size_group, 
  weekday,
  country,
  part_high_risk, 
  part_high_risk_v2,
  part_face_mask,
  n_cnt
  )]


dt_copy <- dt
dt_copy$new <- 1
dt$new <- 0
dta <- rbind(dt, dt_copy)

dta[new == 1, country := "All"]
dta[, table(country)]



# Get percentages ---------------------------------------------------------

## Will remove unkown and not include in perc calculation
## Using the get function turns the varibale name to get
get_perc <- function(dt_, group_var_) {
  top <- dt_[, .(num = .N), by = .(country, get(group_var_))]
  bottom <- dt_[!is.na(get(group_var_)) & !(get(group_var_) %in% c("Unknown", "Other")),
                .(denom = .N), by = country]
  x1 <- merge(top, bottom)
  x1[!(get %in% c("Unknown", "Other")), 
     text:= paste0(formatC(num, big.mark = ","), 
                   " (", 
                   formatC(num/denom*100, digits = 1, format = "f"),
                   "%)")]
  x1[get %in% c("Unknown", "Other"), 
     text:= formatC(num, big.mark = ",")]
  x1
}


## Get percentages
per_adult <- get_perc(dta, "sample_type")
per_age_child   <- get_perc(dta[sample_type == "child"], "part_age_group")
per_age_adult   <- get_perc(dta[sample_type == "adult"], "part_age_group")
per_gen   <- get_perc(dta, "part_gender")
per_hh    <- get_perc(dta, "hh_size_group")
per_day   <- get_perc(dta, "weekday")
per_hr <- get_perc(dta, "part_high_risk")
per_fm <- get_perc(dta, "part_face_mask")


mean_cnt <- dta[, .(text = paste0(
  formatC(mean(n_cnt), digits = 1, format = "f"),
  " (",
  formatC(sd(n_cnt), digits = 1, format = "f"),
  ")"
  )
  ),
  by = country]




## Now reshape the above function made all vars to get

row_count <- dcast(dta, . ~ country, )
row_adult <- dcast(per_adult, get ~ country, value.var = "text")
row_age_child   <- dcast(per_age_child, get ~ country, value.var = "text")
row_age_adult   <- dcast(per_age_adult, get ~ country, value.var = "text")
row_gen   <- dcast(per_gen, get ~ country, value.var = "text")
row_hh    <- dcast(per_hh, get ~ country, value.var = "text")
row_day   <- dcast(per_day, get ~ country, value.var = "text")
row_hr   <- dcast(per_hr, get ~ country, value.var = "text")
row_fm   <- dcast(per_fm, get ~ country, value.var = "text")
row_cnt <- dcast(mean_cnt, . ~ country)

row_count[, All := formatC(All, big.mark = ",")] 
row_count[, UK  := formatC(UK, big.mark = ",")] 
row_count[, BE  := formatC(BE, big.mark = ",")] 
row_count[, NL  := formatC(NL, big.mark = ",")] 
row_count[, CH  := formatC(CH, big.mark = ",")] 

## Formatting for a row
make_row <- function(row_, cat , val, top_row = FALSE){
  row_[, Value := val]
  if(top_row){
    row_[1, Category := cat]
  }else{
  row_[, Category := cat]
  }  
  row_[, .(Category, Value, All, UK, BE, NL, CH)]
}

## Format each row, remember each var is now called get
row_1 <- make_row(row_count, "All", "")
row_2   <- make_row(row_adult, c("Adult", "Child"), "")
row_3   <- make_row(row_age_child, cat = "Age group (Children)", val = row_age_child$get, top_row = TRUE)
row_4   <- make_row(row_age_adult, cat = "Age group (Adult)", val = row_age_adult$get, top_row = TRUE)
row_5   <- make_row(row_gen, cat = "Gender", val = row_gen$get, top_row = TRUE)
row_6   <- make_row(row_hh, cat = "Household size", val = row_hh$get, top_row = TRUE)
row_7   <- make_row(row_day, cat = "Day of week", val = row_day$get, top_row = TRUE)
row_8   <- make_row(row_hr, cat = "High risk", val = row_hr$get, top_row = TRUE)
row_9   <- make_row(row_fm, cat = "Face mask", val = row_fm$get, top_row = TRUE)
row_10   <- make_row(row_cnt, cat = "Contacts mean (sd)", "")

tab1 <- rbind(row_1, 
      row_2,
      row_3,
      row_4,
      row_5,
      row_6,
      row_7,
      row_8,
      row_9,
      row_10
      ) %>% 
  flextable()

tab1


print(tab1, preview = "docx")


dt[, table(country, part_high_risk, useNA = "ifany")]



