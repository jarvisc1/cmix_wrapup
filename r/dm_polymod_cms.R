## dm_polymod_cms


library(socialmixr)
library(data.table)

data("polymod")

source('r/functions/functions.R')

parts_poly = data.table(polymod$participants)
conts_poly = data.table(polymod$contacts)

parts_poly[!is.na(part_age), part_age_est_min := part_age]
parts_poly[!is.na(part_age), part_age_est_max := part_age]
conts_poly[!is.na(cnt_age_exact), cnt_age_est_min := cnt_age_exact]
conts_poly[!is.na(cnt_age_exact), cnt_age_est_max := cnt_age_exact]
conts_poly = conts_poly[!is.na(cnt_age_est_max)]

## Get for each country
parts_poly_uk = parts_poly[country == 'United Kingdom']
parts_poly_nl = parts_poly[country == 'Netherlands']
parts_poly_be = parts_poly[country == 'Belgium']
conts_poly_uk = conts_poly[part_id %in% parts_poly_uk$part_id]
conts_poly_nl = conts_poly[part_id %in% parts_poly_nl$part_id]
conts_poly_be = conts_poly[part_id %in% parts_poly_be$part_id]

## Make at same age breaks
breaks = c(0,5,12,18,30,40,50,60,70,Inf)
popdata_totals_uk = get_popvec(breaks, year_ = 2006, country_ = "uk")
popdata_totals_nl = get_popvec(breaks, year_ = 2006, country_ = "nl")
popdata_totals_be = get_popvec(breaks, year_ = 2006, country_ = "be")


get_domeig <- function(parts_poly_, conts_poly_, popdata_totals_) {
  ct_ac = get_age_table(parts = parts_poly_, conts = conts_poly_ , breaks = breaks)
  cont_per_age_per_part = ct_ac[[1]]
  all_conts = ct_ac[[2]]
  cont_per_age_per_part = correct_truncation(cont_per_age_per_part, all_conts = all_conts, breaks = breaks, trunc=28)
  eg = get_matrix_2(cont_per_age_per_part, breaks, trunc=1000)
  eg_props = symetricise_matrix(eg = eg,popdata_totals=popdata_totals_,breaks = breaks)
  
  max(eigen(matrix(eg_props$aug_mean_sym, nrow = (length(breaks) - 1)))$values)
}


pmod_eig_uk <- get_domeig(parts_poly_uk, conts_poly_uk, popdata_totals_uk)
pmod_eig_nl <- get_domeig(parts_poly_nl, conts_poly_nl, popdata_totals_nl)
pmod_eig_be <- get_domeig(parts_poly_be, conts_poly_be, popdata_totals_be)
pmod_eig_ch <- get_domeig(parts_poly, conts_poly, popdata_totals_uk)

qs::qsave(pmod_eig_uk, file = "outputs/cm_data/pmod_uk_eig.qs")
qs::qsave(pmod_eig_be, file = "outputs/cm_data/pmod_be_eig.qs")
qs::qsave(pmod_eig_nl, file = "outputs/cm_data/pmod_nl_eig.qs")
qs::qsave(pmod_eig_ch, file = "outputs/cm_data/pmod_ch_eig.qs")




