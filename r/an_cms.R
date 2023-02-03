## Contact matrices


library(data.table)
library(ggplot2)
library(viridis)
library(cowplot)
library(patchwork)


# Source user written scripts ---------------------------------------------

source('r/functions/get_minimal_data.R')
source('r/functions/functions.R')
source('r/functions/calc_cm.R')

# Input data ----------------------------------------------------------------

# extract data with useful columns
data =  get_minimal_data()

# decant data into relevant containers
contacts =  data[[1]]
parts =  data[[2]]

#
## set breaks and get population proportions 
breaks = c(0,5,12,18,30,40,50,60,70,Inf)
popdata_uk = get_popvec(breaks, year_ = 2020,country_ = "uk")
popdata_be = get_popvec(breaks, year_ = 2020, country_ = "be")
popdata_nl = get_popvec(breaks, year_ = 2020, country_ = "nl")
popdata_ch = get_popvec(breaks, year_ = 2020, country_ = "ch")


# Get UK ------------------------------------------------------------------
puk <- parts[country == "uk"]
unique_puk_pid <- unique(puk$part_id)
cuk <- contacts[part_id %in% unique_puk_pid]


# Get BE ------------------------------------------------------------------
pbe <- parts[country == "be"]
unique_pbe_pid <- unique(pbe$part_id)
cbe <- contacts[part_id %in% unique_pbe_pid]


# NL ----------------------------------------------------------------------
pnl <- parts[country == "nl"]
unique_pnl_pid <- unique(pnl$part_id)
cnl <- contacts[part_id %in% unique_pnl_pid]


# Switzerland -------------------------------------------------------------
pch <- parts[country == "ch"]
unique_pch_pid <- unique(pch$part_id)
cch <- contacts[part_id %in% unique_pch_pid]

cms_uk = calc_cm(puk, cuk, max_ = 50, weeks_range = 1000, pop_data_ = popdata_uk)
cms_be = calc_cm(pbe, cbe, max_ = 50, weeks_range = 1000, pop_data_ = popdata_be)
cms_nl = calc_cm(pnl, cnl, max_ = 50, weeks_range = 1000, pop_data_ = popdata_nl)
cms_ch = calc_cm(pch, cch, max_ = 50, weeks_range = 1000, pop_data_ = popdata_ch)


# -------------------------------------------------------------------------


cm_plot <- function(cms_, title_ = "All"){
  my_breaks <- c(0,0.05, 0.1, 0.20, 0.5, 1.00, 2,4)
  my_breaks_lab <- c("0","0.05", "0.10", "0.20", "0.5", "1.00", "2.00","4.00")
  tick_labels = c("0-4", "5-11", "12-17", "18-29","30-39",
                  "40-49", "50-59", "60-69", "70+")
  ggplot(cms_[[1]], aes(age_group, age_group_cont, fill= aug_mean_sym)) + 
    geom_tile() + 
    scale_fill_viridis(discrete=FALSE, name='Mean \nContacts', trans = "log",
                       breaks = my_breaks, labels = my_breaks_lab, limits = c(0.02,4))+ 
    ggtitle(title_) +
    geom_text(aes(label = round(aug_mean_sym,1)),
              colour = ifelse(cms_[[1]]$aug_mean_sym >2.5, "black", "white")
                  ) +
    ylab('Contact age group') +
    xlab('Participant age group') +
    scale_x_discrete(labels = tick_labels)+
    scale_y_discrete(labels = tick_labels)+
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          axis.text.x =element_text(angle = 90),
          axis.ticks.x=element_blank(), 
          axis.ticks.y=element_blank(),
          legend.key.size = unit(2, 'cm'),
          legend.key.width = unit(0.5, 'cm'))
    
}


## This works for the moment.
cms_plots <- (cm_plot(cms_uk, title_ = "UK") +
cm_plot(cms_be, title_ = "BE") )/
  (
cm_plot(cms_nl, title_ = "NL") +
cm_plot(cms_ch, title_ = "CH")) +
  patchwork::plot_layout(guides = "collect")


ggsave("outputs/cmatrices.png",plot = cms_plots, height = 10, width = 10)


