

## Packages
library(data.table)
library(ggplot2)
library(patchwork)


# Load data ---------------------------------------------------------------
cnts <- qs::qread('data/wrapup_contacts.qs')
country_levs <- c("all", "uk", "be", "nl", "ch")
country_labs <- c("All", "UK", "BE", "NL", "CH")
cnts[, country := factor(country, levels = country_levs, labels = country_labs)]


names(cnts)

table(cnts$cnt_frequency)
table(cnts$cnt_type)
table(cnts$cnt_main_type)
table(cnts$cnt_total_time)
table(cnts$cnt_phys)
table(cnts$cnt_prec_mask)
table(cnts$cnt_prec_yn)
table(cnts$cnt_prec)

cnts[, cnt_phys := factor(cnt_phys, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_prec := factor(cnt_prec, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_prec_mask := factor(cnt_prec_mask, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_outside := factor(cnt_outside, levels = c(1,0), labels = c("Yes", "No"))]

# Frequency ----------------------------------------------------------------
p_freq_phys <- ggplot(cnts[!is.na(cnt_frequency)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_phys), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "A: Physical contact?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq_prec <- ggplot(cnts[!is.na(cnt_frequency)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_prec), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "B: Any precautions?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        axis.text.y = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq_prec_mask <- ggplot(cnts[!is.na(cnt_frequency)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_prec_mask), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "C: Wore a mask?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq_prec_outside <- ggplot(cnts[!is.na(cnt_frequency) & !is.na(cnt_outside)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_outside), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "D: Met outside?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq <- (p_freq_phys | p_freq_prec | p_freq_prec_mask | p_freq_prec_outside) + 
  plot_layout(
    widths = c(1.01,1,1,1,1),
    guides = "collect")  & theme(legend.position = "bottom")


# Total time --------------------------------------------------------------
p_time_phys <- ggplot(cnts[!is.na(cnt_total_time)]) +
  geom_bar(aes(x = cnt_total_time, fill = cnt_phys), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "A: Physical contact?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_time_prec <- ggplot(cnts[!is.na(cnt_total_time)]) +
  geom_bar(aes(x = cnt_total_time, fill = cnt_prec), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "B: Any precautions?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_time_prec_mask <- ggplot(cnts[!is.na(cnt_total_time)]) +
  geom_bar(aes(x = cnt_total_time, fill = cnt_prec_mask), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "C: Wore a mask?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_time_prec_outside <- ggplot(cnts[!is.na(cnt_total_time) & !is.na(cnt_outside)]) +
  geom_bar(aes(x = cnt_total_time, fill = cnt_outside), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "D: Met outside?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))




p_time <- (p_time_phys | p_time_prec | p_time_prec_mask | p_time_prec_outside) + 
  plot_layout(
    widths = c(1.01,1,1,1,1),
    guides = "collect")  & theme(legend.position = "bottom")

# Type --------------------------------------------------------------------

p_type_phys <- ggplot(cnts[!is.na(cnt_type)]) +
  geom_bar(aes(x = cnt_type, fill = cnt_phys), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "A: Physical") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "A: Physical contact?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_type_prec <- ggplot(cnts[!is.na(cnt_type)]) +
  geom_bar(aes(x = cnt_type, fill = cnt_prec), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "B: Any precautions") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "B: Any precautions?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_type_prec_mask <- ggplot(cnts[!is.na(cnt_type)]) +
  geom_bar(aes(x = cnt_type, fill = cnt_prec_mask), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "C: Wore a mask") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "C: Wore a mask?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_type_prec_outside <- ggplot(cnts[!is.na(cnt_type) & !is.na(cnt_outside)]) +
  geom_bar(aes(x = cnt_type, fill = cnt_outside), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "D: Met outside?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_type <- (p_type_phys | p_type_prec | p_type_prec_mask | p_type_prec_outside) + 
  plot_layout(
    widths = c(1.01,1,1,1,1),
    guides = "collect")  & theme(legend.position = "bottom")



# Setting -----------------------------------------------------------------

p_sett_phys <- ggplot(cnts[!is.na(cnt_main_type)]) +
  geom_bar(aes(x = cnt_main_type, fill = cnt_phys), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "A: Physical") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "A: Physical contact?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_sett_prec <- ggplot(cnts[!is.na(cnt_main_type) & !is.na(cnt_prec)]) +
  geom_bar(aes(x = cnt_main_type, fill = cnt_prec), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "B: Any precautions") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "B: Any precautions?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        axis.text.y = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_sett_prec_mask <- ggplot(cnts[!is.na(cnt_main_type) & !is.na(cnt_prec_mask)]) +
  geom_bar(aes(x = cnt_main_type, fill = cnt_prec_mask), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "C: Wore a mask") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "C: Wore a mask?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))


p_sett_prec_outside <- ggplot(cnts[!is.na(cnt_main_type) & !is.na(cnt_outside)]) +
  geom_bar(aes(x = cnt_main_type, fill = cnt_outside), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "D: Met outside?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_sett <- (p_sett_phys | p_sett_prec | p_sett_prec_mask | p_sett_prec_outside) + 
  plot_layout(
    widths = c(1.01,1,1,1,1),
    guides = "collect")  & theme(legend.position = "bottom")

ggsave(filename = "Outputs/bar_freq.png", plot = p_freq, height = 10, width = 12) 
ggsave(filename = "Outputs/bar_time.png", plot = p_time, height = 10, width = 12) 
ggsave(filename = "Outputs/bar_type.png", plot = p_type, height = 10, width = 12) 
ggsave(filename = "Outputs/bar_sett.png", plot = p_sett, height = 10, width = 12) 

