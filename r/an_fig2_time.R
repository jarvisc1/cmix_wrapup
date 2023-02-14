# an_fig2_time

## Packages
library(data.table)
library(ggplot2)
library(patchwork)


# Load data ---------------------------------------------------------------
cnts <- qs::qread('data/wrapup_contacts.qs')

cnts <- cnts[survey_round == 1000]
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

cnts[, cnt_phys := factor(cnt_phys, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_prec_2m_plus := factor(cnt_prec_2m_plus, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_prec_mask := factor(cnt_prec_mask, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_outside := factor(cnt_outside, levels = c(1,0), labels = c("Yes", "No"))]


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

p_time_prec <- ggplot(cnts[!is.na(cnt_total_time) & !is.na(cnt_prec_2m_plus)]) +
  geom_bar(aes(x = cnt_total_time, fill = cnt_prec_2m_plus), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "B: 2 meter distance?") +
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


ggsave(filename = "Outputs/fig2_bar_time.png", plot = p_time, height = 10, width = 12) 

