
library(socialmixr)

colfunc <- colorRampPalette(c("steelblue", "white"))

# Social contact matrix from Prem et al. (2021)
# Source: https://github.com/kieshaprem/synthetic-contact-matrices
load("data/prem_contact_all.rdata")
prem <- contact_all$CHE
rm(contact_all)
age_prem <- seq(0, 75, 5)
colnames(prem) <- limits_to_agegroups(age_prem)
rownames(prem) <- limits_to_agegroups(age_prem)

par(mfrow = c(1, 2))
barplot(rowSums(prem),
        col = colfunc(3)[2],
        xlab = "Age of individual (years)", ylab = "Number of contacts (per day)")
matrix_plot(prem,
            cex.text = 0, color.palette = colfunc,
            xlab = "Age of individual (years)", ylab = "Age of contact (years)", main = NA)
