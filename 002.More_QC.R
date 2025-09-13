###QC
setwd("./data")
library(tidyverse)
library(patchwork)
library(ggpubr)
#TMT PP
psm_data <- read.table("./TMT/HFX2_FFRA20H1013_PSMs.txt",header = T) #TMT
#psm_data <- read.table("./PP/413797_combine_PSMs.txt",header = T)    #pp
colnames(psm_data)

# mass accuracy
mean_error <- mean(psm_data$DeltaM..ppm., na.rm = TRUE)
sd_error <- sd(psm_data$DeltaM..ppm., na.rm = TRUE)
summary(psm_data$m.z..Da.)
summary(psm_data$DeltaM..ppm.)

p11 <- ggplot(psm_data, aes(x = m.z..Da., y = DeltaM..ppm.)) +    #PP=p22
  geom_point(data = subset(psm_data, abs(DeltaM..ppm. - mean_error) > 2 * sd_error),
             color = "#758a99", alpha = 0.8, size = 1) +
  
  geom_point(data = subset(psm_data, abs(DeltaM..ppm. - mean_error) <= 2 * sd_error),
             color = "#057748", alpha = 0.8, size = 1) +
  
  geom_hline(yintercept = mean_error, 
             color = "gray30", 
             linewidth = 0.8) + 
  
  geom_hline(yintercept = c(mean_error + sd_error, 
                            mean_error - sd_error),
             color = "#be002f", 
             linewidth = 0.5,
             linetype = "dashed") +  
  
  geom_hline(yintercept = c(mean_error + 2*sd_error, 
                            mean_error - 2*sd_error),
             color = "#be002f", 
             linewidth = 0.8,
             linetype = "dashed") +  # ±2SD线
  
  annotate("text", x = Inf, y = mean_error, 
           label = sprintf("Mean: %.2f ppm", mean_error),
           color = "black", hjust = 1, vjust = -0.5, size = 3) +
  
  annotate("text", x = Inf, y = mean_error + 2*sd_error, 
           label = sprintf("+2SD: %.2f ppm", mean_error + 2*sd_error),
           color = "#be002f", hjust = 1, vjust = -0.5, size = 3) +
  
  annotate("text", x = Inf, y = mean_error - 2*sd_error, 
           label = sprintf("-2SD: %.2f ppm", mean_error - 2*sd_error),
           color = "#be002f", hjust = 1, vjust = 1.5, size = 3) +
  
  scale_x_continuous(
    name = "m/z",
    limits = c(300, 1800),
    breaks = seq(300, 1800, 300)
  ) +
  
  scale_y_continuous(
    name = "Mass error (ppm)",
    limits = c(-9, 10),
    breaks = seq(-9, 10, 2)
  ) +
  
  labs(
    title = "",
    subtitle = sprintf("%d PSMs | SD = %.2f ppm | %.1f%% within ±2SD", 
                       nrow(psm_data), 
                       sd_error,
                       100 * mean(abs(psm_data$DeltaM..ppm. - mean_error) <= 2 * sd_error))
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    #panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 8),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    legend.position = "none"  
  )

print(p11)
print(p22)

prot_psm <- read.delim("./TMT/HFX2_FFRA20H1013_PSMs.txt") %>% 
  mutate(Group = "Protein")

phos_psm <- read.delim("./PP/413797_combine_PSMs.txt") %>% 
  mutate(Group = "Phosphoprotein")

combined_psm <- bind_rows(prot_psm, phos_psm) %>%
  mutate(
    Peptide_Length = nchar(gsub("\\[.*?\\]", "", Annotated.Sequence)),
    log10_PEP = -log10(Percolator.PEP + 1e-10)
  )

library(tidyverse)
library(patchwork)
library(ggpubr)

# basic statistics
p_count <- combined_psm %>%
  group_by(Group) %>%
  summarise(
    PSM = n(),
    Peptides = n_distinct(Annotated.Sequence),
    Proteins = n_distinct(Master.Protein.Accessions)
  ) %>%
  pivot_longer(-Group) %>%
  ggplot(aes(x = name, y = value, fill = Group)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::comma(value)), 
            position = position_dodge(width = 0.9), vjust = -0.8,
            size = 2, 
            hjust = 0.5,    
            check_overlap = FALSE,  
            ) +
  labs(title = "Identification Summary", x = "", y = "Count") +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))  

combined <- bind_rows(
  prot_psm %>% mutate(Group = "Proteome"),
  phos_psm %>% mutate(Group = "Phosphoproteome")
) %>%
  mutate(
    Charge = as.integer(Charge),
    MH...Da. = as.numeric(MH...Da.),
    m.z..Da. = as.numeric(m.z..Da.)
  )

# Charge distribution 
p_charge <- ggplot(combined, aes(x = as.factor(Charge), fill = Group)) +
  geom_bar(position = position_dodge(preserve = "single"), alpha = 1) +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7")) +
  labs(title = "Precursor Ion Charge Distribution",
       x = "Charge State",
       y = "PSM Count",
       fill = "Group") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# Monoisotopic mass 
p_mass <- ggplot(combined, aes(x = MH...Da., fill = Group)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7")) +
  labs(title = "Peptide Monoisotopic Mass Distribution",
       x = "Mass (Da)",
       y = "Density") +
  xlim(500, 5000) +  # main mass range
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Precursor m/z 
p_mz <- ggplot(combined, aes(x = m.z..Da., fill = Group)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7")) +
  labs(title = "Precursor m/z Distribution",
       x = "m/z",
       y = "Density") +
  xlim(300, 2000) +  # Typical m/z range
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Missed.Cleavages
p_missed <- combined %>%
  count(Group, X..Missed.Cleavages) %>%
  group_by(Group) %>%
  mutate(Percent = n/sum(n)*100) %>%
  ggplot(aes(x = factor(X..Missed.Cleavages), y = Percent, fill = Group)) +
  geom_col(position = "dodge") +
  labs(title = "Missed Cleavages", x = "Number of Missed Cleavages") +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7"))
# PEP
combined <- bind_rows(
  prot_psm %>% mutate(Group = "Proteome"),
  phos_psm %>% mutate(Group = "Phosphoproteome")
) %>%
  mutate(
     neg_log10_PEP = -log10(ifelse(Percolator.PEP == 0, 1e-10, Percolator.PEP))
  )
# PEP distribution
p_pep <- ggplot(combined, aes(x = neg_log10_PEP, fill = Group)) +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7")) +
  labs(title = "PEP Distribution (-log10 scale)",
       x = "-log10(PEP)",
       y = "Density",
       fill = "Group",
       caption = "Threshold at -log10(PEP) = 2 (PEP = 0.01)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

## Add histogram overlay
p_pep_hist <- ggplot(combined, aes(x = neg_log10_PEP, fill = Group)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.8, position = "identity") +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(values = c("#ffbd39", "#4E79A7")) +
  labs(title = "PEP Distribution with Histogram",
       x = "-log10(PEP)",
       y = "Density") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

## percentage above threshold
pep_stats <- combined %>%
  group_by(Group) %>%
  summarise(
    `Mean PEP` = mean(Percolator.PEP, na.rm = TRUE),
    `Median -log10(PEP)` = median(neg_log10_PEP, na.rm = TRUE),
    `% above threshold` = mean(neg_log10_PEP >= 2) * 100,
    `Total PSM` = n(),
    `High-confidence PSM` = sum(neg_log10_PEP >= 2)
  )

print(pep_stats)

# Combine plots
final_plot <- (p_count| p_charge )/ ( p_missed | p_pep_hist) / 
  (p_mass | p_mz) /  (p11 | p22) +
  plot_annotation(tag_levels = "A") +
  plot_annotation(
    title = "QC Analysis: Proteome vs Phosphoproteome",
    subtitle = "",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      #plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm") 
    )
  )
# ggsave("QC_basedPSM_count_charge_pep_misscleave_mass_mz_A4.pdf", width = 8,height = 11)
# ggsave("QC_basedPSM_count_charge_pep_misscleave_mass_mz.tiff", width = 14,height = 16, dpi = 300)
# ggsave("QC_basedPSM_count_charge_pep_misscleave_mass_mz.jpg", width = 14,height = 16, dpi = 300)

# suitable for A4
perfect_a4_fit <- function(plot, filename) {
  ggsave("temp.pdf", plot, width = a4_width, height = a4_height)
  file_size <- file.info("temp.pdf")$size
  optimal_scale <- ifelse(file_size > 2e6, 0.7, ifelse(file_size > 1e6, 0.8, 0.9))
  optimized_plot <- plot & theme(text = element_text(size = 10 * optimal_scale))
  
  ggsave(filename, optimized_plot, width = a4_width, height = a4_height, dpi = 600)
  
  file.remove("temp.pdf")
  
  return(optimal_scale)
}

font_scale <- perfect_a4_fit(final_plot, "QC_basedPSM_count_charge_pep_misscleave_mass_mz.pdf")
font_scale <- perfect_a4_fit(final_plot, "QC_basedPSM_count_charge_pep_misscleave_mass_mz.tiff")
font_scale <- perfect_a4_fit(final_plot, "QC_basedPSM_count_charge_pep_misscleave_mass_mz.jpg")




