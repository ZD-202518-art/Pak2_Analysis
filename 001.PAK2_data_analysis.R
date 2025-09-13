setwd("./data")
library(tidyverse)
library(ggplot2)
library(ggrepel)

#TMT
psm_data <- read.table("./TMT/HFX2_FFRA20H1013_PSMs.txt",header = T) #TMT
#psm_data <- read.table("./PP/413797_combine_PSMs.txt",header = T)    #pp
colnames(psm_data)
mean_error <- mean(psm_data$DeltaM..ppm., na.rm = TRUE)
sd_error <- sd(psm_data$DeltaM..ppm., na.rm = TRUE)
summary(psm_data$m.z..Da.)
summary(psm_data$DeltaM..ppm.)

p1 <- ggplot(psm_data, aes(x = m.z..Da., y = DeltaM..ppm.)) +
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
             linetype = "dashed") + 
  
    annotate("text", x = Inf, y = mean_error, 
           label = sprintf("Mean: %.2f ppm", mean_error),
           color = "black", hjust = 1, vjust = -0.5, size = 5) +
    annotate("text", x = Inf, y = mean_error + 2*sd_error, 
           label = sprintf("+2SD: %.2f ppm", mean_error + 2*sd_error),
           color = "#be002f", hjust = 1, vjust = -0.5, size = 5) +
    annotate("text", x = Inf, y = mean_error - 2*sd_error, 
           label = sprintf("-2SD: %.2f ppm", mean_error - 2*sd_error),
           color = "#be002f", hjust = 1, vjust = 1.5, size = 5) +
  
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
  
  theme_minimal(base_size = 14) +
  theme(
    #panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none" 
  )

print(p1)

##pp
psm_data <- read.table("./PP/413797_combine_PSMs.txt",header = T)    #pp
colnames(psm_data)
mean_error <- mean(psm_data$DeltaM..ppm., na.rm = TRUE)
sd_error <- sd(psm_data$DeltaM..ppm., na.rm = TRUE)
summary(psm_data$m.z..Da.)
summary(psm_data$DeltaM..ppm.)

p1 <- ggplot(psm_data, aes(x = m.z..Da., y = DeltaM..ppm.)) +
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
             linetype = "dashed") +  
  
    annotate("text", x = Inf, y = mean_error, 
           label = sprintf("Mean: %.2f ppm", mean_error),
           color = "black", hjust = 1, vjust = -0.5, size = 5) +
    annotate("text", x = Inf, y = mean_error + 2*sd_error, 
           label = sprintf("+2SD: %.2f ppm", mean_error + 2*sd_error),
           color = "#be002f", hjust = 1, vjust = -0.5, size = 5) +
    annotate("text", x = Inf, y = mean_error - 2*sd_error, 
           label = sprintf("-2SD: %.2f ppm", mean_error - 2*sd_error),
           color = "#be002f", hjust = 1, vjust = 1.5, size = 5) +
    scale_x_continuous(
    name = "m/z",
    limits = c(300, 1800),
    breaks = seq(300, 1800, 300)
  ) +
    scale_y_continuous(
    name = "Mass error (ppm)",
    limits = c(-11, 11),
    breaks = seq(-11, 11, 2)
  ) +
    labs(
    title = "",
    subtitle = sprintf("%d PSMs | SD = %.2f ppm | %.1f%% within ±2SD", 
                       nrow(psm_data), 
                       sd_error,
                       100 * mean(abs(psm_data$DeltaM..ppm. - mean_error) <= 2 * sd_error))
  ) +
    theme_minimal(base_size = 14) +
    theme(
    #panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"  
  )

print(p1)
##

#QC
library(ggplot2)
library(dplyr)
library(stringr)
qc_tmt <- read.table("./TMT/QC_usage_data_TMT.txt", header = T)
qc_pp <- read.table("./PP/QC_usage_data_PP.txt", header = T)

#peptide length distribution
peptide_data <- qc_tmt
peptide_data <- qc_pp
data <- peptide_data %>%
  mutate(
    core_sequence = str_extract(Sequence, "(?<=\\.).+?(?=\\.)"),
    length = nchar(core_sequence),
    modified = str_detect(core_sequence, "[a-z]")
  )

summary(data$length)
plot_pep_len_distribution <- function(data, fillcolour, group) {
  mean_length <- mean(data$length, na.rm = TRUE)
  max_count <- max(hist(data$length, plot = FALSE)$counts)
  p <- ggplot(data, aes(x = length)) +
      geom_histogram(
      binwidth = 1, 
      fill = fillcolour, 
      color = "white",
      alpha = 1
    ) +
    geom_vline(
      xintercept = mean_length, 
      color = "red", 
      linetype = "dashed",
      linewidth = 0.8
    ) +
    annotate(
      "text", 
      x = mean_length + 2, 
      y = max_count * 0.6,
      label = paste("Mean length:", round(mean_length, 1)),
      color = "black",
      hjust = 0  
    ) +
    labs(
      title = paste("Peptide length distribution in", group),
      x = "Peptide length (amino acids)",
      y = "Peptide number"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    ) +
    scale_x_continuous(breaks = seq(0, max(data$length, na.rm = TRUE), by = 5)) 
  
    ggsave(
    filename = paste0(group, "_01.peptide_length_distribution.pdf"), 
    plot = p,
    width = 10, 
    height = 6
  )
  return(p)
}

plot_pep_len_distribution(data,"#00adb5","proteome")
plot_pep_len_distribution(data,"#ffde7d","phosphoproteome")

mass_error_ppm <- qc_tmt$Delta_Da
library(ggplot2)
library(patchwork)  

mass_errors <- qc_tmt$Delta_Da #TMT
mass_errors <- qc_pp$Delta_Da  #PP

mean_val <- mean(mass_errors, na.rm = TRUE)
sd_val <- sd(mass_errors, na.rm = TRUE)
median_val <- median(mass_errors, na.rm = TRUE)
mad_val <- mad(mass_errors, na.rm = TRUE)

density_plot <- ggplot(data.frame(mass_error = mass_errors), 
                       aes(x = mass_error)) +
  #geom_density(fill = "#3f72af", alpha = 0.5, color = "#3f72af", linewidth = 0.8) + #TMT
  geom_density(fill = "#ffde7d", alpha = 0.5, color = "#ffde7d", linewidth = 0.8) + #pp
  #geom_vline(xintercept = mean_val, color = "#3f72af", linetype = "solid", linewidth = 0.8) +
  geom_vline(xintercept = mean_val, color = "#ffde7d", linetype = "solid", linewidth = 0.8) +
  #geom_vline(xintercept = c(mean_val - sd_val, mean_val + sd_val), 
  #           color = "#3f72af", linetype = "dotted", linewidth = 0.6) +
  geom_vline(xintercept = c(mean_val - sd_val, mean_val + sd_val), 
             color = "#ffde7d", linetype = "dotted", linewidth = 0.6) +
  geom_vline(xintercept = c(mean_val - 2*sd_val, mean_val + 2*sd_val), 
             color = "darkred", linetype = "dashed", linewidth = 0.8) +
  
  stat_function(fun = dnorm, 
                args = list(mean = mean_val, sd = sd_val),
                geom = "area", fill = "#59A14F", alpha = 0.3) +
  
    labs(
    title = '',
    subtitle = "",
    x = "Precursor ion tolerance",
    y = "Density"
  ) +
  
   theme_minimal(base_size = 12) +
   theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    legend.position = "top"
  ) +
    scale_x_continuous(breaks = seq(-0.1, 0.1, by = 0.002)) +
    annotate("text", x = mean_val + 2*sd_val, y = 0.5, 
           label = paste("Mean:", round(mean_val, 4), "ppm"), 
           color = "black", hjust = 0.8) 
density_plot

fit <- lm(density(mass_errors)$y ~ dnorm(density(mass_errors)$x, mean_val, sd_val))
r_squared <- summary(fit)$r.squared

p <- density_plot + annotate("text", x = min(mass_errors), y = max(density(mass_errors)$y),
                             label = paste("R² =", round(r_squared, 4)),
                             hjust = 0, vjust = 1, color = "black")
p          
#ggsave("02.PP_Precursor ion tolerance_density_distribution.pdf")

#03.uniq peptide distribution
#qc_data <- qc_tmt/qc_pp
############## merge TMT-PP & uniq peptide ##################
library(ggplot2)
library(dplyr)
library(patchwork) 
plot_uniq_pep_distribution2 <- function(qc_data, color, group) {
    freq_table <- table(factor(qc_data$uniq, levels = 0:max(qc_data$uniq)))
    cumulative_data <- as.data.frame(freq_table) |>
    transform(
      peptides = as.numeric(as.character(Var1)),
      freq = Freq,
      cum_freq = cumsum(Freq) / sum(Freq) * 100
    ) |>
    filter(peptides >= 1) |>
    mutate(group = group)  
  
  return(cumulative_data)
}

data_tmt <- plot_uniq_pep_distribution2(qc_tmt, "#4E79A7", "TMT")
data_pp <- plot_uniq_pep_distribution2(qc_pp, "#ffbd39", "PP")
combined_data <- bind_rows(data_tmt, data_pp)

combined_plot <- ggplot(combined_data, aes(x = peptides, y = cum_freq, color = group, fill = group)) +
  geom_step(linewidth = 1.2, alpha = 0.8) +
  scale_color_manual(values = c("TMT" = "#4E79A7", "PP" = "#ffbd39")) +
  scale_fill_manual(values = c("TMT" = "#4E79A7", "PP" = "#ffbd39")) +
  scale_x_continuous(
    breaks = seq(0, max(combined_data$peptides), by = 4),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
    labs(
    x = "Number of Unique Peptides",
    y = "Cumulative Percentage of Proteins (%)",
    title = "",
    fill = "Method"
  ) +
    theme_minimal(base_size = 12) +
    theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) 

ggsave(
  filename = "03_TMT_PP_combined_uniq_pep_distribution.pdf",
  plot = combined_plot,
  width = 10,
  height = 7
)

combined_plot

#04.protein coverage
library(ggplot2)
library(dplyr)
qc_tmt <- read.table("./TMT/QC_usage_data_TMT.txt", header = T)
qc_pp <- read.table("./PP/QC_usage_data_PP.txt", header = T)

tmt_counts <- qc_tmt[!qc_tmt$coverage=='-',]
pp_counts <- qc_pp[!qc_pp$coverage=='-',]
tmt_counts$coverage <- as.numeric(tmt_counts$coverage)
pp_counts$coverage <- as.numeric(pp_counts$coverage)
tmt_counts$group <- 'TMT'
pp_counts$group <- 'PP'
bind_rows(tmt_counts, pp_counts) %>%
  mutate(
    coverage = coverage / 100,  
    bin = cut(coverage, 
              breaks = seq(0, 1, 0.1),
              include.lowest = TRUE)
  ) %>%
ggplot(aes(x = bin, fill = group)) +
geom_bar(position = "dodge") +
scale_fill_manual(values = c("TMT" = "#4E79A7", "PP" = "#ffbd39")) +
labs(x = "Protein coverage",
       y = "Protein number",
       fill = "Group") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        legend.title = element_text(face = "bold"))

#05.protein_mass
tmt_counts$MW_kDa <- as.numeric(tmt_counts$MW_kDa)
pp_counts$MW_kDa <- as.numeric(pp_counts$MW_kDa)
tmt_counts$group <- 'TMT'
pp_counts$group <- 'PP'
bind_rows(tmt_counts, pp_counts) %>%
  mutate(
    MW_kDa = round(MW_kDa,0),  
    bin = cut(MW_kDa, 
              breaks = c(seq(0, 100, 10), Inf),  
              include.lowest = TRUE,
              labels = c("0-10", "10-20", "20-30", "30-40", "40-50", 
                         "50-60", "60-70", "70-80", "80-90", "90-100", ">100"))
  ) %>%
  ggplot(aes(x = bin, fill = group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("TMT" = "#4E79A7", "PP" = "#ffbd39")) +
  labs(x = "Protein mass (kDa)",
       y = "Protein number",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        legend.title = element_text(face = "bold"))

##CV
ex_tmt <- read.csv("./exp_TMT/all_sample_TMT.csv",header = T)
ex_pp <- read.csv("./exp_PP/all_sample_pp.csv",header = T)
summary(ex_tmt)
colnames(ex_tmt) <- sub('^PAK2','Pak2',colnames(ex_tmt))
colnames(ex_pp) <- sub('^PAK2','Pak2',colnames(ex_pp))

library(tidyverse)
library(pheatmap)
library(matrixStats)

ex_tmt_order <- ex_tmt[,order(colnames(ex_tmt))]
ex_tmt_order$pro_gene <- paste(ex_tmt$Protein,ex_tmt$Gene,sep = '_')
rownames(ex_tmt_order) <- ex_tmt_order$pro_gene
#ex_tmt_order <- ex_tmt_order[,grep("^Pak",colnames(ex_tmt_order))]

ex_pp_order <- ex_pp[,order(colnames(ex_pp))]
ex_pp_order$pro_gene <- paste(ex_pp$Protein,ex_pp$Gene,sep = '_')
rownames(ex_tmt_order) <- ex_pp_order$pro_gene
#ex_pp_order <- ex_pp_order[,grep("^Pak",colnames(ex_pp_order))]

ex_filt <- ex_tmt_order %>% 
  rowwise() %>% 
  filter(
    sum(c_across(groups$Pak2c_S) > 0) >= 2,
    sum(c_across(groups$Pak2f_S) > 0) >= 2,
    sum(c_across(groups$Pak2f_T) > 0) >= 2,
    sum(c_across(groups$Pak2c_T) > 0) >= 2
  ) %>% 
  ungroup()
#############################
ex_filt_tmt <- ex_filt
#############################

ex_filt <- ex_pp_order %>% 
  rowwise() %>% 
  filter(
    sum(c_across(groups$Pak2c_S) > 0) >= 2,
    sum(c_across(groups$Pak2f_S) > 0) >= 2,
    sum(c_across(groups$Pak2f_T) > 0) >= 2,
    sum(c_across(groups$Pak2c_T) > 0) >= 2
  ) %>% 
  ungroup()

quantile_normalize <- function(matrix) {
  sorted_matrix <- apply(matrix, 2, sort, na.last = TRUE)
  row_means <- rowMeans(sorted_matrix, na.rm = TRUE)
  normalized_matrix <- apply(matrix, 2, function(col) {
  na_idx <- is.na(col)
  valid_col <- col[!na_idx]
  ranks <- rank(valid_col, ties.method = "average")
  result <- rep(NA, length(col))
  result[!na_idx] <- row_means[ranks]
  return(result)
  })
    return(normalized_matrix)
}
expr_mat <- as.matrix(ex_filt)
rownames(expr_mat) <- ex_filt$pro_gene
expr_mat_numeric <- as.data.frame(expr_mat) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.x))))
expr_log <- log2(expr_mat_numeric[,grep("^Pak",colnames(expr_mat))] + 1)
expr_norm <- quantile_normalize(expr_log)
ex_norm <- cbind(rownames(expr_mat), expr_norm)
ex_norm <- as.data.frame(ex_norm)
rownames(ex_norm) <- ex_norm$V1
ex_norm$V1 <- NULL
#############################################
ex_norm_pp <- ex_norm
cv_data_pp <- cv_data
ex_filt_pp <- ex_filt
ex_norm_tmt <- ex_norm
cv_data_tmt <- cv_data
ex_filt_tmt <- ex_filt
#############################################

safe_cv <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA)  
  if (all(x == 0)) return(NA)   
  if (mean(x) == 0) return(NA)  
  sd(x) / mean(x) * 100
}

cv_data <- ex_norm %>%
  mutate(
    CV_Pak2c_S = apply(select(., all_of(groups$Pak2c_S)), 1, safe_cv),
    CV_Pak2f_S = apply(select(., all_of(groups$Pak2f_S)), 1, safe_cv),
    CV_Pak2f_T = apply(select(., all_of(groups$Pak2f_T)), 1, safe_cv),
    CV_Pak2c_T = apply(select(., all_of(groups$Pak2c_T)), 1, safe_cv),
  )
Avg_CV = rowMeans(cv_data[,grep("^CV",colnames(cv_data))])
class(cv_data[,20])

library(tidyverse)
library(cowplot)  
library(gridExtra)

cv_data$Protein <- rownames(cv_data)
cdf_data <- cv_data %>%
  select(Protein, CV_Pak2c_S, CV_Pak2f_S, CV_Pak2f_T, CV_Pak2c_T) %>%
  pivot_longer(
    cols = -Protein,
    names_to = "Group",
    values_to = "CV"
  ) %>%
  mutate(
    Group = case_when(
      Group == "CV_Pak2c_S" ~ "Pak2c_S",
      Group == "CV_Pak2f_S" ~ "Pak2f_S",
      Group == "CV_Pak2f_T" ~ "Pak2f_T",
      Group == "CV_Pak2c_T" ~ "Pak2c_T"
    )
  ) %>%
  drop_na(CV) 

group_colors <- c(
  "Pak2c_S" = "#1f77b4",
  "Pak2f_S" = "#009999",
  "Pak2f_T" = "#FF7400",
  "Pak2c_T" = "#FF0000"
)

cdf_plot_data <- cdf_data %>%
  group_by(Group) %>%
  arrange(CV) %>%
  mutate(Fraction = seq(1/n(), 1, length.out = n())) %>%
  ungroup()

cumulative_plot <- ggplot(cdf_plot_data, aes(x = CV, y = Fraction, color = Group)) +
  geom_step(size = 1.2, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Coefficient of Variation (CV, %)",
    y = "Cumulative Fraction of Proteins",
    title = "Protein Expression Variation Across Groups in proteome/Phosphoproteome"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #legend.position = c(0.75, 0.25), #tmt legend position
    legend.position = c(0.75, 0.75), #pp legend position
    axis.line = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) 

cumulative_plot

#heatmap
ex_norm <- ex_norm_tmt #tmt 
ex_norm <- ex_norm_pp  #pp
ex_norm <- na.omit(ex_norm)
top_var <- order(apply(ex_norm, 1, sd, na.rm = TRUE), decreasing = TRUE)[1:50]
heatmap_data <- ex_norm[top_var, ]
heatmap_data <- as.data.frame(heatmap_data)  
class(heatmap_data$Pak2c_S1)
heatmap_data <- data.frame(row.names(heatmap_data),sapply(heatmap_data, as.numeric) ) 
rownames(heatmap_data) <- heatmap_data$row.names.heatmap_data.
heatmap_data$row.names.heatmap_data. <- NULL
valid_rows <- apply(heatmap_data, 1, function(x) {
  !any(is.na(x)) && !any(is.infinite(x)) && var(x, na.rm = TRUE) > 0
})
table(valid_rows)
heatmap_data[table(valid_rows),]
heatmap_data_valid <- heatmap_data[valid_rows, ]
rownames(heatmap_data_valid) <- rownames(heatmap_data)[valid_rows]
heatmap_data <- as.matrix(heatmap_data_valid)

########################################################
heatmap_data_tmt <- heatmap_data
#write.csv(heatmap_data,"tmt_heatmap_data.csv")
heatmap_data_pp <- heatmap_data
#write.csv(heatmap_data,"pp_heatmap_data.csv")
########################################################
##ffsham，ckosham，ffTAC，ckoTAC ##
tmt_heat_order <- heatmap_data_tmt[,c(grep("Pak2f_S",colnames(heatmap_data_tmt)),
                                      grep("Pak2c_S",colnames(heatmap_data_tmt)),
                                      grep("Pak2f_T",colnames(heatmap_data_tmt)),
                                      grep("Pak2c_T",colnames(heatmap_data_tmt)))]

pp_heat_order <- heatmap_data_pp[,c(grep("Pak2f_S",colnames(heatmap_data_pp)),
                                    grep("Pak2c_S",colnames(heatmap_data_pp)),
                                    grep("Pak2f_T",colnames(heatmap_data_pp)),
                                    grep("Pak2c_T",colnames(heatmap_data_pp)))]
colnames(tmt_heat_order)
colnames(pp_heat_order)

mean_ex <- as.data.frame(tmt_heat_order)
mean_ex$Pak2f_S <- rowMeans(mean_ex[,1:4], na.rm = TRUE)
mean_ex$Pak2c_S <- rowMeans(mean_ex[,5:8], na.rm = TRUE)
mean_ex$Pak2f_T <- rowMeans(mean_ex[,9:12], na.rm = TRUE)
mean_ex$Pak2c_T <- rowMeans(mean_ex[,13:16], na.rm = TRUE)
annotation_col <- data.frame(
  Group = colnames(mean_ex[,17:20]),
  row.names = colnames(mean_ex[,17:20])
)

group_colors <- list(
  Group = c(
    Pak2f_S = "#ff7f0e",
    Pak2c_S = "#1f77b4",
    Pak2f_T = "#2ca02c",
    Pak2c_T = "#d62727"
  )
)

library(pheatmap)
library(ggplot2)
mean_tmt <- pheatmap(mean_ex[,17:20],
                     scale = "row",
                     cluster_cols = FALSE,   
                     cluster_rows = TRUE,   
                     annotation_col = annotation_col,
                     annotation_colors = group_colors,
                     color = colorRampPalette(c("#009688", "white", "#F44336"))(50),
                     #main = "Top 50 Variable Proteins in Proteome",
                     main = "All proteins in Proteome",
                     show_rownames = FALSE,
                     fontsize_row = 8,
                     fontsize_col = 8,
                     border_color = NA)

mean_tmt

##pp
mean_ex <- as.data.frame(pp_heat_order)
mean_ex$Pak2f_S <- rowMeans(mean_ex[,1:4], na.rm = TRUE)
mean_ex$Pak2c_S <- rowMeans(mean_ex[,5:8], na.rm = TRUE)
mean_ex$Pak2f_T <- rowMeans(mean_ex[,9:12], na.rm = TRUE)
mean_ex$Pak2c_T <- rowMeans(mean_ex[,13:16], na.rm = TRUE)
annotation_col <- data.frame(
  Group = colnames(mean_ex[,17:20]),
  row.names = colnames(mean_ex[,17:20])
)

group_colors <- list(
  Group = c(
    Pak2f_S = "#ff7f0e",
    Pak2c_S = "#1f77b4",
    Pak2f_T = "#2ca02c",
    Pak2c_T = "#d62727"
  )
)
mean_pp <- pheatmap(mean_ex[,17:20],
                    scale = "row",
                    cluster_cols = FALSE,  
                    cluster_rows = TRUE,   
                    annotation_col = annotation_col,
                    annotation_colors = group_colors,
                    color = colorRampPalette(c("#009688", "white", "#F44336"))(50),
                    main = "All proteins in Phosphoproteome",
                    show_rownames = FALSE,
                    fontsize_row = 8,
                    fontsize_col = 8,
                    border_color = NA)

mean_pp
ggsave("mean_ex_heatmap_TMT.pdf", plot = mean_tmt)
ggsave("mean_ex_heatmap_PP.pdf", plot = mean_pp)

# p_tmt+p_pp
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(mean_tmt$gtable)
upViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(mean_pp$gtable)
upViewport()
########################### mean_ex_heatmap over ###################

##Sample correlation
correlation_matrix <- cor(tmt_heat_order, use = "pairwise.complete.obs")
library(corrplot)
library(RColorBrewer)
corrplot.mixed(correlation_matrix,
               lower = "number",
               upper = "ellipse",
               tl.pos = c("lt"),
               tl.col = "black",tl.srt = 45,
               mar = c(0,1,1,0),
               number.cex = 0.6
)
p_cor_tmt <- corrplot(correlation_matrix, method = "ellipse",
                      #col = terrain.colors(100),
                      col = colorRampPalette(c("#003371","white", "#9d2933"))(100),
                      mar = c(0, 1, 1, 0),
                      #type = "lower",
                      cl.ratio = 0.15,
                      tl.col = "black",tl.srt = 45,
                      # addCoef.col="white",
                      number.cex = 0.6)

correlation_matrix <- cor(pp_heat_order, use = "pairwise.complete.obs")

#mix_corrplot
corrplot.mixed(correlation_matrix)
p_cor_pp <- corrplot(correlation_matrix, order = "hclust", method = "ellipse",
                     #col = terrain.colors(100),
                     col = colorRampPalette(c("#003371", "#9d2933"))(100),
                     mar = c(0, 1, 1, 0),
                     cl.ratio = 0.15,
                     tl.col = "black",tl.srt = 45,
                     addCoef.col="white",
                     number.cex = 0.5)


## PCA
library(tidyverse)
library(ggfortify) 
library(FactoMineR) 
library(factoextra) 

ex_norm <- heatmap_data_tmt
pca_data <- t(ex_norm)  

sample_groups <- data.frame(
  Sample = rownames(pca_data),
  Group = substr(rownames(pca_data), 1, 7), 
  Replicate = gsub(".*(\\d+)$", "\\1", rownames(pca_data)) 
)

group_colors <- c(
  Pak2c_S = "#1f77b4",
  Pak2f_S = "#ff7f0e",
  Pak2f_T = "#2ca02c",
  Pak2c_T = "#d62727"
)

pca_result <- prcomp(pca_data, scale. = FALSE, center = TRUE)  
variance_percent <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)

pca1 <- autoplot(pca_result, 
                 data = sample_groups, 
                 colour = "Group", 
                 shape = "Replicate",
                 size = 3) +
  geom_text(aes(label = Replicate), hjust = -0.3, vjust = 0.3, size = 3) +
  scale_color_manual(values = group_colors) +  
  labs(title = "Proteome: PCA of Protein Expression",
       subtitle = "",
       x = paste0("PC1 (", variance_percent[1], "%)"),
       y = paste0("PC2 (", variance_percent[2], "%)")) +
  theme_minimal() +
  theme(legend.position = "right")
#ggsave("08.tmt_pca.pdf", plot = pca1)
 
pca2 <- fviz_pca_ind(pca_result,
                     geom.ind = "point",
                     col.ind = sample_groups$Group,
                     palette = group_colors,  
                     addEllipses = TRUE,
                     ellipse.type = "confidence",
                     ellipse.level = 0.95,
                     pointsize = 3,
                     mean.point = FALSE,
                     repel = TRUE,
                     legend.title = "Groups") +
  ggtitle("") +
  xlab(paste0("PC1 (", variance_percent[1], "%)")) +
  ylab(paste0("PC2 (", variance_percent[2], "%)")) +
  theme_bw()
pca2
#ggsave("tmt_pca_95conf_interval.pdf", plot = pca2)

# distance within group
in_g_dist <- function(pca_scores, groups) {
  dist_matrix <- dist(pca_scores[, 1:2])  
  dist_df <- as.data.frame(as.matrix(dist_matrix))
  
  results <- data.frame()
  for(group in unique(groups)) {
    group_samples <- which(groups == group)
    if(length(group_samples) > 1) {
      group_dists <- dist_df[group_samples, group_samples]
      upper_tri <- group_dists[upper.tri(group_dists)]
      results <- rbind(results, data.frame(
        Group = group,
        Mean_Distance = mean(upper_tri),
        SD_Distance = sd(upper_tri)
      ))
    }
  }
  return(results)
}

distance_results <- in_g_dist(pca_result$x, sample_groups$Group)
print(distance_results)
#write.csv("tmt_distance_results.csv")

pca3 <- ggplot(distance_results, aes(x = Group, y = Mean_Distance, fill = Group)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = Mean_Distance - SD_Distance, 
                    ymax = Mean_Distance + SD_Distance),
                width = 0.2) +
  geom_text(aes(label = round(Mean_Distance, 2)), vjust = -1.5, size = 4) +
  scale_fill_manual(values = group_colors) +  
  labs(title = "Within-Group Distances in PCA",
       x = "Group",
       y = "Mean Euclidean Distance") +
  theme_minimal()
pca3
#ggsave("tmt_Within-Group_Euclidean_Distances.pdf", plot = pca3)

replicate_correlation <- function(expr_matrix, groups) {
  results <- data.frame()
  for(group in unique(groups)) {
    group_samples <- expr_matrix[, groups == group]
    cor_matrix <- cor(group_samples, use = "pairwise.complete.obs")
    upper_tri <- cor_matrix[upper.tri(cor_matrix)]
    results <- rbind(results, data.frame(
      Group = group,
      Mean_Correlation = mean(upper_tri),
      SD_Correlation = sd(upper_tri)
    ))
  }
  return(results)
}

corr_results <- replicate_correlation(ex_norm, sample_groups$Group)
print(corr_results)
#write.csv("08.tmt_corr_within_group_results.csv")

pca4 <- ggplot(corr_results, aes(x = Group, y = Mean_Correlation, fill = Group)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = Mean_Correlation - SD_Correlation, 
                    ymax = Mean_Correlation + SD_Correlation),
                width = 0.2) +
  geom_text(aes(label = round(Mean_Correlation, 3)), vjust = -1.5, size = 4) +
  scale_fill_manual(values = group_colors) +  # 分组颜色
  labs(title = "Pearson correlation within Group",
       x = "Group",
       y = "Mean Pearson Correlation") +
  ylim(0, 1.05) +
  theme_minimal()
pca4
ggsave("tmt_corr_within_group_Pearsom.pdf", plot = pca4)

library(cowplot)
plot_grid(pca2, pca3, pca4, labels = c("A", "B", "C"), nrow=1)

## PCA PP 
ex_norm <- heatmap_data_pp
pca_data <- t(ex_norm)  
sample_groups <- data.frame(
  Sample = rownames(pca_data),
  Group = substr(rownames(pca_data), 1, 7),  
  Replicate = gsub(".*(\\d+)$", "\\1", rownames(pca_data))  
)

group_colors <- c(
  Pak2c_S = "#1f77b4",
  Pak2f_S = "#ff7f0e",
  Pak2f_T = "#2ca02c",
  Pak2c_T = "#d62727"
)

pca_result <- prcomp(pca_data, scale. = FALSE, center = TRUE)  
variance_percent <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
pca5 <- autoplot(pca_result, 
                 data = sample_groups, 
                 colour = "Group", 
                 shape = "Replicate",
                 size = 3) +
  geom_text(aes(label = Replicate), hjust = -0.3, vjust = 0.3, size = 3) +
  scale_color_manual(values = group_colors) +  
  labs(title = "Phosphoproteome: PCA of Protein Expression",
       subtitle = "",
       x = paste0("PC1 (", variance_percent[1], "%)"),
       y = paste0("PC2 (", variance_percent[2], "%)")) +
  theme_minimal() +
  theme(legend.position = "right")
pca5
#ggsave("pp_pca.pdf", plot = pca5)
pca6 <- fviz_pca_ind(pca_result,
                     geom.ind = "point",
                     col.ind = sample_groups$Group,
                     palette = group_colors,  
                     addEllipses = TRUE,
                     ellipse.type = "confidence",
                     ellipse.level = 0.95,
                     pointsize = 3,
                     mean.point = FALSE,
                     repel = TRUE,
                     legend.title = "Groups") +
  ggtitle("") +
  xlab(paste0("PC1 (", variance_percent[1], "%)")) +
  ylab(paste0("PC2 (", variance_percent[2], "%)")) +
  theme_bw()
pca6
#ggsave("pp_pca_95conf_interval.pdf", plot = pca6)

in_g_dist <- function(pca_scores, groups) {
  dist_matrix <- dist(pca_scores[, 1:2])  
  dist_df <- as.data.frame(as.matrix(dist_matrix))
  
  results <- data.frame()
  for(group in unique(groups)) {
    group_samples <- which(groups == group)
    if(length(group_samples) > 1) {
      group_dists <- dist_df[group_samples, group_samples]
      upper_tri <- group_dists[upper.tri(group_dists)]
      results <- rbind(results, data.frame(
        Group = group,
        Mean_Distance = mean(upper_tri),
        SD_Distance = sd(upper_tri)
      ))
    }
  }
  return(results)
}

distance_results <- in_g_dist(pca_result$x, sample_groups$Group)
print(distance_results)
#write.csv("pp_distance_results.csv")

pca7 <- ggplot(distance_results, aes(x = Group, y = Mean_Distance, fill = Group)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = Mean_Distance - SD_Distance, 
                    ymax = Mean_Distance + SD_Distance),
                width = 0.2) +
  geom_text(aes(label = round(Mean_Distance, 2)), vjust = -1.5, size = 4) +
  scale_fill_manual(values = group_colors) +  
  labs(title = "Within-Group Distances in PCA",
       x = "Group",
       y = "Mean Euclidean Distance") +
  theme_minimal()
pca7
#ggsave("pp_Within-Group_Euclidean_Distances.pdf", plot = pca7)

replicate_correlation <- function(expr_matrix, groups) {
  results <- data.frame()
  for(group in unique(groups)) {
    group_samples <- expr_matrix[, groups == group]
    cor_matrix <- cor(group_samples, use = "pairwise.complete.obs")
    upper_tri <- cor_matrix[upper.tri(cor_matrix)]
    results <- rbind(results, data.frame(
      Group = group,
      Mean_Correlation = mean(upper_tri),
      SD_Correlation = sd(upper_tri)
    ))
  }
  return(results)
}

corr_results <- replicate_correlation(ex_norm, sample_groups$Group)
print(corr_results)
#write.csv("pp_corr_within_group_results.csv")

pca8 <- ggplot(corr_results, aes(x = Group, y = Mean_Correlation, fill = Group)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = Mean_Correlation - SD_Correlation, 
                    ymax = Mean_Correlation + SD_Correlation),
                width = 0.2) +
  geom_text(aes(label = round(Mean_Correlation, 3)), vjust = -1.5, size = 4) +
  scale_fill_manual(values = group_colors) +  
  labs(title = "Pearson correlation within Group",
       x = "Group",
       y = "Mean Pearson Correlation") +
  ylim(0, 0.9) +
  theme_minimal()
pca8
#ggsave("pp_corr_within_group_Pearsom.pdf", plot = pca8)
#ggsave("pp_ylim_corr_within_group_Pearsom.pdf", plot = pca8)

plot_grid(pca2, pca3, pca4, pca6, pca7, pca8, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          nrow = 2,
          rel_heights = c(1, 0.8))  


### Venn
deg_tmt <- read.csv("./tmt_DEP_heatmap/all_diff_prot _tmt.CSV",header = T)
deg_pp <- read.csv("./pp_DEP_heatmap/all_diff_prot_pp.CSV",header = T)
##venn
library(VennDiagram)
library(ggplot2)
plot_venn <- function(group1, group2, colour1, colour2, name1, name2, pdf_name) {
  list_data <- list(group1$Protein, group2$Protein)
  names(list_data) <- c(name1, name2)
    venn.plot <- venn.diagram(
    x = list_data,
    filename = NULL,
    fill = "white",          
    alpha = 1,               
    col = c(colour1, colour2),
    lwd = 4,                 
    cex = 1,                 
    cat.cex = 1,             
    cat.pos = c(-30, 30),    
    cat.dist = c(0.06, 0.06),
    cat.fontface = "bold",
    margin = 0.2,
    main = "",
    main.cex = 1.2,
    sub = " ",
    rotation.degree = 0,
    euler.d = FALSE,
    scaled = FALSE
  )
  
  pdf_name <- pdf_name 
  pdf(pdf_name, width = 6, height = 6)
  grid.draw(venn.plot)
  dev.off()
  
  grid.newpage()
  grid.draw(venn.plot)
  assign("pvn", venn.plot, envir = .GlobalEnv)
  return(venn.plot)
}

##TAC-Sham effect
plot_venn(
  c_T.vs.c_Stmt, 
  f_T.vs.f_Stmt,
  colour1 = "#00adb5",
  colour2 = "#ffc93c",
  name1 = "Pak2cko_TAC vs. Pak2cko_Sham",  
  name2 = "Pak2ff_TAC vs. Pak2ff_Sham"  ,
  pdf_name = "vn_tmt_TAC_vs.Sham_cko-ff.pdf"
)
p_vn1 <- pvn

#CKO vs FF effect
plot_venn(
  c_T.vs.f_Ttmt, 
  c_S.vs.f_Stmt,
  colour1 = "#9d2933",
  colour2 = "#065279",
  name1 = "Pak2cko_TAC vs. Pak2ff_TAC",
  name2 = "Pak2cko_Sham vs. Pak2ff_Sham",
  pdf_name = "10.vn_tmt_CKO_vs.FF_cko-ff.pdf"
)
p_vn2 <- pvn

###### Phosphoprorein Venn ######
##TAC-Sham effect
plot_venn(
  c_T.vs.c_Spp, 
  f_T.vs.f_Spp,
  colour1 = "#057748",
  colour2 = "#ffb61e",
  name1 = "Pak2cko_TAC vs. Pak2cko_Sham",  
  name2 = "Pak2ff_TAC vs. Pak2ff_Sham"  ,
  pdf_name = "10.vn_pp_TAC_vs.Sham_cko-ff.pdf"
)
p_vn3 <- pvn

#CKO vs FF effect
plot_venn(
  c_T.vs.f_Tpp, 
  c_S.vs.f_Spp,
  colour1 = "#f00056",
  colour2 = "#177cb0",
  name1 = "Pak2cko_TAC vs. Pak2ff_TAC",
  name2 = "Pak2cko_Sham vs. Pak2ff_Sham",
  pdf_name = "10.vn_pp_CKO_vs.FF_cko-ff.pdf"
)
p_vn4 <- pvn


#DEP GO and KEGG
library(clusterProfiler)
library(org.Mm.eg.db)  
library(enrichplot)    
library(ggplot2)
library(forcats) 
library(stringr)
c_T.vs.c_Stmt$gene <- sub(".*_", "", c_T.vs.c_Stmt$pro_gene)
diff_genes <- unique(c_T.vs.c_Stmt$gene)

get_dif_gene <-  function(df) {
  genes <- sub(".*_", "", df$pro_gene)
  return(unique(genes))
}
# diff_genes <- get_dif_gene(c_T.vs.c_Stmt)
# diff_genes <- get_dif_gene(f_T.vs.f_Stmt)
# diff_genes <- get_dif_gene(c_T.vs.f_Ttmt)
# diff_genes <- get_dif_gene(c_S.vs.f_Stmt)

# diff_genes <- get_dif_gene(c_T.vs.c_Spp)
# diff_genes <- get_dif_gene(f_T.vs.f_Spp)
# diff_genes <- get_dif_gene(c_T.vs.f_Tpp)
# diff_genes <- get_dif_gene(c_S.vs.f_Spp)
Get_go_kegg_res <- function(diff_genes,group){ #diff_genes=c_T.vs.c_SXX,group=tmt/pp
  diff_genes <- get_dif_gene(diff_genes)
  
  gene_map <- bitr(diff_genes, 
                   fromType = "SYMBOL",
                   toType = "ENTREZID",
                   OrgDb = org.Mm.eg.db)
  
  entrez_ids <- gene_map$ENTREZID
  
  go_res <- enrichGO(
    gene          = diff_genes,
    OrgDb         = org.Mm.eg.db,
    keyType       = "SYMBOL",
    ont           = "ALL",
    pAdjustMethod = "BH",
    pvalueCutoff  = 0.05,
    qvalueCutoff  = 0.2,
    readable      = TRUE
  )
  
    kegg_res <- enrichKEGG(
    gene         = entrez_ids,  
    organism     = "mmu",
    keyType      = "ncbi-geneid",  
    pvalueCutoff = 0.05   #tmt_fs_ft_= 0.2  
  )
  go_res@result$group <- group
  kegg_res@result$group <- group
  
  assign(paste0("go_", group), go_res, envir = .GlobalEnv)
  assign(paste0("keg_", group), kegg_res, envir = .GlobalEnv)
  
  return(list(go_group = go_res, keg_group = kegg_res))
  kegg_res_df <- as.data.frame(kegg_res@result)
  go_res_df <- as.data.frame(go_res@result)
  write.csv(go_res_df,paste0("12.",group,".go_result.csv"))
  write.csv(kegg_res_df,paste0("12.",group,".kegg_result.csv"))
  # write.csv(kegg_res_df,"12.c_T.vs.c_Stmt.kegg.csv")
  # write.csv(as.data.frame(go_res),"12.c_T.vs.c_Stmt.kegg.csv")
  # write.csv(kegg_res_df,"12.c_T.vs.c_Spp.kegg.csv")
  # write.csv(as.data.frame(go_res),"12.c_T.vs.c_Spp.kegg.csv")
}

Get_go_kegg_res(c_T.vs.c_Stmt,"tmt_ct-cs")
Get_go_kegg_res(c_T.vs.c_Spp,"pp_ct-cs")
Get_go_kegg_res(f_T.vs.f_Stmt,"tmt_ft-fs") #keg p=0.2
Get_go_kegg_res(f_T.vs.f_Spp,"pp_ft-fs")
Get_go_kegg_res(c_T.vs.f_Ttmt,"tmt_ct-ft")
Get_go_kegg_res(c_T.vs.f_Tpp,"pp_ct-ft")
Get_go_kegg_res(c_S.vs.f_Stmt,"tmt_cs-fs")
Get_go_kegg_res(c_S.vs.f_Spp,"pp_cs-fs")
####################################################
plot_go_kegg <- function(res, title, n_show = 10) {
  df <- head(as.data.frame(res)[order(res$p.adjust), ], n_show)
  
    df$GeneRatioValue <- sapply(strsplit(df$GeneRatio, "/"), function(x) {
    as.numeric(x[1]) / as.numeric(x[2])
  })
    df$Description <- stringr::str_wrap(df$Description, width = 30) # 30字符后换行
    ggplot(df, aes(
    x = GeneRatioValue,  
    y = fct_reorder(Description, GeneRatioValue),  
    size = Count,  
    color = -log10(p.adjust)  
  )) +
    geom_point(alpha = 0.8) +
    scale_color_gradient(
      low = "#0074e4", 
      high = "#971549", 
      name = "-log10(p.adj)"
    ) +
    scale_size_continuous(range = c(1, 3), name = "Count") + 
    labs(
      title = title,
      x = "Gene Ratio",
      y = ""
    ) +
    theme_classic(base_size = 10) +
    theme(
      text = element_text(color = "black"),  
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.major.y = element_line(color = "grey90"),
      legend.key.height = unit(0.2, "cm"),  
      legend.key.width = unit(0.4, "cm"),   
      legend.spacing.y = unit(0.1, "cm"),   
      legend.box.spacing = unit(0.1, "cm") 
    )
}

# GO:BP-CC-MP
go_res <- `go_tmt_cs-fs`
kegg_res <- `keg_tmt_cs-fs`
go_res <- `go_pp_cs-fs`
kegg_res <- `keg_pp_cs-fs`

go_res <- `go_tmt_ft-fs`
kegg_res <- `keg_tmt_ft-fs`
go_res <- `go_pp_ft-fs`
kegg_res <- `keg_pp_ft-fs`

go_res <-`go_tmt_ct-ft`
kegg_res <- `keg_tmt_ct-ft`
go_res <-`go_pp_ct-ft`
kegg_res <- `keg_pp_ct-ft`

go_res <- `go_tmt_cs-fs`
kegg_res <- `keg_tmt_cs-fs`
go_res <- `go_pp_cs-fs`
kegg_res <- `keg_pp_cs-fs`
p_bp <- plot_go_kegg(go_res[go_res$ONTOLOGY == "BP", ], "Biological Process", 5)
p_cc <- plot_go_kegg(go_res[go_res$ONTOLOGY == "CC", ], "Cellular Component", 5)
p_mf <- plot_go_kegg(go_res[go_res$ONTOLOGY == "MF", ], "Molecular Function", 5)
p_kegg <- plot_go_kegg(kegg_res, "KEGG Pathways", 15)

library(patchwork)
group_plot <- (p_bp / p_cc / p_mf) | p_kegg 
# gk1 <- group_plot
# gk2 <- group_plot
# gk3 <- group_plot
# gk4 <- group_plot
# pgk1 <- group_plot
# pgk2 <- group_plot
# pgk3 <- group_plot
# pgk4 <- group_plot


c1 <- gk1 / pgk1 + 
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A")  +
  plot_annotation(
    title = "",
    subtitle = "",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      #plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(0.1, 0.2, 0.2, 0.2, "cm") 
    ))

c2 <- gk2 / pgk2 + 
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A")  +
  plot_annotation(
    title = "",
    subtitle = "",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      #plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(0.1, 0.2, 0.2, 0.2, "cm") 
    ))
# ggsave("12.tP_dif_ct-cs.gk_A4.pdf",plot = c1,width = 8.27,height = 11.69)
# ggsave("12.tP_dif_ft-fs.gk.pdf",plot = c2,width = 20,height = 16)
c3 <- gk3 / pgk3 + 
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A")  +
  plot_annotation(
    title = "",
    subtitle = "",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      #plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(0.1, 0.2, 0.2, 0.2, "cm") 
    ))
#ggsave("12.tP_dif_ct-ft.gk.pdf",plot = c3,width = 20,height = 16)
c4 <- gk4 / pgk4 + 
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A")  +
  plot_annotation(
    title = "",
    subtitle = "",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      #plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(0.1, 0.2, 0.2, 0.2, "cm") 
    ))
#ggsave("12.tP_dif_cs-fs.gk.pdf",plot = c4,width = 20,height = 16)

perfect_a4_fit <- function(plot, filename) {
  ggsave("temp.pdf", plot, width = a4_width, height = a4_height)
  file_size <- file.info("temp.pdf")$size
  #optimal_scale <- ifelse(file_size > 2e6, 0.7, ifelse(file_size > 1e6, 0.8, 0.9))
  optimal_scale <- ifelse(file_size > 1e6, 0.7, ifelse(file_size > 1e5, 0.6, 0.82))
  optimized_plot <- plot & theme(text = element_text(size = 10 * optimal_scale))
  ggsave(filename, optimized_plot, width = a4_width, height = a4_height, dpi = 600)
  file.remove("temp.pdf")
  
  return(optimal_scale)
}
#tp_ct-fs
font_forA4_scale <- perfect_a4_fit(c1, "12.A4_tp_ct_cs.gokegg.pdf")
font_forA4_scale <- perfect_a4_fit(c1, "12.A4_tp_ct_cs.gokegg.tiff")
font_forA4_scale <- perfect_a4_fit(c1, "12.A4_tp_ct_cs.gokegg.jpg")
#tp_ft-fs
font_forA4_scale <- perfect_a4_fit(c2, "12.A4_tp_ft-fs.gokegg.pdf")
font_forA4_scale <- perfect_a4_fit(c2, "12.A4_tp_tp_ft-fs.gokegg.tiff")
font_forA4_scale <- perfect_a4_fit(c2, "12.A4_tp_tp_ft-fs.gokegg.jpg")
#tp_ct-ft
font_forA4_scale <- perfect_a4_fit(c3, "12.A4_tp_ct-ft.gokegg.pdf")
font_forA4_scale <- perfect_a4_fit(c3, "12.A4_tp_ct-ft.gokegg.tiff")
font_forA4_scale <- perfect_a4_fit(c3, "12.A4_tp_ct-ft-fs.gokegg.jpg")
#tp_cs-fs
font_forA4_scale <- perfect_a4_fit(c4, "12_new.A4_tp_cs-fs.gokegg.pdf")
font_forA4_scale <- perfect_a4_fit(c4, "12_new.A4_tp_cs-fs.gokegg.tiff")
font_forA4_scale <- perfect_a4_fit(c4, "12_new.A4_tp_tp_cs-fs.gokegg.jpg")
cat(sprintf("Used font scale: %.2f", font_forA4_scale))
############# ############### GO-KEGG over ################################################