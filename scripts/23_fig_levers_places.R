#!/usr/bin/env Rscript
# =============================================================================
# 23 — Fig. 5：杠杆先于地点（现状环境，政策效应 = 干预预测 − 模型现状基线）
# =============================================================================
# a 三个维度上，部门配置之间与空间策略之间的全国均值极差
# b 15 种部门配置的全国均值效应（对空间策略取平均）
# c 21 种空间策略的全国均值效应（对部门配置取平均），与 b 同一横轴
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")

ne <- read.csv("output/csv/national_effects_by_indicator.csv") %>%
  filter(External_Config == "E2", Policy_Config != "Baseline") %>%
  transmute(Policy_Config, Allocation_Strategy,
            CPI = CPI_Mean_PolicyEff, CPS = CPS_Mean_PolicyEff, CPA = CPA_Mean_PolicyEff)

by_cfg <- ne %>% group_by(Policy_Config) %>% summarise(across(CPI:CPA, mean), .groups = "drop")
by_alc <- ne %>% group_by(Allocation_Strategy) %>% summarise(across(CPI:CPA, mean), .groups = "drop")

alloc_lab <- function(a) {
  fixed <- c("统一投资" = "Uniform", "CPDS雪中送炭" = "Favour low sustainability",
             "CPDS锦上添花" = "Favour high sustainability", "CPI雪中送炭" = "Favour low productivity",
             "CPI锦上添花" = "Favour high productivity")
  out <- fixed[a]
  combo <- is.na(out)
  out[combo] <- paste0("Per-area choice ", seq_len(sum(combo)))
  unname(out)
}

# ---- a ------------------------------------------------------------------------
rg <- bind_rows(
  by_cfg %>% summarise(across(CPI:CPA, ~ diff(range(.x)))) %>% mutate(Choice = "Investment portfolio"),
  by_alc %>% summarise(across(CPI:CPA, ~ diff(range(.x)))) %>% mutate(Choice = "Spatial targeting")
) %>% pivot_longer(CPI:CPA, names_to = "Outcome", values_to = "Range") %>%
  mutate(Outcome = factor(Outcome, levels = c("CPI", "CPS", "CPA")))
ratio <- rg %>% group_by(Outcome) %>%
  summarise(r = Range[Choice == "Investment portfolio"] / Range[Choice == "Spatial targeting"],
            y = max(Range), .groups = "drop")

pa <- ggplot(rg, aes(Outcome, Range, fill = Choice)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(data = ratio, aes(Outcome, y, label = sprintf("%.0f\u00d7", r)), inherit.aes = FALSE,
            vjust = -0.5, size = FS_BASE / .pt, family = FONT, fontface = "bold") +
  scale_fill_manual(values = c("Investment portfolio" = "#2BA6A0", "Spatial targeting" = "#E8A33D"), name = NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.33)) +
  labs(x = NULL, y = "Range of national mean gains", title = "Portfolio vs targeting") +
  theme_innov() +
  guides(fill = guide_legend(ncol = 1)) +
  theme(legend.position = "top", legend.justification = "left",
        legend.box.spacing = unit(0.5, "mm"))

# ---- b / c 共用横轴 -------------------------------------------------------------
dot_panel <- function(d, lab, title) {
  d <- d %>% mutate(Total = CPI + CPS + CPA, Lab = lab) %>% arrange(Total) %>%
    mutate(Lab = factor(Lab, levels = unique(Lab))) %>%
    pivot_longer(CPI:CPA, names_to = "Outcome", values_to = "Effect") %>%
    mutate(Outcome = factor(Outcome, levels = c("CPI", "CPS", "CPA")))
  ggplot(d, aes(Effect, Lab, colour = Outcome)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.35, linetype = "dashed") +
    geom_point(size = 1.9, alpha = 0.95) +
    scale_colour_manual(values = PAL_OUTCOME, name = NULL) +
    scale_x_continuous(limits = c(-0.13, 0.21), breaks = seq(-0.1, 0.2, 0.1)) +
    labs(x = "National mean gain", y = NULL, title = title) +
    theme_innov() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
          legend.position = "none")
}
pb <- dot_panel(by_cfg, CONFIG_LAB[by_cfg$Policy_Config], "Portfolios (15)") +
  theme(legend.position = "top", legend.justification = "right",
        legend.box.spacing = unit(0.5, "mm"))
grp_of <- function(a) ifelse(a == "统一投资", "Uniform",
                       ifelse(a %in% c("CPDS雪中送炭", "CPDS锦上添花", "CPI雪中送炭", "CPI锦上添花"),
                              "By county baseline\n(4 strategies)", "Chosen separately for\neach investment area\n(16 strategies)"))
alc_long <- by_alc %>% mutate(Group = factor(grp_of(Allocation_Strategy),
                                             levels = rev(c("Uniform", "By county baseline\n(4 strategies)",
                                                            "Chosen separately for\neach investment area\n(16 strategies)")))) %>%
  pivot_longer(CPI:CPA, names_to = "Outcome", values_to = "Effect") %>%
  mutate(Outcome = factor(Outcome, levels = c("CPI", "CPS", "CPA")))
set.seed(1)
pc <- ggplot(alc_long, aes(Effect, Group, colour = Outcome)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.35, linetype = "dashed") +
  geom_point(size = 1.9, alpha = 0.85, position = position_jitter(width = 0, height = 0.18)) +
  scale_colour_manual(values = PAL_OUTCOME, guide = "none") +
  scale_x_continuous(limits = c(-0.13, 0.21), breaks = seq(-0.1, 0.2, 0.1)) +
  labs(x = "National mean gain", y = NULL, title = "Targeting (21)") +
  theme_innov() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(lineheight = 0.95),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))

W <- FULL_W; H <- 96
fig <- compose(list(
  list(plot = pa, x = 0,     y = 0,    w = 0.25, h = 1,    tag = "A"),
  list(plot = pb, x = 0.26,  y = 0,    w = 0.40, h = 1,    tag = "B", tx = 0.0),
  list(plot = pc, x = 0.67,  y = 0.25, w = 0.33, h = 0.75, tag = "C", tx = 0.0)
))
save_fig(fig, "fig5_levers_places", width_mm = W, height_mm = H)
