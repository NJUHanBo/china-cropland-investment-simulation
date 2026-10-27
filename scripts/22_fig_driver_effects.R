#!/usr/bin/env Rscript
# =============================================================================
# 22 — Fig. 3：驱动效应与各类县的效应分布
# =============================================================================
# a 四个杠杆 × 三个产能维度的平均效应（因果森林 ATE，95% CI）
# b 四条关键路径上各响应类型的县级效应分布，标题给出类间方差占比
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")

# ---- a ------------------------------------------------------------------------
ate <- read.csv("models/causal_forest_results/cf_ate_summary.csv") %>%
  mutate(Lever = factor(LEVER_EN[Treatment], levels = names(PAL_LEVER)),
         Outcome = factor(Outcome, levels = c("CPA", "CPS", "CPI")))

pa <- ggplot(ate, aes(ATE, Outcome, colour = Outcome)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.35, linetype = "dashed") +
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0, linewidth = 0.7, orientation = "y") +
  geom_point(aes(shape = Significant), size = 2.4, fill = "white", stroke = 0.8) +
  geom_text(aes(x = CI_Upper, label = sprintf("%.2f", ATE)), hjust = -0.25,
            size = FS_NOTE / .pt, family = FONT, colour = "grey20") +
  scale_colour_manual(values = PAL_OUTCOME, breaks = c("CPI", "CPS", "CPA"), name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
  scale_x_continuous(limits = c(-0.7, 1.4), breaks = seq(-0.5, 1, 0.5)) +
  facet_grid(Lever ~ ., switch = "y") +
  labs(x = "Average effect (95% CI)", y = NULL, title = "Estimated driver effects") +
  theme_innov() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = FS_TEXT),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.spacing.y = unit(1.2, "mm"),
        panel.background = element_rect(fill = "grey97", colour = NA),
        legend.position = "top", legend.justification = "right",
        legend.box.spacing = unit(0.5, "mm"))

# ---- b ------------------------------------------------------------------------
tau <- read.csv("models/causal_forest_results/cf_tau_matrix.csv", check.names = FALSE)
asg <- read.csv("models/cluster_results/cluster_assignments.csv")
key <- c("利用强度_on_CPI", "利用强度_on_CPS", "保护政策_on_CPS", "产权制度_on_CPS")
tl <- tau %>% select(Code, all_of(key)) %>%
  pivot_longer(-Code, names_to = "path", values_to = "tau") %>%
  mutate(Type = factor(paste0("T", asg$Cluster[match(Code, asg$Code)]), levels = names(PAL_TYPE)),
         lever = LEVER_EN[sub("_on_.*", "", path)], outcome = sub(".*_on_", "", path))

share <- tl %>% group_by(path) %>%
  summarise(between = sum(tapply(tau, Type, length) *
                            (tapply(tau, Type, mean) - mean(tau))^2) / (n() - 1),
            total = var(tau), .groups = "drop") %>%
  mutate(pct = 100 * between / total)
tl <- tl %>% left_join(share, by = "path") %>%
  mutate(Panel = factor(sprintf("%s \u2192 %s\n%.0f%% of variance between types", lever, outcome, pct)))
tl$Panel <- factor(tl$Panel, levels = unique(tl$Panel[match(key, tl$path)]))

pb <- ggplot(tl, aes(Type, tau, fill = Type)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, width = 0.68, colour = "grey25") +
  scale_fill_manual(values = PAL_TYPE, guide = "none") +
  facet_wrap(~ Panel, ncol = 2, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub("T", "", x)) +
  labs(x = "County response type", y = "County-level effect",
       title = "Effects differ in size across county response types") +
  theme_innov() +
  theme(strip.text = element_text(size = FS_TEXT, face = "bold", lineheight = 1.05,
                                  margin = margin(0, 0, 1, 0, "mm")),
        panel.spacing = unit(2, "mm"))

W <- FULL_W; H <- 92
fig <- compose(list(
  list(plot = pa, x = 0,    y = 0, w = 0.40, h = 1, tag = "A"),
  list(plot = pb, x = 0.41, y = 0, w = 0.59, h = 1, tag = "B", tx = 0.0)
))
save_fig(fig, "fig3_driver_effects", width_mm = W, height_mm = H)
