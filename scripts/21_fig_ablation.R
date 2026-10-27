#!/usr/bin/env Rscript
# =============================================================================
# 21 — Fig. 4：接与不接驱动异质性，模拟给出的答案差多少（数据来自 13_ablation.R）
# =============================================================================
# a 各配置"三维同时改善"的县比例，三种模型并列（同一行连线）
# b 全国均值效应：简化模型 vs 主模型（落在对角线上 = 平均答案不变）
# c 空间策略对全国 CPS 均值的影响范围
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")

arm_lab <- c(A1_national = "National coefficients", A2_zones = "Zone coefficients",
             Full = "Response-type coefficients")
cm <- read.csv("output/csv/ablation_config_metrics.csv") %>%
  mutate(Model = factor(arm_lab[Arm], levels = names(PAL_MODEL)),
         Total = CPI + CPS + CPA)
sm <- read.csv("output/csv/ablation_summary.csv") %>%
  mutate(Model = factor(arm_lab[Arm], levels = names(PAL_MODEL)))

ord <- cm %>% filter(Arm == "Full") %>% arrange(Synergy) %>% pull(Policy_Config)
cm$Config <- factor(cm$Policy_Config, levels = ord, labels = CONFIG_LAB[ord])

# ---- a ------------------------------------------------------------------------
rng <- cm %>% group_by(Config) %>% summarise(lo = min(Synergy), hi = max(Synergy), .groups = "drop")
lab_i04 <- cm %>% filter(Policy_Config == "I04")

band <- data.frame(y = which(levels(cm$Config) == CONFIG_LAB[["I04"]]))
pa <- ggplot(cm, aes(Synergy, Config)) +
  geom_rect(data = band, aes(xmin = -Inf, xmax = Inf, ymin = y - 0.5, ymax = y + 0.5),
            inherit.aes = FALSE, fill = "#E6F4F3") +
  geom_segment(data = rng, aes(x = lo, xend = hi, y = Config, yend = Config),
               colour = "grey80", linewidth = 1.6, lineend = "round") +
  geom_point(aes(fill = Model), shape = 21, size = 2.6, stroke = 0.3, colour = "white") +
  geom_text(data = lab_i04, aes(label = sprintf("%.1f%%", Synergy), colour = Model),
            vjust = 1.9, size = FS_NOTE / .pt, family = FONT, fontface = "bold",
            show.legend = FALSE) +
  scale_fill_manual(values = PAL_MODEL, name = NULL,
                    labels = c("National model", "Zone models", "Response-type models")) +
  scale_colour_manual(values = PAL_MODEL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0.01, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Share of triple-gain counties (%)", y = NULL,
       title = "Who benefits depends on the model") +
  theme_innov() +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 2.8))) +
  theme(legend.position = "bottom", legend.justification = "center", legend.location = "plot",
        legend.box.spacing = unit(0.5, "mm"), legend.key.spacing.x = unit(1.5, "mm"),
        axis.line.y = element_blank(), axis.ticks.y = element_blank())

# ---- b ------------------------------------------------------------------------
full_tot <- cm %>% filter(Arm == "Full") %>% select(Policy_Config, Full_Total = Total)
cmp <- cm %>% filter(Arm != "Full") %>% left_join(full_tot, by = "Policy_Config")
rho <- cmp %>% group_by(Model) %>%
  summarise(r = cor(Total, Full_Total, method = "spearman"), .groups = "drop")
lims <- range(c(cmp$Total, cmp$Full_Total))

pb <- ggplot(cmp, aes(Full_Total, Total, fill = Model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.35) +
  geom_point(shape = 21, size = 2.2, stroke = 0.3, colour = "white") +
  annotate("text", x = lims[2], y = lims[1], hjust = 1, vjust = 0, family = FONT,
           size = FS_NOTE / .pt, lineheight = 1.05,
           label = sprintf("Rank correlation\nzones %.2f\nnational %.2f",
                           rho$r[rho$Model == "Zone coefficients"],
                           rho$r[rho$Model == "National coefficients"])) +
  scale_fill_manual(values = PAL_MODEL, guide = "none") +
  coord_equal(xlim = lims, ylim = lims) +
  labs(x = "National mean gain,\nresponse-type models", y = "Simplified models",
       title = "Ranking by mean gain is stable") +
  theme_innov()

# ---- c ------------------------------------------------------------------------
pc <- ggplot(sm, aes(Spa_CPS * 1000, Model, fill = Model)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", Spa_CPS * 1000)), hjust = -0.15,
            size = FS_NOTE / .pt, family = FONT) +
  scale_fill_manual(values = PAL_MODEL, guide = "none") +
  scale_y_discrete(labels = c("National", "Zones", "Response\ntypes")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(sm$Spa_CPS) * 1000 * 1.25)) +
  labs(x = "Range of stability gain across\ntargeting strategies (\u00d70.001)", y = NULL,
       title = "Spatial targeting matters\nonly with heterogeneity") +
  theme_innov() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

W <- FULL_W; H <- 92
fig <- compose(list(
  list(plot = pa, x = 0,       y = 0,    w = 0.60, h = 1,    tag = "A"),
  list(plot = pb, x = 0.605,   y = 0.42, w = 0.395, h = 0.58, tag = "B", tx = 0.0),
  list(plot = pc, x = 0.605,   y = 0,    w = 0.395, h = 0.42, tag = "C", tx = 0.0)
))
save_fig(fig, "fig4_ablation", width_mm = W, height_mm = H)
