#!/usr/bin/env Rscript
# =============================================================================
# 25 — Fig. 7：外部压力下哪些配置顶得住（相对模型现状基线）
# =============================================================================
# a 外部拖累：无干预时，城镇化—气候压力梯度上的全国均值变化（线）；
#   E1/E2/E3 处的竖条 = 15 种配置的全国均值范围（含外部项）
# b 全国 CPS 均值仍高于现状的配置数
# c 四种代表配置在三种环境下的受益面
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")

env_x <- c(E1 = 0.85, E2 = 1.00, E3 = 1.30)
env_lab <- c(E1 = "Low", E2 = "Current", E3 = "High")

sweep <- read.csv("output/csv/external_multiplier_sweep.csv") %>%
  pivot_longer(CPI_mean:CPA_mean, names_to = "Outcome", values_to = "Change") %>%
  mutate(Outcome = factor(sub("_mean", "", Outcome), levels = c("CPI", "CPS", "CPA")))

ne <- read.csv("output/csv/national_effects_by_indicator.csv") %>%
  filter(Policy_Config != "Baseline") %>%
  group_by(External_Config, Policy_Config) %>%
  summarise(CPI = mean(CPI_Mean_Total), CPS = mean(CPS_Mean_Total),
            CPA = mean(CPA_Mean_Total), .groups = "drop")

env_rng <- ne %>% pivot_longer(CPI:CPA, names_to = "Outcome", values_to = "v") %>%
  group_by(External_Config, Outcome) %>%
  summarise(lo = min(v), hi = max(v), .groups = "drop") %>%
  mutate(Outcome = factor(Outcome, levels = c("CPI", "CPS", "CPA")),
         x = env_x[External_Config] + c(CPI = -0.022, CPS = 0, CPA = 0.022)[as.character(Outcome)])

# ---- a ------------------------------------------------------------------------
pa <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35, linetype = "dashed") +
  geom_segment(data = env_rng, aes(x = x, xend = x, y = lo, yend = hi, colour = Outcome),
               linewidth = 2.6, alpha = 0.28, lineend = "round") +
  geom_line(data = sweep, aes(urb_multiplier, Change, colour = Outcome), linewidth = LW_DATA) +
  annotate("text", x = env_x, y = 0.315, label = env_lab, family = FONT, fontface = "bold",
           size = FS_TEXT / .pt, colour = "grey30") +
  scale_colour_manual(values = PAL_OUTCOME, name = NULL) +
  scale_x_continuous(breaks = seq(0.7, 1.5, 0.2)) +
  scale_y_continuous(limits = c(-0.19, 0.33), breaks = seq(-0.1, 0.3, 0.1)) +
  labs(x = "Urbanisation pressure (multiplier of current level)",
       y = "Change from current conditions",
       title = "Effect of external pressure and the range of 15 investment portfolios") +
  theme_innov() +
  theme(legend.position = "inside", legend.position.inside = c(0.99, 0.80),
        legend.justification = c(1, 1), legend.key.height = unit(2.6, "mm"))

# ---- b ------------------------------------------------------------------------
keep <- ne %>% group_by(External_Config) %>%
  summarise(n = sum(CPS >= 0), .groups = "drop") %>%
  mutate(Env = factor(env_lab[External_Config], levels = env_lab))

pb <- ggplot(keep, aes(Env, n)) +
  geom_col(width = 0.6, fill = PAL_OUTCOME[["CPS"]]) +
  geom_text(aes(label = n), vjust = -0.4, family = FONT, size = FS_BASE / .pt, fontface = "bold") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16.5), breaks = c(0, 5, 10, 15)) +
  labs(x = "External pressure", y = "Portfolios with a\nnational stability gain",
       title = "Portfolios that offset pressure") +
  theme_innov()

# ---- c ------------------------------------------------------------------------
syn <- read.csv("output/csv/tradeoff_by_scenario.csv") %>%
  filter(Policy_Config %in% names(PAL_FOCUS)) %>%
  group_by(External_Config, Policy_Config) %>%
  summarise(Synergy = mean(Pct_Full_Synergy), .groups = "drop") %>%
  mutate(Env = factor(env_lab[External_Config], levels = env_lab),
         Config = factor(Policy_Config, levels = names(PAL_FOCUS)))
end_lab <- syn %>% filter(External_Config == "E3")

pc <- ggplot(syn, aes(Env, Synergy, colour = Config, group = Config)) +
  geom_line(linewidth = LW_DATA) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(data = end_lab, aes(label = sprintf("%.0f", Synergy)),
                           nudge_x = 0.22, direction = "y", hjust = 0, segment.size = 0.2,
                           min.segment.length = 0, box.padding = 0.12, family = FONT,
                           size = FS_NOTE / .pt, show.legend = FALSE) +
  scale_colour_manual(values = PAL_FOCUS, labels = CONFIG_LAB[names(PAL_FOCUS)], name = NULL) +
  scale_x_discrete(expand = expansion(add = c(0.25, 0.45))) +
  scale_y_continuous(limits = c(20, 90), breaks = seq(20, 80, 20)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(x = "External pressure", y = "Triple-gain\ncounties (%)",
       title = "Triple-gain share under pressure") +
  theme_innov() +
  theme(legend.position = "right", legend.key.height = unit(3, "mm"),
        legend.key.spacing.y = unit(0.4, "mm"))

W <- FULL_W; H <- 104
fig <- compose(list(
  list(plot = pa, x = 0,    y = 0.44, w = 1,    h = 0.56, tag = "A"),
  list(plot = pb, x = 0,    y = 0,    w = 0.33, h = 0.44, tag = "B"),
  list(plot = pc, x = 0.34, y = 0,    w = 0.66, h = 0.44, tag = "C", tx = 0.0)
))
save_fig(fig, "fig7_pressure", width_mm = W, height_mm = H)
