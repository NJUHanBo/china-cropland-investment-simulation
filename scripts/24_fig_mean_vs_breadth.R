#!/usr/bin/env Rscript
# =============================================================================
# 24 — Fig. 6：最优取决于目标（现状环境）
# =============================================================================
# a 15 种配置按全国均值排名 vs 按受益面（三维同时改善的县比例）排名
# b 四种代表配置在各响应类型中的受益面
# c 四种代表配置的县级结果构成
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")

cm <- read.csv("output/csv/ablation_config_metrics.csv") %>% filter(Arm == "Full")
focus <- c("I04", "I06", "I02", "I12")
col_focus <- PAL_FOCUS

# ---- a 斜率图 ------------------------------------------------------------------
sl <- cm %>% transmute(Policy_Config, Lab = CONFIG_LAB[Policy_Config],
                       `By national mean gain` = Rank_Mean, `By triple-gain share` = Rank_Synergy) %>%
  pivot_longer(c(`By national mean gain`, `By triple-gain share`), names_to = "Ranking", values_to = "Rank") %>%
  mutate(Ranking = factor(Ranking, levels = c("By national mean gain", "By triple-gain share")),
         Col = ifelse(Policy_Config %in% focus, Policy_Config, "other"))
pal_sl <- c(col_focus, other = "grey62")

pa <- ggplot(sl, aes(Ranking, Rank, group = Policy_Config, colour = Col)) +
  geom_line(aes(linewidth = Col != "other")) +
  geom_point(size = 1.8) +
  geom_text(data = filter(sl, Ranking == "By national mean gain"), aes(label = Lab),
            hjust = 1, nudge_x = -0.06, size = FS_TEXT / .pt, family = FONT,
            fontface = ifelse(filter(sl, Ranking == "By national mean gain")$Col != "other", "bold", "plain")) +
  geom_text(data = filter(sl, Ranking == "By triple-gain share", Col != "other"),
            aes(label = paste0("#", Rank)),
            hjust = 0, nudge_x = 0.06, size = FS_TEXT / .pt, family = FONT, fontface = "bold") +
  scale_colour_manual(values = pal_sl, guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 0.9, `FALSE` = 0.4), guide = "none") +
  scale_y_reverse(breaks = 1:15) +
  scale_x_discrete(position = "top", expand = expansion(add = c(1.45, 0.35))) +
  labs(x = NULL, y = NULL, title = "Two rankings of the same 15 portfolios") +
  theme_innov() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(),
        axis.text.x.top = element_text(face = "bold", size = FS_BASE))

# ---- b 各类型受益面 -------------------------------------------------------------
gs <- read.csv("output/csv/ablation_group_synergy.csv") %>%
  filter(Arm == "Full", Policy_Config %in% focus) %>%
  transmute(Policy_Config, Row = paste0("T", Group), Synergy)
gs <- bind_rows(gs, cm %>% filter(Policy_Config %in% focus) %>%
                  transmute(Policy_Config, Row = "All", Synergy)) %>%
  mutate(Row = factor(Row, levels = rev(c("All", names(PAL_TYPE)))),
         Config = factor(CONFIG_LAB[Policy_Config], levels = CONFIG_LAB[focus]))

pb <- ggplot(gs, aes(Config, Row, fill = Synergy)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.0f", Synergy), colour = Synergy > 55),
            size = FS_NOTE / .pt, family = FONT, show.legend = FALSE) +
  geom_hline(yintercept = 1.5, colour = "white", linewidth = 1.6) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15")) +
  scale_fill_gradient(low = "#F2F2F2", high = "#1F7A75", limits = c(0, 100),
                      name = "Triple-gain\ncounties (%)",
                      guide = guide_colourbar(barheight = unit(17, "mm"), barwidth = unit(2.4, "mm"))) +
  scale_x_discrete(position = "top", expand = c(0, 0),
                   labels = CONFIG_LAB[focus]) +
  scale_y_discrete(expand = c(0, 0), labels = function(x) ifelse(x == "All", "All counties", type_name(x))) +
  labs(x = NULL, y = NULL, title = "Triple-gain share by county type") +
  theme_innov() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x.top = element_text(colour = col_focus[focus], face = "bold", size = FS_TEXT - 0.5,
                                       angle = 30, hjust = 0, vjust = 0),
        axis.text.y = element_text(colour = c(PAL_TYPE, All = "black")[levels(gs$Row)],
                                   face = "bold"),
        legend.title = element_text(size = FS_TEXT, lineheight = 1))

# ---- c 结果构成 -----------------------------------------------------------------
cls <- c("Pct_Full_Synergy" = "All three", "Pct_Partial_Synergy" = "Two",
         "Pct_Partial_Tradeoff" = "One", "Pct_Full_Negative" = "None")
comp <- read.csv("output/csv/tradeoff_by_scenario.csv") %>%
  filter(External_Config == "E2", Policy_Config %in% focus) %>%
  group_by(Policy_Config) %>% summarise(across(all_of(names(cls)), mean), .groups = "drop") %>%
  pivot_longer(-Policy_Config, names_to = "Class", values_to = "Pct") %>%
  mutate(Class = factor(cls[Class], levels = rev(cls)),
         Config = factor(CONFIG_LAB[Policy_Config], levels = rev(CONFIG_LAB[focus])))

pc <- ggplot(comp, aes(Pct, Config, fill = Class)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.25) +
  scale_fill_manual(values = c("All three" = "#1F7A75", "Two" = "#8CCFC9",
                               "One" = "#F2B48C", "None" = "#C8423E"),
                    breaks = unname(cls), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.03)), breaks = seq(0, 100, 25)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Share of counties (%)", y = NULL, title = "Dimensions improved per county") +
  theme_innov() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = col_focus[rev(focus)], face = "bold"),
        legend.position = "bottom", legend.location = "plot", legend.justification = "left",
        legend.box.spacing = unit(0.5, "mm"),
        legend.key.spacing.x = unit(1.5, "mm"))

W <- FULL_W; H <- 100
fig <- compose(list(
  list(plot = pa, x = 0,    y = 0,    w = 0.50, h = 1,    tag = "A"),
  list(plot = pb, x = 0.51, y = 0.45, w = 0.49, h = 0.55, tag = "B", tx = 0.0),
  list(plot = pc, x = 0.51, y = 0,    w = 0.49, h = 0.45, tag = "C", tx = 0.0)
))
save_fig(fig, "fig6_mean_vs_breadth", width_mm = W, height_mm = H)
