# =============================================================================
# 31_si_figures.R — 补充图 S1–S7（与主图同一风格，输出 output/innov_figures/si/）
#
#   S1 网络现状预测 vs 观测（校准）
#   S2 因果森林与贝叶斯网络的一致性
#   S3 系数扰动下的组合排序与三维共赢比例
#   S4 广义可加模型与线性模型
#   S5 加入交互项后的预测
#   S6 12 条路径的县级效应（按响应类型）
#   S7 21 种空间配置下各组合的结果范围
#
# 依赖：30_si_analysis.R 的输出；08a–08d 的输出
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggrepel) })
source("lib/fig_style_innov.R")
OUT <- "output/innov_figures/si"

OUT_LAB <- c(CPI = "Change (CPI)", CPS = "Stability (CPS)", CPA = "Acceleration (CPA)")
AREA_LAB <- c("保护政策" = "Conservation", "土地利用" = "Landscape pattern",
              "利用强度" = "Use intensity", "产权制度" = "Soil–water conditions",
              "城市化" = "Urbanisation", "气候变化" = "Climate change")
pt <- function(x) x / .pt
type_scale <- function(...) scale_colour_manual(values = setNames(PAL_TYPE, type_name(names(PAL_TYPE))),
                                                name = NULL, ...)

# ---- S1 校准 -----------------------------------------------------------------
cal <- read.csv("output/si/calibration_points.csv") %>%
  mutate(Type = type_name(paste0("T", Type)), Outcome = factor(Outcome, levels = names(OUT_LAB)))
cal_stat <- cal %>% group_by(Outcome) %>%
  summarise(R2 = cor(Observed, Predicted)^2, sd = sd(Observed - Predicted), .groups = "drop")
s1_scatter <- function(y) {
  d <- filter(cal, Outcome == y); st <- filter(cal_stat, Outcome == y)
  lim <- range(c(d$Observed, d$Predicted))
  ggplot(d, aes(Predicted, Observed, colour = Type)) +
    geom_abline(linewidth = LW_AXIS, linetype = 2, colour = "grey50") +
    geom_point(size = 0.55, alpha = 0.6, stroke = 0) +
    annotate("text", x = lim[2], y = lim[1], hjust = 1, vjust = 0, size = pt(FS_NOTE), family = FONT,
             label = sprintf("R² = %.2f\nresidual s.d. = %.2f", st$R2, st$sd)) +
    scale_x_continuous(breaks = c(0, 0.5, 1)) + scale_y_continuous(breaks = c(0, 0.5, 1)) +
    coord_equal(xlim = lim, ylim = lim) + type_scale() +
    labs(title = OUT_LAB[y], x = "Modelled (no investment)", y = "Observed") +
    theme_innov() + theme(legend.position = "none")
}
cbt <- read.csv("output/si/calibration_by_type.csv") %>%
  mutate(Type = factor(type_name(paste0("T", Type)), levels = rev(type_name(names(PAL_TYPE)))),
         Outcome = factor(Outcome, levels = names(OUT_LAB)))
s1d <- ggplot(cbt, aes(R2, Type, colour = Outcome)) +
  geom_point(size = 2) +
  scale_colour_manual(values = PAL_OUTCOME, labels = OUT_LAB, name = NULL) +
  scale_x_continuous(limits = c(0, 0.4), expand = c(0, 0)) +
  labs(title = "Fit within each type", x = "R²", y = NULL) +
  theme_innov() + theme(legend.position = "bottom", legend.direction = "vertical",
                        legend.key.size = unit(2.6, "mm"), plot.margin = margin(1.5, 3.5, 1.5, 1.5, "mm"))
s1_leg <- cowplot::get_plot_component(
  ggplot(cal, aes(Predicted, Observed, colour = Type)) + geom_point(size = 2) + type_scale() +
    guides(colour = guide_legend(nrow = 1)) + theme_innov() + theme(legend.position = "bottom"),
  "guide-box-bottom")
s1 <- compose(list(
  list(plot = s1_scatter("CPI"), x = 0.00, y = 0.10, w = 0.25, h = 0.90, tag = "A"),
  list(plot = s1_scatter("CPS"), x = 0.25, y = 0.10, w = 0.25, h = 0.90, tag = "B"),
  list(plot = s1_scatter("CPA"), x = 0.50, y = 0.10, w = 0.25, h = 0.90, tag = "C"),
  list(plot = s1d,               x = 0.76, y = 0.00, w = 0.24, h = 1.00, tag = "D"),
  list(plot = s1_leg,            x = 0.00, y = 0.00, w = 0.75, h = 0.09)))
save_fig(s1, "figS1_calibration", FULL_W, 66, OUT)

# ---- S2 因果森林 vs 网络 -------------------------------------------------------
rk <- read.csv("output/csv/sensitivity/cf_bn_policy_ranking.csv") %>%
  filter(Policy_Config != "Baseline") %>%
  mutate(BN_Rank = rank(-BN_Sum), CF_Rank = rank(-CF_Sum), lab = CONFIG_LAB[Policy_Config])
rho_rank <- cor(rk$BN_Sum, rk$CF_Sum, method = "spearman")
write.csv(data.frame(rho_portfolio_rank = rho_rank), "output/si/cf_bn_rank_rho.csv", row.names = FALSE)
s2a <- ggplot(rk, aes(CF_Rank, BN_Rank)) +
  geom_abline(linewidth = LW_AXIS, linetype = 2, colour = "grey60") +
  geom_point(size = 1.8, colour = COL_TAG) +
  geom_text_repel(aes(label = lab), size = pt(FS_NOTE - 0.5), family = FONT, min.segment.length = 0.2,
                  box.padding = 0.25, segment.size = 0.25, seed = 1, max.overlaps = 30) +
  annotate("text", x = 0.6, y = 15.4, hjust = 1, vjust = 0, size = pt(FS_NOTE), family = FONT,
           label = sprintf("Spearman ρ = %.2f", rho_rank)) +
  scale_x_reverse(breaks = c(1, 5, 10, 15)) + scale_y_reverse(breaks = c(1, 5, 10, 15)) +
  coord_cartesian(xlim = c(15.6, 0.4), ylim = c(15.6, 0.4)) +
  labs(title = "Portfolio ranking by national mean gain",
       x = "Rank from causal forest effects (linear sum)", y = "Rank from Bayesian networks") +
  theme_innov()
cc <- read.csv("output/csv/sensitivity/cf_bn_county_comparison.csv")
agree <- bind_rows(lapply(names(OUT_LAB), function(y) {
  b <- cc[[paste0("BN_", y)]]; f <- cc[[paste0("CF_", y)]]
  data.frame(Outcome = y, Metric = c("Same sign", "Rank correlation (ρ)"),
             Value = c(mean(sign(b) == sign(f)), cor(b, f, method = "spearman")))
})) %>% mutate(Outcome = factor(Outcome, levels = rev(names(OUT_LAB))))
s2b <- ggplot(agree, aes(Value, Outcome, fill = Outcome)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", Value)), hjust = -0.15, size = pt(FS_NOTE), family = FONT) +
  facet_wrap(~Metric, ncol = 1) +
  scale_fill_manual(values = PAL_OUTCOME, guide = "none") +
  scale_y_discrete(labels = OUT_LAB) +
  scale_x_continuous(limits = c(0, 1.18), breaks = c(0, 0.5, 1), expand = c(0, 0)) +
  labs(title = "County-level agreement", x = NULL, y = NULL) +
  theme_innov()
s2 <- compose(list(
  list(plot = s2a, x = 0.00, y = 0.00, w = 0.55, h = 1.00, tag = "A"),
  list(plot = s2b, x = 0.56, y = 0.00, w = 0.44, h = 1.00, tag = "B")))
save_fig(s2, "figS2_cf_bn_consistency", FULL_W, 82, OUT)

# ---- S3 系数扰动 ---------------------------------------------------------------
pert <- read.csv("output/si/perturbation_summary.csv") %>%
  mutate(lab = factor(CONFIG_LAB[Policy], levels = CONFIG_LAB[names(sort(tapply(Ref_rank, Policy, mean),
                                                                              decreasing = TRUE))]),
         Noise = factor(sprintf("σ = %.1f", Noise)))
PAL_NOISE <- c("σ = 0.1" = "#9ED9D5", "σ = 0.2" = "#2BA6A0", "σ = 0.5" = "#145C58")
dodge <- position_dodge(width = 0.7)
s3a <- ggplot(pert, aes(y = lab)) +
  geom_linerange(aes(xmin = Rank_lo, xmax = Rank_hi, colour = Noise), position = dodge, linewidth = 1.1) +
  geom_point(data = distinct(pert, lab, Ref_rank), aes(x = Ref_rank), shape = 18, size = 2.2) +
  scale_x_reverse(breaks = c(1, 5, 10, 15)) +
  scale_colour_manual(values = PAL_NOISE, name = "Coefficient noise") +
  labs(title = "Rank by national mean gain", x = "Rank (95% range)", y = NULL) +
  theme_innov() + theme(legend.position = "none")
s3b <- ggplot(pert, aes(y = lab)) +
  geom_linerange(aes(xmin = TG_lo, xmax = TG_hi, colour = Noise), position = dodge, linewidth = 1.1) +
  geom_point(data = distinct(pert, lab, Ref_TG), aes(x = Ref_TG), shape = 18, size = 2.2) +
  scale_colour_manual(values = PAL_NOISE, name = "Coefficient noise") +
  scale_x_continuous(limits = c(0, 85), expand = c(0, 0)) +
  labs(title = "Share of triple-gain counties", x = "% of counties (95% range)", y = NULL) +
  theme_innov() + theme(axis.text.y = element_blank(), legend.position = c(0.82, 0.16))
s3 <- compose(list(
  list(plot = s3a, x = 0.00, y = 0.00, w = 0.58, h = 1.00, tag = "A"),
  list(plot = s3b, x = 0.58, y = 0.00, w = 0.42, h = 1.00, tag = "B", tx = 0.005)))
save_fig(s3, "figS3_perturbation", FULL_W, 92, OUT)

# ---- S4 非线性 -----------------------------------------------------------------
gam <- read.csv("output/csv/sensitivity/nonlinearity_gam_detail.csv") %>%
  mutate(Path = paste0(AREA_LAB[From], " → ", To),
         Type = type_name(paste0("T", Cluster)))
path_ord <- gam %>% group_by(Path) %>% summarise(m = mean(Delta_R2)) %>% arrange(m) %>% pull(Path)
gam$Path <- factor(gam$Path, levels = path_ord)
gm <- gam %>% group_by(Path) %>% summarise(m = mean(Delta_R2), .groups = "drop")
s4a <- ggplot(gam, aes(R2_Linear, R2_GAM, colour = Type)) +
  geom_abline(linewidth = LW_AXIS, linetype = 2, colour = "grey60") +
  geom_point(size = 1.5) + type_scale() +
  coord_equal(xlim = c(-0.02, 0.32), ylim = c(-0.02, 0.32)) +
  labs(title = "Variance explained", x = "R², linear", y = "R², additive model") +
  theme_innov() + theme(legend.position = "none")
s4b <- ggplot(gam, aes(Delta_R2, Path)) +
  geom_vline(xintercept = 0, linewidth = LW_AXIS, colour = "grey60") +
  geom_point(aes(colour = Type), size = 1.5, alpha = 0.9) +
  geom_point(data = gm, aes(m, Path), shape = 124, size = 4) + type_scale() +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "Gain from allowing non-linearity", x = "ΔR² (additive − linear); bar = mean", y = NULL) +
  theme_innov() + theme(legend.position = "right")
s4 <- compose(list(
  list(plot = s4a, x = 0.00, y = 0.00, w = 0.32, h = 1.00, tag = "A"),
  list(plot = s4b, x = 0.33, y = 0.00, w = 0.67, h = 1.00, tag = "B")))
save_fig(s4, "figS4_nonlinearity", FULL_W, 66, OUT)

# ---- S5 交互项 -----------------------------------------------------------------
ic <- read.csv("output/csv/sensitivity/interaction_bn_comparison.csv") %>%
  pivot_longer(-External, names_to = c("Model", "Outcome"), names_sep = "_") %>%
  filter(Model != "Diff") %>%
  mutate(Model = recode(Model, Orig = "Main model", Int = "With interaction terms"),
         Pressure = factor(recode(External, E1 = "Low", E2 = "Current", E3 = "High"),
                           levels = c("Low", "Current", "High")),
         Outcome = factor(Outcome, levels = names(OUT_LAB)))
s5a <- ggplot(ic, aes(Pressure, value, colour = Model, group = Model)) +
  geom_line(linewidth = LW_DATA) + geom_point(size = 1.6) +
  facet_wrap(~Outcome, nrow = 1, scales = "free_y", labeller = as_labeller(OUT_LAB)) +
  scale_colour_manual(values = c("Main model" = "grey45", "With interaction terms" = COL_TAG), name = NULL) +
  labs(title = "National mean prediction without investment", x = "External pressure", y = "Predicted value") +
  theme_innov() + theme(legend.position = "bottom")
icoef <- read.csv("output/csv/sensitivity/interaction_bn_coefficients.csv") %>%
  mutate(Term = recode(Interaction, "利用x气候" = "Use × climate", "利用x城市" = "Use × urbanisation"),
         Type = factor(type_name(paste0("T", Cluster)), levels = type_name(names(PAL_TYPE))),
         Target = factor(Target, levels = names(OUT_LAB)))
s5b <- ggplot(icoef, aes(Type, interaction(Term, Target, sep = " | "), fill = Coefficient)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f", Coefficient)), size = pt(FS_NOTE - 0.5), family = FONT) +
  scale_fill_gradient2(low = "#2E86AB", mid = "white", high = "#D1495B", name = "Coefficient") +
  scale_y_discrete(labels = function(x) sub(" \\| ", " → ", x)) +
  labs(title = "Interaction coefficients by type", x = NULL, y = NULL) +
  theme_innov() + theme(axis.line = element_blank(), axis.ticks = element_blank(),
                        axis.text.x = element_text(angle = 0), legend.position = "right",
                        legend.key.height = unit(5, "mm"))
ip <- read.csv("output/si/interaction_pressure.csv") %>% filter(External == "E3") %>%
  select(Policy, Model, Triple_gain) %>% pivot_wider(names_from = Model, values_from = Triple_gain) %>%
  mutate(focus = Policy %in% names(PAL_FOCUS), lab = ifelse(focus, CONFIG_LAB[Policy], NA))
rho_ip <- cor(ip$Main, ip$Interaction, method = "spearman")
s5c <- ggplot(ip, aes(Main, Interaction)) +
  geom_abline(linewidth = LW_AXIS, linetype = 2, colour = "grey60") +
  geom_point(data = filter(ip, !focus), colour = "grey65", size = 1.5) +
  geom_point(data = filter(ip, focus), aes(colour = Policy), size = 2.2) +
  geom_text_repel(aes(label = lab, colour = Policy), size = pt(FS_NOTE - 0.5), family = FONT,
                  seed = 2, box.padding = 0.3, min.segment.length = 0.2, segment.size = 0.25, na.rm = TRUE) +
  annotate("text", x = 0, y = 58, hjust = 0, vjust = 1, size = pt(FS_NOTE), family = FONT,
           label = sprintf("Spearman ρ = %.2f", rho_ip)) +
  scale_colour_manual(values = PAL_FOCUS, guide = "none") +
  coord_equal(xlim = c(0, 58), ylim = c(0, 58)) +
  labs(title = "Triple-gain share, high pressure", x = "Main model (%)", y = "With interaction terms (%)") +
  theme_innov()
s5 <- compose(list(
  list(plot = s5a, x = 0.00, y = 0.50, w = 0.66, h = 0.50, tag = "A"),
  list(plot = s5c, x = 0.67, y = 0.50, w = 0.33, h = 0.50, tag = "B"),
  list(plot = s5b, x = 0.00, y = 0.00, w = 1.00, h = 0.49, tag = "C")))
save_fig(s5, "figS5_interactions", FULL_W, 124, OUT)

# ---- S6 县级效应 ----------------------------------------------------------------
tau <- read.csv("models/causal_forest_results/cf_tau_matrix.csv", check.names = FALSE)
asg <- read.csv("models/cluster_results/cluster_assignments.csv")
tl <- tau %>% pivot_longer(-Code, names_to = "Path", values_to = "Effect") %>%
  separate(Path, c("Area", "Outcome"), sep = "_on_") %>%
  left_join(asg %>% select(Code, Cluster), by = "Code") %>%
  mutate(Type = factor(type_name(paste0("T", Cluster)), levels = type_name(names(PAL_TYPE))),
         Area = factor(AREA_LAB[Area], levels = AREA_LAB[c("利用强度", "保护政策", "土地利用", "产权制度")]),
         Outcome = factor(OUT_LAB[Outcome], levels = OUT_LAB))
s6 <- ggplot(tl, aes(Type, Effect, fill = Type)) +
  geom_hline(yintercept = 0, linewidth = LW_AXIS, colour = "grey50") +
  geom_boxplot(width = 0.7, linewidth = 0.3, outlier.size = 0.25, outlier.stroke = 0, outlier.alpha = 0.5) +
  facet_grid(Area ~ Outcome, scales = "free_y", labeller = label_wrap_gen(14)) +
  scale_fill_manual(values = setNames(PAL_TYPE, type_name(names(PAL_TYPE))), guide = "none") +
  scale_x_discrete(labels = function(x) sub("Type ", "", x)) +
  labs(x = "County response type", y = "County-level effect") +
  theme_innov() + theme(panel.spacing = unit(2.5, "mm"), strip.text.y = element_text(angle = -90))
save_fig(s6, "figS6_county_effects", FULL_W, 150, OUT)

# ---- S7 空间配置范围 -------------------------------------------------------------
tr <- read.csv("output/si/targeting_range.csv") %>%
  mutate(Group = case_when(Allocation_Strategy == "统一投资" ~ "Uniform",
                           grepl("^(CPDS|CPI)", Allocation_Strategy) ~ "By county baseline",
                           TRUE ~ "Chosen separately for each area"),
         Group = factor(Group, levels = c("Chosen separately for each area", "By county baseline", "Uniform")))
ord <- tr %>% filter(Group == "Uniform") %>% arrange(Triple_gain) %>% pull(Policy_Config)
tr$lab <- factor(CONFIG_LAB[tr$Policy_Config], levels = CONFIG_LAB[ord])
PAL_GRP <- c("Uniform" = "black", "By county baseline" = "#E07A1F", "Chosen separately for each area" = "#9ED9D5")
rng <- tr %>% group_by(lab) %>% summarise(lo = min(Triple_gain), hi = max(Triple_gain),
                                          glo = min(Mean_gain), ghi = max(Mean_gain), .groups = "drop")
s7a <- ggplot(tr, aes(Triple_gain, lab)) +
  geom_linerange(data = rng, aes(xmin = lo, xmax = hi, y = lab), inherit.aes = FALSE,
                 linewidth = 2.6, colour = "grey88") +
  geom_point(aes(colour = Group, size = Group)) +
  scale_colour_manual(values = PAL_GRP, name = NULL, breaks = rev(levels(tr$Group))) +
  scale_size_manual(values = c("Uniform" = 1.9, "By county baseline" = 1.4,
                               "Chosen separately for each area" = 1.0), guide = "none") +
  labs(title = "Share of triple-gain counties", x = "% of counties", y = NULL) +
  theme_innov() + theme(legend.position = c(0.70, 0.13), legend.direction = "vertical")
s7b <- ggplot(tr, aes(Mean_gain, lab)) +
  geom_linerange(data = rng, aes(xmin = glo, xmax = ghi, y = lab), inherit.aes = FALSE,
                 linewidth = 2.6, colour = "grey88") +
  geom_vline(xintercept = 0, linewidth = LW_AXIS, colour = "grey60") +
  geom_point(aes(colour = Group, size = Group)) +
  scale_colour_manual(values = PAL_GRP, guide = "none") +
  scale_size_manual(values = c("Uniform" = 1.9, "By county baseline" = 1.4,
                               "Chosen separately for each area" = 1.0), guide = "none") +
  labs(title = "National mean gain", x = "Sum of mean changes in three dimensions", y = NULL) +
  theme_innov() + theme(axis.text.y = element_blank())
s7 <- compose(list(
  list(plot = s7a, x = 0.00, y = 0.00, w = 0.60, h = 1.00, tag = "A"),
  list(plot = s7b, x = 0.60, y = 0.00, w = 0.40, h = 1.00, tag = "B", tx = 0.005)))
save_fig(s7, "figS7_targeting_range", FULL_W, 90, OUT)
cat(sprintf("Portfolio-rank rho (CF vs BN, 15 portfolios): %.3f\n", rho_rank))
