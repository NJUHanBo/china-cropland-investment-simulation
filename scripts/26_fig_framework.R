#!/usr/bin/env Rscript
# =============================================================================
# 26 — Fig. 1：技术框架（四层，每层放真实对象）
# =============================================================================
# 1 数据：26 个子指标 → 6 维（4 杠杆 + 2 外部条件）；CGI 时序 → CPI / CPS / CPA
# 2 驱动：12 个因果森林 → 1,560 × 12 效应矩阵 → (a) 按效应剖面聚 7 类；(b) ATE
#   显著性修正 DAG（范围内理论边 23 − 2 + 7 = 28）
# 3 耦合：每类一个高斯 BN，共用修正 DAG、系数分类型（真实系数）；CPS → CPA → CPI 链
# 4 情景与评价：15 × 21 × 3 全因子、两层预算守恒；均值与受益面两把尺子
# 注意：模拟用的是各类 BN 的回归系数；τ̂ 只决定分组与结构，箭头按此画
# =============================================================================

suppressMessages({ library(sf); library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")
sf_use_s2(FALSE)

node_en <- c(LEVER_EN, CPI = "CPI", CPS = "CPS", CPA = "CPA")
blank <- theme_void(base_family = FONT) + theme(plot.margin = margin(0, 0, 0, 0))
sub_title <- function(t) labs(title = t)
th_sub <- theme(plot.title = element_text(size = FS_TEXT, face = "bold", hjust = 0.5, lineheight = 1,
                                          family = FONT, margin = margin(0, 0, 1, 0, "mm")))

# =============================================================================
# 1 数据层
# =============================================================================
dims <- data.frame(
  dim = c("Urbanisation", "Climate", "Conservation", "Landscape", "Use intensity", "Soil–water"),
  n   = c(8, 3, 3, 4, 5, 3),
  role = c("External", "External", "Area", "Area", "Area", "Area"),
  note = c("land, population, economy", "temperature, rainfall, CO\u2082",
           "consolidation, subsidies,\nquantity protection", "area, aggregation, connectivity", "fertiliser, irrigation, cropping,\nmachinery, labour",
           "groundwater, soil temp.,\nsoil moisture")
) %>% mutate(y = rev(seq_along(dim)),
             fill = ifelse(role == "External", "grey55", PAL_LEVER[dim]))

p_dims <- ggplot(dims) +
  geom_tile(aes(x = 0.9, y = y, fill = I(fill)), width = 1.7, height = 0.82) +
  geom_text(aes(x = 0.12, y = y, label = dim), hjust = 0, colour = "white", family = FONT,
            fontface = "bold", size = FS_TEXT / .pt) +
  geom_text(aes(x = 1.68, y = y, label = n), hjust = 1, colour = "white", family = FONT,
            fontface = "bold", size = FS_TEXT / .pt) +
  geom_text(aes(x = 1.9, y = y, label = note), hjust = 0, family = FONT, colour = "grey25",
            size = (FS_NOTE - 0.5) / .pt, lineheight = 0.9) +
  annotate("segment", x = -0.12, xend = -0.12, y = 0.6, yend = 4.4, colour = "grey20", linewidth = 0.5) +
  annotate("segment", x = -0.12, xend = -0.12, y = 4.6, yend = 6.4, colour = "grey55", linewidth = 0.5) +
  annotate("text", x = -0.22, y = 2.5, label = "Investment areas", angle = 90, family = FONT,
           size = FS_NOTE / .pt, fontface = "bold") +
  annotate("text", x = -0.22, y = 5.5, label = "External", angle = 90, family = FONT,
           size = FS_NOTE / .pt, colour = "grey45", fontface = "bold") +
  scale_x_continuous(limits = c(-0.3, 3.75), expand = c(0, 0)) +
  sub_title("26 indicators \u2192 6 driver dimensions (ML-weighted)") + blank + th_sub

set.seed(7)
yr <- 2001:2020
cgi <- 0.45 + 0.0016 * (yr - 2001) + 0.00028 * (yr - 2001)^2 + rnorm(20, 0, 0.012)
mini_theme <- function() {
  theme_innov() + th_sub +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
          plot.margin = margin(0, 1.5, 0, 1.5, "mm"))
}
# CPI：两个十年均值之差
m1 <- mean(cgi[yr <= 2010]); m2 <- mean(cgi[yr >= 2011])
p_cpi <- ggplot() +
  geom_line(aes(yr, cgi), colour = "grey70", linewidth = 0.4) +
  annotate("segment", x = 2001, xend = 2010, y = m1, yend = m1, colour = PAL_OUTCOME[["CPI"]], linewidth = 1) +
  annotate("segment", x = 2011, xend = 2020, y = m2, yend = m2, colour = PAL_OUTCOME[["CPI"]], linewidth = 1) +
  annotate("segment", x = 2010.5, xend = 2010.5, y = m1, yend = m2, colour = PAL_OUTCOME[["CPI"]],
           linewidth = 0.5, arrow = arrow(length = unit(1.1, "mm"), type = "closed")) +
  annotate("text", x = 2001, y = max(cgi), hjust = 0, vjust = 1, lineheight = 0.95,
           label = "2011\u201320 mean\nminus 2001\u201310", family = FONT,
           size = (FS_NOTE - 1) / .pt, colour = PAL_OUTCOME[["CPI"]]) +
  labs(title = "Change (CPI)") + mini_theme()
# CPS：县内非波动像元的面积占比
set.seed(3)
px <- expand.grid(c = 1:9, r = 1:6)
px$cls <- sample(c("Improving", "Stable", "Degrading", "Fluctuating"), nrow(px), replace = TRUE,
                 prob = c(0.45, 0.25, 0.1, 0.2))
p_cps <- ggplot(px, aes(c, r, fill = cls)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_manual(values = c(Improving = "#2E86AB", Stable = "#7FB8D6", Degrading = "#B9D7E8",
                               Fluctuating = "grey85"), guide = "none") +
  annotate("text", x = 5, y = -0.2, vjust = 1, lineheight = 0.9,
           label = sprintf("share of pixels not\nfluctuating (%d/%d)", sum(px$cls != "Fluctuating"), nrow(px)),
           family = FONT, size = (FS_NOTE - 1) / .pt, colour = PAL_OUTCOME[["CPS"]]) +
  coord_cartesian(ylim = c(-2.3, 6.5), clip = "off") +
  labs(title = "Stability (CPS)") + mini_theme()
# CPA：逐像元二次项系数
fit <- lm(cgi ~ poly(yr, 2, raw = TRUE))
p_cpa <- ggplot() +
  geom_point(aes(yr, cgi), colour = "grey60", size = 0.6) +
  geom_line(aes(yr, fitted(fit)), colour = PAL_OUTCOME[["CPA"]], linewidth = 1.1) +
  annotate("text", x = 2001, y = max(cgi), hjust = 0, vjust = 1, lineheight = 0.95,
           label = "curvature of a\nquadratic trend", family = FONT,
           size = (FS_NOTE - 1) / .pt, colour = PAL_OUTCOME[["CPA"]]) +
  labs(title = "Acceleration (CPA)") + mini_theme()

# =============================================================================
# 2 驱动层
# =============================================================================
tau <- read.csv("models/causal_forest_results/cf_tau_matrix.csv", check.names = FALSE)
asg <- read.csv("models/cluster_results/cluster_assignments.csv")
tl <- tau %>% mutate(Type = asg$Cluster[match(Code, asg$Code)]) %>%
  arrange(Type, `利用强度_on_CPS`) %>% mutate(row = row_number()) %>%
  pivot_longer(-c(Code, Type, row), names_to = "path", values_to = "tau") %>%
  mutate(lever = factor(LEVER_EN[sub("_on_.*", "", path)], levels = names(PAL_LEVER)),
         outcome = factor(sub(".*_on_", "", path), levels = c("CPI", "CPS", "CPA")),
         col = as.integer(lever) * 4 + as.integer(outcome))
type_bar <- tl %>% distinct(row, Type)
lim <- quantile(abs(tl$tau), 0.98)
p_tau <- ggplot() +
  geom_raster(data = tl, aes(col, row, fill = pmax(pmin(tau, lim), -lim))) +
  geom_tile(data = type_bar, aes(x = 3.3, y = row, fill = NULL, colour = NULL),
            fill = PAL_TYPE[paste0("T", type_bar$Type)], width = 0.9, height = 1) +
  scale_fill_gradient2(low = "#2E6FAE", mid = "white", high = "#C8423E", guide = "none") +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(breaks = c(6, 10, 14, 18), labels = c("Use", "Cons", "Land", "Soil"),
                     position = "top", expand = c(0, 0)) +
  labs(title = "12 causal forests \u2192\ncounty effects (1,560 \u00d7 12)") +
  theme_void(base_family = FONT) + th_sub +
  theme(axis.text.x.top = element_text(size = FS_NOTE - 0.5, family = FONT, colour = "grey25"),
        plot.margin = margin(0, 1, 0, 1))

county <- st_read("boundaries/county_shp.shp", quiet = TRUE)
prov <- st_read("boundaries/province_shp2 2.shp", quiet = TRUE)
st_crs(prov) <- st_crs(county)
county$Type <- paste0("T", asg$Cluster[match(county$Code, asg$Code)])
county_s <- st_simplify(county, dTolerance = 6000, preserveTopology = TRUE)
prov_s <- st_simplify(prov, dTolerance = 8000, preserveTopology = TRUE)
p_map <- ggplot() +
  geom_sf(data = prov_s, fill = "grey92", colour = NA) +
  geom_sf(data = county_s, aes(fill = Type), colour = NA) +
  scale_fill_manual(values = PAL_TYPE, guide = "none") +
  coord_sf(xlim = c(-2700000, 2300000), ylim = c(1500000, 5980000), expand = FALSE, datum = NA) +
  labs(title = "(a) Counties grouped by effect profile\n\u2192 7 county response types") +
  blank + th_sub

rep_df <- read.csv("models/corrected_bn/dag_correction_report.csv") %>%
  transmute(from = node_en[From], to = node_en[To],
            status = recode(CF_Status, kept_theoretical = "Theory", confirmed_by_cf = "Theory",
                            removed_by_cf = "Removed", added_by_cf = "Added"))
pos <- data.frame(
  node = c("Urbanisation", "Climate", "Conservation", "Landscape", "Use intensity", "Soil–water",
           "CPS", "CPA", "CPI"),
  x = c(0, 0, 1, 1, 1, 1, 2, 2, 2),
  y = c(3.2, 1.8, 4, 3, 2, 1, 3.5, 2.5, 1.5)
)
pos$col <- c("grey55", "grey55", PAL_LEVER[c("Conservation", "Landscape", "Use intensity", "Soil–water")],
             rep("grey15", 3))
pos$hj <- c(1, 1, 0.5, 0.5, 0.5, 0.5, 0, 0, 0)
pos$nx <- c(-0.09, -0.09, 0, 0, 0, 0, 0.09, 0.09, 0.09)
pos$ny <- c(0, 0, 0.3, 0.3, 0.3, 0.3, 0, 0, 0)
ed <- rep_df %>% left_join(pos %>% select(from = node, x, y), by = "from") %>%
  left_join(pos %>% select(to = node, xend = x, yend = y), by = "to") %>%
  mutate(curved = x == xend)
pal_edge <- c(Theory = "grey60", Removed = "#C8423E", Added = "#2BA6A0")
p_dag <- ggplot() +
  geom_segment(data = filter(ed, !curved), aes(x, y, xend = xend, yend = yend, colour = status,
                                               linetype = status, linewidth = status),
               arrow = arrow(length = unit(1.2, "mm"), type = "closed")) +
  geom_curve(data = filter(ed, curved), aes(x, y, xend = xend, yend = yend, colour = status,
                                            linetype = status, linewidth = status),
             curvature = -0.45, arrow = arrow(length = unit(1.2, "mm"), type = "closed")) +
  geom_point(data = pos, aes(x, y), colour = pos$col, size = 3.6) +
  geom_label(data = pos, aes(x + nx, y + ny, label = node, hjust = hj), family = FONT,
             size = (FS_NOTE - 0.5) / .pt, label.size = 0, fill = "grey97",
             label.padding = unit(0.4, "mm"), colour = "grey15") +
  scale_colour_manual(values = pal_edge, name = NULL) +
  scale_linetype_manual(values = c(Theory = "solid", Removed = "22", Added = "solid"), name = NULL) +
  scale_linewidth_manual(values = c(Theory = 0.3, Removed = 0.55, Added = 0.6), name = NULL) +
  scale_x_continuous(limits = c(-0.75, 2.45)) + scale_y_continuous(limits = c(0.7, 4.45)) +
  labs(title = "(b) Average effects correct the causal graph\n23 prior links \u2212 2 + 7 = 28") +
  blank + th_sub +
  theme(legend.position = "bottom", legend.text = element_text(size = FS_NOTE, family = FONT),
        legend.key.width = unit(5, "mm"), legend.box.spacing = unit(0, "mm"),
        legend.margin = margin(0, 0, 0, 0))

# =============================================================================
# 3 耦合层：7 个分类型网络（杠杆 → 结果边，线宽 = |系数|，颜色 = 符号）
# =============================================================================
cf <- read.csv("models/corrected_bn/cluster_bn_coefficients.csv") %>%
  mutate(from = node_en[From], to = node_en[To]) %>%
  filter(from %in% names(PAL_LEVER), to %in% c("CPI", "CPS", "CPA"))
gpos <- data.frame(node = c(names(PAL_LEVER), "CPS", "CPA", "CPI"),
                   x = c(0, 0, 0, 0, 1, 1, 1), y = c(4, 3, 2, 1, 3.4, 2.5, 1.6))
gl <- cf %>% left_join(gpos %>% select(from = node, x, y), by = "from") %>%
  left_join(gpos %>% select(to = node, xend = x, yend = y), by = "to") %>%
  mutate(Type = factor(paste0("T", Cluster), levels = names(PAL_TYPE)))
gn <- tidyr::crossing(Type = factor(names(PAL_TYPE), levels = names(PAL_TYPE)), gpos) %>%
  mutate(col = c(PAL_LEVER, CPS = "grey15", CPA = "grey15", CPI = "grey15")[node])
glab <- gpos %>% mutate(Type = factor("T1", levels = names(PAL_TYPE)),
                        lab = c("Use", "Cons", "Land", "Soil", "CPS", "CPA", "CPI"),
                        hj = c(1, 1, 1, 1, 0, 0, 0), xx = ifelse(x == 0, -0.1, 1.1))
p_glyph <- ggplot() +
  geom_segment(data = gl, aes(x, y, xend = xend, yend = yend, linewidth = abs(Coefficient),
                              colour = Coefficient > 0), lineend = "round", alpha = 0.85) +
  geom_point(data = gn, aes(x, y), colour = gn$col, size = 1.6) +
  facet_wrap(~ Type, nrow = 1, labeller = as_labeller(type_name)) +
  scale_linewidth(range = c(0.15, 1.8), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "#C8423E", `FALSE` = "#2E6FAE"), guide = "none") +
  scale_x_continuous(limits = c(-0.12, 1.12)) +
  blank +
  theme(strip.text = element_text(family = FONT, face = "bold", size = FS_TEXT,
                                  colour = PAL_TYPE, margin = margin(0, 0, 0.6, 0, "mm")),
        panel.spacing.x = unit(1.5, "mm"),
        panel.background = element_rect(fill = "grey97", colour = NA))

key_lab <- data.frame(
  lab = c("Use intensity", "Conservation", "Landscape", "Soil–water", "Outcomes:\nCPS, CPA, CPI"),
  col = c(PAL_LEVER, "grey15"), y = c(5, 4, 3, 2, 0.85))
p_key <- ggplot(key_lab, aes(0, y)) +
  geom_point(colour = key_lab$col, size = 1.8) +
  geom_text(aes(x = 0.18, label = lab), hjust = 0, family = FONT, lineheight = 0.95,
            size = (FS_NOTE - 0.5) / .pt, colour = "grey20") +
  annotate("text", x = -0.1, y = 6, label = "Nodes", hjust = 0, family = FONT, fontface = "bold",
           size = FS_TEXT / .pt) +
  scale_x_continuous(limits = c(-0.15, 2.2)) + scale_y_continuous(limits = c(0.2, 6.4)) + blank

chain <- data.frame(node = c("CPS", "CPA", "CPI"), x = c(0, 1, 2), y = 0)
p_chain <- ggplot() +
  annotate("text", x = -0.3, y = 1.95, hjust = 0, label = "Types from (a), links from (b)",
           family = FONT, fontface = "bold", size = FS_NOTE / .pt) +
  annotate("text", x = -0.3, y = 1.5, hjust = 0, family = FONT, size = (FS_NOTE - 0.5) / .pt,
           colour = "grey30", lineheight = 1,
           label = "one Bayesian network per type\nline width = |coefficient|, red +, blue \u2212") +
  annotate("text", x = -0.3, y = 0.88, hjust = 0, family = FONT, fontface = "bold",
           size = FS_NOTE / .pt, label = "Links among outcomes") +
  geom_curve(aes(x = 0.12, y = 0.12, xend = 1.88, yend = 0.12), curvature = -0.3,
             colour = "grey45", linewidth = 0.5, arrow = arrow(length = unit(1.3, "mm"), type = "closed")) +
  geom_segment(aes(x = c(0.16, 1.16), y = 0, xend = c(0.84, 1.84), yend = 0),
               colour = "grey45", linewidth = 0.6, arrow = arrow(length = unit(1.3, "mm"), type = "closed")) +
  geom_point(data = chain, aes(x, y, colour = node), size = 7) +
  geom_text(data = chain, aes(x, y, label = node), colour = "white", family = FONT,
            fontface = "bold", size = (FS_NOTE - 0.5) / .pt) +
  scale_colour_manual(values = PAL_OUTCOME, guide = "none") +
  scale_x_continuous(limits = c(-0.35, 2.35)) + scale_y_continuous(limits = c(-0.6, 2.15)) + blank

# =============================================================================
# 4 情景与评价层：情景矩阵（行 = 部门配置，列 = 空间策略，层 = 环境）+ 按行读出两把尺子
# =============================================================================
cfg_ids <- sprintf("I%02d", 1:15)
cfg_vec <- list(I01 = c(3,3,3,3), I02 = c(9,1,1,1), I03 = c(1,9,1,1), I04 = c(1,1,9,1),
                I05 = c(1,1,1,9), I06 = c(5,5,1,1), I07 = c(5,1,5,1), I08 = c(5,1,1,5),
                I09 = c(1,5,5,1), I10 = c(1,5,1,5), I11 = c(1,1,5,5), I12 = c(4,4,4,0),
                I13 = c(4,4,0,4), I14 = c(4,0,4,4), I15 = c(0,4,4,4))
lev_order <- c("Conservation", "Landscape", "Use intensity", "Soil–water")
row_of <- setNames(15:1, cfg_ids)
tw <- 0.72; tx0 <- -10.2
tok <- bind_rows(lapply(cfg_ids, function(i)
  data.frame(cfg = i, y = row_of[[i]], lever = rep(lev_order, cfg_vec[[i]]), k = 1:12))) %>%
  mutate(x = tx0 + (k - 1) * tw)

alloc_order <- c("统一投资", "CPDS锦上添花", "CPDS雪中送炭", "CPI锦上添花", "CPI雪中送炭",
                 setdiff(ALLOCATION_STRATEGIES_ALL <- unique(read.csv("output/csv/tradeoff_by_scenario.csv")$Allocation_Strategy),
                         c("统一投资", "CPDS锦上添花", "CPDS雪中送炭", "CPI锦上添花", "CPI雪中送炭")))
sc <- read.csv("output/csv/tradeoff_by_scenario.csv") %>%
  filter(External_Config == "E2", Policy_Config %in% cfg_ids) %>%
  mutate(y = row_of[Policy_Config], x = match(Allocation_Strategy, alloc_order) - 0.5)

cmx <- read.csv("output/csv/ablation_config_metrics.csv") %>% filter(Arm == "Full") %>%
  mutate(Total = CPI + CPS + CPA, y = row_of[Policy_Config])
r1 <- 24.5; r2 <- 35.5; rw <- 8.5
cmx <- cmx %>% mutate(mean_len = Total / max(abs(Total)) * rw * 0.62,
                      brd_len = Synergy / 100 * rw)
best_mean <- cmx$Policy_Config[which.max(cmx$Total)]
best_brd  <- cmx$Policy_Config[which.max(cmx$Synergy)]
r1_zero <- r1 + rw * 0.36

lay <- data.frame(o = c(1.6, 0.8), lab = c("High \u00d71.30", "Low \u00d70.85"))
p_mat <- ggplot() +
  # 环境层（矩阵后面错开的两层）
  geom_rect(data = lay, aes(xmin = 0 + o, xmax = 21 + o, ymin = 0.5 - o, ymax = 15.5 - o),
            fill = "white", colour = "grey60", linewidth = 0.3) +
  # 当前环境：真实受益面
  geom_tile(data = sc, aes(x, y, fill = Pct_Full_Synergy), colour = "white", linewidth = 0.15) +
  scale_fill_gradient(low = "#F4F4F4", high = "#1F7A75", limits = c(0, 100), name = "Triple-gain (%)",
                      guide = guide_colourbar(direction = "horizontal", barwidth = unit(16, "mm"),
                                              barheight = unit(1.8, "mm"), title.position = "top")) +
  annotate("text", x = 21.25, y = 15.5, hjust = 0, vjust = 1, label = "Current \u00d71", family = FONT,
           size = (FS_NOTE - 1) / .pt, colour = "grey25", fontface = "bold") +
  # 行头：12 个预算单位
  geom_tile(data = tok, aes(x, y), fill = PAL_LEVER[tok$lever], width = tw * 0.86, height = 0.72) +
  # 列头：空间策略分组
  annotate("segment", x = c(0.1, 1.1, 5.1), xend = c(0.9, 4.9, 20.9), y = 16.1, yend = 16.1,
           colour = "#B5641A", linewidth = 0.5) +
  annotate("text", x = c(0.5, 3, 13), y = 16.45, vjust = 0, family = FONT, size = (FS_NOTE - 1) / .pt,
           colour = "#B5641A",
           label = c("U", "by baseline", "set per investment area (16)")) +
  annotate("text", x = 10.5, y = 18.6, label = "21 spatial targeting strategies (total per area fixed)",
           family = FONT, fontface = "bold", size = FS_NOTE / .pt, colour = "#B5641A") +
  annotate("text", x = tx0 + 5.5 * tw, y = 16.55, vjust = 0, lineheight = 0.9,
           label = "15 investment\nportfolios (12 shares)", family = FONT, fontface = "bold",
           size = FS_NOTE / .pt, colour = "#2E6FAE") +
  # 读出：两把尺子（与矩阵同行）
  annotate("segment", x = r1_zero, xend = r1_zero, y = 0.5, yend = 15.5, colour = "grey50", linewidth = 0.3) +
  geom_rect(data = cmx, aes(xmin = pmin(r1_zero, r1_zero + mean_len), xmax = pmax(r1_zero, r1_zero + mean_len),
                            ymin = y - 0.33, ymax = y + 0.33),
            fill = ifelse(cmx$Policy_Config == best_mean, PAL_FOCUS[["I04"]], "grey72")) +
  geom_rect(data = cmx, aes(xmin = r2, xmax = r2 + brd_len, ymin = y - 0.33, ymax = y + 0.33),
            fill = ifelse(cmx$Policy_Config == best_brd, PAL_FOCUS[["I06"]], "grey72")) +
  annotate("segment", x = r2, xend = r2, y = 0.5, yend = 15.5, colour = "grey50", linewidth = 0.3) +
  annotate("text", x = c(r1 + rw / 2, r2 + rw / 2), y = 16.55, vjust = 0, family = FONT,
           fontface = "bold", size = FS_NOTE / .pt, lineheight = 0.9,
           label = c("National\nmean gain", "Triple-gain\ncounties")) +
  annotate("text", x = c(r1 + rw / 2, r2 + rw / 2), y = -0.4, vjust = 1, family = FONT,
           size = (FS_NOTE - 1) / .pt, lineheight = 0.9,
           colour = c(PAL_FOCUS[["I04"]], PAL_FOCUS[["I06"]]),
           label = c("best: use-\nintensity focus", "best: conservation\n+ landscape")) +
  # 步骤连线：每格 = 一次模拟；行 → 读出
  annotate("segment", x = 22.9, xend = 24.2, y = 8, yend = 8, colour = "grey35", linewidth = 0.5,
           arrow = arrow(length = unit(1.3, "mm"), type = "closed")) +
  annotate("text", x = 10.9, y = -1.55, vjust = 1, family = FONT, size = (FS_NOTE - 1) / .pt,
           colour = "grey25", lineheight = 0.95,
           label = "each cell = 1 scenario simulated for 1,560 counties with the 7 type-specific networks\nfront layer: current external pressure; behind: low (\u00d70.85) and high (\u00d71.30)") +
  scale_x_continuous(limits = c(-11, 44.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-4.6, 19.2), expand = c(0, 0)) +
  blank +
  theme(legend.position = "inside", legend.position.inside = c(0.005, 0.0),
        legend.justification = c(0, 0), legend.title = element_text(size = FS_NOTE - 1, family = FONT),
        legend.text = element_text(size = FS_NOTE - 1.5, family = FONT))

# =============================================================================
# 拼版：四条横带，左侧竖排层名，层间向下箭头
# =============================================================================
W <- FULL_W; H <- 214
band_y <- c(0.80, 0.53, 0.335, 0)        # 各带底边
band_h <- c(0.20, 0.265, 0.19, 0.33)
band_lab <- c("1  Data", "2  Driver analysis", "3  Coupling", "4  Scenarios & evaluation")
L <- 0.045                               # 左侧层名条宽度

canvas <- cowplot::ggdraw()
for (b in 1:4) {
  canvas <- canvas +
    cowplot::draw_grob(rectGrob(gp = gpar(fill = "grey97", col = NA)),
                       x = L + 0.004, y = band_y[b] + 0.004, width = 1 - L - 0.004, height = band_h[b] - 0.008) +
    cowplot::draw_grob(rectGrob(gp = gpar(fill = COL_TAG, col = NA)),
                       x = 0, y = band_y[b] + 0.004, width = L, height = band_h[b] - 0.008) +
    cowplot::draw_label(band_lab[b], x = L / 2, y = band_y[b] + band_h[b] / 2, angle = 90,
                        fontfamily = FONT, fontface = "bold", size = FS_BASE, colour = "white")
}
put <- function(g, p, x, y, w, h) g + cowplot::draw_plot(p, x = x, y = y, width = w, height = h)
X0 <- L + 0.012
fig <- canvas
fig <- put(fig, p_dims,  X0,    0.808, 0.47,  0.188)
fig <- fig + cowplot::draw_label("CGI, 250 m, 2001\u20132020 \u2192 3 county outcomes", x = 0.765,
                                 y = 0.987, fontfamily = FONT, fontface = "bold", size = FS_TEXT)
fig <- put(fig, p_cpi,   0.535, 0.810, 0.153, 0.166)
fig <- put(fig, p_cps,   0.688, 0.810, 0.153, 0.166)
fig <- put(fig, p_cpa,   0.841, 0.810, 0.153, 0.166)
fig <- put(fig, p_tau,   X0,    0.545, 0.19,  0.244)
fig <- put(fig, p_map,   0.27,  0.545, 0.30,  0.244)
fig <- put(fig, p_dag,   0.585, 0.545, 0.41,  0.244)
fig <- put(fig, p_key,   X0,    0.342, 0.13,  0.178)
fig <- put(fig, p_glyph, X0 + 0.13, 0.342, 0.53, 0.178)
fig <- put(fig, p_chain, 0.725, 0.345, 0.27,  0.17)
step <- function(g, num, txt, x) {
  badge <- grobTree(circleGrob(r = unit(1.7, "mm"), gp = gpar(fill = COL_TAG, col = NA)),
                    textGrob(num, gp = gpar(col = "white", fontfamily = FONT, fontface = "bold",
                                            fontsize = FS_NOTE)))
  g + cowplot::draw_grob(badge, x = x - 0.015, y = 0.318 - 0.01, width = 0.03, height = 0.02) +
    cowplot::draw_label(txt, x = x + 0.014, y = 0.318, hjust = 0, fontfamily = FONT,
                        fontface = "bold", size = FS_TEXT, colour = "grey15")
}
fig <- step(fig, "1", "Define: rows, columns, layers", 0.075)
fig <- step(fig, "2", "Simulate every cell", 0.40)
fig <- step(fig, "3", "Read each row with two criteria", 0.69)
fig <- put(fig, p_mat, X0, 0.008, 1 - X0 - 0.006, 0.300)
for (yy in c(0.80, 0.53, 0.335)) {
  fig <- fig + cowplot::draw_line(x = c(0.52, 0.52), y = c(yy + 0.009, yy - 0.006),
                                  colour = "grey35", linewidth = 0.6,
                                  arrow = arrow(length = unit(1.6, "mm"), type = "closed"))
}
# 驱动层内部：效应矩阵 → (a) 分类型；→ (b) 修正结构
fig <- fig +
  cowplot::draw_line(x = c(0.24, 0.285), y = c(0.665, 0.665), colour = "grey35", linewidth = 0.5,
                     arrow = arrow(length = unit(1.4, "mm"), type = "closed")) +
  cowplot::draw_line(x = c(0.15, 0.15, 0.62), y = c(0.548, 0.539, 0.539), colour = "grey35",
                     linewidth = 0.5) +
  cowplot::draw_line(x = c(0.62, 0.62), y = c(0.539, 0.556), colour = "grey35", linewidth = 0.5,
                     arrow = arrow(length = unit(1.4, "mm"), type = "closed"))

save_fig(fig, "fig1_framework", width_mm = W, height_mm = H)
