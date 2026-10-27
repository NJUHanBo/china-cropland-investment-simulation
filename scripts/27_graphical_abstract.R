#!/usr/bin/env Rscript
# =============================================================================
# 27 — 图形摘要（The Innovation 首页；手工矢量绘制，不用生成式 AI）
# =============================================================================
# 版式参照模板首页（Wang et al. 2026）：横幅 174 × 108 mm，浅色天空 + 透视农田背景，
# 上方三段标题条，三张半透明卡片放真实数据小图：
#   左  驱动分析：1,560 县 7 类响应地图
#   中  联动模拟：7 张类型网络 + 15 × 21 情景矩阵（现状压力下三维共赢比例）
#   右  政策答案：两种标准下 15 个组合的排名交叉
# =============================================================================

suppressMessages({ library(sf); library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")
sf_use_s2(FALSE)

W <- 174; H <- 108
npc <- function(x, y, w, h) list(x = x / W, y = y / H, w = w / W, h = h / H)
add <- function(g, p, box) g + cowplot::draw_plot(p, x = box$x, y = box$y, width = box$w, height = box$h)
blank <- theme_void(base_family = FONT) + theme(plot.margin = margin(0, 0, 0, 0))
TXT <- "grey15"

# ---- 背景：天空渐变 + 远山 + 透视农田 ----------------------------------------------
sky <- data.frame(y0 = seq(20, H, length.out = 60)) %>%
  mutate(y1 = lead(y0, default = H), f = (y0 - 20) / (H - 20))
sky_col <- colorRampPalette(c("#F4FAFB", "#D3E9F0"))(nrow(sky))
hill <- function(x0, x1, top, base = 21, n = 60) {
  x <- seq(x0, x1, length.out = n)
  data.frame(x = c(x, x1, x0), y = c(base + (top - base) * sin(pi * (x - x0) / (x1 - x0))^1.6, base, base))
}
vp <- c(87, 70)                       # 灭点
xb <- seq(-120, 294, length.out = 19) # 地面线（y = 0）上的分界
fields <- bind_rows(lapply(seq_len(length(xb) - 1), function(i) bind_rows(lapply(1:2, function(r) {
  yb <- c(0, 9, 21)[r]; yt <- c(0, 9, 21)[r + 1]
  at <- function(xg, y) xg + (vp[1] - xg) * y / vp[2]
  data.frame(id = paste(i, r), x = c(at(xb[i], yb), at(xb[i + 1], yb), at(xb[i + 1], yt), at(xb[i], yt)),
             y = c(yb, yb, yt, yt), col = c("#A9D18E", "#C9DE96", "#8CC27A", "#D9E6A6")[(i + r) %% 4 + 1])
}))))
trees <- data.frame(x = c(6, 11, 33, 52, 70, 101, 122, 139, 160, 168), s = c(1.3, 1, 1.2, 0.9, 1.1, 1, 1.3, 0.9, 1.2, 1))

bg <- ggplot() +
  geom_rect(data = sky, aes(xmin = 0, xmax = W, ymin = y0, ymax = y1), fill = sky_col, colour = NA) +
  geom_polygon(data = hill(-10, 70, 34), aes(x, y), fill = "#C6DFD6", colour = NA) +
  geom_polygon(data = hill(45, 135, 30), aes(x, y), fill = "#B5D6CB", colour = NA) +
  geom_polygon(data = hill(110, 190, 35), aes(x, y), fill = "#C6DFD6", colour = NA) +
  geom_rect(aes(xmin = 0, xmax = W, ymin = 0, ymax = 21), fill = "#B9D98F") +
  geom_polygon(data = fields, aes(x, y, group = id, fill = I(col)), colour = "#F4F7E8", linewidth = 0.35) +
  geom_segment(data = trees, aes(x = x, xend = x, y = 21, yend = 21 + 2.2 * s), colour = "#6B5B45", linewidth = 0.5) +
  geom_point(data = trees, aes(x, 21 + 2.8 * s, size = I(3.2 * s)), colour = "#5E9E62") +
  coord_fixed(xlim = c(0, W), ylim = c(0, H), expand = FALSE, clip = "on") + blank

# ---- 卡片与标题条 ------------------------------------------------------------------
card <- function(x0, x1, y0 = 25, y1 = 93) data.frame(xmin = x0, xmax = x1, ymin = y0, ymax = y1)
cards <- bind_rows(card(4, 56), card(61, 113), card(118, 170))
heads <- data.frame(xmin = c(4, 61, 118), xmax = c(56, 113, 170), lab = c("Driver analysis", "Linked simulation", "Policy answers"),
                    fill = c("#7FC8C3", "#2BA6A0", "#1F7A75"))
chev <- function(x, y) data.frame(x = c(x, x + 1.6, x + 3.2, x + 1.6, x, x + 1.6) , y = c(y + 3, y + 3, y, y - 3, y - 3, y))
chevs <- bind_rows(lapply(c(56.6, 113.6), function(x0) bind_rows(
  mutate(data.frame(x = c(x0, x0 + 1.5, x0 + 3, x0 + 1.5, x0, x0 + 1.5), y = c(62, 62, 59, 56, 56, 59)), id = paste0(x0, "a")),
  mutate(data.frame(x = c(x0, x0 + 1.5, x0 + 3, x0 + 1.5, x0, x0 + 1.5) + 1.6, y = c(62, 62, 59, 56, 56, 59)), id = paste0(x0, "b")))))
over <- ggplot() +
  geom_rect(data = cards, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "white", alpha = 0.88, colour = NA) +
  geom_rect(data = heads, aes(xmin = xmin, xmax = xmax, ymin = 96, ymax = 104, fill = I(fill)), colour = NA) +
  geom_text(data = heads, aes(x = (xmin + xmax) / 2, y = 100, label = lab), colour = "white", family = FONT,
            fontface = "bold", size = 9 / .pt) +
  geom_polygon(data = chevs, aes(x, y, group = id), fill = "#2BA6A0") +
  coord_fixed(xlim = c(0, W), ylim = c(0, H), expand = FALSE) + blank

# ---- 左：响应类型地图 ----------------------------------------------------------------
BND <- "boundaries"
county <- st_read(file.path(BND, "county_shp.shp"), quiet = TRUE)
prov   <- st_read(file.path(BND, "province_shp2 2.shp"), quiet = TRUE)
crs_aea <- st_crs(county); st_crs(prov) <- crs_aea
dash  <- st_transform(st_read(file.path(BND, "南海九段线.shp"), quiet = TRUE), crs_aea)
isles <- st_transform(st_read(file.path(BND, "南海诸岛.shp"), quiet = TRUE), crs_aea)
asg <- read.csv("models/cluster_results/cluster_assignments.csv")
county$Type <- factor(paste0("T", asg$Cluster[match(county$Code, asg$Code)]), levels = names(PAL_TYPE))
prov_s <- st_simplify(prov, dTolerance = 5000, preserveTopology = TRUE)
county_s <- st_simplify(county, dTolerance = 2500, preserveTopology = TRUE)
nation <- st_union(st_buffer(prov_s, 0))
p_map <- ggplot() +
  geom_sf(data = prov_s, fill = "grey92", colour = NA) +
  geom_sf(data = county_s, aes(fill = Type), colour = NA) +
  geom_sf(data = nation, fill = NA, colour = "grey40", linewidth = 0.25) +
  geom_sf(data = isles, colour = "grey50", linewidth = 0.12) +
  geom_sf(data = dash, colour = "grey40", linewidth = 0.3) +
  scale_fill_manual(values = PAL_TYPE, guide = "none") +
  coord_sf(crs = crs_aea, expand = FALSE, datum = NA, xlim = c(-2700000, 2300000), ylim = c(380000, 5980000)) +
  blank
chips <- data.frame(x = 1:7, t = names(PAL_TYPE))
p_chip <- ggplot(chips, aes(x, 0, fill = t)) + geom_tile(width = 0.85, height = 1) +
  geom_text(aes(label = x), colour = "white", family = FONT, fontface = "bold", size = 6.5 / .pt) +
  scale_fill_manual(values = PAL_TYPE, guide = "none") + coord_fixed(expand = FALSE) + blank
txt_plot <- function(lab, size = 7.5, face = "plain", hjust = 0.5, col = TXT, lh = 0.95)
  ggplot() + annotate("text", x = hjust, y = 0.5, label = lab, hjust = hjust, family = FONT, fontface = face,
                      size = size / .pt, colour = col, lineheight = lh) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + blank

# ---- 中：网络叠层 + 情景矩阵 ---------------------------------------------------------
nodes <- data.frame(id = c("C", "L", "U", "S", "CPS", "CPA", "CPI"),
                    x = c(0, 0, 0, 0, 2.6, 3.6, 4.6), y = c(3, 2, 1, 0, 1.5, 2.5, 1.5),
                    col = c(PAL_LEVER[c("Conservation", "Landscape", "Use intensity", "Soil–water")],
                            PAL_OUTCOME[c("CPS", "CPA", "CPI")]))
ed <- data.frame(from = c("C", "C", "C", "L", "L", "U", "U", "U", "S", "S", "CPS", "CPA", "CPS"),
                 to = c("CPI", "CPS", "CPA", "CPI", "CPA", "CPI", "CPS", "CPA", "CPS", "CPA", "CPA", "CPI", "CPI")) %>%
  left_join(nodes %>% select(from = id, x0 = x, y0 = y), by = "from") %>%
  left_join(nodes %>% select(to = id, x1 = x, y1 = y), by = "to")
layers <- data.frame(k = 7:1) %>% mutate(dx = (k - 1) * 0.16, dy = (k - 1) * 0.12, col = PAL_TYPE[k])
p_net <- ggplot() +
  geom_rect(data = layers, aes(xmin = -0.5 + dx, xmax = 6.7 + dx, ymin = -0.55 + dy, ymax = 3.55 + dy),
            fill = "white", colour = layers$col, linewidth = 0.45) +
  geom_segment(data = ed, aes(x = x0, y = y0, xend = x1, yend = y1), colour = "grey55", linewidth = 0.35,
               arrow = arrow(length = unit(0.9, "mm"), type = "closed")) +
  geom_point(data = nodes, aes(x, y), colour = nodes$col, size = 2.7) +
  annotate("text", x = -0.85, y = c(3, 2, 1, 0), label = c("Conservation", "Landscape", "Use intensity", "Soil–water"),
           hjust = 1, family = FONT, size = 6.1 / .pt, colour = TXT) +
  annotate("text", x = c(2.6, 3.6, 5.2), y = c(0.88, 3.05, 1.5), label = c("Stability", "Acceleration", "Change"),
           hjust = c(0.5, 0.5, 0), family = FONT, size = 6.3 / .pt, colour = TXT) +
  coord_cartesian(xlim = c(-3.9, 7.9), ylim = c(-0.7, 4.5), expand = FALSE) + blank

tg <- read.csv("output/si/targeting_range.csv")
ord_p <- tg %>% group_by(Policy_Config) %>% summarise(m = mean(Triple_gain)) %>% arrange(m) %>% pull(Policy_Config)
ord_s <- tg %>% group_by(Allocation_Strategy) %>% summarise(m = mean(Triple_gain)) %>% arrange(m) %>% pull(Allocation_Strategy)
tg <- tg %>% mutate(r = match(Policy_Config, ord_p), c = match(Allocation_Strategy, ord_s))
p_grid <- ggplot(tg, aes(c, r, fill = Triple_gain)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_gradient(low = "#F1F1F1", high = "#1F7A75", limits = c(15, 70), oob = scales::squish, guide = "none") +
  coord_fixed(expand = FALSE) + blank

# ---- 右：两种排名 --------------------------------------------------------------------
cm <- read.csv("output/csv/ablation_config_metrics.csv") %>% filter(Arm == "Full")
hl <- c(I04 = "#E07A1F", I06 = "#2BA6A0")
sl <- cm %>% transmute(P = Policy_Config, a = Rank_Mean, b = Rank_Synergy,
                       col = ifelse(P %in% names(hl), P, "o"))
p_rank <- ggplot(sl) +
  geom_segment(data = filter(sl, col == "o"), aes(x = 0, xend = 1, y = a, yend = b), colour = "grey78", linewidth = 0.35) +
  geom_segment(data = filter(sl, col != "o"), aes(x = 0, xend = 1, y = a, yend = b, colour = col), linewidth = 1.1) +
  geom_point(data = filter(sl, col != "o"), aes(0, a, colour = col), size = 2) +
  geom_point(data = filter(sl, col != "o"), aes(1, b, colour = col), size = 2) +
  annotate("text", x = c(0, 1), y = -0.6, label = c("National\nmean gain", "Counties gaining\non all dimensions"),
           family = FONT, fontface = "bold", size = 6.6 / .pt, colour = TXT, lineheight = 0.9, vjust = 0) +
  annotate("text", x = -0.06, y = 1, label = "Use-intensity\nfocus  #1", hjust = 1, family = FONT, size = 6.6 / .pt,
           colour = hl["I04"], fontface = "bold", lineheight = 0.9) +
  annotate("text", x = 1.06, y = 8, label = "#8\n41.7%", hjust = 0, family = FONT, size = 6.6 / .pt,
           colour = hl["I04"], fontface = "bold", lineheight = 0.9) +
  annotate("text", x = -0.06, y = 10, label = "Conservation +\nlandscape  #10", hjust = 1, family = FONT, size = 6.6 / .pt,
           colour = hl["I06"], fontface = "bold", lineheight = 0.9) +
  annotate("text", x = 1.06, y = 1, label = "#1\n66.6%", hjust = 0, family = FONT, size = 6.6 / .pt,
           colour = hl["I06"], fontface = "bold", lineheight = 0.9) +
  scale_colour_manual(values = hl, guide = "none") +
  scale_y_reverse() + coord_cartesian(xlim = c(-0.95, 1.45), ylim = c(15.6, -3.2), expand = FALSE) + blank

ratio <- data.frame(g = c("What to invest in", "Where to invest"), v = c(1, 1 / 16))
p_ratio <- ggplot(ratio, aes(v, g)) +
  geom_col(aes(fill = g), width = 0.62) +
  geom_text(aes(x = 0.02, label = g), hjust = 0, family = FONT, size = 6.3 / .pt,
            colour = c("white", TXT), nudge_x = c(0, 0.06)) +
  scale_fill_manual(values = c("What to invest in" = "#1F7A75", "Where to invest" = "#9ED9D5"), guide = "none") +
  scale_y_discrete(limits = rev(ratio$g)) + coord_cartesian(xlim = c(0, 1), expand = FALSE) + blank

# ---- 拼装 ----------------------------------------------------------------------------
g <- cowplot::ggdraw(bg) + cowplot::draw_plot(over)
g <- add(g, p_map, npc(5, 33, 50, 51))
g <- add(g, p_chip, npc(13, 27.6, 34, 4.4))
g <- add(g, txt_plot("1,560 counties · 7 response types", 7.2, "bold"), npc(4, 89, 52, 4))
g <- add(g, txt_plot("12 county-level effects from causal forests", 6.4, col = "grey30"), npc(4, 85, 52, 4))

g <- add(g, txt_plot("Type-specific Bayesian networks", 7.2, "bold"), npc(61, 89, 52, 4))
g <- add(g, txt_plot("Causal graph corrected by the driver analysis", 6.4, col = "grey30"), npc(61, 85, 52, 4))
g <- add(g, p_net, npc(62, 63, 50, 22))
g <- add(g, p_grid, npc(70, 30.5, 34, 24.3))
g <- add(g, txt_plot("945 scenarios: 15 portfolios × 21 targeting\nstrategies × 3 pressure levels", 6.4, col = "grey30"), npc(61, 55.5, 52, 6))

g <- add(g, txt_plot("The best portfolio depends on the criterion", 7.2, "bold"), npc(118, 89, 52, 4))
g <- add(g, txt_plot("Ranks of 15 investment portfolios", 6.4, col = "grey30"), npc(118, 85, 52, 4))
g <- add(g, p_rank, npc(119, 40, 50, 45))
g <- add(g, txt_plot("Effect on national gains (15–24×)", 6.6, "bold", col = "grey30"), npc(121, 34, 46, 4))
g <- add(g, p_ratio, npc(122, 26.5, 44, 7.5))

# ---- 底部：三个政策问题，对应三栏 -----------------------------------------------------
qs <- data.frame(x = c(30, 87, 144), lab = c("Where to invest?", "What to invest in?", "Who benefits?"))
p_q <- ggplot(qs) +
  geom_label(aes(x, 0.5, label = lab), family = FONT, fontface = "bold", size = 9.5 / .pt, colour = "#1F7A75",
             fill = alpha("white", 0.92), linewidth = 0, label.r = unit(3.5, "mm"),
             label.padding = unit(c(1.6, 4.2, 1.6, 4.2), "mm")) +
  coord_cartesian(xlim = c(0, W), ylim = c(0, 1), expand = FALSE) + blank
g <- add(g, p_q, npc(0, 6, W, 10))

save_fig(g, "graphical_abstract", W, H, "output/innov_figures")
