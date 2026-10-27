#!/usr/bin/env Rscript
# =============================================================================
# 20 — Fig. 2：七类响应县的分布与效应剖面
# =============================================================================
# a 竖版全国地图（含南海，不用小插图）：县按响应类型着色
# b 七类 × 12 条路径的平均 τ̂
# c 各农业区内响应类型构成（说明按地理分组 ≠ 按响应分组）
#
# 底图：~/LC_paper_workspace/boundaries（与 Nature-track 同一套，Albers 105E/25N/47N）
# =============================================================================

suppressMessages({ library(sf); library(dplyr); library(tidyr) })
source("lib/fig_style_innov.R")
sf_use_s2(FALSE)

BND <- "boundaries"   # 县、省界与南海九段线（不随仓库分发，见 README）
county <- st_read(file.path(BND, "county_shp.shp"), quiet = TRUE)
prov   <- st_read(file.path(BND, "province_shp2 2.shp"), quiet = TRUE)
crs_aea <- st_crs(county)
st_crs(prov) <- crs_aea
dash   <- st_transform(st_read(file.path(BND, "南海九段线.shp"), quiet = TRUE), crs_aea)
isles  <- st_transform(st_read(file.path(BND, "南海诸岛.shp"), quiet = TRUE), crs_aea)

assign_df <- read.csv("models/cluster_results/cluster_assignments.csv")
county$Type <- factor(paste0("T", assign_df$Cluster[match(county$Code, assign_df$Code)]),
                      levels = names(PAL_TYPE))
stopifnot(!any(is.na(county$Type)))
n_type <- table(county$Type)
type_lab <- setNames(sprintf("%s (n = %d)", type_name(names(n_type)), as.integer(n_type)), names(n_type))

prov_s   <- st_simplify(prov, dTolerance = 3000, preserveTopology = TRUE)
county_s <- st_simplify(county, dTolerance = 1500, preserveTopology = TRUE)
nation   <- st_union(st_buffer(prov_s, 0))

# ---- a 地图 --------------------------------------------------------------------
pa <- ggplot() +
  geom_sf(data = prov_s, fill = "grey93", colour = NA) +
  geom_sf(data = county_s, aes(fill = Type), colour = NA) +
  geom_sf(data = prov_s, fill = NA, colour = "white", linewidth = 0.18) +
  geom_sf(data = nation, fill = NA, colour = "grey35", linewidth = 0.3) +
  geom_sf(data = isles, colour = "grey45", linewidth = 0.15) +
  geom_sf(data = dash, colour = "grey35", linewidth = 0.35) +
  scale_fill_manual(values = PAL_TYPE, labels = type_lab, name = "County response type",
                    guide = guide_legend(ncol = 1)) +
  coord_sf(crs = crs_aea, expand = FALSE, datum = NA,
           xlim = c(-2700000, 2300000), ylim = c(380000, 5980000)) +
  theme_innov() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "inside", legend.position.inside = c(0.02, 0.02),
        legend.justification = c(0, 0), legend.key.size = unit(3.4, "mm"),
        plot.margin = margin(0, 0, 0, 0))

# ---- b 效应剖面 ----------------------------------------------------------------
prof <- read.csv("models/cluster_results/cluster_profiles.csv", check.names = FALSE)
prof_long <- prof %>%
  pivot_longer(-c(Cluster, Size), names_to = "path", values_to = "tau") %>%
  separate(path, c("lever", "outcome"), sep = "_on_") %>%
  mutate(lever = factor(LEVER_EN[lever], levels = names(PAL_LEVER)),
         outcome = factor(outcome, levels = c("CPA", "CPS", "CPI")),
         Type = factor(paste0("T", Cluster), levels = names(PAL_TYPE)))

lim <- max(abs(prof_long$tau))
pb <- ggplot(prof_long, aes(Type, outcome, fill = tau)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", tau), colour = abs(tau) > 0.6 * lim),
            size = FS_NOTE / .pt, family = FONT, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15")) +
  scale_fill_gradient2(low = "#2E6FAE", mid = "white", high = "#C8423E",
                       limits = c(-lim, lim), breaks = c(-1, -0.5, 0, 0.5, 1),
                       name = "Effect",
                       guide = guide_colourbar(barheight = unit(30, "mm"),
                                               barwidth = unit(2.4, "mm"))) +
  facet_grid(lever ~ ., switch = "y") +
  scale_x_discrete(position = "top", expand = c(0, 0), labels = function(x) sub("T", "", x)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL, title = "Effect profile of each county response type (1–7)") +
  theme_innov() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(size = FS_TEXT, face = "bold", angle = 0, hjust = 1),
        panel.spacing.y = unit(0.8, "mm"),
        axis.text.x.top = element_text(colour = PAL_TYPE, face = "bold", size = FS_BASE),
        legend.box.spacing = unit(1.5, "mm"))

# ---- c 农业区构成 --------------------------------------------------------------
zone_en <- c(NCP = "Northeast", HP = "Huang-Huai-Hai", WR = "Northwest arid",
             LP = "Loess Plateau", MLYP = "Middle-lower Yangtze", SR = "Sichuan Basin",
             YGP = "Yunnan-Guizhou", SC = "South China")
comp <- assign_df %>%
  mutate(Type = factor(paste0("T", Cluster), levels = rev(names(PAL_TYPE))),
         Zone = factor(zone_en[Zones], levels = rev(zone_en))) %>%
  count(Zone, Type) %>% group_by(Zone) %>%
  mutate(share = n / sum(n) * 100, N = sum(n)) %>% ungroup()
zone_n <- comp %>% distinct(Zone, N)

pc <- ggplot(comp, aes(share, Zone, fill = Type)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.2) +
  geom_text(data = zone_n, aes(x = 101, y = Zone, label = N), inherit.aes = FALSE,
            hjust = 0, size = FS_NOTE / .pt, family = FONT, colour = "grey30") +
  scale_fill_manual(values = PAL_TYPE, guide = "none") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 25), limits = c(0, 109)) +
  labs(x = "Share of counties (%)", y = NULL,
       title = "County response types within agricultural zones") +
  theme_innov() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# ---- 拼图 ----------------------------------------------------------------------
# 左栏地图 76 × 96 mm（单独渲染时恰好铺满）；右栏上下两块
W <- FULL_W; H <- 98
fig <- compose(list(
  list(plot = pa, x = 0,          y = 0,    w = 76 / W, h = 1,    tag = "A", tx = 0.015),
  list(plot = pb, x = 77 / W,     y = 0.47, w = 1 - 77 / W, h = 0.53, tag = "B", tx = 0.0),
  list(plot = pc, x = 77 / W,     y = 0,    w = 1 - 77 / W, h = 0.47, tag = "C", tx = 0.0)
))
save_fig(fig, "fig2_response_types", width_mm = W, height_mm = H)
