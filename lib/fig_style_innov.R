# =============================================================================
# 投稿图统一风格（The Innovation）
# =============================================================================
#
# 所有主图与 SI 图都 source 本文件，字体、字号、线宽、配色、面板标签、导出
# 只在这里改。参照模板：Wang et al. 2026 The Innovation 7(12):101459
# （Roboto 字体；面板标签为实心圆白字；无网格；面板间距小）。
#
# 尺寸：整版宽 174 mm，单栏 85 mm。字号按最终印刷尺寸设定，图不缩放。
# =============================================================================

suppressMessages({
  library(ggplot2); library(patchwork); library(systemfonts)
  library(ragg); library(svglite); library(grid)
})

# Roboto 需装在系统字体目录（cairo_pdf 嵌入字体靠它）；未装时从 fonts/ 注册，
# 只对 ragg 输出生效
FONT_DIR <- "fonts"   # 脚本均在项目根目录运行
if (!"Roboto" %in% system_fonts()$family) {
  register_font("Roboto",
                plain = file.path(FONT_DIR, "Roboto-Regular.ttf"),
                bold  = file.path(FONT_DIR, "Roboto-Bold.ttf"))
}
FONT <- "Roboto"

FULL_W <- 174
HALF_W <- 85

# 字号（pt）：模板印刷件上轴标签约 7 pt，这里统一放大一档
FS_BASE  <- 8.5   # 轴标题、图例标题
FS_TEXT  <- 8     # 刻度、图例文字
FS_TITLE <- 9     # 面板标题
FS_TAG   <- 9     # 面板标签字母
FS_NOTE  <- 7.5   # 图内注释（geom_text 用 pt / .pt 换算）

LW_AXIS <- 0.4
LW_DATA <- 0.7

COL_TAG <- "#2BA6A0"   # 模板面板标签的青绿色

PAL_OUTCOME <- c(CPI = "#D1495B", CPS = "#2E86AB", CPA = "#3BA55D")
PAL_LEVER <- c("Use intensity" = "#E07A1F", "Conservation" = "#3BA55D",
               "Landscape" = "#2E86AB", "Soil–water" = "#8A5AA8")
PAL_TYPE <- c(T1 = "#E8A33D", T2 = "#4C9BE8", T3 = "#D1495B", T4 = "#2A9D8F",
              T5 = "#8A5AA8", T6 = "#5BB35E", T7 = "#8C6D4F")

CONFIG_LAB <- c(
  I01 = "Equal", I02 = "Conservation focus", I03 = "Landscape focus",
  I04 = "Use-intensity focus", I05 = "Soil–water focus",
  I06 = "Conservation + landscape", I07 = "Conservation + use", I08 = "Conservation + soil–water",
  I09 = "Landscape + use", I10 = "Landscape + soil–water", I11 = "Use + soil–water",
  I12 = "Balanced, no soil–water", I13 = "Balanced, no use", I14 = "Balanced, no landscape",
  I15 = "Balanced, no conservation")

# 正文反复出现的四种代表配置（Fig. 6、7 共用）
PAL_FOCUS <- c(I04 = "#E07A1F", I06 = "#2BA6A0", I02 = "#2E86AB", I12 = "#5BB35E")

PAL_MODEL <- c("National coefficients" = "#B8B8B8", "Zone coefficients" = "#E8A33D",
               "Response-type coefficients" = "#2BA6A0")

LEVER_EN <- c("保护政策" = "Conservation", "土地利用" = "Landscape",
              "利用强度" = "Use intensity", "产权制度" = "Soil–water",
              "城市化" = "Urbanisation", "气候变化" = "Climate")

theme_innov <- function(base = FS_BASE) {
  theme_classic(base_size = base, base_family = FONT) +
    theme(
      axis.line        = element_line(linewidth = LW_AXIS, colour = "black"),
      axis.ticks       = element_line(linewidth = LW_AXIS, colour = "black"),
      axis.ticks.length = unit(1.2, "mm"),
      axis.title       = element_text(size = FS_BASE, colour = "black"),
      axis.text        = element_text(size = FS_TEXT, colour = "black"),
      plot.title       = element_text(size = FS_TITLE, face = "bold", hjust = 0.5,
                                      margin = margin(0, 0, 1.5, 0, "mm")),
      legend.title     = element_text(size = FS_BASE),
      legend.text      = element_text(size = FS_TEXT),
      legend.key.size  = unit(3.2, "mm"),
      legend.background = element_blank(),
      legend.margin    = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      strip.background = element_blank(),
      strip.text       = element_text(size = FS_BASE, face = "bold"),
      panel.grid       = element_blank(),
      plot.margin      = margin(1.5, 1.5, 1.5, 1.5, "mm")
    )
}

# 面板标签：青绿实心圆 + 白色字母，贴在面板左上角
tag_grob <- function(letter, fill = COL_TAG) {
  grobTree(circleGrob(r = unit(2.1, "mm"), gp = gpar(fill = fill, col = NA)),
           textGrob(letter, gp = gpar(col = "white", fontfamily = FONT, fontface = "bold",
                                      fontsize = FS_TAG)))
}
add_tag <- function(p, letter, x = 0.012, y = 0.985) {
  p + inset_element(tag_grob(letter), left = x - 0.02, right = x + 0.02,
                    bottom = y - 0.03, top = y + 0.03, align_to = "full", clip = FALSE,
                    on_top = TRUE)
}

# 固定坐标拼图（cowplot）：面板按 [x, y, w, h]（0–1，左下为原点）摆放，
# 标签画在面板左上角。比 patchwork 自动对齐更好控制留白和地图尺寸。
compose <- function(panels) {
  g <- cowplot::ggdraw()
  for (p in panels) {
    g <- g + cowplot::draw_plot(p$plot, x = p$x, y = p$y, width = p$w, height = p$h)
    if (!is.null(p$tag)) {
      tx <- p$x + (p$tx %||% 0.012); ty <- p$y + p$h - (p$ty %||% 0.02)
      g <- g + cowplot::draw_grob(tag_grob(p$tag), x = tx - 0.02, y = ty - 0.02,
                                  width = 0.04, height = 0.04)
    }
  }
  g
}
`%||%` <- function(a, b) if (is.null(a)) b else a

save_fig <- function(plot, name, width_mm = FULL_W, height_mm = 120,
                     out_dir = "output/innov_figures") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(out_dir, name)
  w <- width_mm / 25.4; h <- height_mm / 25.4
  grDevices::cairo_pdf(paste0(f, ".pdf"), width = w, height = h, family = FONT)
  print(plot); invisible(dev.off())
  ragg::agg_png(paste0(f, ".png"), width = w, height = h, units = "in", res = 300)
  print(plot); invisible(dev.off())
  ragg::agg_tiff(paste0(f, ".tiff"), width = w, height = h, units = "in", res = 600,
                 compression = "lzw")
  print(plot); invisible(dev.off())
  cat(sprintf("saved: %s (%d x %d mm)\n", f, round(width_mm), round(height_mm)))
}

# 类型代号 → 图中显示
type_name <- function(x) sub("^T", "Type ", x)
