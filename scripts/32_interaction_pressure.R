# =============================================================================
# 32_interaction_pressure.R — 外部压力结论对交互项是否稳健
#
# 08a 显示：加入 利用强度×气候、利用强度×城市化 后，高压力下 CPA 的方向反转。
# 这里在同一设定（均匀分配，15 组合 × 3 压力）下比较主模型与交互模型的
# 三维共赢比例和"全国稳定性仍为正"的组合数。变化均相对各自模型的现状无投资预测。
# 输出：output/si/interaction_pressure.csv
# =============================================================================

suppressPackageStartupMessages({ library(bnlearn); library(dplyr) })
source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")
source("lib/scenario_functions.R")

basic <- read.csv("data/basic_data.csv")
asg   <- read.csv("models/cluster_results/cluster_assignments.csv")
stopifnot(identical(as.character(basic$Code), as.character(asg$Code)))
cl  <- asg$Cluster; K <- max(cl)
dag <- readRDS("models/corrected_bn/corrected_dag.RDS")
targets <- c("CPI", "CPS", "CPA")

add_int <- function(f) { f$利用x气候 <- f$利用强度 * f$气候变化; f$利用x城市 <- f$利用强度 * f$城市化; f }

# 交互模型（与 08a 相同的构建方式）
f0 <- calculate_ml_guided_factors(basic, verbose = FALSE)
nd <- add_int(data.frame(f0[, c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度")]))
nd$CPI <- basic$CPI; nd$CPS <- basic$CPS; nd$CPA <- basic$CPA
dag_int <- empty.graph(c(nodes(dag), "利用x气候", "利用x城市"))
for (i in seq_len(nrow(arcs(dag)))) dag_int <- set.arc(dag_int, arcs(dag)[i, 1], arcs(dag)[i, 2])
for (fr in c("利用x气候", "利用x城市")) for (to in targets) {
  tmp <- set.arc(dag_int, fr, to); if (acyclic(tmp)) dag_int <- tmp
}
models <- list(
  Main        = list(bns = lapply(1:K, function(k) readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k))),
                     ev = identity),
  Interaction = list(bns = fit_cluster_bns(dag_int, nd, cl), ev = add_int))

ext <- list(E1 = c(0.85, 0.90), E2 = c(1, 1), E3 = c(1.30, 1.10))
urb <- grep("URB", names(basic), value = TRUE); clim <- grep("Clim", names(basic), value = TRUE)
vg  <- driver_var_groups(basic)

run_model <- function(m) {
  pred <- function(d) predict_by_cluster(m$bns, cl, targets, m$ev(calculate_ml_guided_factors(d, verbose = FALSE)))
  ref <- pred(basic)
  bind_rows(lapply(names(ext), function(e) {
    de <- basic; de[, urb] <- de[, urb] * ext[[e]][1]; de[, clim] <- de[, clim] * ext[[e]][2]
    bind_rows(lapply(names(POLICY_CONFIGS), function(p) {
      bm <- 1 + POLICY_CONFIGS[[p]] / 12; dp <- de
      for (d in 1:4) dp[, vg[[d]]] <- de[, vg[[d]]] * bm[d]
      pr <- pred(dp)
      dC <- pr$CPI - ref$CPI; dS <- pr$CPS - ref$CPS; dA <- pr$CPA - ref$CPA
      data.frame(External = e, Policy = p, CPI = mean(dC), CPS = mean(dS), CPA = mean(dA),
                 Triple_gain = mean(dC > 0 & dS > 0 & dA > 0) * 100)
    }))
  }))
}
out <- bind_rows(lapply(names(models), function(n) mutate(run_model(models[[n]]), Model = n)))
write.csv(out, "output/si/interaction_pressure.csv", row.names = FALSE)

w <- tidyr::pivot_wider(out, id_cols = c(External, Policy), names_from = Model,
                        values_from = c(Triple_gain, CPS, CPA))
for (e in names(ext)) {
  x <- filter(w, External == e)
  cat(sprintf("%s: triple-gain rho = %.2f; top3 main = %s; top3 int = %s; CPS>=0 main %d int %d; mean CPA main %.3f int %.3f\n",
              e, cor(x$Triple_gain_Main, x$Triple_gain_Interaction, method = "spearman"),
              paste(x$Policy[order(-x$Triple_gain_Main)][1:3], collapse = ","),
              paste(x$Policy[order(-x$Triple_gain_Interaction)][1:3], collapse = ","),
              sum(x$CPS_Main >= 0), sum(x$CPS_Interaction >= 0),
              mean(x$CPA_Main), mean(x$CPA_Interaction)))
}
print(as.data.frame(filter(w, Policy %in% c("I02", "I04", "I06", "I12")) %>%
  select(External, Policy, Triple_gain_Main, Triple_gain_Interaction)), digits = 3)
