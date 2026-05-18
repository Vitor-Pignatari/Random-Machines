# ============================= métrica ================================
# Substituir por: yardstick::accuracy()

accuracy <- function(truth, pred) {
  mean(as.character(truth) == as.character(pred))
}
