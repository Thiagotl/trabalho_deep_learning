set.seed(123)

thresholds <- seq(0.10, 0.90, by = 0.05)

resultados_threshold <- map_dfr(thresholds, function(th) {
  
  pred_class <- ifelse(rf_pred_prob >= th, "MAU", "BOM") |>
    factor(levels = c("BOM", "MAU"))
  
  resultado_temp <- tibble(
    status = teste_proc$status,
    .pred_MAU = rf_pred_prob,
    .pred_class = pred_class
  )
  
  metricas <- metric_set(precision, recall, f_meas, spec, accuracy)(
    resultado_temp,
    truth = status,
    estimate = .pred_class,
    event_level = "second"
  )
  
  metricas |>
    select(.metric, .estimate) |>
    pivot_wider(names_from = .metric, values_from = .estimate) |>
    mutate(threshold = th, .before = 1)
})

resultados_threshold
resultados_threshold |>
  pivot_longer(
    cols = c(accuracy, precision, recall, f_meas, spec),
    names_to = "metrica",
    values_to = "valor"
  ) |>
  ggplot(aes(x = threshold, y = valor, color = metrica)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Trade-off entre métricas por threshold (classe MAU)",
    x = "Threshold",
    y = "Valor da métrica",
    color = "Métrica"
  ) +
  theme_minimal()

melhor_threshold <- resultados_threshold |>
  filter(f_meas == max(f_meas, na.rm = TRUE))

melhor_threshold


library(pROC)

roc_obj <- roc(
  response = teste_proc$status,
  predictor = rf_pred_prob,
  levels = c("BOM", "MAU"),
  direction = "<"
)

coords(roc_obj, "best", best.method = "youden")


# Cenário 1: equilíbrio F1 (threshold = 0.40)
rf_pred_class_f1 <- ifelse(rf_pred_prob >= 0.40, "MAU", "BOM") |>
  factor(levels = c("BOM", "MAU"))

# Cenário 2: priorizando recall via Youden (threshold ≈ 0.226)
rf_pred_class_youden <- ifelse(rf_pred_prob >= 0.226, "MAU", "BOM") |>
  factor(levels = c("BOM", "MAU"))

table(rf_pred_class_f1)
table(rf_pred_class_youden)



cm_f1 <- conf_mat(
  tibble(status = teste_proc$status, .pred_class = rf_pred_class_f1),
  truth = status,
  estimate = .pred_class
)

cm_youden <- conf_mat(
  tibble(status = teste_proc$status, .pred_class = rf_pred_class_youden),
  truth = status,
  estimate = .pred_class
)

cm_f1
cm_youden
