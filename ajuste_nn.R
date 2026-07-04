thresholds <- seq(0.10, 0.90, by = 0.05)

resultados_threshold_nn <- map_dfr(thresholds, function(th) {
  
  pred_class <- ifelse(nn_pred_prob >= th, "MAU", "BOM") |>
    factor(levels = c("BOM", "MAU"))
  
  resultado_temp <- tibble(
    status = teste_proc$status,
    .pred_MAU = nn_pred_prob,
    .pred_class = pred_class
  )
  
  metric_set(precision, recall, f_meas, spec, accuracy)(
    resultado_temp,
    truth = status,
    estimate = .pred_class,
    event_level = "second"
  ) |>
    select(.metric, .estimate) |>
    pivot_wider(names_from = .metric, values_from = .estimate) |>
    mutate(threshold = th, .before = 1)
})

resultados_threshold_nn

# Gráfico do trade-off entre métricas
resultados_threshold_nn |>
  pivot_longer(
    cols = c(accuracy, precision, recall, f_meas, spec),
    names_to = "metrica",
    values_to = "valor"
  ) |>
  ggplot(aes(x = threshold, y = valor, color = metrica)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Trade-off entre métricas por threshold - Rede Neural (classe MAU)",
    x = "Threshold",
    y = "Valor da métrica",
    color = "Métrica"
  ) +
  theme_minimal()

# Threshold que maximiza o F1
melhor_threshold_nn <- resultados_threshold_nn |>
  filter(f_meas == max(f_meas, na.rm = TRUE))

melhor_threshold_nn

# Threshold via Youden Index (curva ROC)
library(pROC)

roc_obj_nn <- roc(
  response = teste_proc$status,
  predictor = nn_pred_prob,
  levels = c("BOM", "MAU"),
  direction = "<"
)

coords(roc_obj_nn, "best", best.method = "youden")

### CLASSIFICAÇÃO FINAL COM O THRESHOLD ESCOLHIDO ----
# Ajuste "th_escolhido" para o valor que você decidir usar
# (ex: o do F1 máximo, ou o do Youden Index)

th_escolhido <- melhor_threshold_nn$threshold

nn_pred_class_final <- ifelse(nn_pred_prob >= th_escolhido, "MAU", "BOM") |>
  factor(levels = c("BOM", "MAU"))

conf_mat(
  tibble(status = teste_proc$status, .pred_class = nn_pred_class_final),
  truth = status,
  estimate = .pred_class
)
