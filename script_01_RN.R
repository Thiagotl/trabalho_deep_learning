##############################################
### MODELO 2: REDE NEURAL DEEP LEARNING ----
##############################################

# Reaproveita os conjuntos 60/20/20 já criados (treino_proc, validacao_proc, teste_proc)
x_train <- treino_proc    |> select(-status) |> as.matrix()
y_train <- ifelse(treino_proc$status    == "MAU", 1, 0)

x_valid <- validacao_proc |> select(-status) |> as.matrix()
y_valid <- ifelse(validacao_proc$status == "MAU", 1, 0)

x_test  <- teste_proc     |> select(-status) |> as.matrix()
y_test  <- ifelse(teste_proc$status     == "MAU", 1, 0)

### ARQUITETURA ----
modelo_nn <- keras_model_sequential(input_shape = ncol(x_train)) |>
  layer_dense(units = 32, activation = "relu") |>
  layer_dropout(rate = 0.30) |>
  layer_dense(units = 16, activation = "relu") |>
  layer_dropout(rate = 0.20) |>
  layer_dense(units = 1, activation = "sigmoid")

summary(modelo_nn)

modelo_nn |>
  compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "binary_crossentropy",
    metrics = c("accuracy", metric_auc(name = "auc"))
  )

### TREINAMENTO  ----
historico_nn <- modelo_nn |>
  fit(
    x = x_train,
    y = y_train,
    validation_data = list(x_valid, y_valid),
    epochs = 100,
    batch_size = 32,
    callbacks = list(
      callback_early_stopping(
        monitor = "val_loss",
        patience = 10,
        restore_best_weights = TRUE
      )
    ),
    verbose = 1
  )

plot(historico_nn)

nn_pred_prob_valid <- modelo_nn |> predict(x_valid) |> as.numeric()

metricas_thresholds_valid_nn <- avaliar_thresholds(
  probabilidades = nn_pred_prob_valid,
  verdade = validacao_proc$status
)

metricas_thresholds_valid_nn |>
  arrange(desc(f_meas)) |>
  head(10)

melhor_threshold_nn <- metricas_thresholds_valid_nn |>
  arrange(desc(f_meas)) |>
  slice(1) |>
  pull(threshold)

melhor_threshold_nn

### PREDIÇÃO NO TESTE COM O THRESHOLD ESCOLHIDO ----
nn_pred_prob_teste <- modelo_nn |> predict(x_test) |> as.numeric()

nn_pred_class_teste <- ifelse(
  nn_pred_prob_teste >= melhor_threshold_nn,
  "MAU", "BOM"
) |>
  factor(levels = c("BOM", "MAU"))

resultado_nn_teste <- tibble(
  status      = teste_proc$status,
  .pred_MAU   = nn_pred_prob_teste,
  .pred_class = nn_pred_class_teste
)

metricas_nn_teste <- calcular_metricas(resultado_nn_teste) |>
  mutate(threshold = melhor_threshold_nn)

metricas_nn_teste

conf_mat(resultado_nn_teste, truth = status, estimate = .pred_class)



mt_final <- metricas_comparacao |>
  filter(str_detect(conjunto, "^Teste")) |>
  mutate(threshold = melhor_threshold)   



############################################
##### COMPARACAO ----
############################################
comparacao <- bind_rows(
  mt_final       |> mutate(modelo = "Random Forest") |> select(modelo, .metric, .estimate, threshold),
  metricas_nn_teste |> mutate(modelo = "Rede Neural MLP") |> select(modelo, .metric, .estimate, threshold)
)

comparacao

comparacao |>
  ggplot(aes(x = .metric, y = .estimate, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = scales::percent(.estimate, accuracy = 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Random Forest" = "#2C3E50", "Rede Neural MLP" = "#E67E22")) +
  labs(x = "Métrica", y = "Valor", fill = "Modelo") +
  theme_minimal()

### ESCOLHA DO MODELO FINAL ----
metrica_decisao <- "f_meas"  

resumo_final <- comparacao |>
  filter(.metric == metrica_decisao) |>
  arrange(desc(.estimate), threshold)   

resumo_final
modelo_final <- resumo_final |> slice(1) |> pull(modelo)
modelo_final
