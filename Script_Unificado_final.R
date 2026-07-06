# ======================================================================
# Carregando pacotes
# ======================================================================
library(readxl)
library(tidyverse)
library(tidymodels)
library(recipes)
library(ranger)
library(yardstick)
library(keras3)
library(janitor)
library(patchwork)

# ======================================================================
# Definindo funções auxiliares
# ======================================================================

# Função para calcular métricas de classificação
calcular_metricas <- function(resultado) {
  metricas_classe <- metric_set(
    accuracy,
    precision,
    recall,
    f_meas,
    sens,
    spec
  )

  bind_rows(
    metricas_classe(
      resultado,
      truth = status,
      estimate = .pred_class,
      event_level = "second"
    ),
    roc_auc(
      resultado,
      truth = status,
      .pred_MAU,
      event_level = "second"
    )
  )
}

# Plot da matriz de confusão
plot_conf_mat <- function(cm) {
  matriz <- as.data.frame(cm$table)

  ggplot(
    matriz,
    aes(
      x = Truth,
      y = Prediction,
      fill = Freq
    )
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6) +
    scale_fill_gradient(
      low = "white",
      high = "#1F77B4"
    ) +
    labs(
      x = "Classe real",
      y = "Classe prevista"
    ) +
    coord_equal() +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(
        colour = "black",
        fill = NA
      )
    )
}

# ======================================================================
# Fixando uma seed e imporando os dados
# ======================================================================

set.seed(123)

dados <- read_excel(
  "LIVRARIAS_DORELA.xls",
  sheet = 1
) |>
  clean_names()

# ======================================================================
# Limpeza dos dados e análise exploratória
# ======================================================================

# Removendo variáveis desnecessárias
dados_modelo <- dados |>
  mutate(
    status = factor(status, levels = c("BOM", "MAU"))
  ) |>
  select(
    -cliente,
    -statu_sx,
    -atraso
  )

glimpse(dados_modelo)

# Verificando desbalanceamento da variável resposta
table(dados_modelo$status)
prop.table(table(dados_modelo$status))

# ======================================================================
# Divisão dos dados para modelagem
# ======================================================================

# Estratégia:
# 60% treino
# 20% validação (25% do treino)
# 20% teste

set.seed(123)

divisao_inicial <- initial_split(
  dados_modelo,
  prop = 0.80,
  strata = status
)

treino_validacao <- training(divisao_inicial)
teste <- testing(divisao_inicial)

divisao_validacao <- initial_split(
  treino_validacao,
  prop = 0.75,
  strata = status
)

treino <- training(divisao_validacao)
validacao <- testing(divisao_validacao)

# Conferindo a distribuição da classe da variável resposta em cada conjunto
table(treino$status)
table(validacao$status)
table(teste$status)

prop.table(table(treino$status))
prop.table(table(validacao$status))
prop.table(table(teste$status))

# ======================================================================
# Pré-processamento dos dados (Modelo Random Forest)
# ======================================================================

receita <- recipe(status ~ ., data = treino) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

prep_receita <- prep(receita, training = treino)

treino_proc <- bake(prep_receita, new_data = treino)
validacao_proc <- bake(prep_receita, new_data = validacao)
teste_proc <- bake(prep_receita, new_data = teste)

# glimpse(treino_proc)

# ======================================================================
# Treinamento do Random Forest
# ======================================================================

p <- ncol(treino_proc) - 1

rf_modelo <- ranger(
  status ~ .,
  data = treino_proc,
  probability = TRUE,
  num.trees = 500,
  mtry = floor(sqrt(p)),
  min.node.size = 10,
  importance = "impurity",
  seed = 123
)

# ======================================================================
# Predição no conjunto de validação com threshold 0.50
# ======================================================================

# Calculando as probabilidades preditas para a classe "MAU" no conjunto de validação
rf_pred_prob_valid <- predict(
  rf_modelo,
  data = validacao_proc
)$predictions[, "MAU"]

# Definindo a classe predita com threshold 0.50
rf_pred_class_valid_050 <- ifelse(
  rf_pred_prob_valid >= 0.50,
  "MAU",
  "BOM"
) |>
  factor(levels = c("BOM", "MAU"))

# Agrupando resultados em uma tabela
resultado_rf_valid_050 <- tibble(
  status = validacao_proc$status,
  .pred_MAU = rf_pred_prob_valid,
  .pred_class = rf_pred_class_valid_050
)

# Calculando métricas de validação com threshold 0.50
(metricas_rf_valid_050 <- calcular_metricas(resultado_rf_valid_050))

# Montado a matriz de confusão para validação com threshold 0.50
conf_mat(
  resultado_rf_valid_050,
  truth = status,
  estimate = .pred_class
)
# ======================================================================
# Escolha do melhor threshold na validação
# ======================================================================

# Função para avaliar diferentes thresholds e calcular métricas
avaliar_thresholds <- function(
  probabilidades,
  verdade,
  thresholds = seq(0.10, 0.90, by = 0.01)
) {
  map_dfr(thresholds, function(th) {
    pred_class <- ifelse(
      probabilidades >= th,
      "MAU",
      "BOM"
    ) |>
      factor(levels = c("BOM", "MAU"))

    resultado_temp <- tibble(
      status = verdade,
      .pred_MAU = probabilidades,
      .pred_class = pred_class
    )

    metricas_temp <- calcular_metricas(resultado_temp) |>
      select(.metric, .estimate) |>
      pivot_wider(
        names_from = .metric,
        values_from = .estimate
      )

    metricas_temp |>
      mutate(threshold = th) |>
      relocate(threshold)
  })
}

# Avaliando thresholds no conjunto de validação
metricas_thresholds_valid <- avaliar_thresholds(
  probabilidades = rf_pred_prob_valid,
  verdade = validacao_proc$status
)

# Verificando os melhores thresholds
metricas_thresholds_valid |>
  arrange(desc(f_meas)) |>
  head(10)

(melhor_threshold <- metricas_thresholds_valid |>
  arrange(desc(f_meas)) |>
  slice(1) |>
  pull(threshold))

# Plotando as métricas em função do threshold
grafico_threshold <-
  metricas_thresholds_valid |>
  select(
    threshold,
    precision,
    recall,
    f_meas
  ) |>
  pivot_longer(
    -threshold,
    names_to = "Metrica",
    values_to = "Valor"
  )

ggplot(
  grafico_threshold,
  aes(threshold, Valor, color = Metrica)
) +
  geom_line(size = 1) +
  geom_vline(
    xintercept = melhor_threshold,
    linetype = 2
  ) +
  labs(
    x = "Threshold",
    y = "Valor da métrica"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

# Salvando o plot
# ggsave(
#   filename = "grafico_threshold.pdf",
#   width = 5,
#   height = 3,
#   dpi = 300
# )

# ======================================================================
# Avaliação do modelo no conjunto de validação com o melhor threshold
# ======================================================================

rf_pred_class_valid_otimo <- ifelse(
  rf_pred_prob_valid >= melhor_threshold,
  "MAU",
  "BOM"
) |>
  factor(levels = c("BOM", "MAU"))

resultado_rf_valid_otimo <- tibble(
  status = validacao_proc$status,
  .pred_MAU = rf_pred_prob_valid,
  .pred_class = rf_pred_class_valid_otimo
)

# Calculando métricas de validação com o melhor threshold
(metricas_rf_valid_otimo <- calcular_metricas(resultado_rf_valid_otimo))

# Matriz de confusão para validação com o melhor threshold
cm_rf_valid_otimo <- conf_mat(
  resultado_rf_valid_otimo,
  truth = status,
  estimate = .pred_class
)

# Plotando a matriz de confusão para o conjunto de validação com o melhor threshold
plot_conf_mat(cm_rf_valid_otimo)

# Salvando o plot
# ggsave(
#   filename = "mc_rf_valid.pdf",
#   width = 5,
#   height = 3,
#   dpi = 300
# )

# ======================================================================
# Avaliação do modelo no conjunto de teste com o melhor threshold
# ======================================================================

rf_pred_prob_teste <- predict(
  rf_modelo,
  data = teste_proc
)$predictions[, "MAU"]

rf_pred_class_teste <- ifelse(
  rf_pred_prob_teste >= melhor_threshold,
  "MAU",
  "BOM"
) |>
  factor(levels = c("BOM", "MAU"))

resultado_rf_teste <- tibble(
  status = teste_proc$status,
  .pred_MAU = rf_pred_prob_teste,
  .pred_class = rf_pred_class_teste
)

# Calculando métricas de teste com o melhor threshold
(metricas_rf_teste <- calcular_metricas(resultado_rf_teste))

# Matriz de confusão para teste com o melhor threshold
cm_rf_teste <- conf_mat(
  resultado_rf_teste,
  truth = status,
  estimate = .pred_class
)

# Plotando a matriz de confusão para o conjunto de teste
plot_conf_mat(cm_rf_teste)

# Salvando o plot
# ggsave(
#   filename = "mc_rf_teste.pdf",
#   width = 5,
#   height = 3,
#   dpi = 300
# )

# ======================================================================
# Comparação das métricas de validação e teste
# ======================================================================

# Calculando métricas para ambos os conjuntos (validação e teste)
metricas_comparacao <- bind_rows(
  metricas_rf_valid_050 |>
    mutate(conjunto = "Validação - threshold 0.50"),

  metricas_rf_valid_otimo |>
    mutate(
      conjunto = paste0("Validação - threshold ", round(melhor_threshold, 2))
    ),

  metricas_rf_teste |>
    mutate(conjunto = paste0("Teste - threshold ", round(melhor_threshold, 2)))
) |>
  select(conjunto, .metric, .estimate)

# Matriz de confusão para o conjunto de teste
(matriz_rf_teste <- conf_mat(
  resultado_rf_teste,
  truth = status,
  estimate = .pred_class
))

# Formatando a tabela de métricas para melhor visualização
(metricas_comparacao_formatada <- metricas_comparacao |>
  mutate(
    .estimate = round(.estimate, 4)
  ) |>
  pivot_wider(
    names_from = .metric,
    values_from = .estimate
  ) |>
  rename(
    Conjunto = conjunto,
    Acuracia = accuracy,
    Precisao_MAU = precision,
    Recall_MAU = recall,
    F1_MAU = f_meas,
    Sensibilidade = sens,
    Especificidade = spec,
    AUC = roc_auc
  ))

# ======================================================================
# Treinamento do modelo de Rede Neural MLP
# ======================================================================

# Reaproveita os conjuntos de treino, validação e testes já criados
x_train <- treino_proc |> select(-status) |> as.matrix()
y_train <- ifelse(treino_proc$status == "MAU", 1, 0)

x_valid <- validacao_proc |> select(-status) |> as.matrix()
y_valid <- ifelse(validacao_proc$status == "MAU", 1, 0)

x_test <- teste_proc |> select(-status) |> as.matrix()
y_test <- ifelse(teste_proc$status == "MAU", 1, 0)

# ======================================================================
# Definindo a arquitetura da Rede Neural MLP
# ======================================================================

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

# ======================================================================
# Treinamento da Rede Neural MLP
# ======================================================================

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

# Plotando o histórico de treinamento da Rede Neural MLP
historico <- as.data.frame(historico_nn$metrics)
historico$epoch <- seq_len(nrow(historico))

ultima_epoca <- max(historico$epoch)

tema_artigo <-
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

p_loss <-
  ggplot(historico, aes(epoch)) +
  geom_line(aes(y = loss, colour = "Treinamento"), linewidth = 0.8) +
  geom_line(aes(y = val_loss, colour = "Validação"), linewidth = 0.8) +
  geom_vline(
    xintercept = ultima_epoca,
    linetype = "dashed"
  ) +
  labs(
    x = "Época",
    y = "Loss"
  ) +
  tema_artigo

p_acc <-
  ggplot(historico, aes(epoch)) +
  geom_line(aes(y = accuracy, colour = "Treinamento"), linewidth = 0.8) +
  geom_line(aes(y = val_accuracy, colour = "Validação"), linewidth = 0.8) +
  geom_vline(
    xintercept = ultima_epoca,
    linetype = "dashed"
  ) +
  labs(
    x = "Época",
    y = "Acurácia"
  ) +
  tema_artigo

p_auc <-
  ggplot(historico, aes(epoch)) +
  geom_line(aes(y = auc, colour = "Treinamento"), linewidth = 0.8) +
  geom_line(aes(y = val_auc, colour = "Validação"), linewidth = 0.8) +
  geom_vline(
    xintercept = ultima_epoca,
    linetype = "dashed"
  ) +
  labs(
    x = "Época",
    y = "ROC-AUC"
  ) +
  tema_artigo

(grafico_historico <-
  (p_loss | p_acc | p_auc) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  ))

# Salvando plot
# ggsave(
#   filename = "historico_nn.pdf",
#   plot = grafico_historico,
#   width = 11,
#   height = 4,
#   dpi = 300
# )

nn_pred_prob_valid <- modelo_nn |> predict(x_valid) |> as.numeric()

# Avaliando diferentes thresholds para a Rede Neural MLP
metricas_thresholds_valid_nn <- avaliar_thresholds(
  probabilidades = nn_pred_prob_valid,
  verdade = validacao_proc$status
)

# Verificando os melhores thresholds para a Rede Neural MLP
metricas_thresholds_valid_nn |>
  arrange(desc(f_meas)) |>
  head(10)

# Definindo o melhor threshold para a Rede Neural MLP
(melhor_threshold_nn <- metricas_thresholds_valid_nn |>
  arrange(desc(f_meas)) |>
  slice(1) |>
  pull(threshold))

# ======================================================================
# Predição no conjunto de teste com o melhor threshold da Rede Neural MLP
# ======================================================================

# Calculando as probabilidades preditas para a classe "MAU" no conjunto de teste
nn_pred_prob_teste <- modelo_nn |> predict(x_test) |> as.numeric()

# Definindo a classe predita com o melhor threshold da Rede Neural MLP
nn_pred_class_teste <- ifelse(
  nn_pred_prob_teste >= melhor_threshold_nn,
  "MAU",
  "BOM"
) |>
  factor(levels = c("BOM", "MAU"))

# Agrupando resultados em uma tabela para a Rede Neural MLP
resultado_nn_teste <- tibble(
  status = teste_proc$status,
  .pred_MAU = nn_pred_prob_teste,
  .pred_class = nn_pred_class_teste
)

# Calculando métricas de teste para a Rede Neural MLP
(metricas_nn_teste <- calcular_metricas(resultado_nn_teste) |>
  mutate(threshold = melhor_threshold_nn))

# Matriz de confusão para o conjunto de teste da Rede Neural MLP
cm_mlp_teste <- conf_mat(
  resultado_nn_teste,
  truth = status,
  estimate = .pred_class
)

# Plotando a matriz de confusão para o conjunto de teste
plot_conf_mat(cm_mlp_teste)

# Salvando o plot
# ggsave(
#   filename = "mc_mlp_teste.pdf",
#   width = 5,
#   height = 3,
#   dpi = 300
# )

# Metricas finais para comparação entre os modelos Random Forest e Rede Neural MLP
mt_final <- metricas_comparacao |>
  filter(str_detect(conjunto, "^Teste")) |>
  mutate(threshold = melhor_threshold)

# ======================================================================
# Comparação das métricas entre os modelos Random Forest e Rede Neural MLP
# ======================================================================

# Comparando as métricas de teste entre os modelos Random Forest e Rede Neural MLP
(comparacao <- bind_rows(
  mt_final |>
    mutate(modelo = "Random Forest") |>
    select(modelo, .metric, .estimate, threshold),
  metricas_nn_teste |>
    mutate(modelo = "Rede Neural MLP") |>
    select(modelo, .metric, .estimate, threshold)
))

# Plotando a comparação das métricas entre os modelos Random Forest e Rede Neural MLP
comparacao |>
  ggplot(aes(x = .metric, y = .estimate, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::percent(.estimate, accuracy = 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("Random Forest" = "#2C3E50", "Rede Neural MLP" = "#E67E22")
  ) +
  labs(x = "Métrica", y = "Valor", fill = "Modelo") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(face = "bold")
  )

# Salvando o plot
ggsave(
  filename = "comparacao.pdf",
  width = 8,
  height = 5,
  dpi = 300
)

# ======================================================================
# Escolha do modelo final
# ======================================================================

# Metricas de decisão para escolher o modelo final
metrica_decisao <- "f_meas"

# Resumo final das métricas para escolher o modelo final
(resumo_final <- comparacao |>
  filter(.metric == metrica_decisao) |>
  arrange(desc(.estimate), threshold))
