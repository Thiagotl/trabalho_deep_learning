library(readxl)
library(tidyverse)
library(tidymodels)
library(recipes)
library(ranger)
library(yardstick)
library(keras3)
library(janitor)

set.seed(123)
dados <- read_excel(
  "LIVRARIAS_DORELA.xls",
  sheet = 1
) |>
  clean_names()

glimpse(dados)

# Ver distribuição da variável resposta
table(dados$status)
prop.table(table(dados$status))


dados_modelo <- dados |>
  mutate(
    status = factor(status, levels = c("BOM", "MAU"))
  ) |>
  select(
    -cliente,
    -statu_sx,
    -atraso
  )


divisao <- initial_split(
  dados_modelo,
  prop = 0.80,
  strata = status
)

treino <- training(divisao)
teste  <- testing(divisao)

table(treino$status)
table(teste$status)

# PRE PROCESSAMENTO

receita <- recipe(status ~ ., data = treino) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

prep_receita <- prep(receita, training = treino)

treino_proc <- bake(prep_receita, new_data = treino)
teste_proc  <- bake(prep_receita, new_data = teste)

glimpse(treino_proc)

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



#### TREINAMENTO DO RANDOM FOREST ----


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

rf_modelo




# Principais hiperparâmetros:
# num.trees = número de árvores
# mtry = número de variáveis sorteadas em cada divisão
# min.node.size = tamanho mínimo dos nós finais


### PREDICAO DO RANDOM FOREST ----

rf_pred_prob <- predict(rf_modelo, data = teste_proc)$predictions[, "MAU"]

rf_pred_class <- ifelse(rf_pred_prob >= 0.50, "MAU", "BOM") |>
  factor(levels = c("BOM", "MAU"))

resultado_rf <- tibble(
  status = teste_proc$status,
  .pred_MAU = rf_pred_prob,
  .pred_class = rf_pred_class
)

metricas_rf <- calcular_metricas(resultado_rf)

metricas_rf

conf_mat(
  resultado_rf,
  truth = status,
  estimate = .pred_class
)

rf_modelo$variable.importance |>
  sort(decreasing = TRUE) |>
  head(10)

#############################################
### MODELO 2: REDE NEURAL DEEP LEARNING ----
##############################################


divisao_validacao <- initial_split(
  treino_proc,
  prop = 0.80,
  strata = status
)
treino_nn <- training(divisao_validacao)
valid_nn  <- testing(divisao_validacao)

x_train <- treino_nn |> select(-status) |> as.matrix()
y_train <- ifelse(treino_nn$status == "MAU", 1, 0)

x_valid <- valid_nn |> select(-status) |> as.matrix()
y_valid <- ifelse(valid_nn$status == "MAU", 1, 0)

x_test <- teste_proc |> select(-status) |> as.matrix()
y_test <- ifelse(teste_proc$status == "MAU", 1, 0)

### ARQUITETURA DA REDE NEURAL ----
# Camada de entrada: número de variáveis após o pré-processamento
# 1ª camada oculta: 32 neurônios, ativação ReLU
# Dropout: 30%
# 2ª camada oculta: 16 neurônios, ativação ReLU
# Dropout: 20%
# Camada de saída: 1 neurônio, ativação sigmoid

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

### TREINAMENTO DA REDE NEURAL ----
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

### PREDIÇÃO NO TESTE ----
nn_pred_prob <- modelo_nn |>
  predict(x_test) |>
  as.numeric()

nn_pred_class <- ifelse(nn_pred_prob >= 0.50, "MAU", "BOM") |>
  factor(levels = c("BOM", "MAU"))

resultado_nn <- tibble(
  status = teste_proc$status,
  .pred_MAU = nn_pred_prob,
  .pred_class = nn_pred_class
)

### MÉTRICAS (threshold padrão 0.50) ----
metricas_nn <- calcular_metricas(resultado_nn)
metricas_nn

conf_mat(
  resultado_nn,
  truth = status,
  estimate = .pred_class
)



############################################
##### COMPARACAO ----
############################################


comparacao <- bind_rows(
  mt_final |> mutate(modelo = "Random Forest"),
  metricas_nn |> mutate(modelo = "Rede Neural MLP")
) |>
  select(modelo, .metric, .estimate)

comparacao


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
  labs(
    #title = "Comparação entre Random Forest e Rede Neural",
    x = "Métrica",
    y = "Valor",
    fill = "Modelo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
