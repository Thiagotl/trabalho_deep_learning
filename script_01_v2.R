library(readxl)
library(tidyverse)
library(tidymodels)
library(recipes)
library(ranger)
library(yardstick)
library(janitor)

set.seed(123)

# ============================================================
# 1. LEITURA DOS DADOS
# ============================================================

dados <- read_excel(
  "LIVRARIAS_DORELA.xls",
  sheet = 1
) |>
  clean_names()

glimpse(dados)

# Ver distribuição da variável resposta
table(dados$status)
prop.table(table(dados$status))


# ============================================================
# 2. SELEÇÃO E TRATAMENTO INICIAL DAS VARIÁVEIS
# ============================================================

dados_modelo <- dados |>
  mutate(
    status = factor(status, levels = c("BOM", "MAU"))
  ) |>
  select(
    -cliente,
    -statu_sx,
    -atraso
  )


# ============================================================
# 3. DIVISÃO EM TREINO, VALIDAÇÃO E TESTE
# ============================================================
# Estratégia:
# 60% treino
# 20% validação
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

# Conferir distribuição da resposta em cada conjunto
table(treino$status)
table(validacao$status)
table(teste$status)

prop.table(table(treino$status))
prop.table(table(validacao$status))
prop.table(table(teste$status))


# PRÉ-PROCESSAMENTO

receita <- recipe(status ~ ., data = treino) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

prep_receita <- prep(receita, training = treino)

treino_proc    <- bake(prep_receita, new_data = treino)
validacao_proc <- bake(prep_receita, new_data = validacao)
teste_proc     <- bake(prep_receita, new_data = teste)

glimpse(treino_proc)


# FUNÇÃO PARA CALCULAR MÉTRICAS

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


# TREINAMENTO DO RANDOM FOREST

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


# PREDIÇÃO NA VALIDAÇÃO COM THRESHOLD 0.50

rf_pred_prob_valid <- predict(
  rf_modelo,
  data = validacao_proc
)$predictions[, "MAU"]

rf_pred_class_valid_050 <- ifelse(
  rf_pred_prob_valid >= 0.50,
  "MAU",
  "BOM"
) |>
  factor(levels = c("BOM", "MAU"))

resultado_rf_valid_050 <- tibble(
  status = validacao_proc$status,
  .pred_MAU = rf_pred_prob_valid,
  .pred_class = rf_pred_class_valid_050
)

metricas_rf_valid_050 <- calcular_metricas(resultado_rf_valid_050)

metricas_rf_valid_050

conf_mat(
  resultado_rf_valid_050,
  truth = status,
  estimate = .pred_class
)


# ESCOLHA DO MELHOR THRESHOLD NA VALIDAÇÃO

avaliar_thresholds <- function(probabilidades, verdade, thresholds = seq(0.10, 0.90, by = 0.01)) {
  
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

metricas_thresholds_valid <- avaliar_thresholds(
  probabilidades = rf_pred_prob_valid,
  verdade = validacao_proc$status
)

metricas_thresholds_valid |>
  arrange(desc(f_meas)) |>
  head(10)

melhor_threshold <- metricas_thresholds_valid |>
  arrange(desc(f_meas)) |>
  slice(1) |>
  pull(threshold)

melhor_threshold


# RESULTADO NA VALIDAÇÃO COM O MELHOR THRESHOLD

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

metricas_rf_valid_otimo <- calcular_metricas(resultado_rf_valid_otimo)

metricas_rf_valid_otimo

conf_mat(
  resultado_rf_valid_otimo,
  truth = status,
  estimate = .pred_class
)


# AVALIAÇÃO FINAL NO TESTE

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

metricas_rf_teste <- calcular_metricas(resultado_rf_teste)

metricas_rf_teste

conf_mat(
  resultado_rf_teste,
  truth = status,
  estimate = .pred_class
)


# COMPARAÇÃO ENTRE VALIDAÇÃO E TESTE

metricas_comparacao <- bind_rows(
  metricas_rf_valid_050 |>
    mutate(conjunto = "Validação - threshold 0.50"),
  
  metricas_rf_valid_otimo |>
    mutate(conjunto = paste0("Validação - threshold ", round(melhor_threshold, 2))),
  
  metricas_rf_teste |>
    mutate(conjunto = paste0("Teste - threshold ", round(melhor_threshold, 2)))
) |>
  select(conjunto, .metric, .estimate)

metricas_comparacao






