#### 🌍 ─────────────────────────────────────────────── 🌍
#### 🌍 THERESA ROCCO PEREIRA BARBOSA
#### 🌍 Brasileira \| Geocientista \| Dados
#### 🌍 <imakemapas@outlook.com.br> \| +55 24 998417085
#### 🌍 2025-06-22
#### 🌍 ─────────────────────────────────────────────── 🌍

# 1. Setup ---------------------------------------------------------------------
rm(list = ls(all.names = TRUE))
library(stringr)       # Para manipulação de strings
library(terra)         # Para manipulação de dados raster e vetoriais
library(dplyr)         # Para manipulação de dados
library(caret)         # Treinamento de modelos (train, trainControl, predict, etc.
library(tibble)        # Criação e manipulação leve de data.frame com tibble()
library(randomForest)  # Algoritmos usados no train(method = "...")
library(nnet)          # Algoritmos usados no train(method = "...")
library(pROC)          # Cálculo do ROC-AUC multiclasses
library(doParallel)    # Paralelização com caret
library(tictoc)        # Para medir tempo de execução

# Função auxiliar para padronizar strings
pad3 <- function(s) {
  stringr::str_pad(s, 3, side = 'left', pad = '0')
}

# Função auxiliar para garantir que todas as classes estivessem representadas tanto no conjunto
# de treino quanto no de teste, pois sem isso alguns modelos falhavam na etapa de validação.
create_partition_stratified <- function(y, p = 0.7, seed = NULL, max_tries = 100, min_train = 2, min_test = 1) {
  for (attempt in 1:max_tries) {
    if (!is.null(seed)) set.seed(seed + attempt)
    
    unique_classes <- unique(y)
    treino_idx <- c()
    teste_idx  <- c()
    
    for (cls in unique_classes) {
      idx <- which(y == cls)
      if (length(idx) < (min_train + min_test)) next  # classe muito pequena, pula
      
      n_treino <- max(min_train, floor(length(idx) * p))
      n_teste  <- length(idx) - n_treino
      if (n_teste < min_test) next  # se não sobra pra teste, pula
      
      idx_treino <- sample(idx, n_treino)
      idx_teste  <- setdiff(idx, idx_treino)
      
      treino_idx <- c(treino_idx, idx_treino)
      teste_idx  <- c(teste_idx, idx_teste)
    }
    
    treino_labels <- y[treino_idx]
    if (all(table(treino_labels) >= min_train)) {
      return(list(train = sort(treino_idx), test = sort(teste_idx)))
    }
  }
  stop("Não foi possível criar partição válida após ", max_tries, " tentativas.")
}

# 2. Dados ---------------------------------------------------------------------
load('data/dados_limpos.RData')
dffinal$Subclasses         <- as.factor(dffinal$Subclasses)
dffinal$geol               <- as.factor(dffinal$geol)
levels(dffinal$Subclasses) <- make.names(levels(dffinal$Subclasses))

# 3 Separa dataframe com variáveis alvo (y) e co-variáveis (x) -----------------
dfy <- dffinal |> dplyr::select(Subclasses)
dfx <- dffinal |> dplyr::select(-Subclasses, -x, -y, -id) 

# 4. Modelos e grids -----------------------------------------------------------
modelos_train <- c('rf',      'nnet')
funcs         <- c('rfFuncs', 'caretFuncs')
grids <- list(
  rf = expand.grid(
    mtry = c(6, 8, 10, 12, 14)
  ),
  nnet = expand.grid(
    size  = c(7, 9, 11, 13, 15),
    decay = c(0.01, 0.05, 0.1)
  )
)

# 5. Resultados ---------------------------------------------------------------- 
set.seed(666)
nrep           <- 100
vseed          <- sample(1:20000, nrep)
nl             <- nrep * length(modelos_train)
fatores_treino <- names(dfx)[sapply(dfx, is.factor)]
dfresult       <- tibble(
  target               = character(nl),
  repeticao            = integer(nl)  ,
  modelo               = character(nl),
  train_model_accuracy = numeric(nl),
  train_model_kappa    = numeric(nl),
  model_accuracy       = numeric(nl),
  model_kappa          = numeric(nl),
  mean_auc             = numeric(nl),
  null_model_accuracy  = numeric(nl)
)

# 6. Raster stack limpo --------------------------------------------------------
r          <- terra::rast(list.files("raster_stack", pattern = "\\.tif$", full.names = TRUE))
dft        <- as.data.frame(r, xy = TRUE) |> na.omit()
names(dft) <- make.names(names(dft))

for (col in fatores_treino) {
  if (col %in% names(dft)) {
    dft[[col]] <- factor(dft[[col]], levels = levels(dfx[[col]]))
  }
}
crs(r)

# 7. Paralelismo ---------------------------------------------------------------
nc <- detectCores()
print(paste(nc, 'núcleos de cpu disponíveis'))
cl <- makePSOCKcluster(min(15, nc))
doParallel::registerDoParallel(cl)

# 8. Loop de treino e predição ------------------------------------------------
#j=k=1
cont <- 1
for (j in seq_along(modelos_train)) {
  for (k in seq_len(nrep)) {
    
    tictoc::tic(paste0("Modelo: ", modelos_train[j], " | Repetição: ", pad3(k)))
    print(paste0('inicio ', modelos_train[j], "-" ,pad3(k)," | ", Sys.time()))
    
    target <- names(dfy)
    
    fp <- file.path(getwd(), 'models')
    if (!dir.exists(fp)) {
      dir.create(fp, recursive = TRUE)
    }
    
    fm = file.path(getwd(), 'maps')
    if (!dir.exists(fm)) {
      dir.create(fm, recursive = TRUE)
    }
    
    set.seed(vseed[k])
    dfxy           <- data.frame(dfy, dfx)
    names(dfxy)[1] <- target
    dfxy           <- dfxy |>
      dplyr::mutate(
        dplyr::across(where(is.numeric), ~ scale(., center = TRUE, scale = TRUE)[, 1])) |>
      na.omit()
    
    # set.seed(vseed[k])
    # vt     <- caret::createDataPartition(dfxy[,1], p = 0.7, list = FALSE)
    # treino <- dfxy[ vt,]
    # teste  <- dfxy[-vt,]
    
    vt        <- create_partition_stratified(dfxy[,1], p = 0.7, seed = vseed[k])
    treino    <- dfxy[vt$train, ]
    teste     <- dfxy[vt$test,  ]
    
    print(paste0('inicio model_fit ', modelos_train[j], "-",pad3(k)," | ", Sys.time()))
    
    form <- as.formula(paste(target, '~ .'))
    ctrl <- caret::trainControl(
      method     = "repeatedcv", 
      number     = 5,
      repeats    = 10,
      classProbs = TRUE,
      allowParallel = FALSE
    )
    
    set.seed(vseed[k])
    model_fit <- caret::train(
      form      = form,       
      data      = treino,
      method    = modelos_train[j], 
      metric    = 'Accuracy',
      maximize  = TRUE,
      trControl = ctrl,
      tuneGrid  = grids[[modelos_train[j]]]
    )
    print(model_fit)
    plot(model_fit)
    model_fit$bestTune
    model_fit$results
    # Salva o grid completo de resultados
    grid_path <- paste0("models/grid_", modelos_train[j], "_", pad3(k), ".rds")
    saveRDS(model_fit$results, file = grid_path)
    save(model_fit, 
         file = paste0(fp, '/model_', modelos_train[j], '_', pad3(k), '.RData'))
    # Validação Geral
    vp <- predict(model_fit, teste) 
    vm <- caret::confusionMatrix(vp, teste[,1])
    save(vm$table, 
         file = paste0(fp, "/confusionMatrix_", modelos_train[j], "_", pad3(k), ".RData"))
    # Validação por Classe
    by_class_metrics <- vm$byClass
    save(by_class_metrics, 
         file = paste0(fp, "/byClass_", modelos_train[j], "_", pad3(k), ".RData"))
    # Probabilidade Predita
    predicted_probs  <- predict(model_fit, teste, type = "prob")
    save(predicted_probs,
         file = paste0(fp, "/predProbs_", modelos_train[j], "_", pad3(k), ".RData"))
    # ROC AUC
    roc_auc <- pROC::multiclass.roc(teste[,1], predicted_probs)$auc
    save(roc_auc, 
         file = paste0(fp, "/roc_auc_", modelos_train[j], "_", pad3(k), ".RData"))
    # Mean AUC
    mean_auc <- mean(roc_auc)
    
    print(paste0('inicio predicao final e mapa ', modelos_train[j], "-",pad3(k)," | ", Sys.time()))
    
    # Garante que geol seja fator com os mesmos níveis do treino
    if ("geol" %in% names(dft)) {
      dft$geol <- factor(dft$geol, levels = levels(dfxy$geol))
    }
    
    # Aplica o mesmo scale às variáveis numéricas de dft
    dft_scaled <- dft |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ scale(., center = TRUE, scale = TRUE)[, 1]))
    
    # Reaplica os fatores
    for (col in fatores_treino) {
      if (col %in% names(dft_scaled)) {
        dft_scaled[[col]] <- factor(dft_scaled[[col]], levels = levels(dfx[[col]]))
      }
    }
    
    # Mapa Pedito
    dft_pred <- dft_scaled |> dplyr::filter(!if_any(all_of(names(dfx)), is.na))
    va       <- predict(model_fit, dft_pred)
    va       <- factor(va, levels = levels(dfxy[,1]))
    dat1     <- data.frame(x = dft_pred$x, y = dft_pred$y, z = as.integer(va))
    mapa     <- terra::rast(dat1, type = "xyz", crs = terra::crs(r))
    writeRaster(x = mapa, 
                filename = paste0("maps/", modelos_train[j], "_", pad3(k), ".tif"), 
                overwrite = TRUE)
    plot(mapa)
    
    # Mapa Probabilidade:
    vpr   <-predict(model_fit, dft_pred, type = "prob")
    dat2  <- data.frame(x = dft_pred$x, y = dft_pred$y, z = vpr)
    mapap <- terra::rast(dat2, type = "xyz", crs = crs(r))
    writeRaster(mapap, 
                filename = paste0("maps/prob_", modelos_train[j], "_", pad3(k), ".tif"),
                overwrite = TRUE)
    plot(mapap)
    
    dfresult$target[cont]               <- target
    dfresult$repeticao[cont]            <- k
    dfresult$modelo[cont]               <- modelos_train[j]
    dfresult$train_model_accuracy[cont] <- max(model_fit$results$Accuracy)
    dfresult$train_model_kappa[cont]    <- max(model_fit$results$Kappa)
    dfresult$model_accuracy[cont]       <- vm$overall['Accuracy']
    dfresult$model_kappa[cont]          <- vm$overall['Kappa'] 
    dfresult$null_model_accuracy[cont]  <- vm$overall['AccuracyNull']
    dfresult$mean_auc[cont]             <- mean_auc
    
    toc()
    cont <- cont + 1
    
    print(paste0('fim ', modelos_train[j], '-', pad3(k), " | ", Sys.time()))
    
  } # for k
  fn <- paste0('models/resultados_repeat.csv')
  readr::write_csv(dfresult, fn)
} # for j

fn <- paste0('models/resultados_repeat.csv')
readr::write_csv(dfresult, fn)

parallel::stopCluster(cl)
allowParallel = FALSE
