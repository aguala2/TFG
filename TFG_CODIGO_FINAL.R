############################## ENTORNO #######################################
library(readxl)
library(dplyr) #para manipulación de df
library(tidyr) #para manipulación de df
library(ggplot2) #para gráficos
library(DataExplorer) #para realizar el análisis descriptivo con gráficos
library(corrplot)
library(caret)
library(gbm) #para modelizar con Gradient Boosting
options(scipen=999)

###################### CARGA y TRANSFORMACIÓN DE DATOS #########################
EMPRESAS <- read_excel("Bases_datos/Empresas_datosratios.xlsx", 
                       sheet = "Resultados", 
                       col_types = c("text", "text","text", "numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

df <- EMPRESAS
colnames(df) <- c("id", "Nombre_empresa", "País", "Empleados", "NACE", "Sectores", "Activos_totales", "Depreciación", "EBIT", "EBITDA", "Ingresos_explotacion", "Ingresos_netos", "Flujo_caja", "COGS", "Gastos_ID", "deuda_neta", "Cap_bursatil", "deuda_LP", "Ratio_liquidez", "Ratio_liquidez_accionistas", "Ratio_solvencia", "Solvency_ratio", "ROE", "ROE_IngNetos", "ROA", "ROA_IngNetos", "Endeudamiento", "Total_pasivo", "Total_pasivo_corriente", "Efectivo_y_equivalentes", "Dinero_efectivo_o_equivalentes", "Total_pasivo_deuda", "PN")

df <- df %>%
  mutate(Suma_Cap = Cap_bursatil + deuda_neta)

df_sel <- subset(df, select = c(País, Empleados, Sectores, Activos_totales, Depreciación, EBIT, Ingresos_explotacion, Dinero_efectivo_o_equivalentes, Gastos_ID, Suma_Cap)) #Ingresos_netos, Flujo_caja,
datos <- na.omit(df_sel)

######### FACTORIZACIÓN 
datos$País <- as.factor(datos$País)
datos$Sectores <- as.factor(datos$Sectores)

######## OUTLIERS
datos <- datos[datos$País != "Mónaco", ]
datos <- datos[datos$País != "Gibraltar", ]

boxplot.stats(datos$Suma_Cap)$out
out <- boxplot.stats(datos$Suma_Cap)$out
out_ind <- which(datos$Suma_Cap %in% c(out))
datos[out_ind, ]
datos <- datos[-out_ind, ]

boxplot.stats(datos$Activos_totales)$out
out <- boxplot.stats(datos$Activos_totales)$out
out_ind <- which(datos$Activos_totales %in% c(out))
out_ind
datos[out_ind, ]
datos <- datos[-out_ind, ]

boxplot.stats(datos$EBIT)$out
out <- boxplot.stats(datos$EBIT)$out
out_ind <- which(datos$EBIT %in% c(out))
datos <- datos[-out_ind, ]

boxplot.stats(datos$Depreciación)$out
out <- boxplot.stats(datos$Depreciación)$out
out_ind <- which(datos$Depreciación %in% c(out))
datos <- datos[-out_ind, ]

boxplot.stats(datos$Ingresos_explotacion)$out
out <- boxplot.stats(datos$Ingresos_explotacion)$out
out_ind <- which(datos$Ingresos_explotacion %in% c(out))
datos <- datos[-out_ind, ]

boxplot.stats(datos$COGS)$out
out <- boxplot.stats(datos$COGS)$out
out_ind <- which(datos$COGS %in% c(out))
datos <- datos[-out_ind, ]

boxplot.stats(datos$Empleados)$out
out <- boxplot.stats(datos$Empleados)$out
out_ind <- which(datos$Empleados %in% c(out))
datos <- datos[-out_ind, ]



############################# DESCRIPTIVO ####################################
library(ggplot2)
library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) # Paleta de colores
library(dplyr)

### Mapa Europa
# Crear un diccionario de nombres de países en español e inglés
translation_dict <- c("Alemania" = "Germany", 
                      "Países Bajos" = "Netherlands", 
                      "Francia" = "France", 
                      "Austria" = "Austria", 
                      "Luxemburgo" = "Luxembourg", 
                      "Grecia" = "Greece", 
                      "España" = "Spain", 
                      "Turquía" = "Turkey", 
                      "Italia" = "Italy", 
                      "Suecia" = "Sweden", 
                      "Reino Unido" = "United Kingdom", 
                      "Suiza" = "Switzerland", 
                      "Finlandia" = "Finland", 
                      "Portugal" = "Portugal", 
                      "Bélgica" = "Belgium", 
                      "Dinamarca" = "Denmark", 
                      "Irlanda" = "Ireland", 
                      "Noruega" = "Norway", 
                      "Chipre" = "Cyprus", 
                      "Jersey (Reino Unido)" = "Jersey (UK)", 
                      "Islandia" = "Iceland", 
                      "Malta" = "Malta", 
                      "Gibraltar" = "Gibraltar", 
                      "Guernsey (Reino Unido)" = "Guernsey (UK)", 
                      "Mónaco" = "Monaco")


# Cambiar los nombres de los países en el dataframe
datos$País <- recode(datos$País, !!!translation_dict)
empresas_por_pais <- datos %>%
  group_by(País) %>%
  summarise(Count_empresas = n())
empresas_por_pais <- empresas_por_pais[empresas_por_pais$País != "Guernsey (UK)", ]
empresas_por_pais <- empresas_por_pais[empresas_por_pais$País != "Jersey (UK)", ]
empresas_por_pais <- empresas_por_pais[empresas_por_pais$País != "Isle Of Man (Reino Unido)", ]
empresas_por_pais <- empresas_por_pais[empresas_por_pais$País != "French Guiana (Francia)", ]
empresas_por_pais <- empresas_por_pais[empresas_por_pais$País != "Reunion (Francia)", ]

mapa_europa <- ne_countries(scale = "medium", returnclass = "sf", continent = "Europe")

# 3. Unir los datos de empresas con los datos geográficos
library(RColorBrewer)
library(viridis)
color_palette <- brewer.pal(9, "Set1")
mapa_empresas <- left_join(empresas_por_pais, mapa_europa, by = c("País" = "name"))

# Crear el mapa enfocado en Europa
ggplot(mapa_empresas, aes(geometry = geometry, fill = Count_empresas)) +
  geom_sf(color = "white", size = 0.2) +
  scale_fill_distiller(palette = "OrRd", name = "Empresas", direction = 1) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Figura 3: Número de empresas por país",
    caption = "Fuente: elaboración propia con datos de Orbis"
  ) +
  coord_sf(xlim = c(-20, 40), ylim = c(35, 70))  # Ajustar los límites geográficos para enfocar en Europa


# Gráfico circular de la variable "Sectores"
ggplot(data = datos, aes(x = "")) +
  geom_bar(aes(fill = Sectores), width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Figura 4: Distribución por Sectores",
       caption = "Fuente: elaboración propia con datos de Orbis")+
  theme_minimal()


# Gráfico de matriz de correlación
library(pheatmap)
pheatmap(correlation_matrix, 
         display_numbers = TRUE, 
         color = colorRampPalette(c("blue", "white", "red"))(50),
         main = "Figura 5: Matriz de Correlación de Variables")
title(main = "Subtítulo de tu gráfico", line = 1.5)


# Histograma de la variable "Suma_Cap"
ggplot(data = datos, aes(x = Suma_Cap)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Figura 6: Distribución de Capitalización + Deuda",
       caption = "Fuente: elaboración propia con datos de Orbis") +
  theme_minimal()




################################# MODELO ######################################
### DIVISON DATOS
set.seed(22)
parts = createDataPartition(datos$Suma_Cap, p = .7, list = F)
train = datos[parts, ]
test = datos[-parts, ]

test_x = test[, -10] # feature and target array
test_y = test[, 10] 


# Ajustar un modelo GBM 
set.seed(22)
model_gbm <- gbm(
  formula = Suma_Cap ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 2500,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
)  

# Ver resumen del modelo
print(model_gbm)
summary(model_gbm)

#Ver estabilización nº árboles
gbm.perf(model_gbm)

# Hacer predicciones
pred_y = predict.gbm(model_gbm, test_x)
pred_y

### MEDIDAS
residuals = test_y - pred_y
res_2 = residuals^2
RMSE = sqrt(mean(res_2$Suma_Cap))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean = mean(test_y$Suma_Cap)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')


# Calculate evaluation metrics
mse <- mean((test_y$Suma_Cap - pred_y)^2)
mae <- mean(abs(test_y$Suma_Cap - pred_y))
rmse <- sqrt(mse)

# Print evaluation metrics
cat("MAE:", mae, "\n", "MSE:", mse, "\n", "RMSE:", rmse, "\n", "\n")


### GRID SEARCH
# creamos hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(10, 15, 20),
  cv.folds = 5,
  optimal_trees = 0,               # para meter resultados
  min_RMSE = 0                    
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Suma_Cap ~ .,
    distribution = "gaussian",
    data = train,
    n.trees = 1500,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    cv.folds = hyper_grid$cv.folds[i],
    train.fraction = .75,
    n.cores = NULL, 
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)




##### MODELO OPTIMIZADO 
set.seed(22)
model_gbm2 <- gbm(
  formula = Suma_Cap ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 386,
  interaction.depth = 5,
  n.minobsinnode = 20,
  shrinkage = 0.01,
  #cv.folds = 5,
)  

# Ver resumen del modelo
print(model_gbm2)
model_gbm2$var.names[8] <- 'Efectivo'
summary(
  model_gbm2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#Ver estabilización nº árboles
gbm.perf(model_gbm2)

# Hacer predicciones
pred_y2 = predict.gbm(model_gbm2, test_x)
pred_y2

### MEDIDAS
residuals2 = test_y - pred_y2
res_22 = residuals2^2
RMSE2 = sqrt(mean(res_22$Suma_Cap))
cat('The root mean square error of the test data is ', round(RMSE2,3),'\n')

y_test_mean = mean(test_y$Suma_Cap)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss2 =  sum(residuals2^2)
# Calculate R-squared
rsq2  =  1 - (rss2/tss)
cat('The R-square of the test data is ', round(rsq2,3), '\n')


# Calculate evaluation metrics
mse <- mean((test_y$Suma_Cap - pred_y)^2)
mae <- mean(abs(test_y$Suma_Cap - pred_y))
rmse <- sqrt(mse)

# Print evaluation metrics
cat("MAE:", mae, "\n", "MSE:", mse, "\n", "RMSE:", rmse, "\n", "\n")
