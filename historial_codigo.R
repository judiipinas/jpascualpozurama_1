library(ggplot2)
# Histograma para la variable 'RTN2'
ggplot(rna1, aes(x = RTN2)) +
geom_histogram(bins = 30, fill = "blue", color = "black") +
ggtitle("Distribución de RTN2")
# Adapta y repite para otras variables según necesites
# Boxplot de 'RTN2' por 'Class'
ggplot(rna1, aes(x = Class, y = RTN2, fill = Class)) +
geom_boxplot() +
ggtitle("Comparación de RTN2 por Clase")
library(ggplot2)
# Configuración para múltiples gráficos en una sola ventana
par(mfrow=c(2, 3))
# Lista de variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar gráficos
for (var in variables) {
p <- ggplot(rna1, aes_string(x = var)) +
geom_histogram(aes(y = ..density..), bins = 30, fill = "gold", color = "black") +
geom_density(alpha = .2, fill = "#FF6666") +
ggtitle(paste("Distribución de", var))
print(p)
}
# Lista de variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar boxplots para cada variable
p <- ggplot(rna1, aes(x = Class, y = value)) +
geom_boxplot(aes(fill = Class)) +
facet_wrap(~ variable, scales = "free") +
theme_minimal() +
labs(title = "Boxplots por Clase", x = "Clase", y = "Valor")
# Convertir datos a formato largo para facilitar la visualización en ggplot
rna1_long <- reshape2::melt(rna1, id.vars = "Class", measure.vars = variables)
# Imprimir el gráfico
print(p + geom_boxplot(data = rna1_long))
if (!require(reshape2)) install.packages("reshape2", dependencies=TRUE)
library(reshape2)
# Lista de variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar boxplots para cada variable
p <- ggplot(rna1, aes(x = Class, y = value)) +
geom_boxplot(aes(fill = Class)) +
facet_wrap(~ variable, scales = "free") +
theme_minimal() +
labs(title = "Boxplots por Clase", x = "Clase", y = "Valor")
# Convertir datos a formato largo para facilitar la visualización en ggplot
rna1_long <- reshape2::melt(rna1, id.vars = "Class", measure.vars = variables)
# Imprimir el gráfico
print(p + geom_boxplot(data = rna1_long))
# Lista de variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar boxplots para cada variable
p <- ggplot(rna1, aes(x = Class, y = value)) +
geom_boxplot(aes(fill = Class)) +
facet_wrap(~ variable, scales = "free") +
theme_minimal() +
labs(title = "Boxplots por Clase", x = "Clase", y = "Valor")
# Imprimir el gráfico
print(p)
# Lista de variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar boxplots para cada variable
p <- ggplot(rna1, aes(x = Class, y = value)) +
geom_boxplot(aes(fill = Class)) +
facet_wrap(~ variable, scales = "free") +
theme_minimal() +
labs(title = "Boxplots por Clase", x = "Clase", y = "Valor")
# Cargar datos
rna1 <- read.csv("rna1.csv")
# Eliminar variables con cualquier valor ausente
rna1_clean <- rna1[, colSums(is.na(rna1)) == 0]
# Verificar las dimensiones del nuevo conjunto de datos
dim(rna1_clean)
# Cargar la librería necesaria
library(factoextra)
# Realizar PCA
pca_result <- prcomp(rna1_clean[, -1], scale. = TRUE)  # Excluye la columna 'Class' si aún está presente
# Graficar los resultados del PCA
fviz_pca_biplot(pca_result, label = "var", habillage = rna1_clean$Class,
addEllipses = TRUE, ellipse.level = 0.95)
# Cargar la librería necesaria
if (!require(pheatmap)) install.packages("pheatmap")
library(pheatmap)
# Calcular correlaciones entre variables
cor_matrix <- cor(rna1_clean[, -1])  # Excluye la columna 'Class' si es necesario
# Crear el mapa de calor
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean",
clustering_method = "complete",
display_numbers = TRUE)
library(pheatmap)
# Suponiendo que 'rna1_clean' es tu DataFrame limpio
cor_matrix <- cor(rna1_clean[, -1])  # Asumiendo que la primera columna es 'Class'
# Ajusta el tamaño del texto y el ángulo de las etiquetas
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean",
clustering_method = "complete",
display_numbers = FALSE,  # Puedes quitar los números para una vista más clara
fontsize_row = 8,
fontsize_col = 8,
angle_col = 45,  # Rota las etiquetas de las columnas
cellwidth = 20,  # Ajusta según tus necesidades
cellheight = 20,  # Ajusta según tus necesidades
main = "Mapa de Calor de Correlaciones")
library(pheatmap)
# Suponiendo que 'rna1_clean' es tu DataFrame limpio
cor_matrix <- cor(rna1_clean[, -1])  # Asumiendo que la primera columna es 'Class'
# Ajusta el tamaño del texto y el ángulo de las etiquetas
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean",
clustering_method = "complete",
display_numbers = FALSE,  # Puedes quitar los números para una vista más clara
fontsize_row = 8,
fontsize_col = 8,
angle_col = 45,  # Rota las etiquetas de las columnas
cellwidth = 20,  # Ajusta según tus necesidades
cellheight = 20,  # Ajusta según tus necesidades
main = "Mapa de Calor de Correlaciones")
png("heatmap.png", width = 12, height = 12, units = 'in', res = 300)
pheatmap(cor_matrix, ...)
library(pheatmap)
# Suponiendo que 'rna1_clean' es tu DataFrame limpio
cor_matrix <- cor(rna1_clean[, -1])
# Ajusta el tamaño del texto y el ángulo de las etiquetas
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean",
clustering_method = "complete",
display_numbers = FALSE,  # Puedes quitar los números para una vista más clara
fontsize_row = 8,
fontsize_col = 8,
angle_col = 45,  # Rota las etiquetas de las columnas
cellwidth = 20,  # Ajusta según tus necesidades
cellheight = 20,  # Ajusta según tus necesidades
main = "Mapa de Calor de Correlaciones")
PCA<-prcomp(rna1_clean[,1:ncol(rna1_clean)-1],center = TRUE,scale. = TRUE)
# Cargar la librería necesaria
library(factoextra)
# Realizar PCA
pca_result <- prcomp(rna1_clean[, -1], scale. = TRUE)
# Graficar los resultados del PCA
fviz_pca_biplot(pca_result, label = "var", habillage = rna1_clean$Class,
addEllipses = TRUE, ellipse.level = 0.95)
library(pheatmap)
# Suponiendo que 'rna1_clean' es tu DataFrame limpio
cor_matrix <- cor(rna1_clean[, -1])
# Ajusta el tamaño del texto y el ángulo de las etiquetas
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean",
clustering_method = "complete",
fontsize_row = 8,
fontsize_col = 8,
angle_col = 45,  # Rota las etiquetas de las columnas
cellwidth = 20,  # Ajusta según tus necesidades
cellheight = 20,  # Ajusta según tus necesidades
main = "Mapa de Calor de Correlaciones")
library(ggplot2)
# Configuración para múltiples gráficos en una sola ventana
par(mfrow=c(2, 3))
# Lista de algunas variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar gráficos
for (var in variables) {
p <- ggplot(rna1, aes_string(x = var)) +
geom_histogram(aes(y = ..density..), bins = 30, fill = "gold", color = "black") +
geom_density(alpha = .2, fill = "#FF6666") +
ggtitle(paste("Distribución de", var))
print(p)
}
#### Corrección en la creación de particiones
```{r}
# Cargando la librería caret
library(caret)
# Estableciendo la semilla para reproducibilidad
set.seed(12345)
# Creando la partición de datos
index_train <- createDataPartition(rna1$Class, p = 0.67, list = FALSE)
train_set <- rna1[index_train, ]
test_set <- rna1[-index_train, ]
# Verificar las dimensiones de los conjuntos de entrenamiento y prueba
dim(train_set)
dim(test_set)
# Verificar las primeras filas del conjunto de entrenamiento
head(train_set)
# Boxplot agrupado por clase
ggplot(rna1, aes(x=Class, y=variable_numerica, fill=Class)) + geom_boxplot()
# Boxplot agrupado por clase
library(ggplot2)
ggplot2(rna1, aes(x=Class, y=variable_numerica, fill=Class)) + geom_boxplot()
# Boxplot agrupado por clase
library(ggplot2)
ggplot(rna1, aes(x=Class, y=variable_numerica, fill=Class)) + geom_boxplot()
library(ggplot2)
# Configuración para múltiples gráficos en una sola ventana
par(mfrow=c(2, 3))
# Lista de algunas variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS""GMDS","HLA.H","SEMA4A","ETS2","LIMD2","NME3","ZEB1","CDCP1","GIYD2","RTKN2","MANSC1","TAGLN","IFIT3","ARL4C","HTRA1","KIF13B","CPPED1","SKAP2","ASPM","KDM4B","TBXAS1","MT1X","MED13L","SNORA8","RGS1","CBX6","WWC2","TNFRSF12A","ZNF552","MAPRE2","SEMA5A","STAT5A","FLI1","COL15A1","C7orf55","ASF1B","FUT8","LASS4","SQLE","GPC4","AKAP12","AGL","ADAMTS4","EPHB3","MAP3K1","PRNP","PROM2","SLCO3A1","SNHG1","PRKCDBP","MXI1","CSF1R","TANC2","SLC19A2","RHOU","C4orf34","LRIG1","DOCK8","BOC","C11orf52","S100A16","NRARP","TTC23","TBC1D4","DEPDC6","ILDR1","SDC1","STC2","DTWD2","TCF4","ITPR2","DPYD","NME1","EGLN3","CD302","AHR","LAPTM4B","OCLN","HIST1H2BK","HDAC11","C18orf1","C6orf192","AMPD3","COL6A1","RAB3IL1","APBB1IP","PSIP1","EIF2AK2","CSRP2","EIF4EBP3","LYN","WDR76","SAMD9L","ASPH","RBL1","SLC43A3","HN1","TTC39A","MTL5","NES","APOD","RIN3","ALCAM","C1orf38","PLCD3","BSPRY","NTN4","IL1R1","EMP3","ZKSCAN1","FMNL2","OGFRL1","IRF5","IGSF3","DBP","CNN2","CAMK2D","SIGIRR","AKAP9","ICA1","FGD5","DSG2","E2F1","QSOX1","TOB1","CSF3R","SHROOM3","CCDC80","FRMD6","CXCL12","CCNA2","TIGD5","ALDH6A1","POSTN","FZD4","NCAPG2","SDC4","SNED1","PLEKHA4","KCNAB2","SH3KBP1","IGSF9","DNLZ","S1PR3","PTPRE","FLJ23867","PLSCR1","LMO4","IFITM2","LRRC25","TST","NCF4","NCOA7","IL4R","CCDC64B","SGPP1","RUNX3","SLC5A6","IFIH1","PREX1","PLAUR","CDK18","SLC43A2","GK","ICAM2","YPEL2","CBR1","MEX3A","ZNRF3","PTPRM","C1orf162","GAS6","C1QB","PVRL4","CTSK","MRVI1","LEF1","PLCD4","ZNF37B","MEGF9","GINS2","FAM13A","CPT1A","SNX10","TRIM45","ELP2","ALOX5","AMN1","CERCAM","SEMA3C","KRT8","TP53INP2","JAM3","ZNF680","PBX1")
library(ggplot2)
# Configuración para múltiples gráficos en una sola ventana
par(mfrow=c(2, 3))
# Lista de algunas variables a graficar
variables <- c("RTN2", "NDRG2", "CCDC113", "FAM63A", "ACADS")
# Generar gráficos
for (var in variables) {
p <- ggplot(rna1, aes_string(x = var)) +
geom_histogram(aes(y = ..density..), bins = 30, fill = "gold", color = "black") +
geom_density(alpha = .2, fill = "#FF6666") +
ggtitle(paste("Distribución de", var))
print(p)
}
library(ggplot2)
# Histogramas de todas las variables numéricas
hist(rna1$SomeNumericColumn, main="Histogram of SomeNumericColumn", xlab="Values", breaks=30)
table(rna1$Class)
# Generar un boxplot para cada variable numérica, agrupada por 'Class'
numeric_columns <- sapply(rna1, is.numeric)
data_to_plot <- rna1[, numeric_columns]
# Si la columna 'Class' no es un factor, conviértela para mejor visualización
rna1$Class <- as.factor(rna1$Class)
# Usando un loop para graficar cada variable numérica
for (column_name in names(data_to_plot)) {
ggplot(rna1, aes(x = Class, y = .data[[column_name]], fill = Class)) +
geom_boxplot() +
labs(title = paste("Boxplot of", column_name, "by Class"), y = column_name, x = "Class") +
theme_minimal() +
theme(legend.position = "none") +
print()
}
# Cargar la librería necesaria
library(factoextra)
# Realizar PCA
pca_result <- prcomp(rna1_clean[, -1], scale. = TRUE)
# Graficar los resultados del PCA
fviz_pca_biplot(pca_result, label = "var", habillage = rna1_clean$Class,
addEllipses = TRUE, ellipse.level = 0.95)
library(pheatmap)
# Suponiendo que 'rna1_clean' es tu DataFrame limpio
cor_matrix <- cor(rna1_clean[, -1])
# Ajusta el tamaño del texto y el ángulo de las etiquetas
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean",
clustering_method = "complete",
fontsize_row = 8,
fontsize_col = 8,
angle_col = 45,  # Rota las etiquetas de las columnas
cellwidth = 20,  # Ajusta según tus necesidades
cellheight = 20,  # Ajusta según tus necesidades
main = "Mapa de Calor de Correlaciones")
# Cargando la librería caret
library(caret)
# Estableciendo la semilla para reproducibilidad
set.seed(12345)
# Creando la partición de datos
index_train <- createDataPartition(rna1$Class, p = 0.67, list = FALSE)
train_set <- rna1[index_train, ]
test_set <- rna1[-index_train, ]
# Verificar las dimensiones de los conjuntos de entrenamiento y prueba
dim(train_set)
dim(test_set)
# Verificar las primeras filas del conjunto de entrenamiento
head(train_set)
# Cargando las librerías necesarias
library(caret)
library(e1071) # Para Naive Bayes
library(nnet) # Para Redes Neuronales
library(kernlab) # Para Support Vector Machine
library(rpart) # Para Árboles de Clasificación
library(randomForest) # Para Random Forest
install.packages("randomForest")
# Cargando las librerías necesarias
library(caret)
library(e1071) # Para Naive Bayes
library(nnet) # Para Redes Neuronales
library(kernlab) # Para Support Vector Machine
library(rpart) # Para Árboles de Clasificación
library(randomForest) # Para Random Forest
# Estableciendo la semilla para reproducibilidad
set.seed(12345)
# Creando la partición de datos
index_train <- createDataPartition(rna1$Class, p = 0.67, list = FALSE)
train_set <- rna1[index_train, ]
test_set <- rna1[-index_train, ]
knn_metrics <- data.frame()
for (k in c(1, 3, 5, 7, 11)) {
knn_model <- train(Class ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k = k))
predictions <- predict(knn_model, newdata = test_set)
cm <- confusionMatrix(predictions, test_set$Class)
knn_metrics <- rbind(knn_metrics, data.frame(
k = k,
Accuracy = cm$overall['Accuracy'],
Kappa = cm$overall['Kappa'],
Sensitivity = mean(cm$byClass['Sensitivity']),
Specificity = mean(cm$byClass['Specificity'])
))
}
print(knn_metrics)
nb_metrics <- data.frame()
for (laplace in c(0, 1)) {
nb_model <- train(Class ~ ., data = train_set, method = "nb", tuneGrid = data.frame(usekernel = TRUE, fL = laplace))
predictions <- predict(nb_model, newdata = test_set)
cm <- confusionMatrix(predictions, test_set$Class)
nb_metrics <- rbind(nb_metrics, data.frame(
Laplace = laplace,
Accuracy = cm$overall['Accuracy'],
Kappa = cm$overall['Kappa'],
Sensitivity = mean(cm$byClass['Sensitivity']),
Specificity = mean(cm$byClass['Specificity'])
))
}
nb_metrics <- data.frame()
for (laplace in c(0, 1)) {
nb_model <- train(Class ~ ., data = train_set, method = "nb", tuneGrid = data.frame(fL = laplace, usekernel = TRUE, adjust = 1))
predictions <- predict(nb_model, newdata = test_set)
cm <- confusionMatrix(predictions, test_set$Class)
nb_metrics <- rbind(nb_metrics, data.frame(
Laplace = laplace,
Accuracy = cm$overall['Accuracy'],
Kappa = cm$overall['Kappa'],
Sensitivity = mean(cm$byClass['Sensitivity']),
Specificity = mean(cm$byClass['Specificity'])
))
}
print(nb_metrics)
ann_metrics <- data.frame()
ann_models <- list(
list(size = 30),
list(size = c(50, 10))
)
for (arch in ann_models) {
ann_model <- train(Class ~ ., data = train_set, method = "nnet", trace = FALSE, linout = TRUE, tuneGrid = expand.grid(size = arch$size, decay = 0.1))
predictions <- predict(ann_model, newdata = test_set)
cm <- confusionMatrix(predictions, test_set$Class)
ann_metrics <- rbind(ann_metrics, data.frame(
Architecture = paste(arch$size, collapse = '-'),
Accuracy = cm$overall['Accuracy'],
Kappa = cm$overall['Kappa'],
Sensitivity = mean(cm$byClass['Sensitivity']),
Specificity = mean(cm$byClass['Specificity'])
))
}
ann_metrics <- data.frame()
# Definición de las arquitecturas a explorar
architectures <- list(
list(size = 30, decay = 0.1),
list(size = c(50, 10), decay = 0.1)
)
for (arch in architectures) {
ann_model <- train(
Class ~ .,
data = train_set,
method = "nnet",
trace = FALSE,
linout = TRUE,
tuneGrid = expand.grid(size = arch$size, decay = arch$decay)
)
predictions <- predict(ann_model, newdata = test_set)
cm <- confusionMatrix(predictions, test_set$Class)
ann_metrics <- rbind(ann_metrics, data.frame(
Architecture = paste(arch$size, collapse = '-'),
Decay = arch$decay,
Accuracy = cm$overall['Accuracy'],
Kappa = cm$overall['Kappa'],
Sensitivity = mean(cm$byClass['Sensitivity']),
Specificity = mean(cm$byClass['Specificity'])
))
}
svm_metrics <- data.frame()
for (kernel in c('linear', 'rbf')) {
svm_model <- train(Class ~ ., data = train_set, method = "svmRadial", tuneGrid = expand.grid(C = 1, sigma = 0.1))
predictions <- predict(svm_model, newdata = test_set)
cm <- confusionMatrix(predictions, test_set$Class)
svm_metrics <- rbind(svm_metrics, data.frame(
Kernel = kernel,
Accuracy = cm$overall['Accuracy'],
Kappa = cm$overall['Kappa'],
Sensitivity = mean(cm$byClass['Sensitivity']),
Specificity = mean(cm$byClass['Specificity'])
))
}
print(svm_metrics)
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())
library(ggplot2)
# Histograma de una variable seleccionada
ggplot(expression_data, aes(x = `1,6-Anhydro-beta-D-glucose`)) +
geom_histogram(binwidth = 10, fill = "blue", color = "black") +
theme_minimal() +
labs(title = "Histograma de 1,6-Anhydro-beta-D-glucose", x = "Valor", y = "Frecuencia")
if (!require(BiocManager)) install.packages("BiocManager")
installifnot <- function (pkg){
if (!require(pkg, character.only=T)){
BiocManager::install(pkg)
}
}
installifnot("SummarizedExperiment")
installifnot("PCAtools")
installifnot("pd.mogene.1.0.st.v1")
installifnot("mogene10sttranscriptcluster.db")
installifnot("oligo")
installifnot("limma")
installifnot("Biobase")
installifnot("arrayQualityMetrics")
installifnot("genefilter")
installifnot("annotate")
installifnot("xtable")
installifnot("gplots")
installifnot("GOstats")
library(readr)
# Cargar el archivo description.md
metadata <- read_file("C:/Users/judii/Desktop/MÁSTER BIOINFORMÁTICA Y BIOESTADÍSTICA/ANÁLISIS DATOS ÓMICOS/2/PEC1-ADO/description.md")
metadata_info <- paste(metadata, collapse = "\n")
# Imprimir el contenido del archivo de descripción
cat(metadata_info)
library(readr)
human_cachexia <- read_csv("C:/Users/judii/Desktop/MÁSTER BIOINFORMÁTICA Y BIOESTADÍSTICA/ANÁLISIS DATOS ÓMICOS/2/PEC1-ADO/human_cachexia.csv", show_col_types = FALSE)
View (human_cachexia)
# Crear la matriz de expresión
expression_matrix <- as.matrix(human_cachexia)
# Crear un objeto DataFrame para las columnas (muestras)
colData <- DataFrame(SampleID = colnames(expression_matrix))
# Agregar metadatos al objeto colData
colData$Metadata <- rep(metadata_info, ncol(expression_matrix))
# Crear un objeto SummarizedExperiment
se <- SummarizedExperiment(
assays = list(counts = expression_matrix),
colData = colData
)
# Mostrar información general del objeto
se
# Mostrar las primeras filas de la matriz de expresión
head(assay(se))
# Convertir los datos a numéricos, forzando NA si hay valores no numéricos
expression_data <- as.data.frame(assay(se))
expression_data[] <- lapply(expression_data, function(x) as.numeric(as.character(x)))
# Verificar si la conversión generó NAs
if (any(is.na(expression_data))) {
cat("Se encontraron valores no numéricos y se convirtieron a NA.\n")
}
# Eliminar columnas con todos los valores NA (si las hay)
expression_data <- expression_data[, colSums(is.na(expression_data)) < nrow(expression_data)]
# Recalcular la matriz de correlación
corr_matrix <- cor(expression_data, use = "pairwise.complete.obs")e
non_numeric_cols <- sapply(expression_data, function(x) sum(is.na(as.numeric(as.character(x)))))
problematic_cols <- names(non_numeric_cols[non_numeric_cols > 0])
cat("Columnas con valores no numéricos:\n")
print(problematic_cols)
expression_data[] <- lapply(expression_data, function(x) {
x <- as.character(x)
x[grepl("[^0-9.]", x)] <- NA  # Reemplaza cualquier cosa que no sea un número o un punto decimal con NA
as.numeric(x)
})
# Recalcular la matriz de correlación después de la limpieza
corr_matrix <- cor(expression_data, use = "pairwise.complete.obs")
library(ggplot2)
# Histograma de una variable seleccionada
ggplot(expression_data, aes(x = `1,6-Anhydro-beta-D-glucose`)) +
geom_histogram(binwidth = 10, fill = "blue", color = "black") +
theme_minimal() +
labs(title = "Histograma de 1,6-Anhydro-beta-D-glucose", x = "Valor", y = "Frecuencia")
# Boxplot para todas las variables
boxplot(expression_data, main = "Boxplot de todas las variables", las = 2, col = "lightblue")
library(FactoMineR)
library(factoextra)
# Ejecutar PCA
pca_result <- PCA(expression_data, graph = FALSE)
# Visualizar las primeras dos componentes principales
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = "blue",
addEllipses = TRUE, ellipse.type = "confidence",
title = "Análisis de Componentes Principales (PCA)")
# Calcular la distancia entre las muestras
dist_matrix <- dist(expression_data)
hclust_result <- hclust(dist_matrix)
# Visualizar el dendrograma
plot(hclust_result, main = "Dendrograma de Agrupamiento Jerárquico", xlab = "", sub = "")
summary_stats <- summary(expression_data)
print(summary_stats)
boxplot(expression_data, main = "Detección de valores atípicos", outline = TRUE, las = 2)
library(readr)
genefilter::spec
# Cargar el archivo description.md
metadata <- read_file("C:/Users/judii/Desktop/MÁSTER BIOINFORMÁTICA Y BIOESTADÍSTICA/ANÁLISIS DATOS ÓMICOS/2/PEC1-ADO/description.md")
metadata_info <- paste(metadata, collapse = "\n")
# Imprimir el contenido del archivo de descripción
cat(metadata_info)
# Convertir los datos a numéricos, forzando NA si hay valores no numéricos
expression_data <- as.data.frame(assay(se))
expression_data[] <- lapply(expression_data, function(x) as.numeric(as.character(x)))
# Verificar si la conversión generó NAs
if (any(is.na(expression_data))) {
cat("Se encontraron valores no numéricos y se convirtieron a NA.\n")
}
# Eliminar columnas con todos los valores NA (si las hay)
expression_data <- expression_data[, colSums(is.na(expression_data)) < nrow(expression_data)]
# Recalcular la matriz de correlación
corr_matrix <- cor(expression_data, use = "pairwise.complete.obs")e
# Convertir los datos a numéricos, forzando NA si hay valores no numéricos
expression_data <- as.data.frame(assay(se))
expression_data[] <- lapply(expression_data, function(x) as.numeric(as.character(x)))
# Verificar si la conversión generó NAs
if (any(is.na(expression_data))) {
cat("Se encontraron valores no numéricos y se convirtieron a NA.\n")
}
# Eliminar columnas con todos los valores NA (si las hay)
expression_data <- expression_data[, colSums(is.na(expression_data)) < nrow(expression_data)]
# Recalcular la matriz de correlación
corr_matrix <- cor(expression_data, use = "pairwise.complete.obs")
save(expression_data, file = "datos_metadatos.Rda")
save(expression_data, file = "datos_metadatos.Rda")
load("C:/Users/judii/Desktop/MÁSTER BIOINFORMÁTICA Y BIOESTADÍSTICA/ANÁLISIS DATOS ÓMICOS/2/PEC1-ADO/datos_metadatos.Rda")
save(expression_data, file = "datos_metadatos.Rda")
savehistory(file = "historial_codigo.R")
save(expression_data, file = "datos_metadatos.Rda")
savehistory(file = "historial_codigo.R")
