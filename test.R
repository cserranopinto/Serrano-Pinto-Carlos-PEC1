library(metabolomicsWorkbenchR)
study_id <- "ST003521"
#mw_data <- getMWData(study_id)

mw_data = do_query(
  context = 'study',
  input_item = 'study_id',
  input_value = 'ST003521',
  output_item = 'SummarizedExperiment'
)


library(SummarizedExperiment)

# Extraer los dos objetos
se <- mw_data[[1]]
se2 <- mw_data[[2]]

# Unir ambos objetos
#combined_se <- cbind(assay(se1), assay(se2))

# Crear un nuevo objeto SummarizedExperiment con los datos combinados
#colData_combined <- rbind(colData(se1), colData(se2))
#se <- SummarizedExperiment(assays = list(data = combined_se), colData = colData_combined)


library(dplyr)
library(tidyr)


# Convertir datos a data.frame
data_df <- as.data.frame(assay(se))

# Obtener nombres originales
col_names <- colnames(data_df)

# Eliminar el número de replicado, manteniendo solo la condición y el tiempo
col_names_clean <- gsub("_\\d+$", "", col_names)

# Asignar los nombres limpios
colnames(data_df) <- col_names_clean

# Identificar nombres duplicados
duplicated_names <- unique(col_names_clean[duplicated(col_names_clean)])

if (length(duplicated_names) > 0) {
  message("Columnas duplicadas detectadas después de limpiar nombres: ", paste(duplicated_names, collapse = ", "))
  
  # Crear un data frame vacío para los promedios
  data_avg <- data.frame(row.names = rownames(data_df))
  
  # Promediar cada conjunto de columnas duplicadas
  for (name in duplicated_names) {
    data_avg[[name]] <- rowMeans(data_df[, colnames(data_df) == name], na.rm = TRUE)
  }
  
  # Agregar las columnas que no están duplicadas
  non_duplicated_names <- setdiff(col_names_clean, duplicated_names)
  data_avg <- cbind(data_df[non_duplicated_names], data_avg)
  
} else {
  data_avg <- data_df
}

time_point <- "3h"  # Cambiar por "1h" o "6h" si es necesario

# Filtrar las columnas correspondientes al tiempo seleccionado
selected_cols <- grep(paste0("_", time_point, "$"), colnames(data_avg), value = TRUE)
data_time <- data_avg %>%
  select(all_of(selected_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "Value")



library(ggplot2)

ggplot(data_time, aes(x = Condition, y = Value, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = paste("Metabolite Levels at", time_point),
       x = "Condition", y = "Metabolite Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

