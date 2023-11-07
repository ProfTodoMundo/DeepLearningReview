library(pdftools)
# Directorio que contiene los archivos PDF a unir
directorio <- "~/Desktop/MiGithub/DeepLearningReview/Principales"  # Reemplaza con la ruta de tu directorio

# Nombre del archivo de salida (PDF unido)
archivo_salida <- "Referencias.pdf"

# Listar todos los archivos PDF en el directorio
archivos_pdf <- list.files(directorio, pattern = "\\.pdf$", full.names = TRUE)

# Función para unir los archivos PDF
unir_pdf <- function(archivos_pdf, archivo_salida) {
  pdf_combine(archivos_pdf, output = archivo_salida)
}

# Llama a la función para unir los archivos PDF
unir_pdf(archivos_pdf, archivo_salida)
