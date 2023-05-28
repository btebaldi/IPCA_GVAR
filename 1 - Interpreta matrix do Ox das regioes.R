#' Disertacao Andrea Guimaraes
#' 
#' Script para interpretar as matrizes do Ox.
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2023-04-21
#' 


# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# Load library
library(stringr)


# Configs -----------------------------------------------------------------

# Diretorio com as matrizes do Ox
main_dir <- file.path("./Ox/mat_files/Result_Matrix/")
# main_dir <- file.path("C:/Users/bteba/Documents/GitHub/Tese_Andrea_Guimaraes_old/mat_files/Result_Matrix")



# Diretorio para gravação das matrizes do R
output_dir <- file.path("./Output Matrix/")

# lista de arquivos a serem interpretados
mGyL.file_list <-   c(
  "mGy_inv_X_mC",
  "mGy_inv_X_mGyL1",
  "mGy_inv_X_mGyL2",
  "mGy_inv_X_mGyL3",
  "mGy_inv_X_mGyL4",
  # "mGy_inv_X_mGyL5",
  # "mGy_inv_X_mGyL6",
  # "mGy_inv_X_mGyL7",
  # "mGy_inv_X_mGyL8",
  # "mGy_inv_X_mGyL9",
  # "mGy_inv_X_mGyL10",
  # "mGy_inv_X_mGyL11",
  # "mGy_inv_X_mGyL12",
  # "mGy_inv_X_mGyL13",
  "mGy_inv_X_mL"
)

# for(item in mGyL.file_list) {}
for(item in mGyL.file_list) {
  
  # Caminho do arquivo a ser lido
  fileName <- sprintf("%s.mat", item)
  # fileName.mask <- paste(main_dir, "%s.mat", sep = "")
  
  fileName <- file.path(main_dir, fileName)
  # fileName <- sprintf(fileName.mask, item)
  cat("Interpretando o arquivo:", fileName, "\n")
  
  # Abre o aqruivo para leitura
  con <- file(fileName, open="r")
  
  # Faz a leitura das linhas
  line <- readLines(con) 
  
  # Fecha a conexao com o arquivo
  close(con)

  # Interpreta dimensoes da matriz
  matrix_dimensions <- unlist(str_match(line[1], pattern = "^(\\d{1,4}) (\\d{1,4})"))
  matrix_dimensions <- as.numeric(matrix_dimensions[-1])
  
  # Matriz de pesos
  M <- matrix(NA, nrow = matrix_dimensions[1], ncol = matrix_dimensions[2])
  
  col = 1
  row <- 1
  for (i in 2:length(line)){
    
    line.splited <- str_split(line[i], "\\s+", n = Inf, simplify = FALSE)
    line.splited <- as.numeric(unlist(line.splited))
    
    for(j in 2:length(line.splited)){
      M[row, col] = line.splited[j]
      
      col = col + 1
    }
    
    # se chegou ao final, refaz as colunas
    if(col==(matrix_dimensions[2] + 1)){
      row = row+1
      col=1
    }
    
  }
  
  # Salva matriz no diretorio
  file.out <- file.path(output_dir, sprintf("%s.rds", item))
  saveRDS(M, file = file.out)
}
