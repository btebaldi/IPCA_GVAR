#' SCRIPT QUE CONSTRI MATRIZ DE PESOS A PARTIR DE UM EXCEL
#' 
#' Author: Bruno Tebaldi
#' 
#' Date: 2023-05-28


# Setup -------------------------------------------------------------------
rm(list = ls())

library(readxl)


# User defined functions --------------------------------------------------

SaveMatrixForOx <- function(Matrix, file_name, comment = NA, path = NA){
  if(is.na(comment)){comment = file_name}
  
  if(is.na(path)){path = "./export/W_mat"}
  
  # valida se o diretorio existe
  if(!dir.exists(path)){
    stop(sprintf("directory does not exist. [%s]", path))
  }

  # Full path do arquivo a ser escrito
  file.name <- file.path(path, file_name)
  
  #  Escreve o comentario na primeira linha e fecha o arquivo
  fileConn <- file(file.name)
  writeLines( text = sprintf("%1$d %2$d // A %1$d by %2$d matrix (%3$s)", 
                             dim(Matrix)[1],
                             dim(Matrix)[2],
                             comment),
              con = fileConn)
  close(fileConn)
  
  # Escreve todas as outras informacoes
  write.table(x = Matrix, file = file.name,
              append = TRUE,
              col.names = FALSE,
              row.names = FALSE)
}


# Data load ---------------------------------------------------------------
w_Mat <- read_excel("database/Pesos2.xlsx")


#

# Construcao da matriz de pesos do GVAR X_t -------------------------------

M <- matrix(w_Mat$weight, nrow = nrow(w_Mat), ncol = nrow(w_Mat), byrow = FALSE)

#  ajusta diagonal para ser zero
diag(M) <- 0


# ajusta colunas para somar 1
for(i in seq_len(ncol(M))){
  M[,i] <- M[,i]/sum(M[,i])
}


# Save matrix
SaveMatrixForOx(Matrix = M,
                file_name = "PesoIpca.mat", 
                path = "./Ox/mat_files/W_mat")


# Construcao da matriz de pesos do GVAR X*_t --------------------------

M <- matrix(w_Mat$weight, nrow = 1, ncol = nrow(w_Mat), byrow = FALSE)

# Save matrix
SaveMatrixForOx(Matrix = M,
                file_name = "W_w.mat", 
                path = "./Ox/mat_files/W_mat")