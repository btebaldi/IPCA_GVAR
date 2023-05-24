
# Clear all variables
rm(list = ls())

# bibliotecas utilizadas
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)

#  Carrega base de dados do IPCA
IPCA_db <- read_excel("database/IPCA-TodosAnos-unstacked.xlsx", 
                      sheet = "Planilha1",
                      range = cell_limits(ul = c(NA, NA), lr = c(NA, NA)),
                      na = c("#N/A"))

#  Coloca nome na primeira coluna
colnames(IPCA_db)[1] <- "Date"
IPCA_db$Date <- lubridate::ymd(IPCA_db$Date, truncated = 1)

# leitura da base de dados de colunas utilizadas. No Futuro esse processo de
# identificacao das variaveis poderia ser automatico mas o processo de
# agrupamento ainda eh um problema visto que o mesmo eh feito de forma mnual.
dicionario <- read_excel("database/dicionario.xlsx", 
                         # sheet = "Planilha1",
                         range = cell_limits(ul = c(NA, NA), lr = c(NA, NA)))

dicionario <- dicionario %>% select(Cod_Item, Grupo, Descricao)

# determina quais as informacoes que temos na base de dados.
tx_names <- colnames(IPCA_db)

# Constroi o dicionario versao 2
tbl.dic <- tibble(Original = as.character(NA),
                  Tipo = as.character(NA),
                  Id = as.character(NA),
                  .rows = length(tx_names))

# para cada coluna na base de dados organiza ela no dicionorio v2
for(i in seq_along(tx_names)){
  tbl.dic$Original[i] <- tx_names[i]
  
  tbl.dic[i,c("Tipo", "Id")] <- stringr::str_split(string = colnames(IPCA_db)[i], pattern = "-", simplify = TRUE)
}

# extracao do codigo do item a partir do nome da coluna. Algumas colunas nao tem
# codigo e por isso sao excluidas com o comando (is.na)
tbl.dic <- tbl.dic %>% 
  filter(Original != "Date") %>% 
  mutate(Id = stringr::str_replace(string = Id, pattern = "Brasil", replacement = "")) %>% 
  mutate(Id = as.numeric(Id)) %>% 
  filter(!is.na(Id))

tbl.dic <- tbl.dic %>% 
  pivot_wider(id_cols = Id, names_from = Tipo, values_from = Original )

# seleciona apenas as colunas que queremos
tbl.dic <- dicionario %>%
  left_join(tbl.dic, by = c("Cod_Item"="Id") ) %>% 
  rename(Item_col = I, Weight_col = WI) %>% 
  mutate(weight = as.numeric(NA),
         Id_OX = as.integer(NA),
         NOME_OX = as.character(NA))

# determina o Id e Nome no OX (no OX os grupos devem ser sequenciais)
unique_groups <- unique(tbl.dic$Grupo)
for(i in seq_along(unique_groups)){
  idx <- which(tbl.dic$Grupo == unique_groups[i])
  
  tbl.dic$Id_OX[idx] <- i
  tbl.dic$NOME_OX[idx] <- sprintf("R_%d_IPCA", i)
}

# Determina o peso de cada item (ultima data valida)
# Eh possivel que isso gere problema caso a data nao exista dados
for(i in seq_along(tbl.dic$Cod_Item)){
  col_idx <- which(tx_names == tbl.dic$Weight_col[i])
  row_idx <- which.max(IPCA_db$Date)
  tbl.dic$weight[i] <- IPCA_db[[row_idx, col_idx]]
}


# grava dicionario para utilizacao futura.
# tbl.dic

# cleanup
rm(list = dplyr::setdiff(ls(), c("tbl.dic", "IPCA_db")))

IPCA_db <- IPCA_db %>% dplyr::filter(Date >= "2006-07-01")

# estatisticas descritivas
summary(IPCA_db)

# Construcao de series de dados -------------------------------------------








# User defined functions --------------------------------------------------

# dicionario <- tbl.dic
# tbl_IPCA <- IPCA_db
# linhas_dic <- c(5,6)
constroi_serie <- function(linhas_dic, dicionario, tbl_IPCA){
  # determina qual a(s) coluna(s) com os dados
  nomes_col <- dicionario$Weight_col[linhas_dic]
  
  # determina qual o(s) pesos(s) com os dados
  w <- dicionario$weight[linhas_dic]
  
  # ajuste de dados faltantes
  new_var <- rep(0, nrow(tbl_IPCA))
  i <- 1
  for(i in seq_len(nrow(tbl_IPCA))){
    
    # busca serie de dados
    line_M <- tbl_IPCA[i, nomes_col] %>% data.matrix()
    
    # vetor com novo peso
    w_new <- w
    
    # se contem algum NA na serie, faz ajuste de peso e da serie
    if(any(is.na(line_M))){
      w_new[is.na(line_M)] <- 0
      w_new[!is.na(line_M)] <- w_new[!is.na(line_M)] + (w[!is.na(line_M)]/sum(w[!is.na(line_M)])) * sum(w[is.na(line_M)])
      
      line_M[is.na(line_M)] <- 0
    }
    
    # faz a multiplicacao vetorial com pesos para determinar a nova serie
    new_var[i] <- line_M %*% w_new/sum(w_new)
  }
  
  # Determina o novo peso da serie
  new_w <- sum(w)
  
  # retorna uma lista com a serie e o peso
  return(list("new_var" = new_var, "new_w" = new_w))
}


# Data load ---------------------------------------------------------------

Matrix.Data <- matrix(data = NA, nrow = nrow(IPCA_db), ncol = length(unique(tbl.dic$Id_OX)))

colnames(Matrix.Data) <- sprintf("R_%d_IPCA", 1:ncol(Matrix.Data) )

tbl.dic.expandido <- tbl.dic
i <- 5
for (i in unique(tbl.dic$Id_OX)) {
  linhas_dic <- which(tbl.dic$Id_OX == i)
  
  lst <- constroi_serie(linhas_dic = linhas_dic, dicionario = tbl.dic, tbl_IPCA = IPCA_db)
  
  Matrix.Data[,i] <- lst$new_var
  
  # Se for mais de um item adiciona o mesmo no dicionario
  if (length(linhas_dic) > 1){
    descricao <- tbl.dic$Descricao[linhas_dic]
    new_descricao <- paste(descricao, collapse = " + ")
    
    tbl.dic.expandido <- tbl.dic.expandido %>% add_row(Descricao = new_descricao,
                                                       weight = lst$new_w,
                                                       Id_OX = i)
    
  }
  
}


table.Data <- as_tibble(Matrix.Data) %>% mutate(Date = IPCA_db$Date) %>% select(Date, everything())
tbl.dic.expandido

writexl::write_xlsx(table.Data, path = "./database/IPCA-baseOx_v5.xlsx")











