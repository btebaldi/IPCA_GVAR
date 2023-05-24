
# Setup -------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(lubridate)


# User defined functions --------------------------------------------------



# Data load ---------------------------------------------------------------

# carrega dicionario de dados
dicionario <- read_excel("database/dicionario.xlsx")

# Carrega os pesos do IPCA
IPCA_pesos <- read_excel("database/IPCA_pesos.xlsx", 
                         range = "B2:NO333")
IPCA_pesos$Data <- lubridate::ymd(IPCA_pesos$Data, truncated = 1)

M_pesos <- IPCA_pesos %>% filter(Data >= "2021-09-01") %>% select(-Data) %>% colMeans()

# carrega a variacao do IPCA
tbl_IPCA <- read_excel("database/IPCA-baseOx_v1.xlsx") 
tbl_IPCA$Data <- lubridate::ymd(tbl_IPCA$Data, truncated = 1)

# filta a base para comecar em 2000
tbl_IPCA <- tbl_IPCA %>% dplyr::filter(Data >= "2006-07-01")

# estatisticas descritivas
summary(tbl_IPCA)

checkDir <- function(dicionario, tbl_IPCA){
  dicionario$count_zero = as.integer(NA)
  dicionario$count_NA = as.integer(NA)
  
  for(i in seq_len(nrow(dicionario)) ){
    
    col <- dicionario[[i, "NOME_OX"]]
    dicionario[i, "count_zero"] <- sum(tbl_IPCA[[col]] == 0, na.rm = TRUE)
    dicionario[i, "count_NA"] <- sum(is.na(tbl_IPCA[[col]]))
  }
  
  return(dicionario)
}

ids <- c(5,6)

join_colunas <- function(ids, dicionario, M_pesos, tbl_IPCA){
  
  linhas_dic <- which(dicionario$Item %in% ids)
  
  # busca nome do peso
  nomes_peso <- dicionario[linhas_dic, "Peso_Item_IPCA"] %>% pull()
  new_nome_peso <- paste(c("NW", ids), collapse = "_")
  
  # busca nome do Ox
  nomes_ox <- dicionario[linhas_dic, "NOME_OX"] %>% pull()
  new_nome_ox <- paste(c("N", ids), collapse = "_")
  
  # busca descricao do item
  descricao <- dicionario[linhas_dic, "Descricao"] %>% pull()
  new_descricao <- paste(descricao, collapse = " + ")
  
  # join col
  idx_w <- which(x = names(M_pesos) %in% nomes_peso)
  w <- M_pesos[idx_w]
  
  # w_new <- w/sum(w)
  # 
  # for(name in nomes_ox){ tbl_IPCA[[name]][is.na(tbl_IPCA[name])] <- 0 }
  # 
  # new_var <- data.matrix(tbl_IPCA[nomes_ox]) %*% as.matrix(w_new)
  
  # ajuste de dados faltantes
  new_var <- rep(0, nrow(tbl_IPCA))
  for(i in seq_len(nrow(tbl_IPCA))){
    
    line_M <- tbl_IPCA[i, nomes_ox] %>% data.matrix()
    w_new <- w
    if(any(is.na(line_M))){
      w_new[is.na(line_M)] <- 0
      w_new[!is.na(line_M)] <- w_new[!is.na(line_M)] + (w[!is.na(line_M)]/sum(w[!is.na(line_M)])) * sum(w[is.na(line_M)])
      
      line_M[is.na(line_M)] <- 0
    }
    
    new_var[i] <- line_M %*% w_new/sum(w_new)
  }
  
  new_w <- sum(w)
  
  
  # Atualiza tabela
  tbl_IPCA[new_nome_ox] <- new_var
  tbl_IPCA[nomes_ox] <- NULL
  
  
  # Atualiza matriz de peso
  # M_pesos <- M_pesos[-c(idx_w_1, idx_w_2)]
  M_pesos <- c(M_pesos, new_w)
  names(M_pesos)[length(M_pesos)] <- new_nome_peso
  
  
  # Atualiza dicionario
  dicionario <- dicionario %>% add_row(Peso_Item_IPCA = new_nome_peso,
                                       Nome_Item_IPCA = "MULTIPLO",
                                       Item = max(dicionario$Item+1),
                                       NOME_OX = new_nome_ox,
                                       Descricao = new_descricao)
  
  return(list(dicionario, M_pesos, tbl_IPCA))
  
}


# Juncao das colunas ------------------------------------------------------

mlist <- join_colunas(c(5,6), dicionario, M_pesos, tbl_IPCA)
mlist <- join_colunas(c(9,10), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(13,14), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(15,16), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(19,20,21), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(26,32), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(27,28), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(47,46,58), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(42,60), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(53,59), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(54,55), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(62,63), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(64,74, 76), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(78,66), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(91,97,95,100,92,93,95,106,81,98,99,94,90), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(104,105), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(107,108), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(117,118), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(119,116), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(129,130,138,139), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(136,137), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(140,145), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(141,142), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(154,153,158), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(166,165), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(167,168), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(175,185,180), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(178,177), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(183,184), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(186,193,194,195,196,187), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(209,206,213), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(204,202), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(220,221), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(222,224), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(227,230), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(228,231), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(254,255), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(256,257), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(271, 272,269), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(343,349), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(302,303,304), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(305,306), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(307,309, 310), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(316,317, 318), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(330,331,336), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(332,333), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(341,339,348), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(337,345), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(358,357,354), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(360,361,362), mlist[[1]], mlist[[2]], mlist[[3]])
mlist <- join_colunas(c(365,368), mlist[[1]], mlist[[2]], mlist[[3]])

mlist <- join_colunas(c(376,372,377,373), mlist[[1]], mlist[[2]], mlist[[3]])

checkDir(mlist[[1]], mlist[[3]]) %>% arrange(desc(count_NA)) -> dir

tbL_w <- as_tibble(mlist[[2]])
tbL_w$name <- names(mlist[[2]])

i <- 1
tbL_w$used <- 0
tbL_w$NOME_OX <- as.character(NA)
for(i in seq_len(nrow(tbL_w))){
  name <- tbL_w$name[i]
  idx_dic <- which(mlist[[1]]$Peso_Item_IPCA == name)
  name_var <- mlist[[1]]$NOME_OX[idx_dic]
  
  idx_col <- which(colnames(mlist[[3]]) == name_var)
  if(length(idx_col)>0){
    tbL_w$used[i] <- 1
    tbL_w$NOME_OX[i] <- name_var
  }
}

mlist[[2]] <- tbL_w

names(mlist) <- c("Dicionario", "Pesos",  "Dados")

writexl::write_xlsx(mlist, path = "./database/IPCA-baseOx_v2.xlsx")













