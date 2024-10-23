#Author: Danilo Couto Silva danilocoutosilva@prof.educacao.sp.gov.br
#Discribe: Analisa Arquivos CSV do Portal Educação Profissional do Ensino Técnico SEE/SP
#Version: 2.0
#License: GPL

# Verificar se os pacotes estão carregados e instalados, se necessário..
pacotesRequisitados <- c("tidyverse", 
                         "tidyr", 
                         "data.table", 
                         "gridExtra", 
                         "grid",
                         "glue",
                         "RColorBrewer")


for (p in pacotesRequisitados) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}


processa_arquivo <- function() {
  arquivo_csv <- file.choose()
  
  dados <- read.csv(arquivo_csv 
                    ,sep=";"
                    ,header = FALSE
                   )
   dados_limpos <- dados %>% 
     select(-matches(
       c(
       "V3","V5"
       )
     )
     )
   atividades <- dados_limpos %>% 
     select(1,3)
   nome <- dados_limpos %>% select(2)
   
   atividades_df <- data_frame(atividades)
   nome_df <- data_frame(nome)
}