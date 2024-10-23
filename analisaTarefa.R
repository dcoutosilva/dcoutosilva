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

contador <- 0
lista <- list()

processa_arquivo <- function() {
  arquivo_csv <- file.choose()
  
  dados <- read.csv(arquivo_csv, sep = ";", header = FALSE)
  
  dados_limpos <- dados %>% 
    select(-matches(c("V3", "V5")))
  
  atividades <- dados_limpos %>% select(1, 3)
  nome <- dados_limpos %>% select(2)
  
  # Combine into a single data frame
  dados_prontos <- cbind(nome, atividades)
  
  df <- as_tibble(dados_prontos)
  
  return(df)
}

repeat {
  resultado <- processa_arquivo()
  lista[[contador + 1]] <- resultado  # Add result to the list
  contador <- contador + 1
  resultados_combinados <- resultados_combinados %>% arrange_at(1)
  resposta <- readline(prompt = "Deseja processar outro arquivo? (s/n): ")
  if (tolower(resposta) != 's') {
    
    
    # Combine all data frames row-wise (rbind)
    resultados_combinados <- do.call(rbind, lista)
    
    # Garantir que todos os nomes de colunas sejam únicos
    colnames(resultados_combinados) <- make.unique(colnames(resultados_combinados))
    
    # Gerar o relatório em PDF
    namefile <- glue("RelatórioAVAtecDS-{Sys.Date()}.pdf")
    pdf(namefile, height = 15, width = 25, paper = "special", onefile = TRUE)
    
    # Título
    pushViewport(viewport(height = 0.1, width = 1, just = "center", y = 0.95))
    grid.text("Relatório de Tarefas", gp = gpar(fontsize = 16))
    popViewport()
    
    # Data
    grid.text(format(Sys.Date(), "%d-%m-%y"), y = unit(0.93, "npc"), just = "center", gp = gpar(fontsize = 16))
    
    # Tema da tabela
    tema_azul <- ttheme_minimal(
      core = list(bg_params = list(fill = blues9[1:5], col = NA), fg_params = list(fontface = 3)),
      colhead = list(fg_params = list(col = "navyblue", fontface = 4L)),
      rowhead = list(fg_params = list(col = "black", fontface = 3L))
    )
    
    # Tabela
    gridExtra::grid.table(resultados_combinados, theme = tema_azul)
    
    # Fecha o dispositivo gráfico
    dev.off()
    
    # Abrir o PDF
    system(glue("open {namefile}"))
    
    break
  }
}
