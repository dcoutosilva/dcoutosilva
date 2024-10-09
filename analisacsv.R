# Verificar se os pacotes estão carregados e instalados, se necessário
pacotesRequisitados <- c("tidyverse", 
                         "tidyr", 
                         "data.table", 
                         "gridExtra", 
                         "grid",
                         "glue")

for (p in pacotesRequisitados) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

data <- glue("~/Downloads/Amilcare/tecnico/Relatórios/3Bimestre/{Sys.Date()}")
#setwd(data)

contador <- 0

# Definindo a função que realiza o processo de manipulação e exportação dos dados
processa_arquivo <- function() {
  # Escolha do arquivo
  arquivo_csv <- file.choose()
  
  # Leitura do arquivo csv
  dados <- read.csv(arquivo_csv, fileEncoding = 'UTF16', header = TRUE, sep = "\t")
  
  # Excluir algumas colunas que não são necessárias
  dados_limpos <- dados %>% select(-matches(c("X.[0-9]")))
  
  # Separar os dados das atividades dos dados de identificação
  atividades <- dados_limpos %>% select(-1, -2, -3)
  
  # Criar uma variável para armazenar apenas os dados de identificação
  iden <- dados_limpos %>% select(1, 2, 3)
  
  # Dentro de atividades, se estiver concluído será 1, senão é zero
  atividades <- lapply(atividades, function(x) ifelse(x == "Concluído", 1, 0))
  
  # Gerando ele como data.frame
  atividades_df <- data.frame(atividades)
  
  # Criando mais uma coluna para somar todos os concluídos, ou seja todos os 1 por aluno
  atividades_df$total_por_aluno <- rowSums(atividades_df)
  atividades_df$SOMATODAS <- atividades_df$total_por_aluno + atividades_df$total_por_aluno
  
  # Criando mais uma coluna para mostrar a porcentagem dos alunos que concluíram
  atividades_df$porc_por_aluno <- (atividades_df$total_por_aluno / length(atividades)) * 100
  
  # Tornando data.frame
  iden_df <- data.frame(iden)
  
  # Junção de iden e atividades
  dados_prontos <- cbind(iden_df, atividades_df)  
  
  # Transformando em tibble
  df <- as_tibble(dados_prontos)
  
  # Dados prontos
  if (contador < 1) {
    export_dados <- select(df, 1, total_por_aluno, porc_por_aluno)  
  } else {
    export_dados <- select(df, total_por_aluno, porc_por_aluno)
  }
  
  # Nome do arquivo
  arquivo_nome <- switch(basename(arquivo_csv),
                         "progress.carreira_e_compet__ncias_para_o_mercado_de_trabalho_____3___bimestre___2024_03___sis.csv" = "Carreiras ",
                         "progress.intelig__ncia_artificial_____3___bimestre___2024_03.csv" = "IA",
                         "progress.l__gica_e_linguagem_de_programa____o_____3___bimestre___2024_03.csv" = "Lógica",
                         "progress.processos_de_desenvolvimento_de_software_e_metodologias___geis_____3___bimestre___2024_03.csv" = "Met Ageis ",
                         "progress.redes_de_computadores_e_seguran__a_da_informa____o_na_nuvem_____3___bimestre___2024_03.csv" = "Redes ",
                         "progress.versionamento_de_c__digo_e_sistemas_de_mensageria_____3___bimestre___2024_03.csv" = "Versionamento",
                         basename(arquivo_csv))
  
  list(export_dados = export_dados, arquivo_nome = arquivo_nome, dados_completos = df)
}

# Inicializar uma lista para armazenar os resultados e os nomes dos arquivos
resultados_lista <- list()
titulos_lista <- c()
dados_completos_lista <- list()

# Loop para repetir a função conforme desejado pelo usuário
repeat {
  resultado <- processa_arquivo()
  resultados_lista <- append(resultados_lista, list(resultado$export_dados))
  titulos_lista <- append(titulos_lista, resultado$arquivo_nome)
  dados_completos_lista <- append(dados_completos_lista, list(resultado$dados_completos))
  contador <- contador + 1
  resposta <- readline(prompt = "Deseja processar outro arquivo? (s/n): ")
  
  if (tolower(resposta) != "s") {
    # Combinar todos os data.frames em um único data.frame
    resultados_combinados <- do.call(cbind, resultados_lista)
    
    # Adicionar uma coluna com a soma de todas as atividades concluídas para todos os alunos
    resultados_combinados$total_atividades_concluidas <- rowSums(resultados_combinados[, grepl("total_por_aluno", colnames(resultados_combinados))])  
    
    # Garantir que todos os nomes de colunas sejam únicos
    colnames(resultados_combinados) <- make.unique(colnames(resultados_combinados))
    
    # Encontrar os 10 alunos com mais atividades concluídas
    top_alunos <- resultados_combinados %>%
      arrange(desc(total_atividades_concluidas)) %>%
      select(1, total_atividades_concluidas) %>%
      head(10)
    
    # Encontrar os 10 alunos com menos atividades concluídas
    bottom_alunos <- resultados_combinados %>%
      arrange(total_atividades_concluidas) %>%
      filter(total_atividades_concluidas > 0 ) %>% 
      select(1, total_atividades_concluidas) %>%
      head(10)
    
    # Abrir o dispositivo gráfico para o PDF em formato paisagem
    namefile <- glue("RelatórioAVAtecDS-{Sys.Date()}.pdf")
    pdf(namefile, height = 15, width = 25, paper = "special", onefile = TRUE)
    
    # Página 1: Relatório de Resultados
    # Criar viewport para o título principal
    pushViewport(viewport(height = 0.1, width = 1, just = "center", y = 0.95))
    grid.text("Relatório de Resultados", gp = gpar(fontsize = 16))
    popViewport()
    
    # Adicionar a data
    grid.text(format(Sys.Date(), "%d-%m-%y"), y = unit(0.93, "npc"), just = "center", gp = gpar(fontsize = 16))
    
    # Adicionar a tabela combinada ao PDF
    gridExtra::grid.table(resultados_combinados)
    
    # Concatenar as disciplinas em uma string separada por quebra de linha "\n"
    texto_rodape <- paste(titulos_lista, collapse = "\t\t\t")
    
    # Criar viewport para o rodapé
    pushViewport(viewport(height = 0.15, width = 1, y = 0.05)) # Ajuste a altura conforme necessário
    grid.text(texto_rodape, gp = gpar(fontsize = 14), just = "left")
    popViewport()
    
    # Página 2: Top 5 Alunos com mais Atividades Concluídas e 10 Alunos com menos Atividades Concluídas
    grid.newpage()
    
    # Definir layout da página
    pushViewport(viewport(layout = grid.layout(3, 2)))
    
    # Título para os top 5 alunos
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
    grid.text("Top 10 Alunos com Mais Atividades Concluídas", gp = gpar(fontsize = 16))
    popViewport()
    
    # Tabela dos top 5 alunos
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
    gridExtra::grid.table(top_alunos)
    popViewport()
    
    # Título para os 10 alunos com menos atividades
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    grid.text("10 Alunos com Menos Atividades Concluídas", gp = gpar(fontsize = 16))
    popViewport()
    
    # Tabela dos 10 alunos com menos atividades
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
    gridExtra::grid.table(bottom_alunos)
    popViewport()
    # Fecha o dispositivo gráfico
    dev.off()
    
    # Abrir o PDF resultante (apenas no macOS, para outros SOs, use o comando apropriado)
    system(glue("open {namefile}"))
    
    break
  }
}
