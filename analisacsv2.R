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

#install.packages(grid)
#library(grid)

data <- glue("~/Downloads/amilcare/tecnico/Relatórios/4Bimestre/{Sys.Date()}")

#verifica se o caminho existe, senão existir, ele já cria e seta na pasta.

if (!dir.exists(data)){
  dir.create(data, recursive = TRUE)
}

setwd(data)
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
                         "progress.carreira_e_compet__ncias_para_o_mercado_de_trabalho_em_desenvolvimento_de_sistemas_____4___bim___2024_04.csv" = "Carreiras ",
                         "progress.intelig__ncia_artificial_____4___bimestre___2024_04.csv" = "IA",
                         "progress.l__gica_e_linguagem_de_programa____o_____4___bimestre___2024_04.csv" = "Lógica",
                         "progress.processos_de_desenvolvimento_de_software_e_metodologias___geis_____4___bimestre___2024_04.csv" = "Met Ageis ",
                         "progress.redes_de_computadores_e_seguran__a_da_informa____o_na_nuvem_____4___bimestre___2024_04.csv" = "Redes ",
                         "progress.versionamento_de_c__digo_e_sistemas_de_mensageria_____4___bimestre___2024_04.csv" = "Versionamento",
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
    
    # Garantir que todos os nomes de colunas sejam únicos
    colnames(resultados_combinados) <- make.unique(colnames(resultados_combinados))
    
    # Adicionar uma coluna com a soma de todas as atividades concluídas para todos os alunos
    resultados_combinados$total_atividades_concluidas <- rowSums(
      resultados_combinados[
        , grepl(
          "total_por_aluno", colnames(
            resultados_combinados)
          )
        ]
      )  
    
    #Filtrar para remover as linhas onde o total de atividades concluídas é igual a zero.
    #E ordenação em ordem alfabética
    resultados_combinados <- resultados_combinados %>% 
      filter(total_atividades_concluidas > 0) %>% 
      arrange_at(1)
    
    # Encontrar os 10 alunos com mais atividades concluídas
    top_alunos <- resultados_combinados %>%
      arrange(desc(total_atividades_concluidas)) %>%
      select(1, total_atividades_concluidas) %>%
      head(10)
    
    # Encontrar os 10 alunos com menos atividades concluídas
    bottom_alunos <- resultados_combinados %>%
      arrange(total_atividades_concluidas) %>%
      #filter(total_atividades_concluidas > 0 ) %>% 
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
    grid.text(format(
      Sys.Date(),
      "%d-%m-%y"),
      y = unit(
        0.93,
        "npc"), 
      just = "center",
      gp = gpar(fontsize = 16))
    
    #Colocar Tema Azul na Grid
    tema_azul <- ttheme_minimal(
      core=list(bg_params = list(
        fill = blues9[1:5],
        col = NA),
                fg_params = list(fontface=3)),
      colhead = list(fg_params = list(
        col="navyblue", 
        fontface = 4L)),
      rowhead = list(fg_params = list(
        col = "black", 
        fontface = 3L))
    )
    #display.brewer.all() 
    # Adicionar a tabela combinada ao PDF
    gridExtra::grid.table(resultados_combinados, theme = tema_azul)
    
    # Concatenar as disciplinas em uma string separada por quebra de linha "\t"
    texto_rodape <- paste(titulos_lista, collapse = "\t\t\t\t\t\t\t\t\t")
    
    # Criar viewport para o rodapé
    pushViewport(viewport
                 (height = 0.15, 
                   width = 1, 
                   y = 0.05)) # Ajuste a altura conforme necessário
    grid.text(texto_rodape, 
              gp = gpar(fontsize = 14),
              just = "center")
    popViewport()
    
    
    # Página 2: Top 5 Alunos com mais Atividades Concluídas e 10 Alunos com menos Atividades Concluídas
    #grid.newpage()
    
    # Definir layout da página
    #pushViewport(viewport(layout = grid.layout(3, 2)))
    
    # Título para os top 5 alunos
    #pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
    #grid.text("Top 10 Alunos com Mais Atividades Concluídas", gp = gpar(fontsize = 16))
    #popViewport()
    
    # Tabela dos top 5 alunos
    #pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
    #gridExtra::grid.table(top_alunos)
    #popViewport()
    
    # Título para os 10 alunos com menos atividades
    #pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    #grid.text("10 Alunos com Menos Atividades Concluídas", gp = gpar(fontsize = 16))
    #popViewport()
    
    # Tabela dos 10 alunos com menos atividades
    #pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
    #gridExtra::grid.table(bottom_alunos)
    #popViewport()
    
    plot_bar <- function(data, title) {
      media <- mean(resultados_combinados$total_atividades_concluidas)
      
      # Cria o grafico.
      ggplot(data,
             aes(x = reorder(data[[1]],
                             total_atividades_concluidas),
                 y = total_atividades_concluidas)) +
        geom_bar(stat = "identity",
                 fill = "steelblue",
                 color = "black",
                 width = 0.7) +
        geom_hline(yintercept = media,
                   color = "red",
                   linetype = "dashed",
                   size = 1) +
        geom_text(aes
                  (label = total_atividades_concluidas),
                  vjust = -0.5, 
                  color = "black", 
                  size = 3.5) +
        labs(
          title = title,
          x = "Aluno",
          y = "Total de Atividades Concluídas"
        ) +
        theme_minimal(base_size = 12) +  # Tema simples e agradável
        theme(
          plot.title = element_text(hjust = 0.5,
                                    size = 14, 
                                    face = "bold"),  # Centralizar e destacar o título
          axis.text.x = element_text(size = 15,
                                     angle = 90,
                                     hjust = 1),  # Nome do aluno com orientação 90 graus
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12)
        )      +
        annotate("text", 
                 x = mean(seq_along(data[[1]])), #seq_along semelhante a enumerate
                 y = media,#altura de acordo com a media
                 label = sprintf("Média: %.1f", media), 
                 hjust =0.5, vjust = -1, #centraliza a label horinz e vertical
                 color = "red", size = 8, fontface = "italic")
    }
    
    #Criar gráficos para os top e bottom alunos
    plot_top <- plot_bar(top_alunos, "10 Alunos com mais Atividades")
    plot_bottom <- plot_bar(bottom_alunos, "10 Alunos com menos Atividades")
    
    #grid.newpage()
    grid.arrange(plot_top, 
                 plot_bottom, 
                 ncol=2)
    
    # Fecha o dispositivo gráfico
    dev.off()
    
    # Abrir o PDF
    system(glue("open {namefile}"))
    
    break
  }
}
