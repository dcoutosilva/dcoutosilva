#Author: Danilo Couto Silva danilocoutosilva@prof.educacao.sp.gov.br
#Discribe: Analisa Arquivos CSV do Portal Educação Profissional do Ensino Técnico SEE/SP
#Version: 2.0
#License: GPL

# Verificar se os pacotes estão carregados e instalados, se necessário..
pacotesRequisitados <- c("tidyverse"
                         ,"tidyr"
                         ,"data.table"
                         ,"gridExtra"
                         ,"grid"
                         ,"glue"
                         ,"RColorBrewer"
                         ,"stringr"
                         ,"openxlsx"
                         ,"png"
                      )


for (p in pacotesRequisitados) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, repos="https://brieger.esalq.usp.br/CRAN/" ,dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

ano_atual <- format(Sys.Date(), "%Y")
# --- Configurações Iniciais ---
bimestre <- readline(prompt = "Qual Bimestre? : ")
serie <- readline(prompt = "Qual turma será impressa o relatório? (2/3) : ")

# Define o caminho base conforme a série selecionada
if (serie == "3") {
  data <- glue("/home/danilo/Downloads/amilcare/{ano_atual}/relatorios/{bimestre}Bimestre/3A/{Sys.Date()}")  
  serie_nomenclatura <- "3A"
} else { 
  data <- glue("/home/danilo/Downloads/amilcare/{ano_atual}/relatorios/{bimestre}Bimestre/2B/{Sys.Date()}")
  serie_nomenclatura <- "2B"
}
print(data)
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
  dados <- read.csv(arquivo_csv, 
                    fileEncoding = 'UTF16', 
                    header = TRUE, 
                    sep = "\t")#delimitador por tab
  
  # Excluir algumas colunas que não são necessárias
  dados_limpos <- dados %>%
    select(-matches(
      c(
        "X.[0-9]")
    )
    )
  
  # Separar os dados das atividades dos dados de identificação
  atividades <- dados_limpos %>% 
    select(-1, -2, -3)
  
  # Criar uma variável para armazenar apenas os dados de identificação
  iden <- dados_limpos %>% 
    select(1, 2, 3)
  
  # Dentro de atividades, se estiver concluído será 1, senão é zero
  atividades <- lapply(
    atividades, 
    function(x) ifelse(
      x == "Concluído", 1, 0))
  
  # Gerando ele como data.frame
  atividades_df <- data.frame(atividades)
  
  # Criando mais uma coluna para somar todos os concluídos, 
  #ou seja todos os 1 por aluno
  atividades_df$total_por_aluno <- rowSums(atividades_df, na.rm = TRUE)
  
  atividades_df$SOMATODAS <- atividades_df$total_por_aluno + 
    atividades_df$total_por_aluno
  
  # Criando mais uma coluna para mostrar 
  #a porcentagem dos alunos que concluíram
#  atividades_df$porc_por_aluno <- (
#    atividades_df$total_por_aluno / length(
#      atividades)) * 100
  atividades_df$porc_por_aluno <- glue("{round((atividades_df$total_por_aluno / length(atividades)) * 100, 1)}%")  
  # Tornando data.frame
  iden_df <- data.frame(iden)
  
  # Junção de iden e atividades
  dados_prontos <- cbind(iden_df, atividades_df)  
  
  # Transformando em tibble
  df <- as_tibble(dados_prontos)
  
  # Dados prontos
  if (contador < 1) {
    export_dados <- select(
      df, 1, total_por_aluno, porc_por_aluno)  
  } else {
    export_dados <- select(
      df, total_por_aluno, porc_por_aluno)
  }
  mapeamento_cursos <- c(
    "2_51000" = "Lógica e Linguagem de Programação"
    ,"3_51006" = "Programação Mobile"
    ,"3_51008" = "Programação Back-End"
    ,"3_51009" = "Programação Front-End"
    ,"2_51002" = "Redes e Segurança de Computadores"
    ,"2_51003" = "Processos de Desenvolvimento de Software"
    ,"3_51011" = "Projeto Multidisciplinar em Desenvolvimento de Sistemas"
    ,"3_51010" = "Modelagem e Desenvolvimento de Banco de Dados"
    #,"2_51005" = "Carreira e Competências para o Mercado de Trabalho em Desenvolvimento de Sistemas"
    ,"2_9929" =  "Carreira e Competências para o Mercado de Trabalho em Desenvolvimento de Sistemas"
    ,"3_51004" = "Inteligência Artificial"
    ,"3_51001" = "Versionamento de Código e Sistemas de Mensageria"
  )#TODO ARRUMAR O MAPEAMENTO DOS CURSOS
  nome_base <- basename(arquivo_csv)
  
  # 2. Extrair o código final (ex: 2_51000) usando expressão regular
  # Este padrão busca especificamente o final do nome do arquivo antes do .csv
  codigo_extraido <- str_extract(nome_base, "[23]_\\d{4,5}(?=\\.csv)")
  
  # 3. Verificar se o código existe no seu mapeamento
  arquivo_nome <- if (!is.na(codigo_extraido) && codigo_extraido %in% names(mapeamento_cursos)) {
    mapeamento_cursos[codigo_extraido]
  } else {
    nome_base # Caso não encontre, mantém o nome original para não ficar vazio
  }
  
  # Retornar o resultado sem o atributo de nome do vetor
  list(export_dados = export_dados, arquivo_nome = unname(arquivo_nome), dados_completos = df)  
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
      #  filter(total_atividades_concluidas > 0) %>% 
      arrange_at(1)
    
    # Encontrar os 15 alunos com mais atividades concluídas
    top_alunos <- resultados_combinados %>%
      arrange(desc(total_atividades_concluidas)) %>%
      select(1, total_atividades_concluidas) 
    #head(15)
    
    # Abrir o dispositivo gráfico para o PDF em formato paisagem
    base_name <- ifelse(serie == "3", "RelatórioAVAtecDS-3A", "RelatórioAVAtecDS-2B")
    namefile_pdf <- glue("{base_name} - {Sys.Date()}.pdf")
    namefile_xlsx <- glue("{base_name} - {Sys.Date()}.xlsx")
    
    
    pdf(namefile_pdf, height = 15, width = 25, paper = "special", onefile = TRUE)
    
    # Página 1: Relatório de Resultados
    # Criar viewport para o título principal
    pushViewport(viewport(height = 0.1, width = 1, just = "center", y = 0.95))
    grid.text(glue("Relatório de Resultados - Turma {serie_nomenclatura}"), 
              gp = gpar(fontsize = 16, fontface = "bold"))    
    popViewport()
    
    img <- readPNG("/home/danilo/Downloads/amilcare/picture.png")
    g_img <- rasterGrob(img, interpolate = TRUE)
    # Viewport para a Imagem (Canto Superior Direito)
    # Aumentamos o width/height para 5cm para manter a imagem grande
    pushViewport(viewport(x = unit(0.88, "npc"), 
                          y = unit(0.92, "npc"), 
                          width = unit(5, "cm"), 
                          height = unit(5, "cm"),
                          just = c("left", "top"))) # Alinhamento preciso
    grid.draw(g_img)
    popViewport()
    
    # Adiciona a data centralizada 
    grid.text(format(
      Sys.Date(),
      "%d-%m-%y"),
      y = unit(
        0.93,
        "npc"), 
      just = "center",
      gp = gpar(fontsize = 16))
    
    # --- Ajuste Dinâmico da Tabela (Página 1) ---
    
    # 1. Definir larguras e tema base para medição
    largura_maxima_polegadas <- 23 
    tema_base <- ttheme_minimal(
      core = list(bg_params = list(fill = blues9[1:5], col = NA), fg_params = list(fontface = 3, fontsize = 9)),
      colhead = list(fg_params = list(col = "navyblue", fontface = 4L, fontsize = 10)),
      rowhead = list(fg_params = list(col = "black", fontface = 3L, fontsize = 9))
    )
    
    # 2. Criar tabela temporária para medir a largura real
    tabela_teste <- tableGrob(resultados_combinados, theme = tema_base)
    largura_atual <- convertWidth(sum(tabela_teste$widths), "inches", valueOnly = TRUE)
    
    # 3. Agora calculamos a escala baseada na medição real
    escala <- if (largura_atual > largura_maxima_polegadas) largura_maxima_polegadas / largura_atual else 1
    
    # 4. Criar o tema FINAL com a fonte escalada corretamente
    tema_final <- ttheme_minimal(
      core = list(
        bg_params = list(fill = blues9[1:5], col = NA),
        fg_params = list(fontface = 3, fontsize = 9 * escala)
      ),
      colhead = list(
        fg_params = list(col = "navyblue", fontface = 4L, fontsize = 10 * escala)
      ),
      rowhead = list(
        fg_params = list(col = "black", fontface = 3L, fontsize = 9 * escala)
      )
    )
    
    # 5. Gerar e desenhar a tabela definitiva
    tabela_final <- tableGrob(resultados_combinados, theme = tema_final)
    
    pushViewport(viewport(y = 0.5, height = 0.8, width = 0.95))
    grid.draw(tabela_final)
    popViewport()
    
    write.xlsx(resultados_combinados, file = namefile_xlsx, asTable = TRUE, overwrite = TRUE)
    
    # Concatenar as disciplinas em uma string separada por quebra de linha "\t"
    texto_rodape <- paste(titulos_lista, collapse = "\n\t\t\t\t\t\t\t\t")
    
    # Criar viewport para o rodapé
    pushViewport(viewport
                 (height = 0.15, 
                   width = 1, 
                   y = 0.05)) # Ajuste a altura conforme necessário
    grid.text(texto_rodape, 
              gp = gpar(fontsize = 14),
              just = "center")
    popViewport()
    
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
    plot_top <- plot_bar(top_alunos, "Quantidade Total / Aluno")
    #plot_bottom <- plot_bar(bottom_alunos, "15 Alunos com menos Atividades")
    
    #grid.newpage()
    grid.arrange(plot_top 
                 #plot_bottom, 
                 #ncol=2
    )
    
    # Fecha o dispositivo gráfico
    dev.off()
    
    system(glue("open '{namefile_pdf}'"))
    system(glue("open '{namefile_xlsx}'"))
    
    break
  }
}