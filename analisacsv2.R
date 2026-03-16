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
# Define a nomenclatura e o caminho base
serie_nomenclatura <- if (serie == "3") "3A" else "2B"
data_path <- glue("/home/danilo/Downloads/amilcare/{ano_atual}/relatorios/{bimestre}Bimestre/{serie_nomenclatura}/{Sys.Date()}")

if (!dir.exists(data_path)) dir.create(data_path, recursive = TRUE)
setwd(data_path)

# --- NOVA LÓGICA: Seleção de Pasta ---
cat("Selecione a pasta que contém os arquivos CSV...\n")
pasta_arquivos <- tcltk::tk_choose.dir(caption = "Selecione a pasta dos CSVs")
arquivos_csv <- list.files(path = pasta_arquivos, pattern = "\\.csv$", full.names = TRUE)

if (length(arquivos_csv) == 0) stop("Nenhum arquivo CSV encontrado na pasta selecionada.")

# Função de processamento adaptada para receber o caminho do arquivo
processa_arquivo_auto <- function(arquivo_csv, contador_local) {
  dados <- read.csv(arquivo_csv, fileEncoding = 'UTF16', header = TRUE, sep = "\t")
  dados_limpos <- dados %>% select(-matches("X.[0-9]"))
  
  atividades <- dados_limpos %>% select(-1, -2, -3)
  iden <- dados_limpos %>% select(1, 2, 3)
  
  atividades <- lapply(atividades, function(x) ifelse(!is.na(x) & x == "Concluído", 1, 0))
  atividades_df <- data.frame(atividades)
  
  # Uso de na.rm = TRUE para evitar NAs na soma
  atividades_df$total_por_aluno <- rowSums(atividades_df, na.rm = TRUE)
  atividades_df$porc_por_aluno <- glue("{round((atividades_df$total_por_aluno / length(atividades)) * 100, 1)}%")
  
  df <- as_tibble(cbind(data.frame(iden), atividades_df))
  
  if (contador_local < 1) {
    export_dados <- select(df, 1, total_por_aluno, porc_por_aluno)
  } else {
    export_dados <- select(df, total_por_aluno, porc_por_aluno)
  }
  
  mapeamento_cursos <- c(
    "2_51000" = "Lógica e Linguagem de Programação",
    "3_51006" = "Programação Mobile",
    "3_51008" = "Programação Back-End",
    "3_51009" = "Programação Front-End",
    "2_51002" = "Redes e Segurança de Computadores",
    "2_51003" = "Processos de Desenvolvimento de Software",
    "3_9936" = "Projeto Multidisciplinar",
    "3_51010" = "Modelagem de Banco de Dados",
    "2_9929"  = "Carreira e Competências",
    "3_51004" = "Inteligência Artificial",
    "3_51001" = "Versionamento de Código"
  )
  
  codigo <- str_extract(basename(arquivo_csv), "[23]_\\d{4,5}(?=\\.csv)")
  arquivo_nome <- if (!is.na(codigo) && codigo %in% names(mapeamento_cursos)) mapeamento_cursos[codigo] else basename(arquivo_csv)
  
  list(export_dados = export_dados, arquivo_nome = unname(arquivo_nome))
}

resultados_lista <- list()
titulos_lista <- c()

# Loop automático pelos arquivos encontrados
for (i in seq_along(arquivos_csv)) {
  res <- processa_arquivo_auto(arquivos_csv[i], i-1)
  resultados_lista[[i]] <- res$export_dados
  titulos_lista[i] <- res$arquivo_nome
}

# Combinação e Cálculos Finais
resultados_combinados <- do.call(cbind, resultados_lista)
colnames(resultados_combinados) <- make.unique(colnames(resultados_combinados))

# Soma total com tratamento de NA
resultados_combinados$total_atividades_concluidas <- rowSums(
  resultados_combinados[, grepl("total_por_aluno", colnames(resultados_combinados))], 
  na.rm = TRUE
)

resultados_combinados <- resultados_combinados %>% arrange_at(1)
top_alunos <- resultados_combinados %>% select(1, total_atividades_concluidas)

# --- Geração do PDF ---
namefile_pdf <- glue("{serie_nomenclatura}_Relatorio_{Sys.Date()}.pdf")
pdf(namefile_pdf, height = 15, width = 25, paper = "special")
namefile_xlsx <- glue("{serie_nomenclatura}_Relatorio_{Sys.Date()}.xlsx")

# Página 1: Título e Logo
pushViewport(viewport(height = 0.1, width = 1, y = 0.95))
grid.text(glue("Relatório de Resultados - Turma {serie_nomenclatura}"), gp = gpar(fontsize = 18, fontface = "bold"))
popViewport()
pushViewport(viewport(height = 0.05, width = 1, y = 0.90))
grid.text(format(Sys.time(), "%d/%m/%Y %H:%M"), 
          x = unit(0.5, "npc"), 
          y = unit(0.5, "npc"), 
          gp = gpar(fontsize = 10, fontface = "bold", col = "black"))
popViewport()

img <- readPNG("/home/danilo/Downloads/amilcare/picture.png")
grid.raster(img, x = 0.9, y = 0.95, width = unit(4, "cm"), height = unit(4, "cm"), just = c("right", "top"))

# Formatação da Tabela
dados_exibicao <- resultados_combinados
col_totais <- grepl("total_por_aluno", colnames(dados_exibicao))
col_porc <- grepl("porc_por_aluno", colnames(dados_exibicao))

# Quebra de linha nos nomes das disciplinas (cabeçalho)
colnames(dados_exibicao)[col_totais] <- str_wrap(titulos_lista, width = 15)
colnames(dados_exibicao)[col_porc] <- "%"

tema_base <- ttheme_minimal(
  core = list(bg_params = list(fill = blues9[1:5], col = NA), fg_params = list(fontsize = 9)),
  colhead = list(fg_params = list(col = "navyblue", fontface = 4, fontsize = 10, lineheight = 0.8))
)

# Cálculo de escala para caber na página
tab_teste <- tableGrob(dados_exibicao, theme = tema_base)
largura_p <- convertWidth(sum(tab_teste$widths), "inches", valueOnly = TRUE)
escala <- if (largura_p > 23) 23 / largura_p else 1

tema_final <- ttheme_minimal(
  core = list(bg_params = list(fill = blues9[1:5], col = NA), fg_params = list(fontsize = 9 * escala)),
  colhead = list(fg_params = list(col = "navyblue", fontface = 4, fontsize = 10 * escala, lineheight = 0.8))
)

grid.draw(tableGrob(dados_exibicao, theme = tema_final))
write.xlsx(resultados_combinados, file = namefile_xlsx, asTable = TRUE, overwrite = TRUE)


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