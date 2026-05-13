# Author: Danilo Couto Silva danilocoutosilva@prof.educacao.sp.gov.br
# Version: 3.6 (Leitura Robusta com suporte a UTF-16 e limpeza de BOM)

pacotes <- c("tidyverse"
             , "data.table"
             , "gridExtra"
             , "grid"
             , "glue"
             , "stringr"
             , "openxlsx"
             , "png"
             , "dplyr"
             , "tidyr"
             )

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) 
    install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# --- 1. Configurações Iniciais ---
ano_atual <- format(Sys.Date(), "%Y")
bimestre <- readline(prompt = "Qual Bimestre? (1/2/3/4): ")
serie <- readline(prompt = "Qual turma? (2 para 2B / 3 para 3A): ")
serie_nomenclatura <- if (serie == "3") "3A" else "2B"

data_path <- glue("/home/danilo/Downloads/amilcare/{ano_atual}/relatorios/{bimestre}Bimestre/{serie_nomenclatura}/{Sys.Date()}")
if (!dir.exists(data_path)) 
  dir.create(data_path, recursive = TRUE)
setwd(data_path)

cat("Selecione a pasta com os arquivos CSV...\n")
pasta_arquivos <- tcltk::tk_choose.dir(caption = "Selecione a pasta dos CSVs")
arquivos_csv <- list.files(path = pasta_arquivos, pattern = "\\.csv$", full.names = TRUE)

# --- 2. Função de Processamento Robusta ---
# --- 2. Função de Processamento Robusta (Versão Corrigida) ---
processa_arquivo_auto <- function(arquivo_csv) {
  cat("Processando:", basename(arquivo_csv), "\n")
  
  dados <- NULL
  encodings_para_testar <- c("UTF-16", "UTF-8", "Latin-1")
  separadores_para_testar <- c("\t", ";", ",")
  
  for (enc in encodings_para_testar) {
    for (sep_val in separadores_para_testar) {
      dados <- tryCatch({
        con <- file(arquivo_csv, open = "r", encoding = enc)
        res <- read.table(con, 
                          header = TRUE, 
                          sep = sep_val, 
                          check.names = FALSE, 
                          fill = TRUE, 
                          stringsAsFactors = FALSE,
                          quote = "\"",
                          dec = ",") 
        close(con)
        if(ncol(res) > 1) res else NULL
      }, error = function(e) {
        if(exists("con")) close(con)
        NULL
      })
      if(!is.null(dados)) break
    }
    if(!is.null(dados)) break
  }
  
  if (is.null(dados) || nrow(dados) == 0) {
    warning(paste("Arquivo ignorado:", basename(arquivo_csv)))
    return(NULL)
  }
  
  # Limpeza de BOM e caracteres especiais nos nomes das colunas
  nomes_limpos <- iconv(names(dados), to = "ASCII//TRANSLIT")
  names(dados) <- make.names(nomes_limpos, unique = TRUE)
  
  # Define a primeira coluna como Nome_Aluno
  colnames(dados)[1] <- "Nome_Aluno"
  
  # Mapeamento de Disciplinas (Mantido)
  mapeamento <- c("51000" = "Lógica", "51006" = "Mobile", "51008" = "Back-End",
                  "51009" = "Front-End", "51002" = "Redes", "51003" = "Processos",
                  "9936"  = "Projetos", "51010" = "Banco Dados", "9929"  = "Carreiras",
                  "51004" = "IA", "51001" = "Versionamento")
  
  codigo_extraido <- str_extract(basename(arquivo_csv), "\\d{4,5}(?=\\.csv)")
  disc_nome <- if (!is.na(codigo_extraido) && codigo_extraido %in% names(mapeamento)) {
    mapeamento[codigo_extraido] 
  } else {
    basename(arquivo_csv)
  }
  
  # --- Lógica de Cálculo Mesclada ---
  # Remove colunas fantasmas geradas por erros de leitura (ex: X.1, X.2)
  dados_limpos <- dados %>% select(-matches("^X\\.\\d+"))
  
  # Seleciona apenas as colunas de atividades (geralmente da 5ª em diante no relatório da SED)
  # Se o seu CSV tiver estrutura diferente, ajuste o índice inicial (ex: 4 ou 5)
  atividades <- dados_limpos[, 5:ncol(dados_limpos), drop = FALSE]
  
  # Converte "Concluído" em 1 e o restante em 0
  atividades_binarias <- as.data.frame(lapply(atividades, function(x) {
    ifelse(!is.na(x) & x == "Concluído", 1, 0)
  }))
  
  # Calcula totais usando a lógica do seu código antigo
  total_atividades_disciplina <- ncol(atividades_binarias)
  soma_aluno <- rowSums(atividades_binarias, na.rm = TRUE)
  
  resumo <- data.frame(
    Nome_Aluno = dados$Nome_Aluno,
    total = soma_aluno,
    porc = paste0(round((soma_aluno / total_atividades_disciplina) * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  
  colnames(resumo)[2:3] <- c(glue("Total_{disc_nome}"), glue("%_{disc_nome}"))
  return(list(export_dados = resumo, arquivo_nome = disc_nome))
}

# --- 3. Execução e Combinação de Dados ---
lista_bruta <- Filter(Negate(is.null), lapply(arquivos_csv, processa_arquivo_auto))

if (length(lista_bruta) == 0) {
  stop("Nenhum dado processado. Verifique os arquivos CSV.")
}

resultados_lista <- lapply(lista_bruta, `[[`, "export_dados")
titulos_lista <- sapply(lista_bruta, `[[`, "arquivo_nome")

resultados_combinados <- Reduce(function(x, y) full_join(x, y, by = "Nome_Aluno"), resultados_lista)

if (is.null(resultados_combinados)) {
  stop("Erro ao combinar tabelas.")
}

# Limpeza de NAs e formatação final
resultados_combinados <- resultados_combinados %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(across(where(is.character), ~replace_na(.x, "0%")))

resultados_combinados$`Total de Atividades Concluídas` <- resultados_combinados %>%
  select(starts_with("Total_")) %>% rowSums(na.rm = TRUE)

resultados_combinados <- resultados_combinados %>% arrange(Nome_Aluno)

# --- 4. Geração do PDF ---
namefile_pdf <- glue("{serie_nomenclatura}_Relatorio_{Sys.Date()}.pdf")
pdf(namefile_pdf, height = 15, width = 25)

# PÁGINA 1: Tabela
grid.newpage()
pushViewport(viewport(height = 0.1, width = 1, y = 0.95))
grid.text(glue("Relatório de Resultados - Turma {serie_nomenclatura}"), 
          gp = gpar(fontsize = 26, fontface = "bold"))
popViewport()

pushViewport(viewport(height = 0.05, width = 1, y = 0.91))
grid.text(format(Sys.time(), "%d/%m/%Y %H:%M"), 
          gp = gpar(fontsize = 14, fontface = "italic"))
popViewport()

if(file.exists("/home/danilo/Downloads/amilcare/picture.png")){
  img <- readPNG("/home/danilo/Downloads/amilcare/picture.png")
  grid.raster(img, x = 0.9, y = 0.95, width = unit(4, "cm"), 
              height = unit(4, "cm"), just = c("right", "top"))
}

dados_exibicao <- resultados_combinados
colnames(dados_exibicao)[grepl("^Total_", colnames(dados_exibicao))] <- str_wrap(titulos_lista, width = 15)
colnames(dados_exibicao)[grepl("^%_", colnames(dados_exibicao))] <- "%"

tema_base <- ttheme_minimal(core=list(fg_params=list(fontsize=11)), 
                            colhead=list(fg_params=list(fontsize=13, fontface="bold")))
largura_p <- convertWidth(sum(tableGrob(dados_exibicao, theme = tema_base)$widths), "inches", valueOnly = TRUE)
escala <- if (largura_p > 23) 23 / largura_p else 1.5

tema_final <- ttheme_minimal(
  core = list(bg_params = list(fill = blues9[1:5], col = "white"), fg_params = list(fontsize = 10 * escala)),
  colhead = list(fg_params = list(col = "navyblue", fontface = 4, fontsize = 12 * escala, lineheight = 0.8))
)

pushViewport(viewport(y = 0.45, width = 0.98, height = 0.8))
grid.draw(tableGrob(dados_exibicao, theme = tema_final))
popViewport()

# PÁGINA 2: Gráfico
media_val <- mean(resultados_combinados$`Total de Atividades Concluídas`, na.rm = TRUE)

plot_top <- ggplot(resultados_combinados, aes(x = reorder(Nome_Aluno, `Total de Atividades Concluídas`), y = `Total de Atividades Concluídas`)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_hline(yintercept = media_val, color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_text(aes(label = `Total de Atividades Concluídas`), vjust = -0.5, size = 5) +
  annotate("label", x = Inf, y = media_val, label = sprintf("Média: %.1f", media_val), 
           color = "white", fill = "red", fontface = "bold", size = 6, hjust = 1.1) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(title = "Desempenho Total de Atividades por Aluno", x = "Aluno", y = "Total de Atividades Concluídas")

print(plot_top)
dev.off()

# --- 5. Finalização ---
write.xlsx(resultados_combinados, glue("{serie_nomenclatura}_Relatorio_{Sys.Date()}.xlsx"))
system(glue("xdg-open '{namefile_pdf}'"))