#Leitura de dados atraves do google sheets do formulário de ocorrências
#para automação de impressão de Ocorrências Escolares

pacotes <- c("googlesheets4",
             "dplyr",
             "data.table",
             "grid",
             "gridExtra",
             "glue",
             "shiny",
             "DT"
             
             )

for (p in pacotes ) {
  if (!require(p, character.only = TRUE)){
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# URL da planilha Google Sheets
url <- "https://docs.google.com/spreadsheets/d/1YKiFaWF0GmxgfGP9dUBsPG7Vb5s20-xCGNOEBD9xkYE/edit?resourcekey=&gid=738939517#gid=738939517"

# Leitura dos dados do formulário
respostas_formulario <- read_sheet(url)

# UI (interface do usuário)
ui <- fluidPage(
  titlePanel("Seleção de Ocorrências Escolares para Impressão"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate_pdf", "Gerar PDF") # Botão para gerar o PDF
    ),
    
    mainPanel(
      DTOutput("table") # Tabela com seleção de linhas
    )
  )
)

# Server (lógica do servidor)
server <- function(input, output, session) {
  # Renderização da tabela com seleção de múltiplas linhas
  output$table <- renderDT({
    datatable(
      respostas_formulario,
      selection = "multiple", # Permite a seleção de várias linhas
      options = list(
        pageLength = 10 # Exibe 10 linhas por página
      )
    )
  })
  
  # Observador para gerar PDF
  observeEvent(input$generate_pdf, {
    # Obtém as linhas selecionadas na tabela
    selected_rows <- input$table_rows_selected
    
    # Verifica se alguma linha foi selecionada
    if (length(selected_rows) == 0) {
      showModal(modalDialog(
        title = "Nenhuma linha selecionada",
        "Por favor, selecione pelo menos uma ocorrência para gerar o PDF.",
        footer = modalButton("Fechar")
      ))
      return(NULL)
    }
    
    # Filtra as ocorrências selecionadas
    selected_data <- respostas_formulario[selected_rows, ]
    
    # Nome do arquivo PDF
    namefile <- glue("Ocorrências_Selecionadas_{Sys.Date()}.pdf")
    pdf(namefile, onefile = TRUE, width = 8.27, height = 11.69) # tamanho A4
    
    # Geração das páginas no PDF para cada ocorrência selecionada
    for (i in 1:nrow(selected_data)) {
      ocorrencia <- selected_data[i, ]
      
      # Cria uma nova página no PDF para cada ocorrência
      plot.new()
      title(main = glue("Ocorrência {i}"))
      
      # Posição inicial para escrever o conteúdo
      y_pos <- 1 
      
      # Itera pelas colunas para escrever nome e valor
      for (j in seq_along(ocorrencia)) {
        nome_coluna <- names(ocorrencia)[j]
        valor <- as.character(ocorrencia[[j]])  # Converte valor para texto
        
        # Formata o texto para caber na largura da página
        texto_formatado <- strwrap(glue("{nome_coluna}: {valor}"), width = 80)
        
        # Escreve cada linha do texto formatado no PDF
        for (linha in texto_formatado) {
          # Verifica se o texto atinge o limite inferior da página
          if (y_pos < 0.1) {
            plot.new()   # Cria nova página
            y_pos <- 1   # Reseta a posição vertical
          }
          
          text(0.1, y_pos, labels = linha, adj = 0)
          y_pos <- y_pos - 0.05  # Ajusta a posição vertical para a próxima linha
        }
      }
    }
    
    dev.off()
    
    # Mostra um modal informando que o PDF foi gerado
    showModal(modalDialog(
      title = "PDF Gerado",
      "O arquivo PDF foi gerado com sucesso!",
      footer = modalButton("Fechar")
    ))
  })
}

# Executa a aplicação
shinyApp(ui = ui, server = server)
