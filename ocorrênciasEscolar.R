#Leitura de dados atraves do google sheets do formulário de ocorrências
#para automação de impressão de Ocorrências Escolares

pacotes <- c("googlesheets4",
             "dplyr"
             )

for (p in pacotes ) {
  if (!require(p, character.only = TRUE)){
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

#url da planilha
url <- "https://docs.google.com/spreadsheets/d/1YKiFaWF0GmxgfGP9dUBsPG7Vb5s20-xCGNOEBD9xkYE/edit?resourcekey=&gid=738939517#gid=738939517"
#Leitura da URL
respostas_formulario <- read_sheet(url)
print(respostas_formulario)
#utilização do pacote dplyr
glimpse(respostas_formulario)

namefile <- glue("Ocorrência{nome} {Sys.Date()}.pdf")
pdf(namefile, onefile = TRUE)

#pagina 1


