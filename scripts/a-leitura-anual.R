processar_caged <- function(caminho_base, XXX, AAAA, MM = NULL) {
  library(data.table)
  library(janitor)
  library(zip)
  
  caminho <- paste0(caminho_base, "CAGED", XXX, AAAA)
  
  meses <- ifelse(is.null(MM), sprintf("%02d", 1:12), MM)
  
  processar_arquivo <- function(mes) {
    arquivo_zip <- paste0(caminho, mes, ".zip")
    
    temp_dir <- tempdir()
    unzip(arquivo_zip, exdir = temp_dir)
    
    arquivo_txt <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)
    
    if (length(arquivo_txt) == 0) {
      message("Nenhum arquivo txt encontrado em: ", arquivo_zip)
      return(NULL)
    }
    
    dado_completo <- fread(
      file = arquivo_txt, 
      sep = ";", 
      dec = ",", 
      data.table = TRUE, 
      showProgress = FALSE
    ) |> janitor::clean_names()
    
    setorder(dado_completo, uf)
    mapa_caract <- dado_completo[, .(Total = .N), by = uf]
    mapa_caract[, Inicial := cumsum(Total) - Total]
    mapa_caract[, Final := cumsum(Total)]
    
    UF_escolhida <- "32" 
    inicio <- mapa_caract[uf == UF_escolhida, Inicial] 
    final <- mapa_caract[uf == UF_escolhida, Final]
    
    dado <- dado_completo[(inicio + 1):(final)]
    
    return(dado)
  }
  
  dados_processados <- lapply(meses, processar_arquivo)
  
  resultado_final <- rbindlist(dados_processados, use.names = TRUE, fill = TRUE)
  
  return(resultado_final)
}

caminho_base <- "bases/CAGEDMOV2024/"
XXX <- "123"  
AAAA <- "2024"  

dados <- processar_caged(caminho_base, XXX, AAAA)

dados_janeiro <- processar_caged(caminho_base, XXX, AAAA, MM = "01")