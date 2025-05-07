limpar_nomes <- function(nomes) {
  nomes <- stringr::str_to_upper(nomes)
  # Se tiver abjutils: nomes <- abjutils::rm_accent(nomes)
  # Alternativa simples sem abjutils (remove acentos comuns):
  nomes <- iconv(nomes, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  nomes <- stringr::str_trim(nomes)
  return(nomes)
}

resumo_colunas <- function(df) {
  resultados <- lapply(names(df), function(col_name) {
    col_data <- df[[col_name]]
    col_type <- class(col_data)[1]
    n_unique <- length(unique(col_data))
    
    # Obter exemplos (primeiros valores únicos)
    exemplos <- unique(col_data)[1:min(3, n_unique)]
    exemplos <- paste(exemplos, collapse = ", ")
    
    # Verificar se é numérica para calcular min/max
    if(is.numeric(col_data)) {
      min_val <- min(col_data, na.rm = TRUE)
      max_val <- max(col_data, na.rm = TRUE)
      min_max <- paste(min_val, "-", max_val)
    } else {
      min_max <- NA
    }
    
    data.frame(
      coluna = col_name,
      tipo = col_type,
      valores_unicos = n_unique,
      exemplos = exemplos,
      min_max = min_max,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, resultados)
}

limpar_idade <- function(x) {
  # Primeiro, tentar converter diretamente para numérico
  resultado <- tryCatch({
    num <- as.numeric(x)
    
    # Se for um ano de nascimento (valor entre 1900 e 2005)
    if (!is.na(num) && num > 1900 && num < 2005) {
      return(2023 - num)  # Calcular idade em relação a 2023
    }
    
    return(num)
  }, warning = function(w) {
    # Se der warning (conversão falhou), tratar casos especiais
    if (x == "Bom 66") return(66)
    if (x == "49anos") return(49)
    if (x == "57 anos") return(57)
    
    # Extrair apenas os dígitos para outros casos
    numeros <- gsub("[^0-9]", "", x)
    if (numeros == "") return(NA)
    
    # Verificar se é potencialmente um ano de nascimento
    num <- as.numeric(numeros)
    if (num > 1900 && num < 2005) {
      return(2023 - num)
    }
    
    return(num)
  })
  
  return(resultado)
}

categorizar_experiencia <- function(texto) {
  # Remover espaços extras e converter para minúsculas para padronização
  texto_limpo <- stringr::str_trim(tolower(texto))
  texto_limpo <- stringr::str_replace_all(texto_limpo, "\\s+", " ") # Normalizar espaços internos

  # Categorias já existentes (retorna o próprio valor padronizado)
  categorias_padrao <- c(
    "menos de 6 meses", "de 6 meses a 1 ano", "de 1 a 3 anos",
    "de 3 a 5 anos", "de 5 a 10 anos", "de 10 a 15 anos",
    "de 15 a 20 anos", "mais de 20 anos"
  )
  if (texto_limpo %in% categorias_padrao) {
    # Padronizar capitalização se necessário (opcional)
    return(
        dplyr::case_when(
            texto_limpo == "menos de 6 meses" ~ "Menos de 6 meses",
            texto_limpo == "de 6 meses a 1 ano" ~ "de 6 meses a 1 ano",
            texto_limpo == "de 1 a 3 anos" ~ "de 1 a 3 anos",
            texto_limpo == "de 3 a 5 anos" ~ "de 3 a 5 anos",
            texto_limpo == "de 5 a 10 anos" ~ "de 5 a 10 anos",
            texto_limpo == "de 10 a 15 anos" ~ "de 10 a 15 anos",
            texto_limpo == "de 15 a 20 anos" ~ "de 15 a 20 anos",
            texto_limpo == "mais de 20 anos" ~ "Mais de 20 anos",
            TRUE ~ texto_limpo # Fallback
        )
    )
  }

  # Casos especiais
  if (is.na(texto_limpo) || texto_limpo %in% c("", "a", "0.0", "0a", "0", "00", "2s")) {
    return(NA_character_)
  }
  if (stringr::str_detect(texto_limpo, "não trabalha|nao trabalhei")) {
      return("Não aplicável") # Ou pode ser NA, dependendo da análise
  }
   if (stringr::str_detect(texto_limpo, "experiência em urgência")) {
      return(NA_character_) # Texto livre não categorizável
  }


  # Extrair anos e meses usando regex
  anos <- stringr::str_match(texto_limpo, "(\\d+)\\s*a")[, 2]
  meses <- stringr::str_match(texto_limpo, "(\\d+)\\s*m")[, 2]

  # Tratar casos como "10a6m" onde não há espaço
   if (is.na(anos) && is.na(meses)) {
      match_am <- stringr::str_match(texto_limpo, "(\\d+)a(\\d+)m")
      if (!is.na(match_am[1,1])) {
          anos <- match_am[1, 2]
          meses <- match_am[1, 3]
      }
   }

  # Converter para numérico, tratando NAs como 0
  anos_num <- ifelse(is.na(anos), 0, as.numeric(anos))
  meses_num <- ifelse(is.na(meses), 0, as.numeric(meses))

   # Se ambos forem 0 após a extração (não encontrou padrão A ou M válido)
   if (anos_num == 0 && meses_num == 0) {
       # Tentar extrair apenas um número (pode ser ano ou mês, difícil saber)
       # Vamos assumir que um número sozinho >= 1 é ano, < 12 (e não zero) é mês.
       # Isso é uma heurística e pode precisar de ajuste.
        num_isolado <- stringr::str_match(texto_limpo, "^(\\d+)$")[,2]
        if (!is.na(num_isolado)) {
            num <- as.numeric(num_isolado)
            if (num >= 1) anos_num <- num
            # Não vamos inferir meses aqui para evitar confusão, tratar como NA se for só mês < 12?
            # Por agora, se for só numero < 12 e não for mês explícito, retorna NA.
            else return(NA_character_)
        } else {
            # Se não encontrou nenhum padrão reconhecível
             return(NA_character_)
        }
   }


  # Calcular tempo total em anos
  tempo_total_anos <- anos_num + (meses_num / 12)

  # Mapear para categorias
  if (tempo_total_anos == 0) { # caso específico de 0M
      categoria <- "Menos de 6 meses"
  } else if (tempo_total_anos < 0.5) {
    categoria <- "Menos de 6 meses"
  } else if (tempo_total_anos >= 0.5 && tempo_total_anos < 1) {
    categoria <- "de 6 meses a 1 ano"
  } else if (tempo_total_anos >= 1 && tempo_total_anos < 3) {
    categoria <- "de 1 a 3 anos"
  } else if (tempo_total_anos >= 3 && tempo_total_anos < 5) {
    categoria <- "de 3 a 5 anos"
  } else if (tempo_total_anos >= 5 && tempo_total_anos < 10) {
    categoria <- "de 5 a 10 anos"
  } else if (tempo_total_anos >= 10 && tempo_total_anos < 15) {
    categoria <- "de 10 a 15 anos"
  } else if (tempo_total_anos >= 15 && tempo_total_anos < 20) {
    categoria <- "de 15 a 20 anos"
  } else if (tempo_total_anos >= 20) {
    categoria <- "Mais de 20 anos"
  } else {
    categoria <- NA_character_ # Caso não se encaixe em nenhuma categoria
  }

  return(categoria)
}

mapear_escala <- function(x) {
  dplyr::case_when(
    x == 1 ~ 0,
    x == 2 ~ 25,
    x == 3 ~ 50,
    x == 4 ~ 75,
    x == 5 ~ 100,
    TRUE ~ NA_real_
  )
} 