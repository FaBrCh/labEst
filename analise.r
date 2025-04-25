# ==============================================================================
# 1. CONFIGURAÇÃO INICIAL
# ==============================================================================

# Carregar o pacote readxl
library(readxl)

# Carregar pacotes necessários
library(tidyverse)
library(psych)      # Para análises estatísticas descritivas
library(gtsummary)  # Para tabelas de resumo
library(stringr)    # Para manipulação de strings (já usado, garantir carregamento)

# Definir o caminho para os arquivos
caminho_respostas <- "data/respostas.xlsx"
caminho_municipios <- "data/MUNICÍPIOS E SALAS DE VACINAÇÃO SELECIONADOS.xlsx"

# >>> MOVER DEFINIÇÃO DA FUNÇÃO limpar_nomes PARA CÁ <<<
limpar_nomes <- function(nomes) {
  nomes <- stringr::str_to_upper(nomes)
  # Se tiver abjutils: nomes <- abjutils::rm_accent(nomes)
  # Alternativa simples sem abjutils (remove acentos comuns):
  nomes <- iconv(nomes, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  nomes <- stringr::str_trim(nomes)
  return(nomes)
}

# ==============================================================================
# 2. CARREGAMENTO E LIMPEZA INICIAL DOS DADOS
# ==============================================================================

# Ler a planilha "AMOSTRA" do arquivo de municípios, pulando as 4 primeiras linhas
tryCatch({
  df_municipios_amostra <- read_excel(caminho_municipios, sheet = "AMOSTRA", skip = 4)
  
  # Renomear as colunas para nomes limpos
  df_municipios_amostra <- df_municipios_amostra %>%
    rename(
      NUMERO = 1,
      PARTICIPANTE = 2,
      COLETA_FINALIZADA = 3,
      MACRORREGIOES = 4,
      URS = 5, # Unidade Regional de Saúde (Microrregião)
      MUNICIPIO = 6,
      PORTE_IBGE = 7,
      POPULACAO_2021 = 8,
      N_SALAS_VACINA = 9,
      AREA_KM2_2021 = 10,
      DENSIDADE_2010 = 11,
      ESCOLARIZACAO_6A14_2010 = 12,
      IDHM_2010 = 13,
      MORTALIDADE_INF_2020 = 14,
      PIB_PER_CAPITA_2020 = 15
    )

  # Selecionar e limpar colunas relevantes
  df_municipios_info <- df_municipios_amostra %>% 
    select(MUNICIPIO, MACRORREGIOES, URS, PORTE_IBGE) %>% 
    mutate(
      # <<< REMOVER CÓDIGO NUMÉRICO DO NOME DO MUNICÍPIO >>>
      MUNICIPIO = str_replace(MUNICIPIO, "^[0-9]+-", ""),
      # Limpar nome do município para junção (agora sem o código)
      Municipio_join = limpar_nomes(MUNICIPIO), 
      # Limpar possíveis espaços extras nas outras colunas
      MACRORREGIOES = str_trim(MACRORREGIOES),
      URS = str_trim(URS),
      PORTE_IBGE = str_trim(PORTE_IBGE)
    ) %>% 
    # Remover duplicatas baseadas no nome limpo do município, mantendo a primeira ocorrência
    distinct(Municipio_join, .keep_all = TRUE)

  cat("Informações de municípios carregadas e limpas com sucesso.\n")
  # glimpse(df_municipios_info) # Descomentar para verificar a estrutura

}, error = function(e) {
  warning(paste("Erro ao carregar ou processar a planilha 'AMOSTRA' do arquivo de municípios:", e$message))
  df_municipios_info <- NULL # Define como NULL se houver erro
})

# Carregar os dados dos arquivos Excel
dados_respostas <- read_excel(caminho_respostas, sheet = "Respostas ao formulário 1")

# Visualizar os primeiros registros dos dados
head(dados_respostas)

# Verificar e remover colunas completamente vazias
colunas_vazias <- colSums(is.na(dados_respostas)) == nrow(dados_respostas)
dados_limpos <- dados_respostas[, !colunas_vazias]

# verificar quantidade de valores nulos por coluna, e mostrar somente as que tem valores nulos
valores_nulos <- colSums(is.na(dados_limpos))
valores_nulos <- valores_nulos[valores_nulos > 0]

# Criar um dataframe com as informações de valores nulos
df_nulos <- data.frame(
  coluna = names(valores_nulos),
  qtd_nulos = valores_nulos,
  pct_nulos = round(valores_nulos / nrow(dados_limpos) * 100, 1)
)

# Ordenar por quantidade de valores nulos (decrescente)
df_nulos <- df_nulos %>%
  arrange(desc(qtd_nulos))

# Mostrar a tabela formatada com gt
if(!require(gt)) install.packages("gt")
gt::gt(df_nulos) %>%
  gt::cols_label(
    coluna = "Nome da Coluna",
    qtd_nulos = "Quantidade de Nulos",
    pct_nulos = "% de Nulos"
  ) %>%
  gt::tab_header(title = "Valores Nulos por Coluna") %>%
  gt::fmt_percent(pct_nulos, decimals = 1, scale_values = FALSE)

# Remover colunas com alta porcentagem de valores nulos (acima de 99%)
colunas_quase_vazias <- c(
  "Unidade de Atenção Primária à Saúde...12",
  "Unidade de Atenção Primária à Saúde...18",
  "Unidade de Atenção Primária à Saúde...14"
)

# Atualizar dados_limpos removendo estas colunas adicionais
dados_limpos <- dados_limpos %>%
  select(-all_of(colunas_quase_vazias))

# Verificar se foram removidas corretamente
cat("Colunas originais:", ncol(dados_respostas), "\n")
cat("Colunas após remoção de vazias e quase vazias:", ncol(dados_limpos), "\n")
cat("Colunas adicionais removidas:", paste(colunas_quase_vazias, collapse = ", "), "\n")

# Contar linhas originais
linhas_originais <- nrow(dados_limpos)
cat("Número total de linhas antes da remoção:", linhas_originais, "\n")

# Remover linhas com valores nulos
dados_limpos <- dados_limpos %>%
  drop_na()  # Remove linhas que contêm qualquer valor NA

# Contar linhas após remoção
linhas_restantes <- nrow(dados_limpos)
linhas_removidas <- linhas_originais - linhas_restantes

# Exibir resultados
cat("Número de linhas após remoção de NAs:", linhas_restantes, "\n")
cat("Número de linhas removidas:", linhas_removidas, "\n")
cat("Porcentagem de linhas removidas:", round(linhas_removidas/linhas_originais*100, 1), "%\n")

# Verificar tipos de dados e valores de exemplo para todas as colunas
# Usar glimpse para uma visão rápida
glimpse(dados_limpos)

# Criar uma função para obter um resumo mais detalhado de cada coluna
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

# Criar tabela de resumo
tabela_resumo <- resumo_colunas(dados_limpos)

# Mostrar a tabela formatada
gt::gt(tabela_resumo) %>%
  gt::cols_label(
    coluna = "Nome da Coluna",
    tipo = "Tipo de Dado",
    valores_unicos = "Valores Únicos",
    exemplos = "Exemplos de Valores",
    min_max = "Mín-Máx (numéricos)"
  ) %>%
  gt::tab_header(title = "Resumo das Colunas") %>%
  gt::cols_align(
    align = "left",
    columns = c(coluna, tipo, exemplos)
  ) %>%
  gt::cols_align(
    align = "right",
    columns = c(valores_unicos, min_max)
  ) %>%
  gt::tab_options(
    table.font.size = "small"
  )

# Verificar se todas as questões de 1-36 estão presentes
colunas_questionario <- grep("^[0-9]+\\.", names(dados_limpos), value = TRUE)
cat("Número de questões encontradas:", length(colunas_questionario), "\n")
print(colunas_questionario)

# Verificar tipos das colunas das questões
tipos_antes <- sapply(dados_limpos[colunas_questionario], class)
print(table(tipos_antes))

# Verificar se todas as colunas das questões são realmente numéricas
for (col in colunas_questionario) {
  if (!is.numeric(dados_limpos[[col]])) {
    cat("Coluna não numérica encontrada:", col, "\n")
    # Tentar forçar conversão
    dados_limpos[[col]] <- as.numeric(as.character(dados_limpos[[col]]))
  }
}

# Verificar e tratar possíveis valores NA introduzidos na conversão
na_depois_conversao <- colSums(is.na(dados_limpos[colunas_questionario]))
if (sum(na_depois_conversao) > 0) {
  cat("Colunas com NAs após conversão:\n")
  print(na_depois_conversao[na_depois_conversao > 0])
}

# Verificar e corrigir valores problemáticos na coluna de idade
# Primeiro, vamos identificar os valores que não são números puros
idades <- dados_limpos$`Idade do Profissonal (em anos) SOMENTE NÚMEROS`
valores_problematicos <- grep("[^0-9\\.]", idades)

cat("Valores problemáticos encontrados:", length(valores_problematicos), "\n")
cat("Linhas com valores problemáticos:", valores_problematicos, "\n")
cat("Valores:", idades[valores_problematicos], "\n")

# Nova abordagem para limpar as idades
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

# Aplicar a função atualizada
dados_limpos$idade_num <- sapply(dados_limpos$`Idade do Profissonal (em anos) SOMENTE NÚMEROS`, limpar_idade)

# Verificar se os valores problemáticos foram corrigidos
cat("Verificando casos específicos:\n")
for (idx in c(332, 494, 529)) {  # Linhas com valores suspeitos (ajustar se necessário)
  cat("Linha", idx, "- Valor original:", dados_limpos$`Idade do Profissonal (em anos) SOMENTE NÚMEROS`[idx], 
      "Valor corrigido:", dados_limpos$idade_num[idx], "\n")
}

# Verificar novamente idades suspeitas
idades_suspeitas <- dados_limpos$idade_num[dados_limpos$idade_num < 18 | dados_limpos$idade_num > 80]
if (length(idades_suspeitas) > 0) {
  cat("\nIdades potencialmente incorretas após correção:", idades_suspeitas, "\n")
} else {
  cat("\nTodas as idades estão agora dentro dos limites esperados (18-80)\n")
}

# remover idades suspeitas
dados_limpos <- dados_limpos %>%
  filter(idade_num >= 18 & idade_num <= 90)

# Estatísticas atualizadas
summary(dados_limpos$idade_num)

# Histograma atualizado
hist(dados_limpos$idade_num, 
     main="Histograma das Idades (Corrigido)", 
     xlab="Idade", 
     breaks=20,
     col="lightblue")

# ==============================================================================
# 4. TRATAMENTO ADICIONAL POR CARGO E FORMAÇÃO
# ==============================================================================

# Definir nomes das colunas para clareza
col_cargo <- "Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)"
col_formacao <- "Formação"

cat("\n--- Iniciando tratamento por Cargo e Formação ---\n")
linhas_antes_cargo_formacao <- nrow(dados_limpos)
cat(paste("Linhas antes:", linhas_antes_cargo_formacao, "\n"))

# 1. Mudar Cargo: Diretora -> Técnico em Enfermagem
dados_limpos <- dados_limpos %>%
  mutate(!!sym(col_cargo) := if_else(str_trim(!!sym(col_cargo)) == "Diretora", 
                                   "Técnico em Enfermagem", 
                                   !!sym(col_cargo)))

# 2. Excluir Cargo: AUX ADM (ignorando espaços e caixa)
# Contar quantos são AUX ADM antes de remover
n_aux_adm <- sum(str_to_upper(str_trim(dados_limpos[[col_cargo]])) == "AUX ADM", na.rm = TRUE)
cat(paste("Número de linhas com cargo 'AUX ADM' a serem removidas:", n_aux_adm, "\n"))

dados_limpos <- dados_limpos %>%
  filter(str_to_upper(str_trim(!!sym(col_cargo))) != "AUX ADM")

linhas_apos_aux_adm <- nrow(dados_limpos)
cat(paste("Linhas após remover AUX ADM:", linhas_apos_aux_adm, "\n"))

# 3. Nova Regra: Mudar Formação para "Ensino Técnico" se Cargo == Técnico e Formação == Ensino Médio
# Contar quantos serão modificados
n_modificar_formacao_tec <- dados_limpos %>%
    filter(str_trim(!!sym(col_cargo)) == "Técnico em Enfermagem" & 
           str_trim(!!sym(col_formacao)) == "Ensino Médio Completo") %>%
    nrow()
cat(paste("Número de linhas onde Formação será mudada para 'Ensino Técnico':", n_modificar_formacao_tec, "\n"))

dados_limpos <- dados_limpos %>%
  mutate(!!sym(col_formacao) := if_else(str_trim(!!sym(col_cargo)) == "Técnico em Enfermagem" & 
                                      str_trim(!!sym(col_formacao)) == "Ensino Médio Completo",
                                      "Ensino Técnico",
                                      !!sym(col_formacao)))

# 4. Mudar Cargo para "Auxiliar de Enfermagem" se Formação ainda for "Ensino Médio Completo"
# Verificar quantos serão modificados (agora considera apenas os que NÃO foram alterados na etapa 3)
 n_modificar_cargo_aux <- dados_limpos %>% 
    filter(str_trim(!!sym(col_formacao)) == "Ensino Médio Completo") %>% # Não precisa mais checar o cargo aqui
    nrow()
cat(paste("Número de linhas onde Cargo será mudado para 'Auxiliar de Enfermagem' (Formação = Ensino Médio Completo restante):", n_modificar_cargo_aux, "\n"))

dados_limpos <- dados_limpos %>%
  mutate(!!sym(col_cargo) := if_else(str_trim(!!sym(col_formacao)) == "Ensino Médio Completo",
                                   "Auxiliar de Enfermagem",
                                   !!sym(col_cargo)))

linhas_depois_cargo_formacao <- nrow(dados_limpos)
linhas_removidas_total <- linhas_antes_cargo_formacao - linhas_depois_cargo_formacao

cat(paste("Linhas finais após tratamento Cargo/Formação:", linhas_depois_cargo_formacao, "\n"))
cat(paste("Total de linhas removidas nesta etapa (apenas AUX ADM):", linhas_removidas_total, "\n"))
cat("--- Fim tratamento por Cargo e Formação ---\n")

# ==============================================================================
# 4.5 LIMPEZA E CATEGORIZAÇÃO DO TEMPO DE EXPERIÊNCIA (Movido para após trat. Cargo/Formação)
# ==============================================================================

# Definir as colunas de experiência
col_exp_total <- "Tempo de Experiência Total como Profissional da Enfermagem...40"
col_exp_aps <- "Tempo de Experiência na Atenção Primária à Saúde"
col_exp_vac <- "Tempo de Experiência em Salas de Vacinação"
cols_experiencia <- c(col_exp_total, col_exp_aps, col_exp_vac)

# Definir nomes das novas colunas categorizadas
col_exp_total_cat <- paste0(col_exp_total, "_cat")
col_exp_aps_cat <- paste0(col_exp_aps, "_cat")
col_exp_vac_cat <- paste0(col_exp_vac, "_cat")
cols_experiencia_cat <- c(col_exp_total_cat, col_exp_aps_cat, col_exp_vac_cat)

# Função para limpar e categorizar a experiência
categorizar_experiencia <- function(texto) {
  # Remover espaços extras e converter para minúsculas para padronização
  texto_limpo <- str_trim(tolower(texto))
  texto_limpo <- str_replace_all(texto_limpo, "\\s+", " ") # Normalizar espaços internos

  # Categorias já existentes (retorna o próprio valor padronizado)
  categorias_padrao <- c(
    "menos de 6 meses", "de 6 meses a 1 ano", "de 1 a 3 anos",
    "de 3 a 5 anos", "de 5 a 10 anos", "de 10 a 15 anos",
    "de 15 a 20 anos", "mais de 20 anos"
  )
  if (texto_limpo %in% categorias_padrao) {
    # Padronizar capitalização se necessário (opcional)
    return(
        case_when(
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
  if (str_detect(texto_limpo, "não trabalha|nao trabalhei")) {
      return("Não aplicável") # Ou pode ser NA, dependendo da análise
  }
   if (str_detect(texto_limpo, "experiência em urgência")) {
      return(NA_character_) # Texto livre não categorizável
  }


  # Extrair anos e meses usando regex
  anos <- str_match(texto_limpo, "(\\d+)\\s*a")[, 2]
  meses <- str_match(texto_limpo, "(\\d+)\\s*m")[, 2]

  # Tratar casos como "10a6m" onde não há espaço
   if (is.na(anos) && is.na(meses)) {
      match_am <- str_match(texto_limpo, "(\\d+)a(\\d+)m")
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
        num_isolado <- str_match(texto_limpo, "^(\\d+)$")[,2]
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

# Aplicar a função para criar as NOVAS colunas categorizadas
# Usando .after para tentar posicionar as novas colunas (pode variar com a versão do dplyr)
dados_limpos <- dados_limpos %>%
  mutate(
    !!col_exp_total_cat := sapply(.data[[col_exp_total]], categorizar_experiencia, USE.NAMES = FALSE), 
    .after = all_of(col_exp_total)
  ) %>%
  mutate(
    !!col_exp_aps_cat := sapply(.data[[col_exp_aps]], categorizar_experiencia, USE.NAMES = FALSE),
    .after = all_of(col_exp_aps) 
  ) %>%
  mutate(
    !!col_exp_vac_cat := sapply(.data[[col_exp_vac]], categorizar_experiencia, USE.NAMES = FALSE),
    .after = all_of(col_exp_vac)
  )

# Verificar os valores únicos nas novas colunas
valores_distintos_limpos <- list(
  total = unique(dados_limpos[[col_exp_total_cat]]),
  aps = unique(dados_limpos[[col_exp_aps_cat]]),
  vac = unique(dados_limpos[[col_exp_vac_cat]])
)

print("Valores distintos nas colunas CATEGORIZADAS após limpeza:")
print(valores_distintos_limpos)

# Verificar alguns exemplos (colunas originais e novas lado a lado)
print("Exemplos de dados limpos (original vs categorizado):")
print(head(dados_limpos[c(col_exp_total, col_exp_total_cat, col_exp_aps, col_exp_aps_cat, col_exp_vac, col_exp_vac_cat)]))

# Contar NAs introduzidos ou mantidos nas NOVAS colunas
na_counts <- colSums(is.na(dados_limpos[cols_experiencia_cat]))
print("Contagem de NAs nas colunas CATEGORIZADAS após limpeza:")
print(na_counts)

# >>> FILTRAGEM ADICIONAL: Remover quem tem menos de 6 meses em SALA DE VACINAÇÃO (usando a nova coluna) <<<
linhas_antes_filtro_exp_vac <- nrow(dados_limpos)

# Definir valores a serem excluídos
valores_excluir_exp_vac <- c("Menos de 6 meses", "Não aplicável")

# Aplicar o filtro: manter apenas linhas ONDE a coluna NÃO é NA E NÃO está na lista de exclusão
dados_limpos <- dados_limpos %>% 
  filter(
    !is.na(.data[[col_exp_vac_cat]]) & 
    !(.data[[col_exp_vac_cat]] %in% valores_excluir_exp_vac)
  )

linhas_apos_filtro_exp_vac <- nrow(dados_limpos)
linhas_removidas_exp_vac <- linhas_antes_filtro_exp_vac - linhas_apos_filtro_exp_vac

cat(paste0("\\nFiltragem adicional: Removidas ", linhas_removidas_exp_vac, 
           " linhas onde ", col_exp_vac_cat, " era 'Menos de 6 meses', 'Não aplicável' ou NA.\\n",
           "Linhas restantes: ", linhas_apos_filtro_exp_vac, "\\n"))

# ==============================================================================
# 5. CÁLCULO DE ESCORES (DOMÍNIOS E TOTAL)
# ==============================================================================

# Criar um resumo dos domínios conforme definido no README
resumo_dominios <- data.frame(
  dominio = c("Clima de trabalho em equipe", "Clima de Segurança", 
              "Satisfação no trabalho", "Percepção do estresse",
              "Percepção da gestão", "Condições de trabalho",
              "Itens não correlacionados"),
  itens = c("1-6", "7-13", "15-19", "20-23", "24-29", "30-32", "14, 33-36"),
  n_itens = c(6, 7, 5, 4, 6, 3, 5)
)

# Mostrar o resumo dos domínios
gt::gt(resumo_dominios) %>%
  gt::cols_label(
    dominio = "Domínio",
    itens = "Questões",
    n_itens = "Nº de Itens"
  ) %>%
  gt::tab_header(title = "Estrutura do Questionário") %>%
  gt::tab_footnote(
    footnote = "Conforme descrito no README do projeto",
    locations = gt::cells_title()
  )

# Função para mapear os valores originais para a escala 0-100
mapear_escala <- function(x) {
  case_when(
    x == 1 ~ 0,
    x == 2 ~ 25,
    x == 3 ~ 50,
    x == 4 ~ 75,
    x == 5 ~ 100,
    TRUE ~ NA_real_
  )
}

# Aplicar o mapeamento em todas as questões do questionário
dados_mapeados <- dados_limpos %>%
  mutate(across(all_of(colunas_questionario), mapear_escala))

# Calcular domínios corretamente com a nova escala mapeada
dados_mapeados <- dados_mapeados %>%
  mutate(
    # Inverter escala para questões negativas (já mapeadas)
    q2_inv = 100 - dados_mapeados$`2. É difícil falar abertamente se eu percebo um problema com o cuidado ao paciente relacionado a vacinação`,
    q11_inv = 100 - dados_mapeados$`11. Nesta unidade, é difícil discutir sobre erros`,
    q20_inv = 100 - dados_mapeados$`20. Quando minha carga de trabalho é excessiva, meu desempenho é prejudicado`,
    q21_inv = 100 - dados_mapeados$`21. Eu sou menos eficiente no trabalho quando estou cansado (a)`,
    q22_inv = 100 - dados_mapeados$`22. Eu tenho maior probabilidade de cometer erros em situações tensas ou hostis`,
    q23_inv = 100 - dados_mapeados$`23. O cansaço prejudica meu desempenho durante situações de stress (ex: interrupções, inquietude do vacinado, choro)`,
    q36_inv = 100 - dados_mapeados$`36. Falhas na comunicação que levam a atrasos no atendimento são comuns`,
    
    # Cálculo dos escores por domínio
    clima_trabalho_equipe = rowMeans(
      cbind(
        dados_mapeados$`1. As sugestões do (a) enfermeiro (a) são bem recebidas na sala de vacinação`, 
        q2_inv, 
        dados_mapeados$`3. Em relação à vacinação, as discordâncias são resolvidas de modo apropriado (ex: não quem está certo, mas o que é melhor para o paciente)`,
        dados_mapeados$`4. Eu tenho o apoio que necessito de outros membros da equipe para assistência aos pacientes na sala e vacinação`,
        dados_mapeados$`5. É fácil para mim, que atuo na vacinação, fazer perguntas quando existe algo que eu não entendo`,
        dados_mapeados$`6. A equipe de enfermagem daqui trabalha junto como uma equipe bem coordenada`
      ), 
      na.rm = TRUE),
    
    clima_seguranca = rowMeans(
      cbind(
        dados_mapeados$`7. Eu me sentiria seguro (a) se fosse vacinado (a) aqui como paciente`,
        dados_mapeados$`8. Erros são tratados de maneira apropriada pela equipe`,
        dados_mapeados$`9. Eu conheço os meios adequados para encaminhar as questões relacionadas à segurança do paciente nesta unidade`,
        dados_mapeados$`10. Eu recebo retorno apropriado sobre meu desempenho`,
        q11_inv,
        dados_mapeados$`12. Sou encorajado (a) por meus colegas a informar qualquer preocupação que eu possa ter quanto à segurança em sala de vacinação`,
        dados_mapeados$`13. A cultura de segurança em vacinação torna fácil aprender com os erros dos outros`
      ), 
      na.rm = TRUE),
    
    satisfacao_trabalho = rowMeans(
      cbind(
        dados_mapeados$`15. Eu gosto de trabalhar com vacinação`,
        dados_mapeados$`16. Trabalhar aqui é como fazer parte de uma grande família`,
        dados_mapeados$`17. A sala de vacinação é um bom lugar para trabalhar`,
        dados_mapeados$`18. Eu me orgulho de trabalhar na vacinação`,
        dados_mapeados$`19. O prestígio em trabalhar com vacinação é alto`
      ), 
      na.rm = TRUE),
    
    percepcao_estresse = rowMeans(
      cbind(q20_inv, q21_inv, q22_inv, q23_inv), 
      na.rm = TRUE),
    
    percepcao_gestao = rowMeans(
      cbind(
        dados_mapeados$`24. A coordenação apoia meus esforços diários`,
        dados_mapeados$`25. A coordenação não compromete conscientemente a segurança do paciente`,
        dados_mapeados$`26. A coordenação está fazendo um bom trabalho`,
        dados_mapeados$`27. Profissionais problemáticos da equipe são tratados de maneira construtiva por nossa coordenação da unidade`,
        dados_mapeados$`28. Recebo informações adequadas e oportunas sobre eventos que podem afetar o meu trabalho em sala de vacinação`,
        dados_mapeados$`29. Na sala de vacinação, o número e a qualificação dos profissionais são suficientes para lidar com o número de pacientes`
      ), 
      na.rm = TRUE),
    
    condicoes_trabalho = rowMeans(
      cbind(
        dados_mapeados$`30. Esta unidade faz um bom trabalho no treinamento de novos membros da equipe de vacinação`,
        dados_mapeados$`31. Toda informação necessária para a vacinação está disponível rotineiramente para mim`,
        dados_mapeados$`32. Estagiários da minha profissão são adequadamente supervisionados`
      ), 
      na.rm = TRUE)
  )

# Verificar estatísticas dos domínios na escala 0-100
summary(dados_mapeados[c("clima_trabalho_equipe", "clima_seguranca", "satisfacao_trabalho", 
                        "percepcao_estresse", "percepcao_gestao", "condicoes_trabalho")])

# Identificar colunas das questões originais a serem usadas na média geral
colunas_originais_para_media <- setdiff(
  colunas_questionario, 
  c("2. É difícil falar abertamente se eu percebo um problema com o cuidado ao paciente relacionado a vacinação",
    "11. Nesta unidade, é difícil discutir sobre erros",
    "20. Quando minha carga de trabalho é excessiva, meu desempenho é prejudicado",
    "21. Eu sou menos eficiente no trabalho quando estou cansado (a)",
    "22. Eu tenho maior probabilidade de cometer erros em situações tensas ou hostis",
    "23. O cansaço prejudica meu desempenho durante situações de stress (ex: interrupções, inquietude do vacinado, choro)",
    "36. Falhas na comunicação que levam a atrasos no atendimento são comuns")
)

# Colunas invertidas a serem usadas na média geral
colunas_invertidas_para_media <- c("q2_inv", "q11_inv", "q20_inv", "q21_inv", "q22_inv", "q23_inv", "q36_inv")

# Criar a avaliação global do clima de segurança (média das 36 questões mapeadas/invertidas)
dados_mapeados <- dados_mapeados %>%
  mutate(
    clima_seguranca_total = rowMeans(
      select(., all_of(c(colunas_originais_para_media, colunas_invertidas_para_media))),
      na.rm = TRUE
    ),
    # Adicionar a pontuação média dos domínios
    clima_dominio_medio = rowMeans(
      select(., clima_trabalho_equipe, clima_seguranca, satisfacao_trabalho,
             percepcao_estresse, percepcao_gestao, condicoes_trabalho),
      na.rm = TRUE
    )
  )

# Adicionar classificação por pontuação (agora baseada na média das 36 questões)
dados_mapeados <- dados_mapeados %>%
  mutate(
    classificacao_clima = case_when(
      clima_seguranca_total >= 75 ~ "Positivo",
      clima_seguranca_total >= 50 ~ "Neutro",
      TRUE ~ "Negativo"
    )
  )

# >>> JUNÇÃO COM DADOS DOS MUNICÍPIOS <<<
if (!is.null(df_municipios_info)) {
  # Preparar coluna de junção em dados_mapeados (usando a mesma limpeza)
  dados_mapeados <- dados_mapeados %>% 
    mutate(Municipio_join = limpar_nomes(Município))
  
  # Realizar a junção (left_join para manter todos os respondentes)
  dados_mapeados <- left_join(dados_mapeados, 
                              df_municipios_info %>% select(Municipio_join, MACRORREGIOES, URS, PORTE_IBGE), 
                              by = "Municipio_join")
  
  # Verificar quantos NAs foram introduzidos (municípios sem correspondência na planilha)
  nas_macro <- sum(is.na(dados_mapeados$MACRORREGIOES))
  nas_porte <- sum(is.na(dados_mapeados$PORTE_IBGE))
  cat(paste("\nApós junção com dados municipais:", 
            nas_macro, "registros sem MACRORREGIOES,",
            nas_porte, "registros sem PORTE_IBGE.\n"))
  
  # Remover coluna auxiliar de junção, se desejar
  # dados_mapeados <- dados_mapeados %>% select(-Municipio_join)
  
} else {
  warning("Não foi possível realizar a junção com os dados dos municípios devido a erro no carregamento.")
}

# Apresentar resultados da classificação e das pontuações finais em tabelas formatadas

# Tabela 1: Distribuição da Classificação do Clima (Baseada na Média das 36 Questões)
tabela_classificacao <- dados_mapeados %>%
  count(classificacao_clima) %>%
  mutate(
    Percentual = n / sum(n) * 100,
    # Ordenar manualmente para Positivo, Neutro, Negativo
    classificacao_clima = factor(classificacao_clima, levels = c("Positivo", "Neutro", "Negativo"))
  ) %>%
  arrange(classificacao_clima) %>% # Garantir a ordem
  gt::gt() %>% 
  gt::cols_label(
    classificacao_clima = "Classificação do Clima",
    n = "N",
    Percentual = "Percentual (%)"
  ) %>% 
  gt::fmt_number(
    columns = Percentual,
    decimals = 1
  ) %>% 
  gt::tab_header(title = "Distribuição da Classificação Geral do Clima de Segurança") %>% 
  gt::tab_footnote(
    footnote = "Classificação baseada na média das 36 questões (clima_seguranca_total).",
    locations = gt::cells_title()
  )

print(tabela_classificacao)

# Tabela 2: Estatísticas Descritivas das Pontuações Finais (Manual com gt)
stats <- dados_mapeados %>% summarise(
  mean_total = mean(clima_seguranca_total, na.rm = TRUE),
  sd_total = sd(clima_seguranca_total, na.rm = TRUE),
  median_total = median(clima_seguranca_total, na.rm = TRUE),
  p25_total = quantile(clima_seguranca_total, .25, na.rm = TRUE),
  p75_total = quantile(clima_seguranca_total, .75, na.rm = TRUE),
  min_total = min(clima_seguranca_total, na.rm = TRUE),
  max_total = max(clima_seguranca_total, na.rm = TRUE),

  mean_dom = mean(clima_dominio_medio, na.rm = TRUE),
  sd_dom = sd(clima_dominio_medio, na.rm = TRUE),
  median_dom = median(clima_dominio_medio, na.rm = TRUE),
  p25_dom = quantile(clima_dominio_medio, .25, na.rm = TRUE),
  p75_dom = quantile(clima_dominio_medio, .75, na.rm = TRUE),
  min_dom = min(clima_dominio_medio, na.rm = TRUE),
  max_dom = max(clima_dominio_medio, na.rm = TRUE)
)

df_summary <- tibble(
  Pontuacao = c("Escore Total (36 Questões)", "Escore Médio Domínios"),
  Média = c(round(stats$mean_total, 1), round(stats$mean_dom, 1)),
  `Desvio Padrão` = c(round(stats$sd_total, 1), round(stats$sd_dom, 1)),
  Mediana = c(round(stats$median_total, 1), round(stats$median_dom, 1)),
  `P25 - P75` = c(
    paste0(round(stats$p25_total, 1), " - ", round(stats$p75_total, 1)),
    paste0(round(stats$p25_dom, 1), " - ", round(stats$p75_dom, 1))
  ),
  `Mín - Máx` = c(
    paste0(round(stats$min_total, 1), " - ", round(stats$max_total, 1)),
    paste0(round(stats$min_dom, 1), " - ", round(stats$max_dom, 1))
  )
)

resumo_pontuacoes <- df_summary %>% 
  gt::gt() %>%
  gt::cols_label(
    Pontuacao = "Pontuação",
    Média = "Média",
    `Desvio Padrão` = "DP",
    Mediana = "Mediana",
    `P25 - P75` = "IQR (P25-P75)",
    `Mín - Máx` = "Min-Max"
  ) %>%
  gt::tab_header(title = "Tabela 2: Estatísticas Descritivas das Pontuações Finais (Escala 0-100)")

print(resumo_pontuacoes)

# ==============================================================================
# 6. EXPORTAÇÃO DOS RESULTADOS PRELIMINARES (DADOS COM ESCORES)
# ==============================================================================

# Instalar e carregar o pacote writexl se necessário
if (!require("writexl")) {
  install.packages("writexl")
}
library(writexl)

# Criar o diretório de saída se ele não existir
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("Diretório 'output' criado.\n")
} else {
  cat("Diretório 'output' já existe.\n")
}

# Definir o caminho do arquivo de saída
caminho_saida_xlsx <- file.path(output_dir, "dados_mapeados_com_pontuacoes.xlsx")

# Exportar o dataframe para XLSX
write_xlsx(dados_mapeados, path = caminho_saida_xlsx)

cat("DataFrame 'dados_mapeados' exportado com sucesso para:", caminho_saida_xlsx, "\n")

# ==============================================================================
# 7. GRÁFICOS LIKERT POR DOMÍNIO
# ==============================================================================

# Instalar e carregar pacotes necessários
if (!require("likert")) install.packages("likert")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("patchwork")) install.packages("patchwork") # Adicionar patchwork
library(likert)
library(RColorBrewer)
library(dplyr) # Certificar que dplyr está carregado
library(patchwork) # Carregar patchwork

# Definir os níveis da escala Likert em português
likert_levels <- c("Discordo Totalmente", "Discordo Pouco", "Neutro", "Concordo Pouco", "Concordo Totalmente")

# >>> Definir nomes das perguntas com pontuação invertida <<<
# (Mantido caso seja útil para referência, mas não usado para formatação abaixo)
perguntas_invertidas <- c(
  "2. É difícil falar abertamente se eu percebo um problema com o cuidado ao paciente relacionado a vacinação",
  "11. Nesta unidade, é difícil discutir sobre erros",
  "20. Quando minha carga de trabalho é excessiva, meu desempenho é prejudicado",
  "21. Eu sou menos eficiente no trabalho quando estou cansado (a)",
  "22. Eu tenho maior probabilidade de cometer erros em situações tensas ou hostis",
  "23. O cansaço prejudica meu desempenho durante situações de stress (ex: interrupções, inquietude do vacinado, choro)",
  "36. Falhas na comunicação que levam a atrasos no atendimento são comuns" 
)

# Função para preparar os dados para o pacote likert
prepare_likert_data <- function(df, columns) {
  df_subset <- df %>% select(all_of(columns))
  df_likert <- df_subset %>%
    mutate(across(everything(), ~factor(.x, levels = 1:5, labels = likert_levels, ordered = TRUE)))
  return(as.data.frame(df_likert))
}

# Definir colunas por domínio
# ... (código existente cols_clima_equipe, etc.) ...

# Preparar os dados para cada domínio
# ... (código existente data_likert_equipe, etc.) ...

# Criar objetos likert
# ... (código existente likert_equipe, etc.) ...

# Definir uma paleta de cores divergente personalizada
# ... (código existente cores_personalizadas) ...

# Gerar os gráficos de barras divergentes (salvar objetos, ajustar temas para patchwork)

# --- Clima de Trabalho em Equipe ---
p_equipe <- plot(likert_equipe, centered = TRUE, center = 3, colors = cores_personalizadas, 
                 group.order = cols_clima_equipe) + 
  ggtitle("Clima de Trabalho em Equipe") + # Título mais curto
  # guides(fill = guide_legend(title = "Nível de Concordância")) + # Guia será coletado pelo patchwork
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.position = "none", # Remover legenda individual
        axis.text.y = element_text(size = 8, face = "plain")) # Tamanho reduzido e face normal
# Modificar rótulos do eixo Y para marcar perguntas invertidas
labels_equipe <- ifelse(cols_clima_equipe %in% perguntas_invertidas,
                       paste0(cols_clima_equipe, " (R)"),
                       cols_clima_equipe)
wrapped_labels_equipe <- stringr::str_wrap(labels_equipe, width = 60) # Ajustar largura conforme necessário
p_equipe <- p_equipe + scale_y_discrete(labels = wrapped_labels_equipe)

# --- Clima de Segurança ---
p_seguranca <- plot(likert_seguranca, centered = TRUE, center = 3, colors = cores_personalizadas, 
                   group.order = cols_clima_seguranca) + 
  ggtitle("Clima de Segurança") + # Título mais curto
  # guides(fill = guide_legend(title = "Nível de Concordância")) + 
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.position = "none", # Remover legenda individual
        axis.text.y = element_text(size = 8, face = "plain")) # Tamanho reduzido e face normal
# Modificar rótulos do eixo Y
labels_seguranca <- ifelse(cols_clima_seguranca %in% perguntas_invertidas,
                          paste0(cols_clima_seguranca, " (R)"),
                          cols_clima_seguranca)
wrapped_labels_seguranca <- stringr::str_wrap(labels_seguranca, width = 60)
p_seguranca <- p_seguranca + scale_y_discrete(labels = wrapped_labels_seguranca)

# --- Satisfação no Trabalho ---
p_satisfacao <- plot(likert_satisfacao, centered = TRUE, center = 3, colors = cores_personalizadas, 
                    group.order = cols_satisfacao) + 
  ggtitle("Satisfação no Trabalho") + # Título mais curto
  # guides(fill = guide_legend(title = "Nível de Concordância")) + 
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.position = "none", # Remover legenda individual
        axis.text.y = element_text(size = 8, face = "plain")) # Tamanho reduzido e face normal
# Apenas quebrar linhas
wrapped_labels_satisfacao <- stringr::str_wrap(cols_satisfacao, width = 60)
p_satisfacao <- p_satisfacao + scale_y_discrete(labels = wrapped_labels_satisfacao)

# --- Percepção do Estresse ---
p_estresse <- plot(likert_estresse, centered = TRUE, center = 3, colors = cores_personalizadas, 
                  group.order = cols_estresse) + 
  ggtitle("Percepção do Estresse") + # Título mais curto
  # guides(fill = guide_legend(title = "Nível de Concordância")) + 
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.position = "none", # Remover legenda individual
        axis.text.y = element_text(size = 8, face = "plain")) # Tamanho reduzido e face normal
# Modificar rótulos do eixo Y
labels_estresse <- ifelse(cols_estresse %in% perguntas_invertidas,
                          paste0(cols_estresse, " (R)"),
                          cols_estresse)
wrapped_labels_estresse <- stringr::str_wrap(labels_estresse, width = 60)
p_estresse <- p_estresse + scale_y_discrete(labels = wrapped_labels_estresse)

# --- Percepção da Gestão ---
p_gestao <- plot(likert_gestao, centered = TRUE, center = 3, colors = cores_personalizadas, 
                group.order = cols_gestao) + 
  ggtitle("Percepção da Gestão") + # Título mais curto
  # guides(fill = guide_legend(title = "Nível de Concordância")) + 
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.position = "none", # Remover legenda individual
        axis.text.y = element_text(size = 8, face = "plain")) # Tamanho reduzido e face normal
# Apenas quebrar linhas
wrapped_labels_gestao <- stringr::str_wrap(cols_gestao, width = 60)
p_gestao <- p_gestao + scale_y_discrete(labels = wrapped_labels_gestao)

# --- Condições de Trabalho ---
p_condicoes <- plot(likert_condicoes, centered = TRUE, center = 3, colors = cores_personalizadas, 
                   group.order = cols_condicoes) + 
  ggtitle("Condições de Trabalho") + # Título mais curto
  # guides(fill = guide_legend(title = "Nível de Concordância")) + 
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        legend.position = "none", # Remover legenda individual
        axis.text.y = element_text(size = 8, face = "plain")) # Tamanho reduzido e face normal
# Apenas quebrar linhas
wrapped_labels_condicoes <- stringr::str_wrap(cols_condicoes, width = 60)
p_condicoes <- p_condicoes + scale_y_discrete(labels = wrapped_labels_condicoes)

# <<< COMBINAR GRÁFICOS COM PATCHWORK (LEGENDA EMBAIXO) >>>
combined_likert_plot <- (p_equipe + p_seguranca + p_satisfacao) / 
                        (p_estresse + p_gestao + p_condicoes) + 
                        plot_layout(guides = 'collect', legend.position = 'bottom') + # Coletar legendas E posicionar embaixo
                        plot_annotation(title = 'Distribuição das Respostas por Domínio', 
                                        theme = theme(plot.title = element_text(hjust = 0.5, size=14)))

# Imprimir o gráfico combinado
print(combined_likert_plot)

# Nota sobre o centramento e cores:
# Estes gráficos estão centrados no nível 'Neutro' (nível 3 da escala 1-5). 
# As cores ajudam a identificar a direção (ex: azul para concordância, vermelho para discordância).

# ==============================================================================
# 8. VISUALIZAÇÕES DOS ESCORES DOS DOMÍNIOS (BOXPLOT)
# ==============================================================================

# Gráfico da pontuação média por domínio
medias_dominios <- dados_mapeados %>%
  summarise(
    "Clima de Trabalho em Equipe" = mean(clima_trabalho_equipe, na.rm = TRUE),
    "Clima de Segurança" = mean(clima_seguranca, na.rm = TRUE),
    "Satisfação no Trabalho" = mean(satisfacao_trabalho, na.rm = TRUE),
    "Percepção do Estresse" = mean(percepcao_estresse, na.rm = TRUE),
    "Percepção da Gestão" = mean(percepcao_gestao, na.rm = TRUE),
    "Condições de Trabalho" = mean(condicoes_trabalho, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "dominio", values_to = "media")

ggplot(medias_dominios, aes(x = reorder(dominio, -media), y = media, fill = media >= 75)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("< 75", "≥ 75"),
                    name = "Avaliação") +
  theme_minimal() +
  labs(title = "Pontuação Média por Domínio", 
       x = "Domínio", 
       y = "Pontuação Média (0-100)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1)

# Gráfico de boxplot para os domínios na escala 0-100
dados_dominios_long <- dados_mapeados %>%
  select(clima_trabalho_equipe, clima_seguranca, satisfacao_trabalho, 
         percepcao_estresse, percepcao_gestao, condicoes_trabalho) %>%
  pivot_longer(cols = everything(), 
               names_to = "dominio", 
               values_to = "escore")

ggplot(dados_dominios_long, aes(x = dominio, y = escore)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribuição dos escores por domínio (Escala 0-100)",
       x = "Domínio", 
       y = "Escore") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Manter ou ajustar tamanho aqui
        axis.title.x = element_text(size = 12), # Aumentar título eixo X
        axis.title.y = element_text(size = 12), # Aumentar título eixo Y
        plot.title = element_text(hjust = 0.5, size = 14)) + # Aumentar e centralizar título
  geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)


# ==============================================================================
# 9. ANÁLISES DESCRITIVAS POR VARIÁVEIS DEMOGRÁFICAS E PROFISSIONAIS
# ==============================================================================

# Análise por características demográficas e profissionais

# 1. Resumo estatístico da pontuação total por cargo
resumo_por_cargo <- dados_mapeados %>%
  group_by(`Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`) %>%
  summarise(
    n = n(),
    media = mean(clima_seguranca_total, na.rm = TRUE),
    dp = sd(clima_seguranca_total, na.rm = TRUE),
    mediana = median(clima_seguranca_total, na.rm = TRUE),
    min = min(clima_seguranca_total, na.rm = TRUE),
    max = max(clima_seguranca_total, na.rm = TRUE),
    positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
    pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
  ) %>%
  arrange(desc(media))

# Visualizar resumo por cargo
gt::gt(resumo_por_cargo) %>%
  gt::cols_label(
    `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)` = "Cargo",
    n = "N",
    media = "Média",
    dp = "Desvio Padrão",
    mediana = "Mediana",
    min = "Mínimo",
    max = "Máximo",
    positivo = "Clima Positivo (n)",
    pct_positivo = "Clima Positivo (%)"
  ) %>%
  gt::fmt_number(
    columns = c(media, dp, mediana, min, max),
    decimals = 1
  ) %>%
  gt::fmt_number(
    columns = pct_positivo,
    decimals = 1
  ) %>%
  gt::tab_header(title = "Análise do Clima de Segurança por Cargo")

# 2. Análise por formação/escolaridade
resumo_por_formacao <- dados_mapeados %>%
  group_by(`Formação`) %>%
  summarise(
    n = n(),
    media = mean(clima_seguranca_total, na.rm = TRUE),
    dp = sd(clima_seguranca_total, na.rm = TRUE),
    mediana = median(clima_seguranca_total, na.rm = TRUE),
    positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
    pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
  ) %>%
  arrange(desc(media))

# Visualizar tabela de resumo por formação
gt::gt(resumo_por_formacao) %>%
  gt::cols_label(
    `Formação` = "Formação",
    n = "N",
    media = "Média",
    dp = "Desvio Padrão",
    mediana = "Mediana",
    positivo = "Clima Positivo (n)",
    pct_positivo = "% Positivo"
  ) %>%
  gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>%
  gt::tab_header(title = "Clima de Segurança por Formação")

# 3. Análise por faixa etária
dados_mapeados <- dados_mapeados %>%
  mutate(
    faixa_etaria = case_when(
      idade_num < 20 ~ "< 20 anos",
      idade_num >= 20 & idade_num <= 30 ~ "20-30 anos",
      idade_num >= 31 & idade_num <= 40 ~ "31-40 anos",
      idade_num >= 41 & idade_num <= 50 ~ "41-50 anos",
      idade_num >= 51 & idade_num <= 60 ~ "51-60 anos",
      idade_num > 60 ~ "> 60 anos",
      TRUE ~ "Não informado" # Mantém caso haja NAs ou inesperados
    ),
    # Converter para fator ordenado com as novas categorias
    faixa_etaria = factor(faixa_etaria, 
                          levels = c("< 20 anos", "20-30 anos", "31-40 anos", 
                                      "41-50 anos", "51-60 anos", "> 60 anos", 
                                      "Não informado"))
  )

resumo_por_idade <- dados_mapeados %>%
  group_by(faixa_etaria) %>%
  summarise(
    n = n(),
    media = mean(clima_seguranca_total, na.rm = TRUE),
    dp = sd(clima_seguranca_total, na.rm = TRUE),
    mediana = median(clima_seguranca_total, na.rm = TRUE),
    positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
    pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
  )

# Visualizar tabela de resumo por faixa etária
gt::gt(resumo_por_idade) %>%
  gt::cols_label(
    faixa_etaria = "Faixa Etária",
    n = "N",
    media = "Média",
    dp = "Desvio Padrão",
    mediana = "Mediana",
    positivo = "Clima Positivo (n)",
    pct_positivo = "% Positivo"
  ) %>%
  gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>%
  gt::tab_header(title = "Clima de Segurança por Faixa Etária")

# 4. Análise por sexo (corrigido)
resumo_por_sexo <- dados_mapeados %>%
  group_by(Sexo) %>%  # Usando "Sexo" com S maiúsculo
  summarise(
    n = n(),
    media = mean(clima_seguranca_total, na.rm = TRUE),
    dp = sd(clima_seguranca_total, na.rm = TRUE),
    mediana = median(clima_seguranca_total, na.rm = TRUE),
    positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
    pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
  )

# Visualizar tabela de resumo por sexo
gt::gt(resumo_por_sexo) %>%
  gt::cols_label(
    Sexo = "Sexo",
    n = "N",
    media = "Média",
    dp = "Desvio Padrão",
    mediana = "Mediana",
    positivo = "Clima Positivo (n)",
    pct_positivo = "% Positivo"
  ) %>%
  gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>%
  gt::tab_header(title = "Clima de Segurança por Sexo")

# 5. Análise por Município
resumo_por_municipio <- dados_mapeados %>%
  group_by(Município) %>%
  summarise(
    n = n(),
    media = mean(clima_seguranca_total, na.rm = TRUE),
    dp = sd(clima_seguranca_total, na.rm = TRUE),
    mediana = median(clima_seguranca_total, na.rm = TRUE),
    positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
    pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
  ) %>%
  arrange(desc(n)) # Ordenar por número de respondentes

# Visualizar tabela de resumo por município
gt::gt(resumo_por_municipio) %>%
  gt::cols_label(
    Município = "Município",
    n = "N",
    media = "Média",
    dp = "Desvio Padrão",
    mediana = "Mediana",
    positivo = "Clima Positivo (n)",
    pct_positivo = "% Positivo"
  ) %>%
  gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>%
  gt::tab_header(title = "Clima de Segurança por Município")

# 6. Análise por Porte do Município (NOVO)
if ("PORTE_IBGE" %in% names(dados_mapeados)) {
  resumo_por_porte <- dados_mapeados %>% 
    filter(!is.na(PORTE_IBGE)) %>% # Filtrar NAs se houver
    group_by(PORTE_IBGE) %>% 
    summarise(
      n = n(),
      media = mean(clima_seguranca_total, na.rm = TRUE),
      dp = sd(clima_seguranca_total, na.rm = TRUE),
      mediana = median(clima_seguranca_total, na.rm = TRUE),
      positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
      pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
    ) %>% 
    arrange(PORTE_IBGE) # Ordenar por porte
  
  print(
    gt::gt(resumo_por_porte) %>% 
      gt::cols_label(
        PORTE_IBGE = "Porte IBGE",
        n = "N",
        media = "Média",
        dp = "DP",
        mediana = "Mediana",
        positivo = "Clima Positivo (n)",
        pct_positivo = "% Positivo"
      ) %>% 
      gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>% 
      gt::tab_header(title = "Clima de Segurança por Porte do Município (IBGE)")
  )
} else {
  cat("\nColuna 'PORTE_IBGE' não encontrada para análise por porte.\n")
}

# 7. Análise por Macrorregião (NOVO)
if ("MACRORREGIOES" %in% names(dados_mapeados)) {
  resumo_por_macro <- dados_mapeados %>% 
    filter(!is.na(MACRORREGIOES)) %>% # Filtrar NAs
    group_by(MACRORREGIOES) %>% 
    summarise(
      n = n(),
      media = mean(clima_seguranca_total, na.rm = TRUE),
      dp = sd(clima_seguranca_total, na.rm = TRUE),
      mediana = median(clima_seguranca_total, na.rm = TRUE),
      positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
      pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
    ) %>% 
    arrange(MACRORREGIOES)
  
  print(
    gt::gt(resumo_por_macro) %>% 
      gt::cols_label(
        MACRORREGIOES = "Macrorregião",
        n = "N",
        media = "Média",
        dp = "DP",
        mediana = "Mediana",
        positivo = "Clima Positivo (n)",
        pct_positivo = "% Positivo"
      ) %>% 
      gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>% 
      gt::tab_header(title = "Clima de Segurança por Macrorregião")
  )
} else {
  cat("\nColuna 'MACRORREGIOES' não encontrada para análise por macrorregião.\n")
}

# 8. Análise por URS (Microrregião) (NOVO) - Pode gerar muitas linhas
if ("URS" %in% names(dados_mapeados)) {
  resumo_por_urs <- dados_mapeados %>% 
    filter(!is.na(URS)) %>% 
    group_by(URS) %>% 
    summarise(
      n = n(),
      media = mean(clima_seguranca_total, na.rm = TRUE),
      dp = sd(clima_seguranca_total, na.rm = TRUE),
      mediana = median(clima_seguranca_total, na.rm = TRUE),
      positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
      pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n()
    ) %>% 
    arrange(URS)
  
  # cat("\n--- Análise por URS (Microrregião) ---\n") # Comentado ou removido
  # print(resumo_por_urs, n = Inf) # Removido
  
  # <<< Criar tabela GT para URS >>>
  print(
    gt::gt(resumo_por_urs) %>% 
      gt::cols_label(
        URS = "URS (Microrregião)",
        n = "N",
        media = "Média",
        dp = "DP",
        mediana = "Mediana",
        positivo = "Clima Positivo (n)",
        pct_positivo = "% Positivo"
      ) %>% 
      gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>% 
      gt::tab_header(title = "Clima de Segurança por URS (Microrregião)") %>% 
      # Adicionar opção para ajustar tamanho da fonte se a tabela for muito grande
      gt::tab_options(table.font.size = "small") 
  )
  
  # cat("--- Fim Análise por URS ---\n") # Comentado ou removido
  
} else {
  cat("\nColuna 'URS' não encontrada para análise por URS/Microrregião.\n")
}

# 9. Análise Cruzada: Clima de Segurança por Porte do Município e Cargo (NOVO)
col_cargo_analise <- "Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)"
if ("PORTE_IBGE" %in% names(dados_mapeados) && col_cargo_analise %in% names(dados_mapeados)) {
  
  resumo_porte_cargo <- dados_mapeados %>% 
    filter(!is.na(PORTE_IBGE) & !is.na(.data[[col_cargo_analise]])) %>% # Filtrar NAs em ambas as colunas
    group_by(PORTE_IBGE, .data[[col_cargo_analise]]) %>% 
    summarise(
      n = n(),
      media = mean(clima_seguranca_total, na.rm = TRUE),
      dp = sd(clima_seguranca_total, na.rm = TRUE),
      mediana = median(clima_seguranca_total, na.rm = TRUE),
      positivo = sum(classificacao_clima == "Positivo", na.rm = TRUE),
      pct_positivo = 100 * sum(classificacao_clima == "Positivo", na.rm = TRUE) / n(),
      .groups = 'drop' # Evita agrupamento residual
    ) %>% 
    # <<< CONVERTER PORTE PARA FATOR PARA ORDENAÇÃO CORRETA >>>
    mutate(PORTE_IBGE = factor(PORTE_IBGE, levels = c("Pequeno Porte", "Médio Porte", "Grande Porte"))) %>% 
    # Ordenar por Porte (fator), depois por Cargo
    arrange(PORTE_IBGE, .data[[col_cargo_analise]]) 

  print(
    gt::gt(resumo_porte_cargo) %>% 
      gt::cols_label(
        PORTE_IBGE = "Porte IBGE",
        !!sym(col_cargo_analise) := "Cargo", # Renomeia dinamicamente
        n = "N",
        media = "Média",
        dp = "DP",
        mediana = "Mediana",
        positivo = "Clima Positivo (n)",
        pct_positivo = "% Positivo"
      ) %>% 
      gt::fmt_number(columns = c(media, dp, mediana, pct_positivo), decimals = 1) %>% 
      gt::tab_header(title = "Clima de Segurança por Porte do Município e Cargo") %>% 
      # <<< REMOVER AGRUPAMENTO VISUAL >>>
      # gt::tab_row_group(group = "PORTE_IBGE") %>% 
      # gt::row_group_order(groups = c("Pequeno Porte", "Médio Porte", "Grande Porte")) %>% 
      gt::tab_options(table.font.size = "small")
  )
  
  # <<< Adicionar Gráfico Boxplot Cruzado: Porte vs Cargo >>>
  print(
    ggplot(dados_mapeados %>% 
             filter(!is.na(PORTE_IBGE) & !is.na(.data[[col_cargo_analise]])) %>% 
             # Ordenar Porte para o gráfico
             mutate(PORTE_IBGE = factor(PORTE_IBGE, levels = c("Pequeno Porte", "Médio Porte", "Grande Porte"))), 
           aes(x = PORTE_IBGE, y = clima_seguranca_total, fill = .data[[col_cargo_analise]])) +
      geom_boxplot(position = position_dodge(preserve = 'single')) + # position_dodge para separar cargos
      theme_minimal() +
      labs(title = "Clima de Segurança por Porte do Município e Cargo",
           x = "Porte IBGE", 
           y = "Pontuação (0-100)",
           fill = "Cargo") + # Título da legenda
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top") + # Legenda no topo
      geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1)
  )
  
  # <<< Adicionar Gráfico de Barras Médias Cruzado: Porte vs Cargo >>>
  print(
    ggplot(resumo_porte_cargo, # Usar o dataframe já sumarizado
           aes(x = PORTE_IBGE, y = media, fill = .data[[col_cargo_analise]])) +
      # Usar stat="identity" pois 'media' já é a altura desejada
      # Usar position="dodge" para barras lado a lado
      geom_bar(stat = "identity", position = position_dodge(preserve = 'single')) +
      theme_minimal() +
      labs(title = "Média do Clima de Segurança por Porte do Município e Cargo",
           x = "Porte IBGE", 
           y = "Pontuação Média (0-100)",
           fill = "Cargo") + # Título da legenda
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top") + # Legenda no topo
      geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1)
  )
  
} else {
  cat("\nColunas 'PORTE_IBGE' ou 'Cargo...' não encontradas para análise cruzada.\n")
}

# ==============================================================================
# 10. VISUALIZAÇÕES GRÁFICAS DAS ANÁLISES DESCRITIVAS
# ==============================================================================

# Visualizações gráficas
# 1. Boxplot por cargo
ggplot(dados_mapeados, aes(x = reorder(`Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`, clima_seguranca_total, FUN = median), 
                          y = clima_seguranca_total, fill = `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Clima de Segurança por Cargo",
       x = "Cargo", 
       y = "Pontuação (0-100)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)

# 2. Boxplot por faixa etária
ggplot(dados_mapeados, aes(x = faixa_etaria, y = clima_seguranca_total, fill = faixa_etaria)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Clima de Segurança por Faixa Etária",
       x = "Faixa Etária", 
       y = "Pontuação (0-100)") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)

# 4. Comparação dos domínios por cargo
dominio_cargo <- dados_mapeados %>%
  pivot_longer(
    cols = c(clima_trabalho_equipe, clima_seguranca, satisfacao_trabalho, 
             percepcao_estresse, percepcao_gestao, condicoes_trabalho),
    names_to = "dominio",
    values_to = "pontuacao"
  ) %>%
  group_by(`Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`, dominio) %>%
  summarise(
    media = mean(pontuacao, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(dominio_cargo, aes(x = dominio, y = media, fill = `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Média dos Domínios por Cargo",
       x = "Domínio", 
       y = "Pontuação Média (0-100)",
       fill = "Cargo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1)

  # 5. Boxplot por Porte (NOVO)
if ("PORTE_IBGE" %in% names(dados_mapeados)) {
  print(
    ggplot(dados_mapeados %>% filter(!is.na(PORTE_IBGE)), 
           aes(x = PORTE_IBGE, y = clima_seguranca_total, fill = PORTE_IBGE)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Clima de Segurança por Porte do Município (IBGE)",
           x = "Porte IBGE", 
           y = "Pontuação (0-100)") +
      theme(legend.position = "none") +
      geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)
  )
}

# 6. Boxplot por Macrorregião (NOVO)
if ("MACRORREGIOES" %in% names(dados_mapeados)) {
  print(
    ggplot(dados_mapeados %>% filter(!is.na(MACRORREGIOES)), 
           aes(x = reorder(MACRORREGIOES, clima_seguranca_total, FUN = median), 
               y = clima_seguranca_total, fill = MACRORREGIOES)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Clima de Segurança por Macrorregião",
           x = "Macrorregião", 
           y = "Pontuação (0-100)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)
  )
}

# ==============================================================================
# 11. MAPA ESTÁTICO (CENTROIDES) DO CLIMA DE SEGURANÇA POR MUNICÍPIO E REGIÃO
# ==============================================================================

# Instalar pacotes necessários se não estiverem instalados
if (!require("sf")) install.packages("sf")
if (!require("geobr")) install.packages("geobr")
if (!require("stringr")) install.packages("stringr")
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("abjutils")) install.packages("abjutils") # Opcional, mas útil para rm_accent

library(sf)
library(geobr)
library(stringr)
library(ggplot2)
library(dplyr) # Certificar que dplyr está carregado
# library(abjutils) # Carregar se instalado

# 1. Obter dados geográficos (Mantido e Adicionado Regiões)
mg_mun <- read_municipality(code_muni = "MG", year = 2020)

# Tentar carregar dados das regiões de saúde
mg_regions <- NULL # Inicializar como NULL
tryCatch({
  mg_regions_temp <- read_health_region(year = 2019)
  if (nrow(mg_regions_temp) > 0) {
    mg_regions <- mg_regions_temp %>% filter(abbrev_state == "MG")
  }
}, error = function(e) {
  warning(paste("Erro ao baixar/processar dados das regiões de saúde:", e$message))
})

# Verificar se mg_regions foi carregado corretamente
if (is.null(mg_regions) || nrow(mg_regions) == 0) {
  warning("Não foi possível carregar ou encontrar dados das regiões de saúde para MG. O mapa será gerado sem elas.")
  plotar_regioes = FALSE
} else {
  plotar_regioes = TRUE
  cat("Dados das regiões de saúde carregados com sucesso.\n")
}

# Verificar a estrutura dos dados geográficos
# glimpse(mg_mun)
# glimpse(mg_regions)
# plot(mg_mun$geom) # Visualização rápida
# plot(mg_regions$geom)

# 2. Preparar os dados para junção (Mantido)
# <<< REMOVER DEFINIÇÃO ANTIGA DA FUNÇÃO limpar_nomes DAQUI >>>
# limpar_nomes <- function(nomes) {
#   nomes <- stringr::str_to_upper(nomes)
#   # Se tiver abjutils: nomes <- abjutils::rm_accent(nomes)
#   # Alternativa simples sem abjutils (remove acentos comuns):
#   nomes <- iconv(nomes, from = 'UTF-8', to = 'ASCII//TRANSLIT')
#   nomes <- stringr::str_trim(nomes)
#   return(nomes)
# }

# Limpar nomes no resumo e nos dados geográficos
resumo_por_municipio_mapa <- resumo_por_municipio %>% 
  mutate(Municipio_limpo = limpar_nomes(Município))

mg_mun_mapa <- mg_mun %>%
  mutate(Municipio_limpo = limpar_nomes(name_muni))

# Verificar alguns nomes limpos para garantir a correspondência
# head(resumo_por_municipio_mapa$Municipio_limpo)
# head(mg_mun_mapa$Municipio_limpo)

# 3. Juntar os dados de pontuação aos dados geográficos (Mantido)
mg_map_data <- left_join(mg_mun_mapa, 
                         resumo_por_municipio_mapa %>% select(Municipio_limpo, Município, media, n),
                         by = "Municipio_limpo")

# Verificar NAs após a junção (Mantido)
cat("Municípios do mapa sem correspondência nos dados:", sum(is.na(mg_map_data$media)), "\n")

# Filtrar municípios COM dados e calcular centroides
mg_map_data_filtered <- mg_map_data %>% filter(!is.na(media))
mg_centroids_filtered <- st_centroid(mg_map_data_filtered)

# 4. Criar o mapa estático com ggplot2

# Iniciar o plot base
p <- ggplot() +
  # Camada 1: Limites de TODOS os municípios (fundo)
  geom_sf(data = mg_mun, fill = "white", color = "grey85", size = 0.1)

# Adicionar camada das regiões SE ela foi carregada
if (plotar_regioes) {
  p <- p + geom_sf(data = mg_regions, fill = NA, color = "black", size = 0.6)
}

# Adicionar camada dos centroides e demais elementos
p <- p + 
  # Camada 3: Centroides dos municípios COM DADOS (pontos)
  geom_sf(data = mg_centroids_filtered, aes(size = n, color = media),
          shape = 16, # Círculo sólido
          alpha = 0.7) + # Adicionar transparência
  # Escala de cores para a pontuação média
  scale_color_viridis_c(option = "plasma", name = "Pontuação Média\n(0-100)") + 
  # Escala de tamanho para o número de respondentes (ajuste 'range' para aumentar o tamanho)
  scale_size_continuous(name = "Nº Respondentes", range = c(2, 10)) + # Ajuste range (min, max size)
  # Títulos e tema
  labs(title = "Clima de Segurança Médio e Nº de Respondentes por Município",
       subtitle = ifelse(plotar_regioes, 
                         "Pontos nos centroides municipais | Linhas pretas = Macro-regiões de Saúde", 
                         "Pontos nos centroides municipais"),
       caption = "Fonte: Dados da pesquisa | Tamanho do ponto ~ Nº Respondentes | Cor do ponto ~ Pontuação Média") +
  theme_void() + # Tema limpo
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 8),
    legend.position = "right"
   )

# Exibir o mapa final
print(p)

# ==============================================================================
# 13. MAPA ESTÁTICO POR MACRORREGIÃO DE SAÚDE
# ==============================================================================

# Verificar se os dados das regiões e os resumos estão disponíveis
if (!is.null(mg_regions) && exists("resumo_por_macro") && !is.null(resumo_por_macro)) {
  
  cat("\nIniciando criação do mapa por macrorregião...\n")
  
  # 1. Preparar dados geográficos das macrorregiões
  mg_regions_map <- mg_regions %>% 
    mutate(Macro_limpo = limpar_nomes(name_health_region)) %>% 
    # Selecionar colunas relevantes para evitar conflitos de nome
    select(code_health_region, Macro_limpo, geom) 
  
  # 2. Preparar dados de resumo por macrorregião
  resumo_macro_map <- resumo_por_macro %>% 
    mutate(Macro_limpo = limpar_nomes(MACRORREGIOES)) %>% 
    # Selecionar colunas relevantes
    select(Macro_limpo, MACRORREGIOES, media, n) # Manter nome original para rótulos
    
  # 3. Juntar dados geográficos e de resumo
  mg_map_macro_data <- left_join(mg_regions_map, resumo_macro_map, by = "Macro_limpo")
  
  # Verificar NAs após junção (macrorregiões sem dados de respondentes)
  nas_mapa_macro <- sum(is.na(mg_map_macro_data$media))
  if (nas_mapa_macro > 0) {
    cat(paste("Aviso: Encontradas", nas_mapa_macro, "macrorregiões geográficas sem dados de pontuação correspondentes.\n"))
  }
  
  # Calcular centroides para rótulos (APENAS das regiões COM dados)
  mg_macro_centroids <- mg_map_macro_data %>% 
    filter(!is.na(media)) %>% 
    st_centroid() 

  # 4. Criar o mapa com ggplot2
  p_macro <- ggplot() +
    # Camada de fundo: limites municipais (opcional)
    geom_sf(data = mg_mun, fill = NA, color = "grey85", size = 0.1) +
    # Camada principal: Macrorregiões coloridas pela média
    geom_sf(data = mg_map_macro_data, aes(fill = media), color = "black", size = 0.5) +
    # Camada de rótulos: Nomes das macrorregiões nos centroides
    geom_sf_text(data = mg_macro_centroids, aes(label = MACRORREGIOES), 
                 size = 2.5, # Ajustar tamanho da fonte
                 color = "black", 
                 check_overlap = TRUE) + # Evita sobreposição excessiva
    # Escala de cores
    scale_fill_viridis_c(option = "plasma", name = "Pontuação Média\n(0-100)", 
                         na.value = "grey90") + # Cor para regiões sem dados
    # Títulos e tema
    labs(title = "Clima de Segurança Médio por Macrorregião de Saúde",
         caption = "Fonte: Dados da pesquisa | Cor preenchimento ~ Pontuação Média") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.caption = element_text(hjust = 0.5, size = 8),
      legend.position = "right"
    )
  
  # Exibir o mapa
  print(p_macro)
  
} else {
  cat("\nNão foi possível gerar o mapa por macrorregião. Verifique se os dados geográficos (mg_regions) e o resumo (resumo_por_macro) foram carregados/calculados corretamente.\n")
}

