# ==============================================================================
# 1. CONFIGURAÇÃO INICIAL
# ==============================================================================

# Carregar o pacote readxl
library(readxl)

# Carregar pacotes necessários
library(plyr)       # Para a função ddply (dependência de likert)
library(tidyverse)
library(psych)      # Para análises estatísticas descritivas
library(gtsummary)  # Para tabelas de resumo
library(stringr)    # Para manipulação de strings

# Carregar funções auxiliares
source("scripts/funcoes_auxiliares.R")

# Definir o caminho para os arquivos
caminho_respostas <- "data/respostas2.xlsx"
caminho_municipios <- "data/MUNICÍPIOS E SALAS DE VACINAÇÃO SELECIONADOS.xlsx"

# Criar subdiretórios para imagens e tabelas
imagens_dir <- file.path(output_dir, "imagens")
if (!dir.exists(imagens_dir)) {
  dir.create(imagens_dir)
  cat("Diretório 'output/imagens' criado.\n")
} else {
  cat("Diretório 'output/imagens' já existe.\n")
}

tabelas_dir <- file.path(output_dir, "tabelas")
if (!dir.exists(tabelas_dir)) {
  dir.create(tabelas_dir)
  cat("Diretório 'output/tabelas' criado.\n")
} else {
  cat("Diretório 'output/tabelas' já existe.\n")
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
# Criar tabela formatada
tabela_nulos <- gt::gt(df_nulos) %>%
  gt::cols_label(
    coluna = "Nome da Coluna",
    qtd_nulos = "Quantidade de Nulos",
    pct_nulos = "% de Nulos"
  ) %>%
  gt::tab_header(title = "Valores Nulos por Coluna") %>%
  gt::fmt_percent(pct_nulos, decimals = 1, scale_values = FALSE)

# Exibir a tabela
print(tabela_nulos)

# Salvar tabela de valores nulos
gtsave(tabela_nulos, filename = file.path(tabelas_dir, "tabela_valores_nulos.html"))
cat("Tabela 'tabela_valores_nulos.html' salva em", tabelas_dir, "\n")

# Remover colunas com alta porcentagem de valores nulos (acima de 99%)
colunas_quase_vazias <- c(
  "Unidade de Atenção Primária",
  "Unidade de Atenção Primária à Saúde...19",
  "Unidade de Atenção Primária à Saúde...13"
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

# Criar tabela de resumo
tabela_resumo <- resumo_colunas(dados_limpos)

# Mostrar a tabela formatada
tabela_resumo_gt <- gt::gt(tabela_resumo) %>% # Atribuir a um objeto
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

print(tabela_resumo_gt) # Imprimir o objeto

# Salvar tabela de resumo das colunas
gtsave(tabela_resumo_gt, filename = file.path(tabelas_dir, "tabela_resumo_colunas.html"))
cat("Tabela 'tabela_resumo_colunas.html' salva em", tabelas_dir, "\n")

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
png(file = file.path(imagens_dir, "histograma_idades.png"), width = 600, height = 400)
hist(dados_limpos$idade_num, 
     main="Histograma das Idades (Corrigido)", 
     xlab="Idade", 
     breaks=20,
     col="lightblue")
dev.off()

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
col_exp_total <- "Tempo de Experiência Total como Profissional da Enfermagem...41"
col_exp_aps <- "Tempo de Experiência na Atenção Primária à Saúde"
col_exp_vac <- "Tempo de Experiência em Salas de Vacinação"
cols_experiencia <- c(col_exp_total, col_exp_aps, col_exp_vac)

# Definir nomes das novas colunas categorizadas
col_exp_total_cat <- paste0(col_exp_total, "_cat")
col_exp_aps_cat <- paste0(col_exp_aps, "_cat")
col_exp_vac_cat <- paste0(col_exp_vac, "_cat")
cols_experiencia_cat <- c(col_exp_total_cat, col_exp_aps_cat, col_exp_vac_cat)

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

# Salvar tabela de classificação do clima
gtsave(tabela_classificacao, filename = file.path(tabelas_dir, "tabela_classificacao_clima.html"))
cat("Tabela 'tabela_classificacao_clima.html' salva em", tabelas_dir, "\n")

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

# Salvar tabela de estatísticas descritivas das pontuações finais
gtsave(resumo_pontuacoes, filename = file.path(tabelas_dir, "tabela_estatisticas_pontuacoes.html"))
cat("Tabela 'tabela_estatisticas_pontuacoes.html' salva em", tabelas_dir, "\n")

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

# Criar subdiretórios para imagens e tabelas
imagens_dir <- file.path(output_dir, "imagens")
tabelas_dir <- file.path(output_dir, "tabelas")

if (!dir.exists(imagens_dir)) {
  dir.create(imagens_dir)
  cat("Diretório 'imagens' criado em", output_dir, "\n")
} else {
  cat("Diretório 'imagens' já existe em", output_dir, "\n")
}

if (!dir.exists(tabelas_dir)) {
  dir.create(tabelas_dir)
  cat("Diretório 'tabelas' criado em", output_dir, "\n")
} else {
  cat("Diretório 'tabelas' já existe em", output_dir, "\n")
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
if (!require("patchwork")) install.packages("patchwork") 

library(likert)
library(RColorBrewer)
library(dplyr) 
library(patchwork) 
library(stringr) 
library(ggplot2)

# Definir os níveis da escala Likert em português
likert_levels <- c("Discordo Totalmente", "Discordo Pouco", "Neutro", "Concordo Pouco", "Concordo Totalmente")

# Definir nomes das perguntas com pontuação invertida
perguntas_invertidas_base <- c(
  "2. É difícil falar abertamente se eu percebo um problema com o cuidado ao paciente relacionado a vacinação",
  "11. Nesta unidade, é difícil discutir sobre erros",
  "20. Quando minha carga de trabalho é excessiva, meu desempenho é prejudicado",
  "21. Eu sou menos eficiente no trabalho quando estou cansado (a)",
  "22. Eu tenho maior probabilidade de cometer erros em situações tensas ou hostis",
  "23. O cansaço prejudica meu desempenho durante situações de stress (ex: interrupções, inquietude do vacinado, choro)",
  "36. Falhas na comunicação que levam a atrasos no atendimento são comuns" 
)

# Função para preparar os dados para o pacote likert
prepare_likert_data <- function(df, columns_list, inverted_list) {
  df_subset <- df %>% select(all_of(columns_list))
  
  # Adicionar "NEG: " ao INÍCIO dos nomes das colunas invertidas
  new_colnames <- sapply(columns_list, function(col_name) {
    if (col_name %in% inverted_list) {
      # Extrair número e o resto do texto para formatar
      num_pergunta <- str_extract(col_name, "^[0-9]+\\.")
      texto_pergunta <- str_replace(col_name, "^[0-9]+\\.\\s*", "")
      return(paste0(num_pergunta, " NEG: ", texto_pergunta)) # Adiciona NEG após o número
    } else {
      return(col_name)
    }
  })
  colnames(df_subset) <- new_colnames
  
  df_likert <- df_subset %>%
    mutate(across(everything(), ~factor(.x, levels = 1:5, labels = likert_levels, ordered = TRUE)))
  return(as.data.frame(df_likert))
}

# Definir colunas por domínio (mantidas da versão anterior)
cols_clima_equipe <- c("1. As sugestões do (a) enfermeiro (a) são bem recebidas na sala de vacinação", "2. É difícil falar abertamente se eu percebo um problema com o cuidado ao paciente relacionado a vacinação", "3. Em relação à vacinação, as discordâncias são resolvidas de modo apropriado (ex: não quem está certo, mas o que é melhor para o paciente)","4. Eu tenho o apoio que necessito de outros membros da equipe para assistência aos pacientes na sala e vacinação", "5. É fácil para mim, que atuo na vacinação, fazer perguntas quando existe algo que eu não entendo", "6. A equipe de enfermagem daqui trabalha junto como uma equipe bem coordenada")
cols_clima_seguranca <- c("7. Eu me sentiria seguro (a) se fosse vacinado (a) aqui como paciente", "8. Erros são tratados de maneira apropriada pela equipe", "9. Eu conheço os meios adequados para encaminhar as questões relacionadas à segurança do paciente nesta unidade", "10. Eu recebo retorno apropriado sobre meu desempenho", "11. Nesta unidade, é difícil discutir sobre erros", "12. Sou encorajado (a) por meus colegas a informar qualquer preocupação que eu possa ter quanto à segurança em sala de vacinação", "13. A cultura de segurança em vacinação torna fácil aprender com os erros dos outros")
cols_satisfacao <- c("15. Eu gosto de trabalhar com vacinação", "16. Trabalhar aqui é como fazer parte de uma grande família", "17. A sala de vacinação é um bom lugar para trabalhar", "18. Eu me orgulho de trabalhar na vacinação", "19. O prestígio em trabalhar com vacinação é alto")
cols_estresse <- c("20. Quando minha carga de trabalho é excessiva, meu desempenho é prejudicado", "21. Eu sou menos eficiente no trabalho quando estou cansado (a)", "22. Eu tenho maior probabilidade de cometer erros em situações tensas ou hostis", "23. O cansaço prejudica meu desempenho durante situações de stress (ex: interrupções, inquietude do vacinado, choro)")
cols_gestao <- c("24. A coordenação apoia meus esforços diários", "25. A coordenação não compromete conscientemente a segurança do paciente", "26. A coordenação está fazendo um bom trabalho", "27. Profissionais problemáticos da equipe são tratados de maneira construtiva por nossa coordenação da unidade", "28. Recebo informações adequadas e oportunas sobre eventos que podem afetar o meu trabalho em sala de vacinação", "29. Na sala de vacinação, o número e a qualificação dos profissionais são suficientes para lidar com o número de pacientes")
cols_condicoes <- c("30. Esta unidade faz um bom trabalho no treinamento de novos membros da equipe de vacinação", "31. Toda informação necessária para a vacinação está disponível rotineiramente para mim", "32. Estagiários da minha profissão são adequadamente supervisionados")

# Preparar os dados para cada domínio, agora passando a lista de invertidas
data_likert_equipe <- prepare_likert_data(dados_limpos, cols_clima_equipe, perguntas_invertidas_base)
data_likert_seguranca <- prepare_likert_data(dados_limpos, cols_clima_seguranca, perguntas_invertidas_base)
data_likert_satisfacao <- prepare_likert_data(dados_limpos, cols_satisfacao, perguntas_invertidas_base)
data_likert_estresse <- prepare_likert_data(dados_limpos, cols_estresse, perguntas_invertidas_base)
data_likert_gestao <- prepare_likert_data(dados_limpos, cols_gestao, perguntas_invertidas_base)
data_likert_condicoes <- prepare_likert_data(dados_limpos, cols_condicoes, perguntas_invertidas_base)

# Criar objetos likert
likert_equipe <- likert(data_likert_equipe); likert_seguranca <- likert(data_likert_seguranca); likert_satisfacao <- likert(data_likert_satisfacao); likert_estresse <- likert(data_likert_estresse); likert_gestao <- likert(data_likert_gestao); likert_condicoes <- likert(data_likert_condicoes)

# Definir uma paleta de cores divergente personalizada
cores_base_rdbu <- brewer.pal(5, "RdBu"); cores_personalizadas <- c(cores_base_rdbu[1], cores_base_rdbu[2], "gray75", cores_base_rdbu[4], cores_base_rdbu[5])

# Função auxiliar para criar plots Likert
criar_plot_likert_item <- function(likert_obj, data_likert_modificado, titulo_plot, 
                                  cores_lik, 
                                  tamanho_titulo_plot = 12, tamanho_eixo_y = 9, 
                                  tamanho_leg_texto = 9, tamanho_leg_titulo = 10, 
                                  mostrar_legenda = TRUE, largura_wrap_labels = 50,
                                  mostrar_porcentagens = TRUE) { 
  
  nomes_colunas_plot <- colnames(data_likert_modificado)
  labels_y_wrapped <- stringr::str_wrap(nomes_colunas_plot, width = largura_wrap_labels)

  p_final <- plot(likert_obj, centered = TRUE, center = 3, colors = cores_lik, group.order = nomes_colunas_plot,
                  plot.percents = mostrar_porcentagens, 
                  plot.percent.low = FALSE, 
                  plot.percent.high = FALSE,
                  percent.text.size = 3 
                  ) +
    ggtitle(titulo_plot) +
    scale_y_discrete(labels = labels_y_wrapped) +
    theme(
      plot.title = element_text(hjust = 0.5, size = tamanho_titulo_plot),
      # Adicionar margem à direita do texto do eixo Y
      axis.text.y = element_text(size = tamanho_eixo_y, hjust = 0, margin = margin(t=0, r=40, b=0, l=0)), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.text = element_text(size = tamanho_leg_texto),
      legend.title = element_text(size = tamanho_leg_titulo)
    )
  
  if (mostrar_legenda) {
    p_final <- p_final + guides(fill = guide_legend(title = "Nível de Concordância"))
  } else {
    p_final <- p_final + theme(legend.position = "none")
  }
  return(p_final)
}


# --- 1. Gerar e Imprimir Gráficos Likert Individuais ---
cat("\n--- Gerando Gráficos Likert Individuais por Domínio ---\n")

# Ajustar tamanho_eixo_y e largura_wrap para individuais
tam_eixo_y_ind <- 12 # Mantido o ajuste do usuário
larg_wrap_ind <- 100 # Aumentado de 80 para 100
tam_titulo_ind <- 14 # Novo: Tamanho para o título dos gráficos individuais


p_eq_ind <- criar_plot_likert_item(likert_equipe, data_likert_equipe, "Clima de Trabalho em Equipe", cores_personalizadas, tamanho_titulo_plot = tam_titulo_ind, tamanho_eixo_y = tam_eixo_y_ind, largura_wrap_labels = larg_wrap_ind)
print(p_eq_ind)
ggsave(p_eq_ind, file = file.path(imagens_dir, "likert_clima_equipe.png"), width = 8, height = 6)
cat("Gráfico 'likert_clima_equipe.png' salvo em", imagens_dir, "\n")

p_seg_ind <- criar_plot_likert_item(likert_seguranca, data_likert_seguranca, "Clima de Segurança", cores_personalizadas, tamanho_titulo_plot = tam_titulo_ind, tamanho_eixo_y = tam_eixo_y_ind, largura_wrap_labels = larg_wrap_ind)
print(p_seg_ind)
ggsave(p_seg_ind, file = file.path(imagens_dir, "likert_clima_seguranca.png"), width = 8, height = 6)
cat("Gráfico 'likert_clima_seguranca.png' salvo em", imagens_dir, "\n")

p_sat_ind <- criar_plot_likert_item(likert_satisfacao, data_likert_satisfacao, "Satisfação no Trabalho", cores_personalizadas, tamanho_titulo_plot = tam_titulo_ind, tamanho_eixo_y = tam_eixo_y_ind, largura_wrap_labels = larg_wrap_ind)
print(p_sat_ind)
ggsave(p_sat_ind, file = file.path(imagens_dir, "likert_satisfacao.png"), width = 8, height = 6)
cat("Gráfico 'likert_satisfacao.png' salvo em", imagens_dir, "\n")

p_est_ind <- criar_plot_likert_item(likert_estresse, data_likert_estresse, "Percepção do Estresse", cores_personalizadas, tamanho_titulo_plot = tam_titulo_ind, tamanho_eixo_y = tam_eixo_y_ind, largura_wrap_labels = larg_wrap_ind)
print(p_est_ind)
ggsave(p_est_ind, file = file.path(imagens_dir, "likert_estresse.png"), width = 8, height = 6)
cat("Gráfico 'likert_estresse.png' salvo em", imagens_dir, "\n")

p_ges_ind <- criar_plot_likert_item(likert_gestao, data_likert_gestao, "Percepção da Gestão", cores_personalizadas, tamanho_titulo_plot = tam_titulo_ind, tamanho_eixo_y = tam_eixo_y_ind, largura_wrap_labels = larg_wrap_ind)
print(p_ges_ind)
ggsave(p_ges_ind, file = file.path(imagens_dir, "likert_gestao.png"), width = 8, height = 6)
cat("Gráfico 'likert_gestao.png' salvo em", imagens_dir, "\n")

p_con_ind <- criar_plot_likert_item(likert_condicoes, data_likert_condicoes, "Condições de Trabalho", cores_personalizadas, tamanho_titulo_plot = tam_titulo_ind, tamanho_eixo_y = tam_eixo_y_ind, largura_wrap_labels = larg_wrap_ind)
print(p_con_ind)
ggsave(p_con_ind, file = file.path(imagens_dir, "likert_condicoes.png"), width = 8, height = 6)
cat("Gráfico 'likert_condicoes.png' salvo em", imagens_dir, "\n")

# --- 2. Preparar Gráficos para Patchwork e Combinar ---
cat("\n--- Gerando Gráfico Likert Combinado dos Domínios ---\n")

# Para patchwork, não mostrar porcentagens e ajustar tamanhos
p_eq_patch <- criar_plot_likert_item(likert_equipe, data_likert_equipe, "Clima Trab. Equipe", cores_personalizadas, mostrar_legenda=FALSE, mostrar_porcentagens=FALSE, tamanho_titulo_plot=9, tamanho_eixo_y=9, largura_wrap_labels=60)
p_seg_patch <- criar_plot_likert_item(likert_seguranca, data_likert_seguranca, "Clima Segurança", cores_personalizadas, mostrar_legenda=FALSE, mostrar_porcentagens=FALSE, tamanho_titulo_plot=9, tamanho_eixo_y=9, largura_wrap_labels=60)
p_sat_patch <- criar_plot_likert_item(likert_satisfacao, data_likert_satisfacao, "Satisfação Trab.", cores_personalizadas, mostrar_legenda=FALSE, mostrar_porcentagens=FALSE, tamanho_titulo_plot=9, tamanho_eixo_y=9, largura_wrap_labels=60)
p_est_patch <- criar_plot_likert_item(likert_estresse, data_likert_estresse, "Percepção Estresse", cores_personalizadas, mostrar_legenda=FALSE, mostrar_porcentagens=FALSE, tamanho_titulo_plot=9, tamanho_eixo_y=9, largura_wrap_labels=60)
p_ges_patch <- criar_plot_likert_item(likert_gestao, data_likert_gestao, "Percepção Gestão", cores_personalizadas, mostrar_legenda=FALSE, mostrar_porcentagens=FALSE, tamanho_titulo_plot=9, tamanho_eixo_y=9, largura_wrap_labels=60)
p_con_patch <- criar_plot_likert_item(likert_condicoes, data_likert_condicoes, "Condições Trab.", cores_personalizadas, mostrar_legenda=FALSE, mostrar_porcentagens=FALSE, tamanho_titulo_plot=9, tamanho_eixo_y=9, largura_wrap_labels=60)

combined_likert_plot <- (p_eq_patch + p_seg_patch + p_sat_patch) / 
                        (p_est_patch + p_ges_patch + p_con_patch) + 
                        plot_layout(guides = 'collect') + 
                        plot_annotation(title = 'Distribuição das Respostas por Domínio (Perguntas invertidas com NEG:)', 
                                        theme = theme(plot.title = element_text(hjust = 0.5, size=14))) &
                        theme(legend.position = 'bottom') # Aplicar theme para a legenda aqui

print(combined_likert_plot)
ggsave(combined_likert_plot, file = file.path(imagens_dir, "likert_combinado.png"), width = 12, height = 8)
cat("Gráfico 'likert_combinado.png' salvo em", imagens_dir, "\n")

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

p_medias_dominios <- ggplot(medias_dominios, aes(x = reorder(dominio, -media), y = media, fill = media >= 75)) +
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

print(p_medias_dominios)
ggsave(p_medias_dominios, file = file.path(imagens_dir, "pontuacao_media_dominios.png"), width = 8, height = 6)
cat("Gráfico 'pontuacao_media_dominios.png' salvo em", imagens_dir, "\n")

# Gráfico de boxplot para os domínios na escala 0-100
dados_dominios_long <- dados_mapeados %>%
  select(clima_trabalho_equipe, clima_seguranca, satisfacao_trabalho, 
         percepcao_estresse, percepcao_gestao, condicoes_trabalho) %>%
  pivot_longer(cols = everything(), 
               names_to = "dominio", 
               values_to = "escore")

p_boxplot_dominios <- ggplot(dados_dominios_long, aes(x = dominio, y = escore)) +
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

print(p_boxplot_dominios)
ggsave(p_boxplot_dominios, file = file.path(imagens_dir, "boxplot_dominios.png"), width = 8, height = 6)
cat("Gráfico 'boxplot_dominios.png' salvo em", imagens_dir, "\n")


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
tabela_resumo_cargo <- gt::gt(resumo_por_cargo) %>%
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

print(tabela_resumo_cargo)
gtsave(tabela_resumo_cargo, filename = file.path(tabelas_dir, "tabela_resumo_cargo.html"))
cat("Tabela 'tabela_resumo_cargo.html' salva em", tabelas_dir, "\n")

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
tabela_resumo_formacao <- gt::gt(resumo_por_formacao) %>%
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

print(tabela_resumo_formacao)
gtsave(tabela_resumo_formacao, filename = file.path(tabelas_dir, "tabela_resumo_formacao.html"))
cat("Tabela 'tabela_resumo_formacao.html' salva em", tabelas_dir, "\n")

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
tabela_resumo_idade <- gt::gt(resumo_por_idade) %>%
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

print(tabela_resumo_idade)
gtsave(tabela_resumo_idade, filename = file.path(tabelas_dir, "tabela_resumo_idade.html"))
cat("Tabela 'tabela_resumo_idade.html' salva em", tabelas_dir, "\n")

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
tabela_resumo_sexo <- gt::gt(resumo_por_sexo) %>%
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

print(tabela_resumo_sexo)
gtsave(tabela_resumo_sexo, filename = file.path(tabelas_dir, "tabela_resumo_sexo.html"))
cat("Tabela 'tabela_resumo_sexo.html' salva em", tabelas_dir, "\n")

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
tabela_resumo_municipio <- gt::gt(resumo_por_municipio) %>%
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

print(tabela_resumo_municipio)
gtsave(tabela_resumo_municipio, filename = file.path(tabelas_dir, "tabela_resumo_municipio.html"))
cat("Tabela 'tabela_resumo_municipio.html' salva em", tabelas_dir, "\n")

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
  
  tabela_resumo_porte <- gt::gt(resumo_por_porte) %>% 
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
  
  print(tabela_resumo_porte)
  gtsave(tabela_resumo_porte, filename = file.path(tabelas_dir, "tabela_resumo_porte.html"))
  cat("Tabela 'tabela_resumo_porte.html' salva em", tabelas_dir, "\n")
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
  
  tabela_resumo_macro <- gt::gt(resumo_por_macro) %>% 
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
  
  print(tabela_resumo_macro)
  gtsave(tabela_resumo_macro, filename = file.path(tabelas_dir, "tabela_resumo_macro.html"))
  cat("Tabela 'tabela_resumo_macro.html' salva em", tabelas_dir, "\n")
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
  
  # <<< Criar tabela GT para URS >>>
  tabela_resumo_urs <- gt::gt(resumo_por_urs) %>% 
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
  
  print(tabela_resumo_urs)
  gtsave(tabela_resumo_urs, filename = file.path(tabelas_dir, "tabela_resumo_urs.html"))
  cat("Tabela 'tabela_resumo_urs.html' salva em", tabelas_dir, "\n")
  
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

  tabela_resumo_porte_cargo <- gt::gt(resumo_porte_cargo) %>% 
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
  
  print(tabela_resumo_porte_cargo)
  gtsave(tabela_resumo_porte_cargo, filename = file.path(tabelas_dir, "tabela_resumo_porte_cargo.html"))
  cat("Tabela 'tabela_resumo_porte_cargo.html' salva em", tabelas_dir, "\n")
  
  # <<< Adicionar Gráfico Boxplot Cruzado: Porte vs Cargo >>>
  p_boxplot_porte_cargo <- ggplot(dados_mapeados %>% 
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
  
  print(p_boxplot_porte_cargo)
  ggsave(p_boxplot_porte_cargo, file = file.path(imagens_dir, "boxplot_porte_cargo.png"), width = 8, height = 6)
  cat("Gráfico 'boxplot_porte_cargo.png' salvo em", imagens_dir, "\n")
  
  # <<< Adicionar Gráfico de Barras Médias Cruzado: Porte vs Cargo >>>
  p_barras_porte_cargo <- ggplot(resumo_porte_cargo, # Usar o dataframe já sumarizado
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
  
  print(p_barras_porte_cargo)
  ggsave(p_barras_porte_cargo, file = file.path(imagens_dir, "barras_porte_cargo.png"), width = 8, height = 6)
  cat("Gráfico 'barras_porte_cargo.png' salvo em", imagens_dir, "\n")
  
} else {
  cat("\nColunas 'PORTE_IBGE' ou 'Cargo...' não encontradas para análise cruzada.\n")
}

# ==============================================================================
# 10. VISUALIZAÇÕES GRÁFICAS DAS ANÁLISES DESCRITIVAS
# ==============================================================================

# Visualizações gráficas
# 1. Boxplot por cargo
p_boxplot_cargo <- ggplot(dados_mapeados, aes(x = reorder(`Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`, clima_seguranca_total, FUN = median), 
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

print(p_boxplot_cargo)
ggsave(p_boxplot_cargo, file = file.path(imagens_dir, "boxplot_cargo.png"), width = 8, height = 6)
cat("Gráfico 'boxplot_cargo.png' salvo em", imagens_dir, "\n")

# 2. Boxplot por faixa etária
p_boxplot_idade <- ggplot(dados_mapeados, aes(x = faixa_etaria, y = clima_seguranca_total, fill = faixa_etaria)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Clima de Segurança por Faixa Etária",
       x = "Faixa Etária", 
       y = "Pontuação (0-100)") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)

print(p_boxplot_idade)
ggsave(p_boxplot_idade, file = file.path(imagens_dir, "boxplot_idade.png"), width = 8, height = 6)
cat("Gráfico 'boxplot_idade.png' salvo em", imagens_dir, "\n")

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

p_barras_dominio_cargo <- ggplot(dominio_cargo, aes(x = dominio, y = media, fill = `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)`)) +
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

print(p_barras_dominio_cargo)
ggsave(p_barras_dominio_cargo, file = file.path(imagens_dir, "barras_dominio_cargo.png"), width = 8, height = 6)
cat("Gráfico 'barras_dominio_cargo.png' salvo em", imagens_dir, "\n")

# 5. Boxplot por Porte (NOVO)
if ("PORTE_IBGE" %in% names(dados_mapeados)) {
  p_boxplot_porte <- ggplot(dados_mapeados %>% filter(!is.na(PORTE_IBGE)), 
           aes(x = PORTE_IBGE, y = clima_seguranca_total, fill = PORTE_IBGE)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Clima de Segurança por Porte do Município (IBGE)",
           x = "Porte IBGE", 
           y = "Pontuação (0-100)") +
      theme(legend.position = "none") +
      geom_hline(yintercept = 75, linetype = "dashed", color = "green", size = 1.5) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1.5)
  
  print(p_boxplot_porte)
  ggsave(p_boxplot_porte, file = file.path(imagens_dir, "boxplot_porte.png"), width = 8, height = 6)
  cat("Gráfico 'boxplot_porte.png' salvo em", imagens_dir, "\n")
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
# if (!require("sf")) install.packages("sf")
# if (!require("geobr")) install.packages("geobr")
# if (!require("stringr")) install.packages("stringr")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("dplyr")) install.packages("dplyr") # Certificar que dplyr está carregado

library(sf)
library(geobr)
library(stringr)
library(ggplot2)
library(dplyr)

# <<< INÍCIO: Código para criar geometrias das Macrorregiões da Pesquisa (adaptado da Seção 13) >>>
# 0. Verificar e carregar dados necessários para macrorregiões da pesquisa
if (!exists("df_municipios_info") || !("MACRORREGIOES" %in% names(df_municipios_info)) || !("Municipio_join" %in% names(df_municipios_info))) {
  stop("Dataframe 'df_municipios_info' com colunas 'MACRORREGIOES' e 'Municipio_join' não encontrado. Verifique o carregamento na Seção 2.")
}
if (!exists("mg_mun") && exists("read_municipality")) { # mg_mun será carregado abaixo se não existir
  cat("Objeto 'mg_mun' será carregado.\n")
} else if (!exists("mg_mun")) {
  stop("Objeto 'mg_mun' não encontrado e função 'read_municipality' (do geobr) não parece estar disponível.")
}
if (!exists("limpar_nomes") || !is.function(limpar_nomes)) {
  stop("A função 'limpar_nomes' não foi encontrada. Verifique se 'scripts/funcoes_auxiliares.R' foi carregado.")
}

# Carregar mg_mun se ainda não foi carregado nesta seção (geralmente é)
if (!exists("mg_mun")) {
  mg_mun <- read_municipality(code_muni = "MG", year = 2020)
  cat("Dados geográficos dos municípios ('mg_mun') carregados para a Seção 11.\n")
}

# Adicionar a coluna MACRORREGIOES (da pesquisa) aos dados geográficos dos municípios
mg_mun_com_macro_da_pesquisa_sec11 <- mg_mun %>%
  mutate(Municipio_join = limpar_nomes(name_muni)) %>%
  left_join(df_municipios_info %>% distinct(Municipio_join, .keep_all = TRUE) %>% select(Municipio_join, MACRORREGIOES), by = "Municipio_join")

# Criar geometrias das Macrorregiões da pesquisa dissolvendo os limites municipais
sf_macrorregioes_dissolvidas_sec11 <- mg_mun_com_macro_da_pesquisa_sec11 %>%
  filter(!is.na(MACRORREGIOES)) %>% 
  group_by(MACRORREGIOES) %>%
  summarize(
    geometry = st_union(geom), 
    .groups = 'drop'         
  ) %>%
  mutate(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 500)) # Simplificação

cat("Geometrias das macrorregiões da pesquisa preparadas para a Seção 11.\n")
# <<< FIM: Código para criar geometrias das Macrorregiões da Pesquisa >>>


# 1. Obter dados geográficos (mg_mun já deve estar carregado)
# mg_regions (regiões de saúde) não serão mais usadas neste gráfico
# plotar_regioes = FALSE # Não usamos mais esta lógica

# Verificar se mg_mun existe, se não, carregar (já feito acima, mas como dupla checagem)
if (!exists("mg_mun")) {
  mg_mun <- read_municipality(code_muni = "MG", year = 2020)
  cat("Dados geográficos de 'mg_mun' carregados (verificação secundária).\n")
}

# 2. Preparar os dados para junção (para os centroides)
# limpar_nomes já deve estar carregada
# resumo_por_municipio deve existir da Seção 9
if (!exists("resumo_por_municipio")) {
    stop("Objeto 'resumo_por_municipio' não encontrado. Execute a Seção 9 primeiro.")
}
resumo_por_municipio_mapa <- resumo_por_municipio %>% 
  mutate(Municipio_limpo = limpar_nomes(Município))

mg_mun_mapa <- mg_mun %>%
  mutate(Municipio_limpo = limpar_nomes(name_muni))

# 3. Juntar os dados de pontuação aos dados geográficos (para os centroides)
mg_map_data <- left_join(mg_mun_mapa, 
                         resumo_por_municipio_mapa %>% select(Municipio_limpo, Município, media, n),
                         by = "Municipio_limpo")

# Verificar NAs após a junção (Mantido)
cat("Municípios do mapa sem correspondência nos dados (para centroides):", sum(is.na(mg_map_data$media)), "\n")

# Filtrar municípios COM dados e calcular centroides
mg_map_data_filtered <- mg_map_data %>% filter(!is.na(media))
if(nrow(mg_map_data_filtered) == 0){
    stop("Nenhum dado municipal com pontuação média para gerar centroides. Verifique 'resumo_por_municipio'.")
}
mg_centroids_filtered <- st_centroid(mg_map_data_filtered)

# 4. Criar o mapa estático com ggplot2

# Iniciar o plot base
p_centroides_macrofundo <- ggplot() +
  # Camada 1: Macrorregiões da Pesquisa (fundo principal)
  geom_sf(data = sf_macrorregioes_dissolvidas_sec11, fill = "grey92", color = "black", size = 0.6, alpha = 0.8) +
  
  # Camada 2: Limites de TODOS os municípios (REMOVIDA PARA SIMPLICIDADE DO FUNDO)
  # geom_sf(data = mg_mun, fill = NA, color = "grey88", size = 0.05, alpha = 0.6) +

  # Camada 3: Centroides dos municípios COM DADOS (pontos)
  geom_sf(data = mg_centroids_filtered, aes(size = n, color = media),
          shape = 16, # Círculo sólido
          alpha = 0.7) + # Adicionar transparência
  
  # Escala de cores para a pontuação média (igual ao mapa original de centroides)
  scale_color_viridis_c(option = "plasma", name = "Pontuação Média\n(0-100)") + 
  
  # Escala de tamanho para o número de respondentes (igual ao mapa original de centroides)
  scale_size_continuous(name = "Nº Respondentes", range = c(2, 10)) + 
  
  # Títulos e tema
  labs(title = "Clima de Segurança Médio e Nº de Respondentes por Município",
       subtitle = "Pontos nos centroides municipais | Fundo: Macrorregiões da Pesquisa", # Subtítulo ATUALIZADO
       caption = "Fonte: Dados da pesquisa | Tamanho do ponto ~ Nº Respondentes | Cor do ponto ~ Pontuação Média") +
  theme_void() + # Tema limpo
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 8),
    legend.position = "right"
   )

# Exibir o mapa final
print(p_centroides_macrofundo)

# Salvar o mapa final
ggsave(file.path(imagens_dir, "mapa_centroides_macrofundo.png"), p_centroides_macrofundo, width = 10, height = 8, units = "in", dpi = 300)

# ==============================================================================
# 13. MAPA ESTÁTICO POR MACRORREGIÃO DE SAÚDE
# ==============================================================================

# Objetivo: Criar um mapa coroplético de MG, onde as macrorregiões da pesquisa
# são coloridas de acordo com a pontuação média do clima de segurança.
# As geometrias das macrorregiões da pesquisa serão construídas dissolvendo
# os limites dos municípios que pertencem a cada uma.

# Pacotes necessários (sf, dplyr, ggplot2, geobr já devem estar carregados de seções anteriores)
# library(sf)
# library(dplyr)
# library(ggplot2)
# library(geobr)
# library(stringr) # Para str_wrap nos rótulos (opcional)

# 1. Verificar e carregar dados necessários
if (!exists("df_municipios_info") || !("MACRORREGIOES" %in% names(df_municipios_info)) || !("Municipio_join" %in% names(df_municipios_info))) {
  stop("Dataframe 'df_municipios_info' com colunas 'MACRORREGIOES' e 'Municipio_join' não encontrado. Verifique o carregamento na Seção 2.")
}
if (!exists("mg_mun")) {
  warning("Dados geográficos dos municípios ('mg_mun') não encontrados. Tentando carregar novamente.")
  mg_mun <- read_municipality(code_muni = "MG", year = 2020) # Usar o mesmo ano da Seção 11
  cat("Dados de 'mg_mun' carregados para o mapa de macrorregiões.\n")
}
if (!exists("resumo_por_macro") || !("MACRORREGIOES" %in% names(resumo_por_macro))) {
  stop("O objeto 'resumo_por_macro' com a coluna 'MACRORREGIOES' não foi encontrado. Execute a Seção 9, item 7 primeiro.")
}
if (!exists("limpar_nomes") || !is.function(limpar_nomes)) {
  stop("A função 'limpar_nomes' não foi encontrada. Verifique se 'scripts/funcoes_auxiliares.R' foi carregado.")
}

# 2. Preparar dados municipais com a informação da Macrorregião da pesquisa
# Adicionar a coluna MACRORREGIOES (da pesquisa) aos dados geográficos dos municípios
mg_mun_com_macro_da_pesquisa <- mg_mun %>%
  mutate(Municipio_join = limpar_nomes(name_muni)) %>% # Limpa nome do município dos dados geográficos
  left_join(df_municipios_info %>% distinct(Municipio_join, .keep_all = TRUE) %>% select(Municipio_join, MACRORREGIOES), by = "Municipio_join")

# Verificar quantos municípios não tiveram correspondência de Macrorregião
municipios_sem_macro_assoc <- mg_mun_com_macro_da_pesquisa %>% filter(is.na(MACRORREGIOES))
if (nrow(municipios_sem_macro_assoc) > 0) {
  cat("Aviso:", nrow(municipios_sem_macro_assoc), 
      "municípios dos dados geográficos não puderam ser associados a uma macrorregião da pesquisa (pode ser normal se nem todos os municípios de MG estão na sua amostra de municípios).\n")
  # Para depuração, pode-se imprimir alguns:
  # print(head(municipios_sem_macro_assoc %>% select(name_muni, Municipio_join)))
}

# 3. Criar geometrias das Macrorregiões da pesquisa dissolvendo os limites municipais
sf_macrorregioes_dissolvidas <- mg_mun_com_macro_da_pesquisa %>%
  filter(!is.na(MACRORREGIOES)) %>% # Processar apenas municípios com macrorregião atribuída
  group_by(MACRORREGIOES) %>%
  summarize(
    geometry = st_union(geom), # st_union para geometrias sf
    .groups = 'drop'          # Remover agrupamento residual
  ) %>%
  # Adicionar simplificação das geometrias para reduzir o "ruído" visual interno
  mutate(geometry = st_simplify(geometry, preserveTopology = TRUE, dTolerance = 500)) # dTolerance em metros

if (nrow(sf_macrorregioes_dissolvidas) == 0) {
  stop("Nenhuma geometria de macrorregião pôde ser criada pela dissolução dos municípios. Verifique a coluna 'MACRORREGIOES' em 'df_municipios_info' e a junção.")
} else {
  cat(nrow(sf_macrorregioes_dissolvidas), "geometrias de macrorregiões da pesquisa foram criadas dissolvendo municípios.\n")
}

# 4. Juntar as geometrias das macrorregiões com os dados de pontuação média
# 'resumo_por_macro' já tem 'MACRORREGIOES', 'media', 'n', etc.
map_data_macro_final <- sf_macrorregioes_dissolvidas %>%
  left_join(resumo_por_macro, by = "MACRORREGIOES") # Junção pela coluna 'MACRORREGIOES'

# Verificar NAs na coluna 'media' após a junção final
macros_plot_sem_media <- map_data_macro_final %>% filter(is.na(media))
if (nrow(macros_plot_sem_media) > 0) {
  cat("Aviso:", nrow(macros_plot_sem_media), 
      "macrorregiões (polígonos criados) não tiveram correspondência de pontuação média em 'resumo_por_macro'. Essas não serão coloridas.\n")
  # Para depuração:
  # print(macros_plot_sem_media %>% st_drop_geometry() %>% select(MACRORREGIOES))
}

# Filtrar para plotar apenas macrorregiões com dados de média, se necessário, ou plotar todas e as sem média ficarão sem cor (fill=NA)
map_data_para_plotar <- map_data_macro_final # Vamos plotar todas, as sem media ficarão com a cor padrão de NA da escala

if (nrow(map_data_para_plotar %>% filter(!is.na(media))) == 0) {
  stop("Nenhuma macrorregião com dados de pontuação para plotar. Verifique 'resumo_por_macro'.")
}

# 5. Criar o mapa coroplético
# Definir limites da escala de cor com base nos dados disponíveis em resumo_por_macro
min_media <- min(resumo_por_macro$media, na.rm = TRUE)
max_media <- max(resumo_por_macro$media, na.rm = TRUE)

mapa_macro_coropleto <- ggplot() +
  # Camada 1: Polígonos das macrorregiões da pesquisa, coloridos pela pontuação média
  geom_sf(data = map_data_para_plotar, aes(fill = media), color = "black", size = 0.5, alpha = 0.9) +
  
  # scale_fill_viridis_c(
  #   option = "plasma", 
  #   name = "Pontuação Média\n(0-100)", 
  #   limits = c(floor(min_media/10)*10, ceiling(max_media/10)*10), # Arredonda para dezena mais próxima
  #   na.value = "grey80" # Cor para macrorregiões sem dados de média
  # ) +
  scale_fill_distiller( # Usando RColorBrewer para uma paleta sequencial mais clara
    palette = "YlGnBu", 
    direction = 1, # Cores mais escuras para valores mais altos
    name = "Pontuação Média\n(0-100)",
    limits = c(floor(min_media/10)*10, ceiling(max_media/10)*10),
    breaks = waiver(), # Deixar ggplot decidir os breaks ou definir explicitamente ex: seq(60, 90, by = 5)
    na.value = "grey80"
  ) +
  
  # Opcional: Adicionar rótulos com o nome da macrorregião (pode precisar de ajuste de tamanho/posição)
  geom_sf_text(
    data = map_data_para_plotar %>% filter(!is.na(media)), # Rotular apenas as com média
    aes(label = str_wrap(MACRORREGIOES, width = 10)), # str_wrap para quebrar nomes longos
    size = 3.5, # Aumentado o tamanho do texto
    color = "black", 
    fontface = "bold", # Nomes em negrito
    fun.geometry = st_centroid, # Posiciona o texto no centroide
    check_overlap = TRUE # Evita sobreposição de texto (pode remover alguns rótulos)
  ) +

  labs(
    title = "Clima de Segurança Médio por Macrorregião de Planejamento em MG",
    subtitle = "Macrorregiões da pesquisa coloridas pela pontuação média.",
    caption = paste0("Fonte: Dados da pesquisa. N total de respondentes nas macrorregiões com dados: ", 
                     sum(resumo_por_macro$n[resumo_por_macro$MACRORREGIOES %in% (map_data_para_plotar %>% filter(!is.na(media)) %>% pull(MACRORREGIOES))], na.rm = TRUE))
  ) +
  theme_void() + # Tema limpo, sem eixos ou grades
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 8),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", colour = "white") # Fundo branco
  )

# Exibir o mapa
print(mapa_macro_coropleto)

# Salvar o mapa coroplético
ggsave(file.path(imagens_dir, "mapa_macro_coropleto.png"), mapa_macro_coropleto, width = 10, height = 8, units = "in", dpi = 300)

cat("\nMapa coroplético por macrorregião (baseado em municípios dissolvidos) gerado.\n")

# Limpeza opcional de variáveis intermediárias grandes
# rm(mg_mun_com_macro_da_pesquisa, sf_macrorregioes_dissolvidas, map_data_macro_final, map_data_para_plotar)