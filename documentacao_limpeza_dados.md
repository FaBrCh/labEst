# Documentação das Decisões de Limpeza de Dados

Este arquivo documenta as principais etapas de limpeza e tratamento aplicadas ao conjunto de dados da pesquisa sobre clima de segurança na vacinação.

## 1. Tratamento de Colunas

*   **Colunas Vazias:** Colunas completamente vazias foram removidas automaticamente.
*   **Colunas Quase Vazias:** As colunas `Unidade de Atenção Primária à Saúde...12`, `Unidade de Atenção Primária à Saúde...18`, `Unidade de Atenção Primária à Saúde...14` foram removidas devido a uma alta porcentagem de valores nulos (>99%).

## 2. Tratamento de Linhas (Respondentes)

*   **Valores Nulos:** Linhas (respondentes) que continham *qualquer* valor nulo (`NA`) em *qualquer* coluna foram removidas do conjunto de dados (`drop_na()`). Esta foi uma abordagem conservadora para garantir a completude dos dados para as análises subsequentes.
*   **Idades Fora do Intervalo:** Respondentes com idades calculadas fora do intervalo esperado (menor que 18 ou maior que 90 anos) foram removidos após a limpeza da coluna de idade.
*   **Experiência Mínima/Inválida em Sala de Vacinação:** Respondentes que indicaram ter "Menos de 6 meses", "Não aplicável", ou tiveram um valor ausente (`NA`) na coluna categorizada `Tempo de Experiência em Salas de Vacinação_cat` foram excluídos da análise final.

## 2.5. Tratamento Específico de Cargos e Formação (Aplicado Após Limpeza de Nulos e Idade)

Foram aplicadas as seguintes regras sequencialmente:

1.  **Modificação de Cargo "Diretora":** O cargo `Diretora` na coluna `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)` foi alterado para `Técnico em Enfermagem`.
2.  **Exclusão de Cargo "AUX ADM":** Respondentes cujo cargo na coluna `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)` era `AUX ADM` (considerando variações de caixa e espaços) foram **removidos** da análise.
3.  **Ajuste Formação para Técnicos:** Se o cargo (após etapa 1) fosse `Técnico em Enfermagem` E a `Formação` fosse `Ensino Médio Completo`, a `Formação` foi **alterada** para `Ensino Técnico`.
4.  **Ajuste Cargo para Auxiliares:** Para todos os respondentes restantes cuja `Formação` ainda era `Ensino Médio Completo` (ou seja, não foram afetados pela etapa 3), o `Cargo na Atenção Primária à Saúde (APS) / Unidade Básica de Saúde (UBS)` foi **alterado** para `Auxiliar de Enfermagem`.

## 3. Limpeza da Coluna de Idade (`Idade do Profissonal (em anos) SOMENTE NÚMEROS`)

*   **Conversão Direta:** Tentativa inicial de converter valores diretamente para numérico.
*   **Cálculo a partir do Ano de Nascimento:** Valores numéricos entre 1900 e 2005 foram interpretados como ano de nascimento, e a idade foi calculada como `2023 - ano_nascimento`.
*   **Casos Específicos (Texto):**
    *   "Bom 66" -> 66
    *   "49anos" -> 49
    *   "57 anos" -> 57
*   **Extração de Dígitos:** Para outros valores não numéricos, extraiu-se apenas os dígitos. Se o número resultante estivesse entre 1900 e 2005, foi tratado como ano de nascimento. Se não houvesse dígitos, o resultado era `NA`.
*   **Resultado:** Uma nova coluna `idade_num` foi criada com as idades numéricas limpas.

## 4. Limpeza das Colunas de Tempo de Experiência

As colunas originais `Tempo de Experiência Total como Profissional da Enfermagem...40`, `Tempo de Experiência na Atenção Primária à Saúde`, e `Tempo de Experiência em Salas de Vacinação` foram mantidas. Novas colunas foram criadas para armazenar os valores categorizados:
*   `Tempo de Experiência Total como Profissional da Enfermagem...40_cat`
*   `Tempo de Experiência na Atenção Primária à Saúde_cat`
*   `Tempo de Experiência em Salas de Vacinação_cat`

A padronização foi feita usando a função `categorizar_experiencia` com as seguintes etapas:

*   **Padronização Inicial:** Remoção de espaços extras, conversão para minúsculas.
*   **Categorias Existentes:** Valores que já correspondiam às categorias finais (ex: "de 1 a 3 anos") foram mantidos e padronizados.
*   **Valores Especiais Tratados como Nulos:** `NA`, "", "a", "0.0", "0a", "0", "00", "2s".
*   **Texto "Não Trabalha":** Valores contendo "não trabalha" ou "nao trabalhei" foram mapeados para a categoria "Não aplicável".
*   **Texto Livre Não Categorizável:** Entradas como "Experiência em urgência..." foram mapeadas para `NA`.
*   **Extração de Anos ('A') e Meses ('M'):** Usou expressões regulares para extrair números associados a 'A' e 'M' (ex: "17A", "5M", "10A6M", "3 A").
*   **Conversão para Anos:** O tempo total foi calculado em anos (meses / 12).
*   **Mapeamento para Categorias:** O tempo total em anos foi mapeado para as seguintes categorias:
    *   < 0.5 anos: "Menos de 6 meses"
    *   0.5 a < 1 ano: "de 6 meses a 1 ano"
    *   1 a < 3 anos: "de 1 a 3 anos"
    *   3 a < 5 anos: "de 3 a 5 anos"
    *   5 a < 10 anos: "de 5 a 10 anos"
    *   10 a < 15 anos: "de 10 a 15 anos"
    *   15 a < 20 anos: "de 15 a 20 anos"
    *   >= 20 anos: "Mais de 20 anos"
*   **Tratamento de Nulos:** Se a extração falhasse ou o valor não se encaixasse, era retornado `NA`.

## 5. Tratamento das Colunas do Questionário (Questões 1-36)

*   **Conversão para Numérico:** Todas as colunas das questões foram forçadas para o tipo numérico. `NA`s introduzidos foram verificados.
*   **Mapeamento para Escala 0-100:** As respostas originais (1 a 5) foram mapeadas para uma nova escala:
    *   1 -> 0
    *   2 -> 25
    *   3 -> 50
    *   4 -> 75
    *   5 -> 100
*   **Inversão de Questões Negativas:** As questões 2, 11, 20, 21, 22, 23 e 36 tiveram suas pontuações na escala 0-100 invertidas (`100 - pontuacao_mapeada`) antes do cálculo dos escores dos domínios.

## 6. Criação de Escores e Classificações

*   **Escores dos Domínios:** Calculados como a média das questões correspondentes (já na escala 0-100 e com inversões aplicadas).
*   **Escore Total (`clima_seguranca_total`):** Média direta das 36 questões do questionário (após mapeamento para 0-100 e inversão das questões negativas). Reflete a percepção geral individual.
*   **Média dos Domínios (`clima_dominio_medio`):** Média aritmética dos 6 escores de domínio calculados (`clima_trabalho_equipe`, `clima_seguranca`, etc.). Reflete o equilíbrio entre os diferentes aspectos do clima.
*   **Classificação do Clima:** Baseada no **`clima_seguranca_total`** (média das 36 questões):
    *   >= 75: "Positivo"
    *   >= 50 e < 75: "Neutro"
    *   < 50: "Negativo"

Esta documentação ajuda a rastrear as transformações realizadas e a entender como os dados finais foram obtidos. 