
library(readxl)
library(dplyr)
library(readr)
library(genderBR)
library(stringr)

al_ef14 <- read_delim("data/dados_apa_ef14/APA_alunos_nao_anonimizados_ef14.csv",
                      delim = ";", escape_double = FALSE,
                      locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
al_ef14$gender <- get_gender(str_replace_all(al_ef14$nome, "[^[:alpha:] ]", ""))
cnames <- do.call(c, sapply(colnames(al_ef14), FUN = function(cname) {
  if (length(unique(al_ef14[[cname]])) > 1) cname
}))
al_ef14 <- al_ef14[,cnames]
al_ef14 <- al_ef14[!is.na(al_ef14$cod_escola),]

al_ef14.by <- al_ef14 %>% group_by(aluno_id, gender, ciclo, id_turma, cod_escola) %>%
  summarise(coesao = mean(coesao_num, na.rm = T),
            tipologia_textual = mean(tipologia_textual_num, na.rm = T),
            segmentacao = mean(segmentacao_num, na.rm = T),
            pontuacao = mean(pontuacao_num, na.rm = T),
            ortografia = mean(ortografia_num, na.rm = T),
            adequacao_a_proposta = mean(adequacao_a_proposta_num, na.rm = T))
al_ef14.by <- mutate(as.data.frame(al_ef14.by),
                 score = (segmentacao+coesao+pontuacao+ortografia+
                            tipologia_textual+adequacao_a_proposta)/6)



al_ef59 <- read_delim("data/dados_apa_ef59/APA_alunos_anonimizados_ef59.csv",
                      delim = ",", escape_double = FALSE,
                      locale = locale(encoding = "UTF-8"), trim_ws = TRUE)
al_ef59$gender <- get_gender(str_replace_all(al_ef59$nome, "[^[:alpha:] ]", ""))
cnames <- do.call(c, sapply(colnames(al_ef59), FUN = function(cname) {
  if (length(unique(al_ef59[[cname]])) > 1) cname
}))
al_ef59 <- al_ef59[,cnames]
al_ef59 <- al_ef59[!is.na(al_ef59$cod_escola),]


al_ef59.by <- al_ef59 %>% group_by(aluno_id, gender, ciclo, id_turma, cod_escola) %>%
  summarise(coesao = mean(coesao_num, na.rm = T),
            coerencia_tematica = mean(coerencia_tematica_num, na.rm = T),
            registro_formal = mean(registro_formal_num, na.rm = T))
al_ef59.by <- mutate(as.data.frame(al_ef59.by),
                     score = (coesao+coerencia_tematica+registro_formal)/3)

#tipologia textual tem valor 1

## escolas

es_ef14 <- read_csv("data/dados_apa_ef14/APA_escolas_ef14.csv")
cnames <- do.call(c, sapply(colnames(es_ef14), FUN = function(cname) {
  if (length(unique(es_ef14[[cname]])) > 1) cname
}))
es_ef14 <- es_ef14[,cnames]
cnames <- c("cod_escola","escola","regiao","uf","municipio","localizacao",
            "localidade_diferenciada","dependencia_administrativa","porte")
es_ef14 <- unique(es_ef14[,cnames])


es_ef59 <- read_csv("data/dados_apa_ef59/APA_escola_ef59.csv")
cnames <- do.call(c, sapply(colnames(es_ef59), FUN = function(cname) {
  if (length(unique(es_ef59[[cname]])) > 1) cname
}))
es_ef59 <- es_ef59[,cnames]
cnames <- c("cod_escola","escola","regiao","uf","municipio","localizacao",
            "localidade_diferenciada","dependencia_administrativa","porte")
es_ef59 <- unique(es_ef59[,cnames])

escolas <- unique(rbind(es_ef14,es_ef59))


## write xlsx

writexl::write_xlsx(
  list(escolas = escolas,
       alunos_ef14 = al_ef14.by, alunos_ef59 = al_ef59.by), "data/data.xlsx")

