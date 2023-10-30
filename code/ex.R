library(readxl)
library(dplyr)

library(stringr)
library(rshinystatistics)

library(readxl)
library(rstatix)
library(tidyverse)

library(ggpubr)

library(shiny)
library(esquisse)
library(scales)
library(knitr)
library(rmarkdown)

library(utils)
library(ggpubr)
library(ggplot2)
library(randomcoloR)

library(readxl)

dat <- read_excel("data/data.xlsx", sheet = "alunos_ef14")

escolas <- read_excel("data/data.xlsx", sheet = "escolas")
edat <- merge(dat, escolas, by = "cod_escola", all.x = T)

## score ~ ciclo + Error(id/ciclo)

data <- edat[,c("aluno_id","ciclo","score")]
data$ciclo <- factor(edat$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id, time)

(res.aov <- anova_test(ldat, dv = score, wid = id, within = time))

(pwc <- ldat %>% emmeans_test(score ~ time, detailed = T,
                              p.adjust.method = "bonferroni"))


pwc <- add_xy_position(pwc, x = "time", fun = "mean_se")
ggline(get_emmeans(pwc), x = "time", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  stat_pvalue_manual(pwc, hide.ns = T, tip.length = F)

## score ~ ciclo*localizacao + Error(id/ciclo)

data <- edat[,c("aluno_id","localizacao","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","localizacao","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)


(res.aov <- anova_test(ldat, dv = score, wid = id, between = localizacao, within = time))


(pwc1 <- ldat %>% group_by(time) %>%
    emmeans_test(score ~ localizacao, detailed = T, p.adjust.method = "bonferroni"))

pwc1 <- add_xy_position(pwc1, x = "time", fun = "mean_se", dodge = 0.25)
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc1), x = "time", y = "emmean", color = "localizacao",
       palette = "jco", position = pd, ylab = "score") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = localizacao),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc1, hide.ns = T, tip.length = F, linetype = 1)



(pwc2 <- ldat %>% group_by(localizacao) %>%
    emmeans_test(score ~ time, detailed = T, p.adjust.method = "bonferroni"))

emms <- get_emmeans(pwc2)
plots <- lapply(unique(gwc$localizacao), FUN = function(x) {
  emms <- gwc[which(gwc$localizacao == x),]
  ggline(emms, x = "time", y = "emmean", color = "#FF0000", ylab = "score") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = "#FF0000"),
                  width = 0.2) +
    stat_pvalue_manual(add_xy_position(
      pwc2[which(pwc2$localizacao == x),],x = "time", fun = "mean_se"),
      hide.ns = T, color = "#FF0000", tip.length = F) +
    labs(color = x)+ theme(legend.text = element_blank())
})

plots[[1]]
plots[[2]]


pwc2 <- add_xy_position(pwc2, x = "time", fun = "mean_se")
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc1), x = "time", y = "emmean", color = "localizacao",
       palette = "jco", position = pd, ylab = "score") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = localizacao),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc2, hide.ns = T, color = "localizacao",
                     position = pd, tip.length = F)




## score ~ ciclo*gender + Error(id/ciclo)

data <- dat[,c("aluno_id","gender","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","gender","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)



(res.aov <- anova_test(ldat, dv = score, wid = id, between = gender, within = time))

(pwc1 <- ldat %>% group_by(gender) %>%
    emmeans_test(score ~ time, detailed = T, p.adjust.method = "bonferroni"))



pwc1 <- add_xy_position(pwc1, x = "time", fun = "mean_se")
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc1), x = "time", y = "emmean", color = "gender",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = gender),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc1, hide.ns = T, color = "gender",
                     position = pd, tip.length = F) +
  ggplot2::scale_color_manual(values=c("#FF007F","#4D4DFF"))


(pwc2 <- ldat %>% group_by(time) %>%
    emmeans_test(score ~ gender, detailed = T, p.adjust.method = "bonferroni"))

pwc2 <- add_xy_position(pwc2, x = "time", fun = "mean_se", dodge = 0.2)
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc2), x = "time", y = "emmean", color = "gender",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = gender),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc2, hide.ns = T, tip.length = F, linetype = 1) +
  ggplot2::scale_color_manual(values=c("#FF007F","#4D4DFF"))


## score ~ ciclo*regiao + Error(id/ciclo)

data <- edat[,c("aluno_id","regiao","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","regiao","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)


(res.aov <- anova_test(ldat, dv = score, wid = id, between = regiao, within = time))

(pwc1 <- ldat %>% group_by(regiao) %>%
    emmeans_test(score ~ time, detailed = T, p.adjust.method = "bonferroni"))


pwc1 <- add_xy_position(pwc1, x = "time", fun = "mean_se")
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc1), x = "time", y = "emmean", color = "regiao",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = regiao),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc1, hide.ns = T, color = "regiao",
                     position = pd, tip.length = F)


(pwc2 <- ldat %>% group_by(time) %>%
    emmeans_test(score ~ regiao, detailed = T, p.adjust.method = "bonferroni"))

pwc2 <- add_xy_position(pwc2, x = "time", fun = "mean_se", dodge = 0.2)
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc2), x = "time", y = "emmean", color = "regiao",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = regiao),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc2, hide.ns = T, tip.length = F, linetype = 1)




## score ~ ciclo*uf + Error(id/ciclo)

data <- edat[,c("aluno_id","uf","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","uf","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)

get_summary_stats(group_by(ldat,uf, time))

(res.aov <- anova_test(ldat, dv = score, wid = id, between = uf, within = time))

(pwc1 <- ldat %>% group_by(uf) %>%
    emmeans_test(score ~ time, detailed = T, p.adjust.method = "bonferroni"))


pwc1 <- add_xy_position(pwc1, x = "time", fun = "mean_se")
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc1), x = "time", y = "emmean", color = "uf",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = uf),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc1, hide.ns = T, color = "uf",
                     position = pd, tip.length = F)


(pwc2 <- ldat %>% group_by(time) %>%
    emmeans_test(score ~ uf, detailed = T, p.adjust.method = "bonferroni"))

pwc2 <- add_xy_position(pwc2, x = "time", fun = "mean_se", dodge = 0.2)
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc2), x = "time", y = "emmean", color = "regiao",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = regiao),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc2, hide.ns = T, tip.length = F, linetype = 1)




## score ~ ciclo*localidade + Error(id/ciclo)

data <- edat[,c("aluno_id","localidade_diferenciada","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","localidade","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)

get_summary_stats(group_by(ldat,localidade, time))



## score ~ ciclo*dependencia + Error(id/ciclo)

data <- edat[,c("aluno_id","dependencia_administrativa","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","dependencia","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)

get_summary_stats(group_by(ldat, dependencia, time))



## score ~ ciclo*uf + Error(id/ciclo)

data <- edat[,c("aluno_id","uf","ciclo","score")]
data$ciclo <- factor(data$ciclo, c("Primeiro Ciclo","Segundo Ciclo",
                                   "Terceiro Ciclo","Quarto Ciclo"))

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","uf","c1","c2","c3","c4")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)

get_summary_stats(group_by(ldat,uf, time))

(res.aov <- anova_test(ldat, dv = score, wid = id, between = uf, within = time))

(pwc1 <- ldat %>% group_by(uf) %>%
    emmeans_test(score ~ time, detailed = T, p.adjust.method = "bonferroni"))


pwc1 <- add_xy_position(pwc1, x = "time", fun = "mean_se")
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc1), x = "time", y = "emmean", color = "uf",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = uf),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc1, hide.ns = T, color = "uf",
                     position = pd, tip.length = F)


(pwc2 <- ldat %>% group_by(time) %>%
    emmeans_test(score ~ uf, detailed = T, p.adjust.method = "bonferroni"))

pwc2 <- add_xy_position(pwc2, x = "time", fun = "mean_se", dodge = 0.2)
pd <- position_dodge(width = 0.25)
ggline(get_emmeans(pwc2), x = "time", y = "emmean", color = "regiao",
       palette = "jco", position = pd) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = regiao),
                position = pd, width = 0.2) +
  stat_pvalue_manual(pwc2, hide.ns = T, tip.length = F, linetype = 1)







####################################

data <- dat[,c("aluno_id","gender","ciclo","score")]

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]
colnames(wdat) <- c("id","gender","c1","c4","c2","c3")

ldat <- gather(wdat, key = time, value = score, c1, c2, c3, c4) %>%
  convert_as_factor(id,time)



(res.aov <- anova_test(ldat, dv = score, wid = id, between = gender, within = time))

(pwc <- ldat %>% group_by(gender) %>%
    pairwise_t_test(score ~ time, paired = T, detailed = T,
                    p.adjust.method = "bonferroni"))

(pwc <- ldat %>% group_by(time) %>%
    pairwise_t_test(score ~ gender, paired = F, detailed = T,
                    p.adjust.method = "bonferroni"))



ggpubr::ggboxplot(
  ldat, x = "time", y = "score",
  color = "gender", palette = "jco"
)
bxp


sdat <- ldat
sdat$score <- log(sdat$score)

library(ggplot2)
install.packages('ggridges')
library(ggridges)

ggplot(ldat, aes(x = score, y = time)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

sdat <- rshinystatistics::get.descriptives(ldat, "score","time")

ggplot(sdat, aes(x=time, y=median)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9))
+



pwc <- add_xy_position(pwc, x = "time")
ggboxplot(sdat, x = "time", y = "score") + coord_cartesian(ylim = c(1, 1.25))

ggboxplot(sdat, x = "time", y = "score") + coord_cartesian(ylim = c(1, 1.25))

bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )





dat.by <- dat %>% group_by(id_aluno, ciclo) %>%
  summarise(segmentacao = mean(segmentacao_num, na.rm = T),
            coesao = mean(coesao_num, na.rm = T),
            pontuacao = mean(pontuacao_num, na.rm = T),
            ortografia = mean(ortografia_num, na.rm = T),
            tipologia_textual = mean(tipologia_textual_num, na.rm = T),
            adequacao_a_proposta = mean(adequacao_a_proposta_num, na.rm = T))

dat.by <- mutate(as.data.frame(dat.by),
                 score = (segmentacao+coesao+pontuacao+ortografia+
                            tipologia_textual+adequacao_a_proposta)/6)
dat.by <- dat.by[,c("id_aluno","ciclo","score")]

wdat <- spread(data, ciclo, score)
wdat <- wdat[complete.cases(wdat),]

colnames(wdat) <- c("wid","c1","c4","c2","c3")


ldat <- as.data.frame(wdat %>%
                        gather(key = time, value = score, c1, c2, c3, c4) %>%
                        convert_as_factor(wid, time))
head(ldat)


ldat %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")


ggboxplot(ldat, x = "time", y = "score", add = "jitter")


res.fried <- ldat %>% friedman_test(score ~ time |wid)
res.fried


ldat %>% friedman_effsize(score ~ time |wid)


pwc <- ldat %>%
  wilcox_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni", detailed = T)
pwc


ex.dat <- ldat %>%
  group_by(time) %>%
  identify_outliers(score)

ldat.wo <- ldat[!ldat$wid %in% ex.dat$wid,]

ldat %>%
  group_by(time) %>%
  shapiro_test(score)




ggqqplot(ldat, "score", facet.by = "time")




res.aov <- ::anova_test(ldat, dv = score, wid = wid, within = time)


pwc <- ldat %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    detailed = T,
    p.adjust.method = "bonferroni"
  )
pwc




pdat <- ldat[!(ldat$wid %in% ex.dat$wid),]


bxp

data <- as.data.frame(ldat.cc)

res.aov <- rstatix::anova_test(data, dv = coesao, wid = wid, within = time)
