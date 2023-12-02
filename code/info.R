
install.packages("echarts4r.assets")

library(readxl)


dat <- read_excel("data/data.xlsx", sheet = "alunos_ef14")

escolas <- read_excel("data/data.xlsx", sheet = "escolas")
edat <- merge(dat, escolas, by = "cod_escola", all.x = T)


get_info <- function(data, c1, c2 = NULL, c3 = NULL, c4 = NULL, not = NULL, op="intersect") {
  l = list(
    total = unique(data$aluno_id[which(data$ciclo == c1)]),
    male = unique(data$aluno_id[which(data$ciclo == c1 & data$gender == "Male")]),
    female = unique(data$aluno_id[which(data$ciclo == c1 & data$gender == "Female")]),
    na = unique(data$aluno_id[which(data$ciclo == c1 & is.na(data$gender))]))
  if (!is.null(c2)){
    l$total = unique(do.call(op, list(l$total, data$aluno_id[which(data$ciclo == c2)])))
    l$male = unique(do.call(op, list(l$male, data$aluno_id[which(data$ciclo == c2 & data$gender == "Male")])))
    l$female = unique(do.call(op, list(l$female, data$aluno_id[which(data$ciclo == c2 & data$gender == "Female")])))
    l$na = unique(do.call(op, list(l$na, data$aluno_id[which(data$ciclo == c2 & is.na(data$gender))])))
  }
  if (!is.null(c3)){
    l$total = unique(do.call(op, list(l$total, data$aluno_id[which(data$ciclo == c3)])))
    l$male = unique(do.call(op, list(l$male, data$aluno_id[which(data$ciclo == c3 & data$gender == "Male")])))
    l$female = unique(do.call(op, list(l$female, data$aluno_id[which(data$ciclo == c3 & data$gender == "Female")])))
    l$na = unique(do.call(op, list(l$na, data$aluno_id[which(data$ciclo == c3 & is.na(data$gender))])))
  }
  if (!is.null(c4)){
    l$total = unique(do.call(op, list(l$total, data$aluno_id[which(data$ciclo == c4)])))
    l$male = unique(do.call(op, list(l$male, data$aluno_id[which(data$ciclo == c4 & data$gender == "Male")])))
    l$female = unique(do.call(op, list(l$female, data$aluno_id[which(data$ciclo == c4 & data$gender == "Female")])))
    l$na = unique(do.call(op, list(l$na, data$aluno_id[which(data$ciclo == c4 & is.na(data$gender))])))
  }
  if (!is.null(not)) {
    for (no in not) {
      l$total = unique(setdiff(l$total, data$aluno_id[which(data$ciclo == no)]))
      l$male = unique(setdiff(l$male, data$aluno_id[which(data$ciclo == no & data$gender == "Male")]))
      l$female = unique(setdiff(l$female, data$aluno_id[which(data$ciclo == no & data$gender == "Female")]))
      l$na = unique(setdiff(l$na, data$aluno_id[which(data$ciclo == no & is.na(data$gender))]))
    }
  }
  return(l)
}

## determine numbers for the flow chart of participants

info <- rbind(
  c1 = as.data.frame(lapply(get_info(edat, "Primeiro Ciclo"), length)),
  droup_c1 = as.data.frame(lapply(
    get_info(edat, "Primeiro Ciclo",
             "Segundo Ciclo", "Terceiro Ciclo", "Quarto Ciclo", op="setdiff"), length)),

  new_c2 = as.data.frame(lapply(
    get_info(edat, "Segundo Ciclo",
             "Primeiro Ciclo", "Terceiro Ciclo", "Quarto Ciclo", op="setdiff"), length)),
  c1_c2 = as.data.frame(lapply(
    get_info(edat, "Primeiro Ciclo", "Segundo Ciclo"), length)),
  c2 = as.data.frame(lapply(
    get_info(edat, "Segundo Ciclo"), length)),
  droup_c2 = as.data.frame(lapply(
    get_info(edat, "Segundo Ciclo",
             "Terceiro Ciclo", "Quarto Ciclo", "Primeiro Ciclo", op="setdiff"), length)),

  new_c3 = as.data.frame(lapply(
    get_info(edat, "Terceiro Ciclo",
             "Segundo Ciclo", "Primeiro Ciclo", "Quarto Ciclo", op="setdiff"), length)),
  c1_c3_not_c2 = as.data.frame(lapply(
    get_info(edat, "Primeiro Ciclo",
             "Terceiro Ciclo", not="Segundo Ciclo"), length)),
  c2_c3_not_c1 = as.data.frame(lapply(
    get_info(edat, "Segundo Ciclo",
             "Terceiro Ciclo", not="Primeiro Ciclo"), length)),
  c1_c2_c3 = as.data.frame(lapply(
    get_info(edat, "Primeiro Ciclo", "Segundo Ciclo", "Terceiro Ciclo"), length)),
  c3 = as.data.frame(lapply(
    get_info(edat, "Terceiro Ciclo"), length)),
  droup_c3 = as.data.frame(lapply(
    get_info(edat, "Terceiro Ciclo",
             "Segundo Ciclo", "Quarto Ciclo", "Primeiro Ciclo", op="setdiff"), length)),

  new_c4 = as.data.frame(lapply(
    get_info(edat, "Quarto Ciclo",
             "Terceiro Ciclo", "Segundo Ciclo", "Primeiro Ciclo", op="setdiff"), length)),
  c1_c4_not_c2_c3 = as.data.frame(lapply(
    get_info(edat, "Primeiro Ciclo", "Quarto Ciclo",
             not = c("Segundo Ciclo", "Terceiro Ciclo")), length)),
  c2_c4_not_c1_c3 = as.data.frame(lapply(
    get_info(edat, "Segundo Ciclo", "Quarto Ciclo",
             not = c("Primeiro Ciclo", "Terceiro Ciclo")), length)),
  c3_c4_not_c1_c2 = as.data.frame(lapply(
    get_info(edat, "Terceiro Ciclo", "Quarto Ciclo",
             not = c("Primeiro Ciclo", "Segundo Ciclo")), length)),
  c1c2_c4_not_c3 = as.data.frame(lapply(
    get_info(edat, "Primeiro Ciclo", "Segundo Ciclo", "Quarto Ciclo",
             not = "Terceiro Ciclo"), length)),
  c2c3_c4_not_c1 = as.data.frame(lapply(
    get_info(edat, "Segundo Ciclo", "Terceiro Ciclo", "Quarto Ciclo",
             not = "Primeiro Ciclo"), length)),

  c1_c2_c3_c4 = as.data.frame(lapply(get_info(edat, "Primeiro Ciclo", "Segundo Ciclo", "Terceiro Ciclo", "Quarto Ciclo"), length)),
  c4 = as.data.frame(lapply(get_info(edat, "Quarto Ciclo"), length))
)
View(info)


## Generate Icons for the text

fatores <- c("gender","localizacao","regiao","uf","dependencia_administrativa")
names(fatores) <- fatores

(info.distr <- lapply(
  fatores,
  FUN = function(col) {
    sapply(unique(edat[[col]]), FUN = function(val) {
      if (is.na(val))
        length(unique(edat$aluno_id[which(is.na(edat[[col]]))]))
      else
        length(unique(edat$aluno_id[which(edat[[col]] == val)]))
    })
  })
)


## Gender infograph

library(echarts4r)
library(echarts4r.assets)

icons <- list(
  male = list(df = data.frame(
    icon=c("Male"), value=c(round(100*info.distr$gender[["Male"]]/102427,2)),
    path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13')
    ), color = '#4D4DFF', n = info.distr$gender[["Male"]]
  ),
  female = list(df = data.frame(
    icon=c("Female"), value=c(round(100*info.distr$gender[["Female"]]/102427,2)),
    path = c('path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z')
    ), color = '#FF007F', n = info.distr$gender[["Female"]]
  ),
  undeclared = list(df = data.frame(
    icon=c("Undeclared"), value=c(round(100*info.distr$gender[[3]]/102427,2)),
    path = c('path://')
  ), color = 'grey', n = info.distr$gender[[3]])
)

gender_icons <- lapply(icons, FUN = function(icon) {
  icon$df %>%
    e_charts(icon) %>%
    e_x_axis(splitLine=list(show = FALSE),
             axisTick=list(show=FALSE),
             axisLine=list(show=FALSE),
             axisLabel= list(show=FALSE)) %>%
    e_y_axis(max=100,
             splitLine=list(show = FALSE),
             axisTick=list(show=FALSE),
             axisLine=list(show=FALSE),
             axisLabel=list(show=FALSE)) %>%
    e_color(color = c(icon$color,'gray')) %>%
    e_pictorial(value, symbol = path, z=10, name= 'realValue',
                symbolBoundingData= 100, symbolClip= TRUE) %>%
    e_pictorial(value, symbol = path, name= 'background',
                symbolBoundingData= 100) %>%
    e_labels(position = "bottom", offset= c(0, 10),
             textStyle =list(fontSize= 20, fontFamily= 'Arial',
                             fontWeight ='bold',
                             color= icon$color),
             formatter=paste0("{@[1]}% {@[0]} \n (n = ",icon$n,")")) %>%
    e_legend(show = FALSE) %>%
    e_theme("westeros")
})


## Geomap infograph -- install.packages("geobr")

library(geobr)
library(ggplot2)
library(sf)
library(dplyr)

brasil <- read_country(year = 2020)

country.df <- as.data.frame(info.distr$uf)
colnames(country.df) <- c("Participants")
country.df[["abbrev_state"]] <- rownames(country.df)
country.df <- merge(brasil, country.df, by="abbrev_state")


options(scipen = 999)

ggplot() +
  geom_sf(data=country.df,  aes(fill=Participants), color = "#666666")+
  geom_sf_text(data=country.df, aes(label = Participants), size = 3) +
  scale_fill_gradient(low = "#FFFFFF", high = "#00AA00",
                      name="Total") +
  labs(title="Students who participated in the program", size=8) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


rbind(
  round(100*info.distr$regiao/sum(info.distr$regiao),2),
  info.distr$regiao)

rbind(
  round(100*info.distr$localizacao/sum(info.distr$localizacao),2),
  info.distr$localizacao)


## ## ## ## ## ## ## ## ## ## ##

ggplot() +
  geom_sf(data=country.df,  aes(fill=Participants), color = "grey")+
  geom_sf_text(data=country.df, aes(label = Participants),size = 3) +
  scale_fill_gradient(low = "white", high = "#00AA00",
                      name="Total") +
  labs(title="Students who participated", size=8) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


part.analyzed <- get_info(
  edat, "Primeiro Ciclo","Segundo Ciclo","Terceiro Ciclo","Quarto Ciclo")


info.analyzed <- lapply(
  fatores,
  FUN = function(col) {
    sapply(unique(edat[[col]]), FUN = function(val) {
      length(unique(intersect(edat$aluno_id[which(edat[[col]] == val)], part.analyzed$total)))
    })
  })


df <- as.data.frame(info.analyzed$uf)
colnames(df) <- c("Participants")
df[["abbrev_state"]] <- rownames(df)

df <- merge(brasil, df, by="abbrev_state")





info.distr[1]

states <- read_state(
  year = 2019,
  showProgress = FALSE
)


ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal()



unique(edat[,c("regiao","aluno_id")]))


