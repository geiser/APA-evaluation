wants <- c('ggplot2','ggpubr','templates','PerformanceAnalytics','utils','randomcoloR',
           'rshinystatistics','rstatix')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(ggpubr)
library(ggplot2)
library(randomcoloR)

library(rshinystatistics)
library(rstatix)

library(knitr)
library(templates)
library(markdown)

library(readxl)

## Individual Students Rmarkdown files generation

library(ggsci)
library(scales)


### for: alunos do 1ro até 4to ano

dat <- read_excel("data/data.xlsx", sheet = "alunos_ef14")

escolas <- read_excel("data/data.xlsx", sheet = "escolas")
edat <- merge(dat, escolas, by = "cod_escola", all.x = T)

dvs <- c("score", "segmentacao", "coesao", "pontuacao",
         "ortografia", "tipologia_textual", "adequacao_a_proposta")

for (dv in dvs) {

  colors <- list()
  colors[["gender"]] = c("#FF007F","#4D4DFF")
  colors[["localizacao"]] = c("#AA00FF","#00CCCC")

  txt <- do.call(paste0, c(collapse = "\n", lapply(
    c("gender","localizacao","regiao","porte"), FUN = function(iv) {

      ldat <- edat[!is.na(edat[[iv]]),]
      ldat <- rshinystatistics::remove_group_data(ldat, dv, c("time",iv), n.limit = 30)

      values = sort(unique(ldat[[iv]]))

      if (!iv %in% names(colors))
        colors[[iv]] = pal_jco("default")(length(values))

      params = list(
        title = paste0(' ANOVA: ',dv,' ~ time*',iv,' + Error(id/time)'),
        dv = dv, iv = iv, pcolors = colors[[iv]],
        fig.width = 10, fig.height = 6)

      params[["pwc.plots"]] = do.call(paste0, c(
        collapse = "\n", lapply(1:length(values), FUN = function(i) {
          tfile = "templates/pwc-plot.Rmd"
          plot.params = list(
            dv = dv, iv = iv, value = values[i], color = colors[[iv]][i],
            fig.width = 10, fig.height = 6
          )
          do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), plot.params))
        })
      ))
      params[["pwc.plots2"]] = do.call(paste0, c(
        collapse = "\n", lapply(1:length(values), FUN = function(i) {
          tfile = "templates/pwc-plot2.Rmd"
          plot.params = list(
            dv = dv, iv = iv, value = values[i], color = colors[[iv]][i],
            fig.width = 10, fig.height = 6
          )
          do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), plot.params))
        })
      ))

      do.call(tmpl, c(
        list(".t" = paste(readLines("templates/pwc-one-factor-sub.Rmd"),
                          collapse="\n")), params))
    }))
  )

  info <- list(file.path = "../data/data.xlsx", dv = dv, sheet = "alunos_ef14",
               output = paste0(getwd(),'/code/aov-students-1_4-',dv,'.Rmd'),
               fig.width = 10, fig.height = 6)
  info[["one.factor"]] = txt

  writeLines(do.call(
    tmpl, c(list(".t" = paste(
      readLines("templates/aov-student.Rmd"), collapse="\n")), info)),
    info$output, useBytes=T)
  #rmarkdown::render(info$output, output_format = "github_document")
}



### for: alunos do 5to até 9no ano

dat <- read_excel("data/data.xlsx", sheet = "alunos_ef59")

escolas <- read_excel("data/data.xlsx", sheet = "escolas")
edat <- merge(dat, escolas, by = "cod_escola", all.x = T)


ciclos.infos <- list(list(
  name = "c1_c2",
  ciclos.short = c("c1", "c2"),
  ciclos.full = c("Primeiro Ciclo", "Segundo Ciclo")
),
list(
  name = "c2_c3",
  ciclos.short = c("c2", "c3"),
  ciclos.full = c("Segundo Ciclo", "Terceiro Ciclo")
),
list(
  name = "c1_c3",
  ciclos.short = c("c1", "c3"),
  ciclos.full = c("Primeiro Ciclo", "Terceiro Ciclo")
))


for (dv in c("score", "coesao", "coerencia_tematica", "registro_formal")) {

  colors <- list()
  colors[["gender"]] = c("#FF007F","#4D4DFF")
  colors[["localizacao"]] = c("#AA00FF","#00CCCC")

  for (cinfo in ciclos.infos) {

    txt <- do.call(paste0, c(collapse = "\n", lapply(
      c("gender","localizacao","regiao","porte"), FUN = function(iv) {

        ldat <- edat[!is.na(edat[[iv]]),]
        ldat <- rshinystatistics::remove_group_data(ldat, dv, c("time",iv), n.limit = 30)

        values = sort(unique(ldat[[iv]]))

        if (!iv %in% names(colors))
          colors[[iv]] = pal_jco("default")(length(values))

        params = list(
          title = paste0(' ANOVA: ',dv,' ~ time*',iv,' + Error(id/time)'),
          dv = dv, iv = iv, pcolors = colors[[iv]],
          fig.width = 10, fig.height = 6)

        params[["pwc.plots"]] = do.call(paste0, c(
          collapse = "\n", lapply(1:length(values), FUN = function(i) {
            tfile = "templates/pwc-plot.Rmd"
            plot.params = list(
              dv = dv, iv = iv, value = values[i], color = colors[[iv]][i],
              fig.width = 10, fig.height = 6
            )
            do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), plot.params))
          })
        ))
        params[["pwc.plots2"]] = do.call(paste0, c(
          collapse = "\n", lapply(1:length(values), FUN = function(i) {
            tfile = "templates/pwc-plot2.Rmd"
            plot.params = list(
              dv = dv, iv = iv, value = values[i], color = colors[[iv]][i],
              fig.width = 10, fig.height = 6
            )
            do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), plot.params))
          })
        ))

        params[["ciclos.full"]] = cinfo$ciclos.full
        params[["ciclos.short"]] = cinfo$ciclos.short

        do.call(tmpl, c(
          list(".t" = paste(readLines("templates/pwc-one-factor-sub.Rmd"),
                            collapse="\n")), params))
      }))
    )

    info <- list(file.path = "../data/data.xlsx", dv = dv, sheet = "alunos_ef59",
                 output = paste0(getwd(),'/code/aov-students-5_9-',dv,'-',cinfo$name,'.Rmd'),
                 fig.width = 10, fig.height = 6)
    info[["one.factor"]] = txt

    info[["ciclos.full"]] = cinfo$ciclos.full
    info[["ciclos.short"]] = cinfo$ciclos.short

    writeLines(do.call(
      tmpl, c(list(".t" = paste(
        readLines("templates/aov-student.Rmd"), collapse="\n")), info)),
      info$output, useBytes=T)

    #rmarkdown::render(info$output, output_format = "github_document")
  }
}


