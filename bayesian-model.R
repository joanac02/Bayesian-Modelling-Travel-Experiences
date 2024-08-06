
if (!require("pacman")) install.packages("pacman")
pacman::p_load("rstan"
               , "quadprog"
               , "pbivnorm"
               , "CompQuadForm"
               , "mvtnorm"
               , "sandwich"
               , "future",
               , "backports"
               , "brms"
               , "rstudioapi"
               , "semPlot"
               , "blavaan"
               , "readr"
               , "psych"
               , "semptools"
               , "lavaan")

if (!require("cmdstanr")) install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


dataset <- readr::read_csv("mmc1.csv")

df <- describe(dataset)

#df

# Função para criar histogramas de conjuntos de variáveis
plot_histogram <- function(variables, main_title) {

    # Extrair apenas as colunas desejadas
    subset_data <- dataset[, variables]

    # Calcular as frequências das respostas de 1 a 7 para o conjunto selecionado
    resposta_frequencias <- as.data.frame(table(unlist(subset_data)))

    # Criar o histograma
    barplot <- barplot(resposta_frequencias$Freq,
                       names.arg = resposta_frequencias$Var1,
                       main = main_title,
                       xlab = "Resposta",
                       ylab = "Frequência",
                       col = "steelblue",
                       ylim = c(0, max(resposta_frequencias$Freq) + 100))

    # Adicionar os valores absolutos em cima de cada barra
    text(x = barplot, y = resposta_frequencias$Freq + 10,
         labels = resposta_frequencias$Freq, pos = 3)
}

par(mfrow=c(6,2))

plot_histogram(c("Ident1", "Ident2", "Ident3"), "Histograma do Construto Identificação")
plot_histogram(c("Inter1", "Inter2", "Inter3"), "Histograma do Construto Interiorização")
plot_histogram(c("Comp1", "Comp2", "Comp3"), "Histograma do Construto Conformidade")
plot_histogram(c("Pjoy1", "Pjoy2", "Pjoy3"), "Histograma do Construto Prazer")
plot_histogram(c("AS1", "AS2", "AS3", "AS4"), "Histograma do Construto Experiência")
plot_histogram(c("AM1", "AM2", "AM3"), "Histograma do Construto Altruísmo")
plot_histogram(c("PF1", "PF2", "PF3"), "Histograma do Construto Realização")
plot_histogram(c("ER2", "ER3"), "Histograma do Construto Ambiente")
plot_histogram(c("PR1", "PR3", "PR4"), "Histograma do Construto Pessoal")
plot_histogram(c("RR2", "RR3"), "Histograma do Construto Relações")
plot_histogram(c("SR1", "SR2", "SR3"), "Histograma do Construto Segurança")


# Correlações do construto «Identification»
round(cor(dataset[,1:3]),2)

psych::alpha(dataset[,1:3])

# Correlações do construto «Internalization»
round(cor(dataset[,4:6]),2)

psych::alpha(dataset[,4:6])

# Correlações do construto «Compliance»
round(cor(dataset[,7:9]),2)

psych::alpha(dataset[,7:9])

# Correlações do construto «Perceived enjoyment»
round(cor(dataset[,10:12]),2)

psych::alpha(dataset[,10:12])

# Correlações dos construto «Actual travel experience sharing»
round(cor(dataset[,13:16]),2)

psych::alpha(dataset[,13:16])

# Correlações do construto «Altruistic motivations»
round(cor(dataset[,17:19]),2)

psych::alpha(dataset[,17:19])

# Correlações dos construto «Personal fulfillment and self-actualization»
round(cor(dataset[,20:22]),2)

psych::alpha(dataset[,20:22])

# Correlações do construto «Environmental reasons»
round(cor(dataset[,23:24]),2)

psych::alpha(dataset[,23:24])

# Correlações do construto «Personal reasons»
round(cor(dataset[,25:27]),2)

psych::alpha(dataset[,25:27])

# Correlações do construto «Relationship reasons»
round(cor(dataset[,28:29]),2)

psych::alpha(dataset[,28:29])

# Correlações dos construto «Security and privacy reasons»
round(cor(dataset[,30:32]),2)

psych::alpha(dataset[,30:32])

# Especificação do modelo 1
model.cfa_Ident <- 'Identidade =~ Ident1 + Ident2 + Ident3

                    Ident1 ~~ Ident1
                    Ident2 ~~ Ident2
                    Ident3 ~~ Ident3

'

modelfit.cfa1 <-  bcfa(model.cfa_Ident
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")

semPaths(modelfit.cfa1
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))

summary(modelfit.cfa1, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)


model.cfa_Inter <- 'Interiorizacao =~ Inter1 + Inter2 + Inter3

                    Inter1 ~~ Inter1
                    Inter2 ~~ Inter2
                    Inter3 ~~ Inter3

                    '

modelfit.cfa2 <-  bcfa(model.cfa_Inter
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")


semPaths(modelfit.cfa2
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))

summary(modelfit.cfa2, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)


model.cfa_Comp <- 'Conformidade =~ Comp1 + Comp2 + Comp3

                    Comp1 ~~ Comp1
                    Comp2 ~~ Comp2
                    Comp3 ~~ Comp3

'

modelfit.cfa3 <-  bcfa(model.cfa_Comp
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")


semPaths(modelfit.cfa3
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))

summary(modelfit.cfa3, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

model.cfa_Pjoy <- 'Prazer =~ Pjoy1 + Pjoy2 + Pjoy3

                    Pjoy1 ~~ Pjoy1
                    Pjoy2 ~~ Pjoy2
                    Pjoy3 ~~ Pjoy3


'

modelfit.cfa4 <-  bcfa(model.cfa_Pjoy
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")


semPaths(modelfit.cfa4
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa4, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

model.cfa_ATES <- 'Experiencia =~ AS1 + AS2 + AS3 + AS4

                    AS1 ~~ AS1
                    AS2 ~~ AS2
                    AS3 ~~ AS3
                    AS4 ~~ AS4


'

modelfit.cfa5 <-  bcfa(model.cfa_ATES
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")

semPaths(modelfit.cfa5
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa5, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

model.cfa_AM <- 'Altruismo =~ AM1 + AM2 + AM3

                  AM1 ~~ AM1
                  AM2 ~~ AM2
                  AM3 ~~ AM3

'

modelfit.cfa6 <-  bcfa(model.cfa_AM
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")

semPaths(modelfit.cfa6
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa6, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)


model.cfa_PF <- 'Realizacao =~ PF1 + PF2 + PF3

                  PF1 ~~ PF1
                  PF2 ~~ PF2
                  PF3 ~~ PF3

'

modelfit.cfa7 <-  bcfa(model.cfa_PF
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")

semPaths(modelfit.cfa7
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa7, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

model.cfa_ER <- 'Ambiente =~ ER2 + ER3

                  ER2 ~~ ER2
                  ER3 ~~ ER3

'

modelfit.cfa8 <-  bcfa(model.cfa_ER
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")

semPaths(modelfit.cfa8
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa8, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)


model.cfa_PR <- 'Pessoal =~ PR1 + PR3 + PR4

                  PR1 ~~ PR1
                  PR3 ~~ PR3
                  PR4 ~~ PR4


'

modelfit.cfa9 <-  bcfa(model.cfa_PR
                       , data=dataset
                       , n.chains=4
                       , burnin=2000
                       , std.lv=TRUE
                       , sample=2000
                       , target = "stan")

semPaths(modelfit.cfa9
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))



summary(modelfit.cfa9, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

model.cfa_RR <- 'Relacoes =~ RR2 + RR3

                  RR2 ~~ RR2
                  RR3 ~~ RR3

'

modelfit.cfa10 <-  bcfa(model.cfa_RR
                        , data=dataset
                        , n.chains=4
                        , burnin=2000
                        , std.lv=TRUE
                        , sample=2000
                        , target = "stan")

semPaths(modelfit.cfa10
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa10, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)

model.cfa_SR <- 'Seguranca =~ SR1 + SR2 + SR3

                SR1 ~~ SR1
                SR2 ~~ SR2
                SR3 ~~ SR3


'

modelfit.cfa11 <-  bcfa(model.cfa_SR
                        , data=dataset
                        , n.chains=4
                        , burnin=2000
                        , std.lv=TRUE
                        , sample=2000
                        , target = "stan")

semPaths(modelfit.cfa11
         , whatLabels = "est"
         , sizeMan = 7
         , node.width = 1
         , edge.label.cex = 0.75
         , style = "ram"
         , mar = c(10,5,10,5))


summary(modelfit.cfa11, standardized=T,
        rsquare=T, neff=TRUE, postmedian=T)


# Modelo CFA

model.cfa <- '
              # Modelo de Medida
             Idn =~ Ident1 + Ident2 + Ident3
             Int =~ Inter1 + Inter2 + Inter3
             Cnf =~ Comp1 + Comp2 + Comp3
             Prz =~ Pjoy1 + Pjoy2 + Pjoy3
             Exp =~ AS1 + AS2 + AS3 + AS4
             Alt =~ AM1 + AM2 + AM3
             Rlz =~ PF1 + PF2 + PF3
             Amb =~ ER2 + ER3
             Pss =~ PR1 + PR3 + PR4
             Rlc =~ RR2 + RR3
             Sgr =~ SR1 + SR2 + SR3

              # Variâncias
              Ident1 ~~ Ident1
              Ident2 ~~ Ident2
              Ident3 ~~ Ident3
              Inter1 ~~ Inter1
              Inter2 ~~ Inter2
              Inter3 ~~ Inter3
               Comp1 ~~ Comp1
               Comp2 ~~ Comp2
               Comp3 ~~ Comp3
               Pjoy1 ~~ Pjoy1
               Pjoy2 ~~ Pjoy2
               Pjoy3 ~~ Pjoy3
                AS1 ~~ AS1
                AS2 ~~ AS2
                AS3 ~~ AS3
                AS4 ~~ AS4
                AM1 ~~ AM1
                AM2 ~~ AM2
                AM3 ~~ AM3
                PF1 ~~ PF1
                PF2 ~~ PF2
                PF3 ~~ PF3
                ER2 ~~ ER2
                ER3 ~~ ER3
                PR1 ~~ PR1
                PR3 ~~ PR3
                PR4 ~~ PR4
                RR2 ~~ RR2
                RR3 ~~ RR3
                SR1 ~~ SR1
                SR2 ~~ SR2
                SR3 ~~ SR3


'

modelfit.cfa <-  blavaan::bcfa(model.cfa, data=dataset, std.lv=TRUE,
                               n.chains = 4, burnin=2000,
                               sample=2000, target = "stan")

cfaP1 <- semPlot::semPaths(modelfit.cfa
                           , what = "path"
                           , whatLabels = "std"
                           , sizeMan = 3
                           , node.width = 1
                           , style = "mx"
                           , edge.label.cex = .5
                           , curvePivot = TRUE
                           , groups = "latents"
                           , pastel = TRUE
                           , curve = 2
                           , layout = "tree2"
                           , layoutSplit = FALSE
                           , normalize = FALSE
                           , residScale = 10
                           , mar = c(5,5,5,5)
)


indicator_order <- c(
    "Ident1", "Ident2", "Ident3"
    , "Inter1", "Inter2", "Inter3"
    , "Comp1", "Comp2", "Comp3"
    , "Pjoy1", "Pjoy2", "Pjoy3"
    , "AS1", "AS2", "AS3" , "AS4"
    , "AM1", "AM2", "AM3"
    , "PF1", "PF2", "PF3"
    , "ER2", "ER3"
    , "PR1", "PR3", "PR4"
    , "RR2", "RR3"
    , "SR1", "SR2", "SR3"
)


indicator_factor <- c(
    "Idn", "Idn", "Idn"
    , "Int", "Int", "Int"
    , "Cnf", "Cnf", "Cnf"
    , "Prz", "Prz", "Prz"
    , "Exp", "Exp", "Exp", "Exp"
    , "Alt", "Alt", "Alt"
    , "Rlz", "Rlz", "Rlz"
    , "Amb", "Amb"
    , "Pss", "Pss", "Pss"
    , "Rlc", "Rlc"
    , "Sgr", "Sgr", "Sgr"
)


factor_layout <- matrix(
    c(
        "Idn", NA, NA, NA, "Alt", NA, "Rlz", NA, NA, NA, "Rlc"
        , NA, NA, NA, NA, NA, NA, NA, NA, "Amb", NA, NA
        , "Int", NA, NA, "Prz", NA, NA, "Exp", NA, NA, NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, "Pss", NA, NA
        , "Cnf", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Sgr"
    ), byrow = TRUE, 11,5)

factor_point_to <- matrix(
    c(
        "left", NA, NA, NA,"up", NA, "up", NA, NA, NA, "right"
        , NA, NA, NA, NA, NA, NA, NA, NA, "right", NA, NA
        , "left", NA, NA, "up", NA, NA, "down", NA, NA, NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, "right", NA, NA
        , "left", NA, NA, NA, NA, NA, NA, NA, NA, NA, "right"
    ), byrow = TRUE, 11,5)

cfaP2 <- semptools::set_cfa_layout(cfaP1
                                   , indicator_order = indicator_order
                                   , indicator_factor = indicator_factor
                                   , factor_layout = factor_layout
                                   , factor_point_to = factor_point_to
                                   , loading_position = 0.8)

plot(cfaP2)

summary(modelfit.cfa)


# Modelo SEM

model.sem <- '
           # Definição ddo modelo de medida


           Idn =~ Ident1 + Ident2 + Ident3
             Int =~ Inter1 + Inter2 + Inter3
             Cnf =~ Comp1 + Comp2 + Comp3
             Prz =~ Pjoy1 + Pjoy2 + Pjoy3
             Exp =~ AS1 + AS2 + AS3 + AS4
             Alt =~ AM1 + AM2 + AM3
             Rlz =~ PF1 + PF2 + PF3
             Amb =~ ER2 + ER3
             Pss =~ PR1 + PR3 + PR4
             Rlc =~ RR2 + RR3
             Sgr =~ SR1 + SR2 + SR3

           # Regressões entre construtos

           Prz ~ Idn + Int + Cnf
           Exp ~ Prz + Amb + Alt + Rlz + Pss + Rlc + Sgr

'

modelfit.sem <- blavaan::bsem(model.sem
                              , data=dataset
                              , std.lv=T
                              , n.chains = 5
                              , burnin=1000 #10000
                              , sample=4000 #20000
                              , target = "stan")

semP1 <- semPlot::semPaths(modelfit.sem
                           , what = "path"
                           , whatLabels = "std"
                           , sizeMan = 3
                           , node.width = 1
                           , style = "ram"
                           , edge.label.cex = .5
                           , curvePivot = TRUE
                           , groups = "latents"
                           , pastel = TRUE
                           , curve = 2
                           , layout = "tree2"
                           , layoutSplit = FALSE
                           , normalize = FALSE
                           , residScale = 10
                           , mar = c(5,5,5,5)
)


indicator_order <- c(
    "Ident1", "Ident2", "Ident3"
    , "Inter1", "Inter2", "Inter3"
    , "Comp1", "Comp2", "Comp3"
    , "Pjoy1", "Pjoy2", "Pjoy3"
    , "AS1", "AS2", "AS3" , "AS4"
    , "AM1", "AM2", "AM3"
    , "PF1", "PF2", "PF3"
    , "ER2", "ER3"
    , "PR1", "PR3", "PR4"
    , "RR2", "RR3"
    , "SR1", "SR2", "SR3"
)


indicator_factor <- c(
    "Idn", "Idn", "Idn"
    , "Int", "Int", "Int"
    , "Cnf", "Cnf", "Cnf"
    , "Prz", "Prz", "Prz"
    , "Exp", "Exp", "Exp", "Exp"
    , "Alt", "Alt", "Alt"
    , "Rlz", "Rlz", "Rlz"
    , "Amb", "Amb"
    , "Pss", "Pss", "Pss"
    , "Rlc", "Rlc"
    , "Sgr", "Sgr", "Sgr"
)


factor_layout <- matrix(
    c(
        "Idn", NA, NA, NA, "Alt", NA, "Rlz", NA, NA, NA, "Rlc"
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , NA, NA, NA, NA, NA, NA, NA, NA, "Amb", NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , "Int", NA, NA, "Prz", NA, NA, "Exp", NA, NA, NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , NA, NA, NA, NA, NA, NA, NA, NA, "Pss", NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , "Cnf", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Sgr"
    ), byrow = TRUE, 11,9)

factor_point_to <- matrix(
    c(
        "left", NA, NA, NA,"up", NA, "up", NA, NA, NA, "right"
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , NA, NA, NA, NA, NA, NA, NA, NA, "right", NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , "left", NA, NA, "up", NA, NA, "down", NA, NA, NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , NA, NA, NA, NA, NA, NA, NA, NA, "right", NA, NA
        , NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA
        , "left", NA, NA, NA, NA, NA, NA, NA, NA, NA, "right"
    ), byrow = TRUE, 11,9)

semP2 <- semptools::set_cfa_layout(semP1
                                   , indicator_order = indicator_order
                                   , indicator_factor = indicator_factor
                                   , factor_layout = factor_layout
                                   , factor_point_to = factor_point_to
                                   , loading_position = 0.8)

plot(semP2)

summary(modelfit.sem, standardized=T,rsquare=T,postmedian=TRUE)

fitMeasures(modelfit.sem)

blavaan::blavFitIndices(modelfit.sem)

