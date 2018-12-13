library(readr)
library(ggplot2)

objectives.cenario.1 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario1.txt", 
                        sep = ",", 
                        na.strings = "",
                        header = F)
names(objectives.cenario.1) = c("user", "diversity", "affinity")
objectives.cenario.1$cenario = "cenario1"

objectives.cenario.2 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario2.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.2) = c("user", "diversity", "affinity")
objectives.cenario.2$cenario = "cenario2"

objectives.cenario.3 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario3.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.3) = c("user", "diversity", "affinity")
objectives.cenario.3$cenario = "cenario3"

objectives.cenario.4 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario4.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.4) = c("user", "diversity", "affinity")
objectives.cenario.4$cenario = "cenario4"

objectives.cenario.5 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario5.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.5) = c("user", "diversity", "affinity")
objectives.cenario.5$cenario = "cenario5"

objectives.cenario.6 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario6.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.6) = c("user", "diversity", "affinity")
objectives.cenario.6$cenario = "cenario6"

objectives.cenario.7 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario7.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.7) = c("user", "diversity", "affinity")
objectives.cenario.7$cenario = "cenario7"

objectives.cenario.8 = fread("/local/datasets/experimento/results_word2vec/objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario8.txt", 
                             sep = ",", 
                             na.strings = "",
                             header = F)
names(objectives.cenario.8) = c("user", "diversity", "affinity")
objectives.cenario.8$cenario = "cenario8"

objectives.w2v = fread("/local/Scripts/word2vec/MOAD/data/objectives.sample1000.nsga.word2vec.top10.pop10.gen20.txt",
                           sep = ",", 
                           na.strings = "",
                           header = F)
names(objectives.w2v) = c("user", "diversity", "affinity")
objectives.w2v$cenario = "w2v"

objectives.random = rbind(objectives.cenario.1[c(1:126),],
                          objectives.cenario.2[c(127:252),],
                          objectives.cenario.3[c(253:378),],
                          objectives.cenario.4[c(379:504),],
                          objectives.cenario.5[c(505:630),],
                          objectives.cenario.6[c(631:756),],
                          objectives.cenario.7[c(757:882),],
                          objectives.cenario.8[c(883:997),])
objectives.random$cenario = "random"

data.plot = rbind(objectives.cenario.1,
                  objectives.cenario.2,
                  objectives.cenario.3,
                  objectives.cenario.4,
                  objectives.cenario.5,
                  objectives.cenario.6,
                  objectives.cenario.7,
                  objectives.cenario.8,
                  objectives.random,
                  objectives.w2v)

plot.melt = melt(data.plot, id.vars = c("user", "cenario"), measure.vars = c("diversity", "affinity"))
plot.melt.diversity = filter(plot.melt, variable == "diversity")
plot.melt.affinity = filter(plot.melt, variable == "affinity")

f.div <- ggplot(plot.melt.diversity, aes(cenario, value, group = cenario, color = cenario)) + geom_boxplot()
f.aff <- ggplot(plot.melt.affinity, aes(cenario, value, group = cenario, color = cenario)) + geom_boxplot()

