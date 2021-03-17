library(tidyverse)

setwd("D:\\KTU\\Taikomoji matematika\\4 Ketvirtas semestras\\3. PDTIV\\Laboratorinis 2\\KTU-duomenu-vizualizacija\\laboratorinis\\R")
duom <- read_csv("D:\\KTU\\Taikomoji matematika\\4 Ketvirtas semestras\\3. PDTIV\\Laboratorinis 2\\KTU-duomenu-vizualizacija\\laboratorinis\\data\\lab_sodra.csv")
colnames(duom)

#Duomenis atsirenku pagal savo ekonomines veiklos koda
duom1 <- duom %>% filter(ecoActCode == 682000)

duom1 %>% filter(avgWage < 6000) %>% ggplot(aes(x = avgWage)) +
  geom_histogram(binwidth = 100, colour = alpha("white", 0.3)) +
  scale_x_continuous(breaks = seq(from = 0, to = 6000, by = 1000)) +
  labs(title = 'Histogram of average wages', x = 'Average wage', y = 'Count') +
  theme(plot.title = element_text(hjust = 0, size = 12)) +
  theme_classic() 

ggsave(file = "D:\\KTU\\Taikomoji matematika\\4 Ketvirtas semestras\\3. PDTIV\\Laboratorinis 2\\KTU-duomenu-vizualizacija\\laboratorinis\\img\\plot1.png", width = 10, height = 4)
dev.off()

#2 UZDUOTIS

duom2 <- duom1
#Pervadinau stulpeli "month" i "date"
names(duom2)[names(duom2)=="month"] <- "date"
#Sukuriau nauja stulpeli "month", kuriame iskyriau tik menesio reiksme
duom2 <- duom2 %>% mutate("month" = as.numeric(substr(date, 5, 6)))
#Isfiltravau pavadinimus tu kompaniju, kuriose buvo fiksuotas didziausias menesinis atlyginimas
names <- duom2 %>% group_by(name) %>% summarise('maxWage' = max(avgWage, na.rm = T)) %>% top_n(n = 5, wt = maxWage) %>% select(name)
#Is duomenu failo atsirinkau tik tas kompanijas, kuriu pavadinimai yra data frame "names"
duom3 <- duom2 %>% filter(name %in% names$name)

duom3 %>% ggplot(aes(x = month, y = avgWage, col = name)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  labs(title = 'Average wage by company', x = 'Months', y = 'Average wage') +
  theme(plot.title = element_text(hjust = 0, size = 12), legend.text = element_text(size = 7), legend.title = element_text(size = 8))+
  guides(fill = guide_legend(title="Company's name"))+
  theme_classic() 

ggsave(file = "D:\\KTU\\Taikomoji matematika\\4 Ketvirtas semestras\\3. PDTIV\\Laboratorinis 2\\KTU-duomenu-vizualizacija\\laboratorinis\\img\\plot2.png", width = 10, height = 4)
dev.off()

#3 UZDUOTIS

#Is pries tai atrinktu duomenu isskyriau tik viena stulpeli "numInsured"
duom4 <- duom3 %>% group_by(name) %>% summarise('maxInsured' = max(numInsured, na.rm = T))

duom4 %>% ggplot(aes(x = reorder(name, desc(maxInsured)), y = maxInsured, fill = name)) +
  geom_col()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  labs(title = 'Number of insured workers by company', x = 'Name of a company', y = 'Number of insured workers') +
  theme(plot.title = element_text(hjust = 0, size = 12), legend.text = element_text(size = 7), legend.title = element_text(size = 8))+
  guides(fill = guide_legend(title="Company's name"))+
  theme_classic()

ggsave(file = "D:\\KTU\\Taikomoji matematika\\4 Ketvirtas semestras\\3. PDTIV\\Laboratorinis 2\\KTU-duomenu-vizualizacija\\laboratorinis\\img\\plot3.png", width = 10, height = 4)
dev.off()
