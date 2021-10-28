
# Installering af pakker
install.packages("tidyverse")
library(tidyverse)

install.packages("readxl")
library(readxl)

_____________________________________________________________________________

## Opgave 1

#indlæs excelarket og kaldt det df for dataframe
df <- read_excel("Materiale til NEFUS Datasprint 2021/bfi_oa2019.xlsx", sheet = 2)
df
# Vis variablerne (kollonner)

colnames(df)

# valg af relevante kollonner

df_rettet <- df %>%
  select(Bidrags.ID, BFI.Hovedområde, BFI.Niveau, `OA-status`,`Grøn lokalt`, `Grøn eksternt`, `Gylden med APC`, `Gylden uden APC`) %>% 
  distinct() 


#OA status i alt
  
df_rettet %>% count(`OA-status`)

# Grøn OA pr. BFI niveau

banan <- df_rettet %>% 
  pivot_longer(cols = c(`Grøn lokalt`, `Grøn eksternt`), names_to = 'grøn', values_to = 'xxx') %>% 
  filter(!is.na(xxx), !is.na(BFI.Niveau)) %>% 
  count(grøn, BFI.Niveau)


# Vis frame
view(banan)

# Udskriv til excel 

install.packages("writexl")
library(writexl)

write_xlsx(banan , "banan.xlsx")




_________________________________________
## Opgave 2


# sherparomea

# Indlæs excel

df2 <- read_excel("Materiale til NEFUS Datasprint 2021/Materiale til NEFUS Datasprint 2021/Opgaver og datasæt/Opgave 2 - data/sherpa_bfi2021.xlsx")
df2 %>% group_by(publication_id) %>%  view()

#Vis variabler (kolonne navne)

colnames(df2)

#beskrivelse af data: BFI tidsskrifter der overholder om indikatorens OA krav.

df2 %>% count(location)
rep_OA <- c("any_repository", "institutional_repository" , "non_commercial_institutional_repository", "any_website")
compliant <- df2 %>% 
  filter(embargo<13|is.na(embargo)) %>% 
  filter(location %in% rep_OA) %>% 
  filter(additional_oa_fee == 'no') %>% 
  filter(article_version != 'submitted') %>% 
  select(publication_id) %>% 
  distinct() %>% 
  pull()

# vis procent

bfi_compliant_OAI <- df2 %>% group_by(bfi_faggruppenr, bfi_faggruppe) %>% 
  summarise(total = n_distinct(publication_id), 
            OAI = n_distinct(publication_id[publication_id %in% compliant]),
            andel = OAI/total*100) %>%  view()

# Arbejde med visualisering af opgave 1
install.packages("ggplot2")
library(ggplot2)

# Visualisering af andel OAI compliant tidsskrifer pr faggruppe

ggplot(data = bfi_compliant_OAI) +
  geom_point(mapping = aes(x = factor(bfi_faggruppenr), y = andel), fill = "red")


vis_1= ggplot(bfi_compliant_OAI) + geom_point(aes(x=bfi_faggruppenr, y=andel))


ggplot(data = bfi_compliant_OAI) +
  geom_col(mapping = aes(x = bfi_faggruppenr, y = OAI))

#bfi faggruppenummer omdannes til en faktor

  
ggplot(data = bfi_compliant_OAI) +
  geom_col(mapping = aes(x = factor(bfi_faggruppenr), y = andel,fill = factor(bfi_faggruppenr)) )





# visualisering af opgave 1

ggplot(data = df_rettet) +
  geom_(aes(x = `Gylden med APC` , y = BFI.Hovedområde, fill ='red' ))




