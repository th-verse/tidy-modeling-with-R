
# packages ----------------------------------------------------------------

# install.packages("skimr")
# install.packages("funModeling")
# install.packages("sjPlot")
# install.packages("janitor")

library(tidyverse)
library(tidymodels)
library(janitor)

# data --------------------------------------------------------------------

data("crickets")

crickets %>% slice_sample(n = 6)

skimr::skim(crickets)
funModeling::df_status(crickets)
sjPlot::view_df(crickets,
                show.frq = TRUE,
                show.prc = TRUE)

janitor::tabyl(crickets$species) %>%
  adorn_totals()

crickets %>% 
  ggplot(aes(x = temp, y = rate, color = species))+
  geom_point()+
  geom_smooth(method = lm)

# 1 - Percebe diferença uma diferença; Possui uma correlação positiva.
# 2 - Quanto maior a temperatura maior a taxa do cantar do grilo.
# 3 - Parece que o O.exclamations possui uma taxa de canto maior que O.niveus

# model -------------------------------------------------------------------

mdl_crickets <-
  lm(rate ~ temp + species, data = crickets)

mdl_crickets

summary(mdl_crickets)

# formas de avaliar o modelo
dados_pesquisa <- 
  mdl_crickets %>% 
  broom::tidy() %>% 
  mutate(p.value = scales::pvalue(p.value))

# mdl_crickets %>% 
#  broom::glance()

# mdl_crickets %>% 
#   broom::augment()

# escrita dinamica
`r Para cada unidade adicional da variavel` 
dados_pesquisa$term[[2]]

`r a taxa de cantar dos grilos variam em`
dados_pesquisa$estimate[[2]]

`r tendo um nivel elevado de significancia estatistica`
dados_pesquisa$p.value[[2]]

# criando modelo de interação
mdl_crickets

mdl_crickets_int <- 
  lm(rate ~ temp + species + temp:species, data = crickets)

summary(mdl_crickets_int)

# comparando os dois modelos
anova(mdl_crickets, mdl_crickets_int)


# agrupando por especies
data_species <- 
crickets %>% 
  group_nest(species)

data_species %>% 
  mutate(mdl = map(data, ~ lm(rate ~ temp, data = .x))) %>% 
  mutate(coef = map(mdl, broom::tidy)) %>% 
  select(species, coef) %>% 
  unnest(coef)
  



