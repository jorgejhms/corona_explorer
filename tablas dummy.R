library(tidyverse)
library(fts)

### Usando la data de positivos

Dummy <- positivos
Población <- sample(5000:300000, 1872)

Dummy <- Dummy_pobl %>%
  select(1:3)

Dummy <- unique(Dummy)

### Dummy Población

Dummy_pobl <- Dummy
Dummy_pobl$Población <- Población

Dummy_pobl

### Dummy Otras variables

IMC <- round(runif(1872, min=16, max=40),1)
Pobreza <- round(runif(1872, min=0.1, max=81.3),1)

Dummy_vars <- Dummy
Dummy_vars$IMC <- IMC
Dummy_vars$Pobreza <- Pobreza

Dummy_vars

##-- Indicador posible fórmula --##
## positivos/población - vacunados/poblacion + Pobreza + IMC

## Dividirlo por tres cuartiles por el momento

