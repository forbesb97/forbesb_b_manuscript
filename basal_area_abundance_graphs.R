# ==============================================================================
#
# Calculate basal area of tree species in Pepperwood and Saddle Mountain and
# determine most common species by number, to see which is the most prominent 
# species to report 
#
# ==============================================================================

library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

# ================================= User inputs ================================

c1_trad <- read_csv('D:/trad_data/c1_biomass_attributes.csv')
c6_trad <- read_csv('D:/trad_data/c6_trad_field-data.csv')

figure_out <- 'D:/Analyses/figures/species_abundance_basal-area.png'

# ================== abundance of each species in each campaign ================

c1_trad <- c1_trad %>%
  select(Plot.x, Tag, 'DBH (cm)', AK_species) %>%
  rename(dbh = 'DBH (cm)') %>%
  filter(!is.na(dbh))

c6_trad <- c6_trad %>%
  filter(!is.na(DBH)) %>% 
  select(Plot, Tag, DBH, Species) %>%
  rename(dbh = DBH)

c1_n_sp <- c1_trad %>%
  count(AK_species) %>%
  arrange(desc(n)) %>%
  rename(Species=AK_species)

c6_n_sp <- c6_trad %>%
  count(Species) %>%
  arrange(desc(n))

# ================== basal area of each species in each campaign ===============

# BA = pi x (DBH/2)^2 / 10000 where ba = m2 and dbh = cm

#mutate(df, height + 10)

c1_ba <- c1_trad %>%
  mutate((pi * ((dbh/2)^2)) / 10000) %>%
  rename(basal_area =`(pi * ((dbh/2)^2))/10000`)

c6_ba <- c6_trad %>%
  mutate((pi * ((dbh/2)^2)) / 10000)%>%
  rename(basal_area =`(pi * ((dbh/2)^2))/10000`)

c1_ba_sp <-  c1_ba %>%
  group_by(AK_species) %>% 
  summarise(ba_sp = sum(basal_area)) %>%
  arrange(desc(ba_sp))%>%
  rename(Species=AK_species)

c6_ba_sp <-  c6_ba %>%
  group_by(Species) %>% 
  summarise(ba_sp = sum(basal_area))%>%
  arrange(desc(ba_sp))

# ================================ combine campaigns ============================

c1 <- c1_ba_sp %>%
  full_join(c1_n_sp, by='Species')

c6 <- c6_ba_sp %>%
  full_join(c6_n_sp, by='Species')


# c1 <- c1 %>%
#   mutate(Species = recode(Species,
#          QUEGAR = 'Quercus garryana',
#          QUEAGR = 'Quercus agrifolia',
#          UMBCAL = 'Umbellulaira californica',
#          PSEMEN = 'Pseudosuga menziesii',
#          ARBMEN = 'Arbutus menziesii',
#          QUEKEL = 'Quercus kelloggi',
#          QUEDOU = 'Quercus douglassii',
#          AESCAL = 'Aesculus californica',
#          ARCMAN = 'Arctostaphylos manzanita',
#          NOTDEN = 'Notholithocarpos densiflorus',
#          HETARB = 'Heteromeles arbutifolia'))
# 
# c6 <- c6 %>%
#   mutate(Species = recode(Species,
#           QUAG = 'Quercus agrifolia',
#           PSME = 'Pseudosuga menziesii',
#           UMCA = 'Umbellulaira californica',
#           QUGA = 'Quercus garryana',
#           QUKE = 'Quercus kelloggi',
#           SESE = 'Sequoia sempervirens',
#           ARMA = 'Arctostaphylos manzanita',
#           ARME = 'Arbutus menziesii',
#           QULO = 'Quercus lobata'))

c1 <- c1 %>%
  mutate(Species = recode(Species,
         QUEGAR = 'QUGA',
         QUEAGR = 'QUAG',
         UMBCAL = 'UMCA',
         PSEMEN = 'PSME',
         ARBMEN = 'ARME',
         QUEKEL = 'QUKE',
         QUEDOU = 'QUDO',
         AESCAL = 'AECA',
         ARCMAN = 'ARMA',
         NOTDEN = 'NODE',
         HETARB = 'HEAR'))

c6 <- c6 %>%
  mutate(Species = recode(Species,
          QUAG = 'QUAG',
          PSME = 'PSME',
          UMCA = 'UMCA',
          QUGA = 'QUGA',
          QUKE = 'QUKE',
          SESE = 'SESE',
          ARMA = 'ARMA',
          ARME = 'ARME',
          QULO = 'QULO'))

total <- full_join(c1, c6, by="Species")

total <- total %>%
  replace_na(list (n.x=0,
                   n.y=0,
                   ba_sp.x=0,
                   ba_sp.y=0))%>%
  mutate(n.x + n.y) %>% 
  mutate(ba_sp.x + ba_sp.y) %>%
  rename(c1_n = n.x,
         c6_n = n.y,
         n_total = 'n.x + n.y',
         c1_ba = ba_sp.x,
         c6_ba = ba_sp.y,
         ba_total = 'ba_sp.x + ba_sp.y'
         )

total <- total %>%
  mutate(n_percent= n_total/ sum(n_total),
         ba_percent= ba_total/ sum(ba_total))
  

# =================================== Set theme ================================

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 24),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 16),
    axis.text.x = element_blank(),
    line = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(color = 'black'),
    panel.border = element_blank(),
    legend.position = 'none',
    plot.caption = element_text(color = "grey25", face = "italic", size = 12)
  )
)

# ==================================== Figures =================================


abundance <- ggplot(data=total, aes(x=reorder(Species, -n_percent), y=n_percent)) +
  geom_bar(stat = 'identity', aes( fill=Species)) +
  geom_text(aes(label=Species), vjust=-0.3, size=4.5) +
  ylab('% Abundance')+
  scale_fill_manual(values=c("#FCCDE5", "#FFFFB3",  "#D9D9D9","#BEBADA", "#FB8072" ,"#80B1D3",
                             "#8DD3C7", "#FDB462", "#B3DE69", "#BC80BD",'burlywood',"#CCEBC5", "#FFED6F")) +
  ylim(0,0.35)
abundance

basal_area <- ggplot(data=total, aes(x=reorder(Species, -ba_percent), y=ba_percent)) +
  geom_bar(stat = 'identity', aes(fill=Species)) +
  geom_text(aes(label=Species), vjust=-0.3, size=4.5) +
  ylab('% Basal Area') +
  scale_fill_manual(values=c("#FCCDE5", "#FFFFB3",  "#D9D9D9","#BEBADA", "#FB8072" ,"#80B1D3",
                             "#8DD3C7", "#FDB462", "#B3DE69", "#BC80BD",'burlywood',"#CCEBC5", "#FFED6F"))+
  ylim(0,0.35)
basal_area

figure <-
  ggarrange(
    abundance,
    basal_area,
    ncol = 2,
    nrow = 1,
    widths = c(7.5, 7.5),
    heights = c(7.5)
  )

figure <- annotate_figure(figure,
                               bottom = text_grob('Species\n ', 
                                                  family = 'serif', 
                                                  size = 24))
ggsave(
  figure_out,
  plot=figure,
  width = 15,
  height = 8,
  units = 'in',
  dpi = 300 )








