library(dplyr)
library(tidyr)
library(ggplot2)

raw_data<-readxl::read_excel("Test-HUC8loadperunit2007and2019.xlsx",
                             sheet = "Sector - All Agencies")
data<-raw_data %>% 
  pivot_longer(cols=c(7:27),names_to="output",values_to="loadrates") %>% 
  select(-Sector,-Allocation,-Agency,-Unit) %>% 
  mutate(year=gsub(' .*', '', output),
         pollutant=gsub('.*_|Load.*', '', output),
         load_type=gsub('.*LoadRate', '', output)) %>% 
  select(-output)

#for watershed prgress rates by pollutant
data2<-data %>% 
  group_by(Geography, year, load_type, pollutant) %>% 
  summarise(totals=sum(loadrates,na.rm=TRUE), .groups='drop') %>% 
  pivot_wider(names_from = year, values_from =totals)

results<-data2 %>% 
  mutate(cb_gb=(`2019`-`2007`)/(WIP-`2007`)*100,
         gc_g=(WIP-`2019`)/WIP*100,
         cg_bg=1-(`2019`-WIP)/(`2007`-WIP),
         wip_higher=case_when(
           `2007`- WIP <=0  ~ "*",
           TRUE~        "-"))

EOS<-filter(results, load_type=="EOS")
EOT<-filter(results, load_type=="EOT")

#for james progress by sector
data3<-data %>% 
  group_by(year,load_type,pollutant,LoadSource) %>% 
  summarise(totals=sum(loadrates,na.rm=TRUE), .groups='drop') %>% 
  pivot_wider(names_from = year, values_from =totals)
results3<-data3 %>% 
  mutate(cb_gb=(`2019`-`2007`)/(WIP-`2007`)*100,
         gc_g=(WIP-`2019`)/WIP*100,
         cg_bg=1-(`2019`-WIP)/(`2007`-WIP),
         wip_higher=case_when(
           `2007`- WIP <=0  ~ "*",
           TRUE~        NA_character_))

#for watershed progress by sector
results4<-data %>% 
  pivot_wider(names_from = year, values_from=loadrates) %>% 
  mutate(cb_gb=(`2019`-`2007`)/(WIP-`2007`)*100,
         gc_g=(WIP-`2019`)/WIP*100,
         cg_bg=1-(`2019`-WIP)/(`2007`-WIP),
         wip_higher=case_when(
           `2007`- WIP <=0  ~ "*",
           TRUE~        NA_character_))

#plots!
#x=watershed, y=loadrates, color=pollutant, shape=sector
a<-results4 %>% 
  filter(pollutant!="Amount") %>% 
  filter(load_type=="EOT") %>% 
  ggplot(.)+
  theme_minimal()+
  geom_point(aes(x=Geography, y=cb_gb,color=LoadSource, shape=pollutant), size=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+geom_hline(aes(yintercept=0))
a

b<-results %>% 
  filter(pollutant!="Amount") %>% 
  filter(load_type=="EOT") %>% 
  filter(wip_higher!="*") %>% 
  ggplot(.)+
  theme_minimal()+
  geom_point(aes(x=Geography, y=cb_gb,color=pollutant), size=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+geom_hline(aes(yintercept=0))
b

c<-results3 %>% 
  filter(pollutant!="Amount") %>% 
  filter(load_type=="EOT") %>% 
  ggplot(.)+
  theme_minimal()+
  geom_point(aes(x=LoadSource, y=cb_gb,color=pollutant), size=3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+geom_hline(aes(yintercept=0))
c
