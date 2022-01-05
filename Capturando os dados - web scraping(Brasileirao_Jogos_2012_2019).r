library(stringr)
library(rvest)
library(glue)

for(i in 2019:2021) {
  
  url <- glue("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/{i}")
  
  
  resultados <- url %>% 
    read_html() %>% 
    html_nodes(".aside-rodadas")
  
  casa <- resultados %>% 
    html_nodes(".pull-left .time-sigla") %>% 
    html_text()
  
  
  fora <- resultados %>% 
    html_nodes(".pull-right .time-sigla") %>% 
    html_text()
  
  
  placar <-  resultados %>% 
    html_nodes(".partida-horario") %>%
    html_text() %>%
    str_extract("[0-9]{1}\ x\ [0-9]{1}")
  
  rodada <- 0:(length(placar)-1) %/% 10 + 1
  
  df <- if( i == 2019)
  { 
    
    data.frame(cbind(rodada = rodada,
                     casa = casa,
                     placar = placar,
                     fora = fora,
                     ano = rep(i,length(rodada) ) ) ) }
  else{
    
    data.frame(rbind(df, cbind(rodada = rodada,
                               casa = casa,
                               placar = placar,
                               fora = fora,
                               ano = rep(i,length(rodada) ) ) ))
  }
  
  
}

#write.csv(df, "Brasileirao_Jogos_2012_2019.csv") #salvando df


summary(df) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()