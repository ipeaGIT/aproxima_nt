source("../../../acesso_oport_kaue/R/fun/setup.R")

library(readxl)

# open locations
locations <- read_xlsx("../data-raw/Aproxima_SelecaoTerrenosSPU.xlsx", skip = 1)

# select columns
locations_select <- locations %>% dplyr::select(code_muni = CODIBGE, Bairro, lon = vl_longitude, lat = vl_latitude) %>%
  # create id
  mutate(id_location = 1:n()) %>%
  # filter only munis that are in the acess project 
  filter(code_muni %in% munis_df_2019$code_muni) %>%
  # bring abrev_muni
  left_join(dplyr::select(munis_df_2019, code_muni, abrev_muni), by = "code_muni") %>%
  # clean bairros' names
  mutate(bairro = str_to_title(Bairro)) %>%
  mutate(bairro = ifelse(bairro == "Ipiranga - Eixo Ferroviário Cptm", "Ipiranga", 
                         ifelse(bairro == "Mooca - Eixo Ferroviário Cptm", "Mooca", bairro)))

# transform to sf
locations_sf <- locations_select %>% to_spatial()

# open hex files
# the hex files didnt contain the sigla_muni, so I had to add it
hex <- map2(sprintf("../../../data/hex_municipio/2019/hex_%s_09_2019.rds", unique(locations_select$abrev_muni)), unique(locations_select$abrev_muni),
              function(x, y) read_rds(x) %>% mutate(sigla_muni = y)) %>%
  rbindlist() %>%
  st_sf(crs = 4326)

# spatial join to identify in which hexagon each location is located

identify_locations <- function(sigla_munii) {
  
  locations_filter <- locations_sf %>% filter(abrev_muni == sigla_munii) %>% 
    # we wont need this column futher on
    select(-abrev_muni)
  
  hex_filter <- hex %>% dplyr::filter(sigla_muni == sigla_munii)
  
  # spatial join!!!!!!!!!!!!!!
  locations_hex <- st_join(locations_filter, hex_filter)
}

# apply function and bind
location_hex <- lapply(unique(locations_select$abrev_muni), identify_locations) %>% rbindlist() %>% st_sf(crs = 4326)
  # mutate(id = 1:n())
  



# open acess data
acess <- read_rds("../../../data/output_base_final/2019/dados2019_AcessOport_v1.0_20200116_interno.rds") %>%
  filter(!is.na(R003))

# compare access (teste) ------------------------------------------------------------------------------

# # filter aproxima munis
# acess_muni <- acess %>% filter(sigla_muni %in% unique(locations_select$abrev_muni))
# 
# # plot?
# 
# # teste: CMATT60 sao paulo
# # location_hex_test <- location_hex
# location_hex_test <- location_hex %>% filter(sigla_muni == "for")
# 
# 
# acess_teste <- acess_muni %>%
#   filter(sigla_muni == "for") %>%
#   filter(modo == "tp") %>%
#   filter(pico == 1) %>%
#   select(sigla_muni, id_hex, P001, matches("CMATT30|CMATT60")) %>%
#   st_set_geometry(NULL)
#   # para formato longo
#   # gather(tt, valor, CMATT30, CMATT60)
# 
# # aces values for locatoins
# locations_acess <- acess_teste %>% filter(id_hex %in% location_hex_test$id_hex) %>%
#   left_join(select(location_hex_test, id_hex, Bairro), by = "id_hex")
# 
# # open funs
# source("ggplot_funs.R")
# 
# density_range <- (range(density(acess_teste$CMATT30)$y)[2] -range(density(acess_teste$CMATT30)$y)[1])/2  
# 
# ggplot(data = acess_teste, aes(CMATT30))+
#   geom_density(aes(weight = P001/sum(P001)))+
#   geom_vline(data = locations_acess, aes(xintercept = CMATT30), color = "red", size = 2)+
#   geom_text(data = locations_acess, aes(x=CMATT30, label=Bairro, y=density_range), colour="black", angle=90, vjust = -1, hjust = "middle")+
#   stat_mean(linetype = 15, color = "red")+
#   stat_percentile_x(probs = c(0.5, 0.85), linetype = 2)+
#   stat_percentile_xlab(probs = c(0.5, 0.85), hjust=1, vjust=-0.5, angle=90)+
#   scale_x_percent()
#   # theme_ipsum_rc(grid = FALSE)+
#   # facet_grid(sigla_muni ~ tt, scales = "free")



# FUNS TO PLOTS -------------------------------------------------------------------------------

# open funs
source("ggplot_funs.R")

# filter aproxima munis
acess_muni <- acess %>% filter(sigla_muni %in% unique(locations_select$abrev_muni))

# ajeitar TMI (Inf = 120 min ) e trunca para > 30 min
acess_long <- acess_muni %>%
  mutate_at(vars(matches("TMI")), function (x) ifelse(is.infinite(x), 120, x)) %>%
  mutate_at(vars(matches("TMI")), function (x) ifelse(x > 30, 30, x))
  # st_set_geometry(NULL)

# acess_long_cma <- acess_long %>%
#   gather(tipo, valor, CMATT15:CMAEM120) %>%
#   separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
#   separate(indicador, c("indicador", "atividade"), sep = 3)
# 
# acess_long_tmi <- acess_long %>%
#   gather(tipo, valor, TMIST:TMIEM) %>%
#   separate(tipo, c("indicador", "atividade"), sep = 3)
  
# quais terrenos estao em hex que nao tem informacoes de acess?
location_hex_fora <- location_hex %>%
  filter(id_hex %nin% acess_long$id_hex) %>%
  mutate(id = 1:n()) %>%
  select(id_hex, id) %>%
  sfc_as_cols()

location_hex_dentro <- acess_long %>%
  st_centroid() %>%
  sfc_as_cols() %>%
  distinct(id_hex, lon, lat) %>%
  mutate(id = 1:n())

# qual o hex com informacao mais proximo dos hex sem informacao?
# pegar os pontos mais proximos nao-problematicos dos problematicos
uui <- RANN::nn2(select(location_hex_dentro, lon, lat), 
                 select(location_hex_fora, lon, lat), 1)

# corrigir entao o locationhex
location_hex_fora_corrigido <- location_hex_fora %>%
  mutate(id_hex_novo = location_hex_dentro[c(as.numeric(uui$nn.idx)), ]$id_hex) %>%
  select(id_hex, id_hex_novo)

# substituir no location_hex
location_hex_corrigido <- setDT(location_hex)[setDT(location_hex_fora_corrigido), 
                                              on = "id_hex", id_hex := i.id_hex_novo]


make_aproxima_plots <- function(muni) {

  # filter muni 
  location_hex_test <- location_hex_corrigido %>% filter(sigla_muni == muni)
  
  # muni name
  muni_name <- filter(munis_df_2019, abrev_muni == muni) %>% .$name_muni

  # PLOT 1
  acess_atividade <- acess_long %>%
    filter(sigla_muni == muni) %>%
    # filter(indicador == "CMA") %>%
    filter(modo %in% c("caminhada", "tp")) %>%
    # filter(atividade == "TT") %>%
    filter(pico == 1) %>%
    select(sigla_muni, id_hex, P001, modo, matches("CMATT30|CMATT60"))
    # para formato longo
    # gather(tt, valor, CMATT30, CMATT60)
  
  # CMATT30 a pe
  acess_plot1 <- acess_atividade %>%
    filter(modo == "caminhada") %>%
    # ordenar por acessibilidade e populacao
    arrange(CMATT30, P001) %>%
    mutate(cumsum_pop = cumsum(P001)/sum(P001))
  
  terrenos_plo1 <- acess_plot1 %>%
    filter(id_hex %in% location_hex_test$id_hex) %>%
    left_join(select(location_hex_test, id_hex, bairro), by = "id_hex") %>%
    distinct(sigla_muni, id_hex, bairro, .keep_all = TRUE)
  
  # density_range1 <- (range(density(acess_plot1$CMATT30)$y)[2] -range(density(acess_plot1$CMATT30)$y)[1])/2  
  

    
  
  
  plot1 <- acess_plot1 %>%
    ggplot(aes(CMATT30)) +
    geom_line(aes(y=cumsum_pop, x=CMATT30)) +
    geom_vline(xintercept = acess_plot1$CMATT30[which( round(acess_plot1$cumsum_pop, 2) == .5)][1], linetype = 2) +
    # stat_percentile_x(probs = c(0.5), linetype = 2) +
    # stat_percentile_xlab(probs = c(0.5), hjust=1, vjust=-0.5, angle=90, show.legend = TRUE)+
    geom_point(data = terrenos_plo1, aes(x = CMATT30, y = cumsum_pop, color = bairro), size = 3)+
    ggrepel::geom_label_repel(data = terrenos_plo1, aes(x = CMATT30, y = cumsum_pop, label = scales::percent(CMATT30, accuracy = 0.1), color = bairro),
                              # nudge_x = 0.05,
                              # min.segment.length = 0.5,
                              size = 3,
                              point.padding = unit(0.15, "cm"),
                              alpha = 0.9,
                              show.legend = FALSE)+

    # geom_text(x= )
    # geom_histogram(aes(weight = P001/sum(P001)))+
    # geom_vline(data = locations_acess, aes(xintercept = CMATT30, color = bairro), size = 1)+
    # geom_text(data = locations_acess, aes(x=CMATT30, label=Bairro, y=density_range1), colour="black", angle=90, vjust = -1, hjust = "middle")+
    # stat_mean(linetype = 15, color = "red")+
    # geom_hline(aes(yintercept = mean(CMATT30, na.rm = FALSE)), color="gray") +
  
    scale_x_percent()+
    scale_y_percent()+
    labs(title = "A pé",
         x = "Proporção de empregos acessíveis em até 30 min")
    # labs(title = "Percentual de oportunidades de empregos acessíveis em até 30 minutos por transporte público")
    # theme_ipsum()+
    # theme(legend.position = "bottom")
  
  
  # CMATT60 tp
  acess_plot2 <- acess_atividade %>%
    filter(modo == "tp") %>%
    # ordenar por acessibilidade e populacao
    arrange(CMATT60, P001) %>%
    mutate(cumsum_pop = cumsum(P001)/sum(P001))
  
  terrenos_plot2 <- acess_plot2 %>%
    filter(id_hex %in% location_hex_test$id_hex) %>%
    left_join(select(location_hex_test, id_hex, bairro), by = "id_hex") %>%
    distinct(sigla_muni, id_hex, bairro, .keep_all = TRUE)
  
  if (nrow(acess_plot2) != 0) {
    
    
#  density_range2 <- (range(density(acess_plot2$CMATT60)$y)[2] -range(density(acess_plot2$CMATT60)$y)[1])/2  
  
  plot2 <- acess_plot2 %>%
    ggplot(aes(CMATT60))+
    # geom_freqpoly(aes(weight = P001/sum(P001)))+
    geom_line(aes(y=cumsum_pop, x=CMATT60))+
    # geom_vline(data = locations_acess, aes(xintercept = CMATT60, color = bairro), size = 2)+
    geom_point(data = terrenos_plot2, aes(x = CMATT60, y = cumsum_pop, color = bairro), size = 3) +
    geom_vline(xintercept = acess_plot2$CMATT60[which( round(acess_plot2$cumsum_pop, 2) == .5)][1], linetype = 2) +
    # stat_percentile_xlab(probs = c(0.5), hjust=1, vjust=-0.5, angle=90)+
    ggrepel::geom_label_repel(data = terrenos_plot2, aes(x = CMATT60, y = cumsum_pop, label = scales::percent(CMATT60, accuracy = 0.1), color = bairro),
                              # nudge_x = 0.05,
                              # min.segment.length = 0.5,
                              size = 3,
                              point.padding = unit(0.15, "cm"),
                             alpha = 0.9,
                             show.legend = FALSE)+
    scale_x_percent()+
    scale_y_percent()+
    labs(title = "Transporte Público",
         x = "Proporção de empregos acessíveis em até 60 min")
  
# violin  
  # acess_plot2 %>%
  #   ggplot()+
  #   geom_violin(aes(y=CMATT60, x= muni, weight = P001/sum(P001)), stat = "ydensity")
  #   
  # 
  }
  
  
  # PLOT 2
  acess_atividade2 <- acess_long %>%
    filter(sigla_muni == muni) %>%
    # filter(indicador == "CMA") %>%
    filter(modo %in% c("caminhada")) %>%
    # filter(atividade == "TT") %>%
    filter(pico == 1) %>%
    select(sigla_muni, id_hex, P001, modo, matches("TMISM|TMIEM"))
  
  # # aces values for locatoins
  # locations_acess_tmi <- acess_atividade2 %>% filter(id_hex %in% location_hex_test$id_hex) %>%
  #   left_join(select(location_hex_test, id_hex, bairro), by = "id_hex") %>%
  #   distinct(sigla_muni, id_hex, bairro, .keep_all = TRUE)
  
  # TMISM
  acess_plot3 <- acess_atividade2 %>%
    # ordenar por acessibilidade e populacao
    arrange(TMISM, P001) %>%
    mutate(cumsum_pop = cumsum(P001)/sum(P001))
  
  terrenos_plot3 <- acess_plot3 %>%
    filter(id_hex %in% location_hex_test$id_hex) %>%
    left_join(select(location_hex_test, id_hex, bairro), by = "id_hex") %>%
    distinct(sigla_muni, id_hex, bairro, .keep_all = TRUE)
  
  
  # TMISM a pe
  # density_range3 <- (range(density(acess_atividade2$TMISM)$y)[2] -range(density(acess_atividade2$TMISM)$y)[1])/2  
  
  plot3 <- acess_plot3 %>%
    ggplot(aes(TMISM))+
    # geom_freqpoly(aes(weight = P001/sum(P001)))+
    geom_line(aes(y=cumsum_pop, x=TMISM))+
    # geom_vline(data = locations_acess_tmi, aes(xintercept = TMISM, color = bairro), size = 2)+
    geom_point(data = terrenos_plot3, aes(x = TMISM, y = cumsum_pop, color = bairro), size = 3)+
    # stat_mean(linetype = 15, color = "red")+
# stat_percentile_x(aes(linetype = "Mediana"), probs = c(0.5))+
    geom_vline(xintercept = acess_plot3$TMISM[which( round(acess_plot3$cumsum_pop, 2) == .5)][1], aes(linetype = "Mediana")) +
    
    # stat_percentile_xlab(probs = c(0.5), hjust=1, vjust=-0.5, angle=90)+
    ggrepel::geom_label_repel(data = terrenos_plot3, aes(x = TMISM, y = cumsum_pop, label = round(TMISM, 0), color = bairro),
                              # nudge_x = 0.05,
                              # min.segment.length = 0.5,
                              size = 3,
                              point.padding = unit(0.15, "cm"),
                             alpha = 0.9,
                             show.legend = FALSE)+
    scale_linetype_manual(name = "", values = c("Mediana" = 2))+
    scale_x_continuous(breaks = c(0, 10, 20, 30), labels = c("0", "10", "20", "30+"))+
    scale_y_percent()+
    labs(title = "A pé",
         x = "Tempo até a escola mais próxima (min)")
  
  # TMIEM a pe
  acess_plot4 <- acess_atividade2 %>%
    # ordenar por acessibilidade e populacao
    arrange(TMIEM, P001) %>%
    mutate(cumsum_pop = cumsum(P001)/sum(P001))
  
  terrenos_plot4 <- acess_plot4 %>%
    filter(id_hex %in% location_hex_test$id_hex) %>%
    left_join(select(location_hex_test, id_hex, bairro), by = "id_hex") %>%
    distinct(sigla_muni, id_hex, bairro, .keep_all = TRUE)
  
  # TMIEM a pe
  # density_range4 <- (range(density(acess_atividade2$TMIEM)$y)[2] -range(density(acess_atividade2$TMIEM)$y)[1])/2  
  
  plot4 <- acess_plot4 %>%
    ggplot(aes(TMISM))+
    # geom_freqpoly(aes(weight = P001/sum(P001)))+
    geom_line(aes(y=cumsum_pop, x=TMIEM))+
    # geom_vline(data = locations_acess_tmi, aes(xintercept = TMISM, color = bairro), size = 2)+
    geom_point(data = terrenos_plot4, aes(x = TMIEM, y = cumsum_pop, color = bairro), size = 3)+
    # stat_mean(linetype = 15, color = "red")+
#    stat_percentile_x(probs = c(0.5), linetype = 2)+
    geom_vline(xintercept = acess_plot4$TMIEM[which( round(acess_plot4$cumsum_pop, 2) == .5)][1], linetype = 2) +
    
    
    # stat_percentile_xlab(probs = c(0.5), hjust=1, vjust=-0.5, angle=90)+
    ggrepel::geom_label_repel(data = terrenos_plot4, aes(x = TMIEM, y = cumsum_pop, label = round(TMIEM, 0), color = bairro),
                              # nudge_x = 0.05,
                              # min.segment.length = 0.5,
                              size = 3,
                              point.padding = unit(0.15, "cm"),
                             alpha = 0.9,
                             show.legend = FALSE)+
    scale_y_percent()+
    scale_x_continuous(breaks = c(0, 10, 20, 30), labels = c(0, 10, 20, "30+"))+
    labs(title = "A pé",
         x = "Tempo até o hospital mais próximo (min)")
  
  library(patchwork)
  
  if (nrow(acess_plot2) != 0) {

    fim <- (plot1 + plot2) / (plot3 + plot4) +  plot_annotation(title = muni_name, tag_levels = "A") + plot_layout(guides = 'collect') & 
      theme_bw() & theme(legend.position = "bottom", 
                         legend.box="horizontal",
                         legend.title = element_text(size = 8),
                         legend.text = element_text(size = 7),
                         # legend.background = element_rect(fill="grey95", 
                         #                                  size=0.5, linetype="solid"),
                         panel.grid.minor = element_blank(),
                         axis.title = element_text(size = 7),
                         axis.text = element_text(size = 6)) & 
      labs(y = "População", color = "Bairro do Terreno")
     

  } else {fim <- (plot1) / (plot3 + plot4) +  plot_annotation(title = muni_name, tag_levels = "A") + plot_layout(guides = 'collect') & 
    theme_bw() & theme(legend.position = "bottom", 
                       legend.box="horizontal",
                       legend.title = element_text(size = 8),
                       legend.text = element_text(size = 7),
                       # legend.background = element_rect(fill="grey95", 
                       #                                  size=0.5, linetype="solid"),
                       panel.grid.minor = element_blank(),
                       axis.title = element_text(size = 8),
                       axis.text = element_text(size = 6)) & 
    labs(y = "População", color = "Bairro do Terreno") }
  
  
  
    
  
  # writeout
  ggsave(sprintf("../figures/aproxima_%s.png", muni), fim, dpi = 300)
  
}


# apply function
walk(unique(locations_select$abrev_muni), make_aproxima_plots)
