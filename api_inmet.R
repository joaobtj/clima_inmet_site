library(httr2)
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
source("ET0_calc.R")

## Download from INMET API - Estações
## https://portal.inmet.gov.br/manual/manual-de-uso-da-api-esta%C3%A7%C3%B5es

# data_ini <- "2012-01-01" # "2016-02-20"
# data_fim <- "2013-01-01" # "2016-02-22"
# station <- "A860" # Curitibanos-SC
# alt = 978.10
# lat = -27.288624
# tst <- get_inmet(data_ini, data_fim, station, alt, lat)


get_inmet <- function(data_ini, data_fim, station, alt, lat) {

  ## Hora
  url_base_hora <- "https://apitempo.inmet.gov.br/estacao"
  url_hora <- glue::glue(url_base_hora, data_ini, data_fim, station, .sep = "/")

  dados_hora <- httr2::request(url_hora) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      data = lubridate::ymd_hm(paste(DT_MEDICAO, HR_MEDICAO)),
      data = lubridate::with_tz(data, tzone = "Etc/GMT+3")
    ) %>%    
    janitor::clean_names() %>%
    dplyr::select(-c(dc_nome, vl_latitude, uf, vl_longitude, cd_estacao, dt_medicao, hr_medicao, ten_bat)) %>%
    dplyr::mutate(across(.cols = !data, as.numeric)) %>%
    dplyr::relocate(data)




  ## Dia
  dados_dia <- dados_hora %>%
    mutate(data = lubridate::date(data)) %>%
    group_by(data) %>%
    summarise(
      chuva = sum(chuva),
      tem_max = max(tem_max),
      tem_min = min(tem_min),
      tem_med = mean(tem_ins),
      umd_max = max(umd_max),
      umd_min = min(umd_min),
      umd_med = mean(umd_ins),
      ven_vel_med = mean(ven_vel),
      rad_glo = sum(rad_glo),
      et0 = ET0_calc(tem_max, tem_min, tem_med, umd_med, ven_vel_med, rad_glo /1000, alt = alt, lat = lat)
    ) %>%
    dplyr::slice_head(n=-1) %>%
    dplyr::slice_tail(n=-1) %>%
    dplyr::arrange(data)
}
