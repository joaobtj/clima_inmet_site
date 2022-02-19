library(httr2)
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
source("ET0_calc.R")

## Download from INMET API - Estações
## https://portal.inmet.gov.br/manual/manual-de-uso-da-api-esta%C3%A7%C3%B5es

# data_ini <- "2015-10-14" # "2016-02-20"
# data_fim <- "2015-10-20" # "2016-02-22"
# station <- "A860" # Curitibanos-SC
# alt = 978.10
# lat = -27.288624

get_inmet <- function(data_ini, data_fim, station, alt, lat) {
  # ## dia (do API)
  # url_base_dia <- "https://apitempo.inmet.gov.br/estacao/diaria"
  # url_dia <- glue::glue(url_base_dia, data_ini, data_fim, station, .sep = "/")
  #
  # data_dia <- httr2::request(url_dia) %>%
  #   httr2::req_perform() %>%
  #   httr2::resp_body_json() %>%
  #   dplyr::bind_rows() %>%
  #   select(-c(DC_NOME, VL_LATITUDE, UF, VL_LONGITUDE, CD_ESTACAO))

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
    dplyr::select(-c(DC_NOME, VL_LATITUDE, UF, VL_LONGITUDE, CD_ESTACAO, DT_MEDICAO, HR_MEDICAO)) %>%
    janitor::clean_names() %>%
    dplyr::mutate(across(.cols = !data, as.numeric)) %>%
    dplyr::relocate(data)




  ## Dia
  dados_dia <- dados_hora %>%
    mutate(data = lubridate::date(data)) %>%
    group_by(data) %>%
    summarise(
      chuva = sum(chuva),
      temp_max = max(tem_max),
      temp_min = min(tem_min),
      temp_med = mean(tem_ins),
      umid_max = max(umd_max),
      umid_min = min(umd_min),
      umid_med = mean(umd_ins),
      vel_vento_med = mean(ven_vel),
      rad_glo = sum(rad_glo),
      et0 = ET0_calc(temp_max, temp_min, temp_med, umid_med, vel_vento_med, rad_glo / 1000, alt = alt, lat = lat)
    ) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(desc(data))
}
