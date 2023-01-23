library(tidyverse)
library(xml2)
library(lubridate)

ciselniky_xml <- read_xml("https://ms14opendata.mssf.cz/MatDat.xml")

cis_op_list <- ciselniky_xml |>
  xml_ns_strip() |>
  xml_find_all("//HL") |>
  as_list()

cis_op <- tibble(data = cis_op_list) |>
  hoist(data,
        op_kod = "KOD",
        op_nazev = "NAZEV",
        op_zkratka = "ZKRATKA",
        .simplify = TRUE, .transform = unlist) |>
  drop_na(op_zkratka) |>
  select(-data) |>
  add_row(tibble(op_zkratka = "NOK (MMR)", op_nazev = "Národní orgán pro koordinaci"), op_kod = "NOK")

eval <- download.file("https://ms14opendata.mssf.cz/Evaluace.xml", "data-input/Evaluace.xml")
# prj <- download.file("https://ms14opendata.mssf.cz/SeznamProjektu.xml", "data-input/SeznamProjektu.xml")
eval <- read_xml("data-input/Evaluace.xml")
# prj <- read_xml("~/data-input/SeznamProjektu.xml")

evals <- eval |>
  xml_ns_strip() |>
  xml_find_all("//EVALUACE")

evals_list <- as_list(evals)

handle_null <- function(x) {
  if(is.null(x)) return(NA_character_) else return(x)
}

handle_null2 <- function(x) {
  if(is.null(x)) return(NA_character_) else return(x[[1]])
}


evals_df <- evals_list |>
  map(~map(.x, `[[`, 1)) |>
  map_dfr(~as_tibble(.x, .name_repair = janitor::make_clean_names)) |>
  pivot_longer(starts_with("ETAPA")) |>
  mutate(value = map_chr(value, handle_null)) |>
  rename(etapa = value) |>
  select(-name) |>
  distinct() |>
  rename(eval_id = id, eval_kod = kod, etapa_kod = etapa) |>
  mutate(op_kod = ifelse(grepl("NOK", eval_kod), str_sub(eval_kod, 1, 3),
                         str_sub(eval_kod, 1, 2))) |>
  left_join(cis_op, by = "op_kod")

etapy_list <- eval %>%
  xml_find_all(".//ETAPA") |>
  as_list()

etapy_df0 <- tibble(
  data = etapy_list) |>
  mutate(data2 = data) |>
  unnest_wider(data, simplify = TRUE,
               names_repair = janitor::make_clean_names,
               transform = unlist) |>
  select(-starts_with("zamereni")) |>
  hoist(data2,
        zamereni = "ZAMERENI") |>
  select(-data2) |>
  # mutate(zamereni = map(zamereni, 1)) |>
  # mutate(zamereni = map(zamereni, 1) |> map(handle_null) |> map(unlist)) |>
  rename(etapa_kod = kod) |>
  hoist(zamereni,
        zamereni_vyber = "NAZEV",
        zamereni_popis = "POPIS", .simplify = TRUE) |>
  mutate(across(starts_with("zamereni_"), \(x) map_chr(x, handle_null2)))



etapy_df0 |>
  count(map_int(prilohy, length))

links_df <- etapy_df0 |>
  select(etapa_kod, starts_with("prilohy")) |>
  pivot_longer(starts_with("prilohy"), names_to = "priloha_nazev", values_to = "priloha_data") |>
  select(-priloha_nazev) |>
  filter(lengths(priloha_data) > 0) |>
  hoist(.col = priloha_data,
        linkdms = "LINKDMS",
        link = "LINK",
        nazpril = "NAZPRIL", .remove = TRUE)


etapy_df <- etapy_df0 |>
  select(-prilohy) |>
  mutate(datumukonrealskut = as.Date(datumukonrealskut),
         rok_ukonceni = year(datumukonrealskut)) |>
  rename(popis_etapy = popis)

evals_all <- evals_df |>
  full_join(etapy_df, by = "etapa_kod") |>
  full_join(links_df, by = "etapa_kod") |>
  group_by(eval_kod) |>
  mutate(eval_ma_vystup = any(!is.na(linkdms)),
         eval_ma_etapy = any(!is.na(etapa_kod)),
         eval_ma_etapy_ukoncene = any(stav == "ukončeno")) |>
  rename(eval_nazev = nazeveval,
         etapa_nazev = nazev,
         eval_popis = popis,
         etapa_popis = popis_etapy,
         etapa_stav = stav,
         etapa_komentar = komentar,
         file_url = linkdms,
         file_text = nazpril,
         etapa_ukon_datum = datumukonrealskut,
         eval_typ_hledisko = typhlediska,
         eval_typ_faze = typdlefaze,
         eval_zamereni_vyber = zamereni_vyber,
         eval_zamereni_popis = zamereni_popis) |>
  mutate(etapa_ukon_rok = year(etapa_ukon_datum),
         eval_lbl = paste(op_zkratka, " • ", eval_nazev),
         eval_is_impact = str_detect(eval_typ_hledisko, "[Dd]opad"),
         eval_is_impact_txt = if_else(eval_is_impact, "Dopad/výsledky", "Jiná"),
         eval_ma_etapy_txt = if_else(eval_ma_etapy, "Ano", "Ne"),
         eval_ma_etapy_ukoncene_txt = if_else(eval_ma_etapy_ukoncene, "Ano", "Ne"),
         eval_ma_vystup_txt = if_else(eval_ma_vystup, "Ano", "Ne"),
         ) |>
  replace_na(list(eval_ma_etapy_txt = "Ne",
                  eval_ma_vystup_txt = "Ne",
                  eval_ma_etapy_ukoncene_txt = "Ne")) |>
  ungroup()

write_rds(evals_all, "data-interim/evals_all.rds")

write_rds(links_df, "data-interim/links.rds")
write_rds(etapy_df, "data-interim/etapy.rds")
write_rds(evals_df, "data-interim/evals.rds")


etapy_df |> filter(etapa_kod == "10.003.01") |> select(starts_with("zamereni"))
gt::gt(etapy_df[c("etapa_kod", "zamereni_vyber", "zamereni_popis")])

