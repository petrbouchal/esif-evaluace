get_base_name <- function(filename) {
  str_remove_all(filename, "\\.[a-zA-Z]{1,4}$|") |> str_replace_all("_", " ")
}

is_english <- function(filename) {
  filename_orig <- filename
  filename <- tolower(filename)

  grepl("interim|evaluation|inception|report|summary|eng", filename, ignore.case = TRUE) | grepl("AJ|EN", filename_orig)
}

is_kraj <- function(filename) {
  filename_orig <- filename
  filename <- tolower(filename)

  grepl("kraj|regio", filename, ignore.case = TRUE)
}

get_icons <- function(filename) {

  filename_orig <- filename
  filename <- tolower(filename)

  iconname <- case_when(grepl("příloh|priloh", filename) ~ "paperclip",
                        grepl("závěr|zaver", filename) ~ "flag-checkered",
                        grepl("průběž|interim|prubez", filename) ~ "spinner",
                        grepl("smlouva", filename) ~ "section",
                        grepl("vstup|inceptio", filename) ~ "circle-play",
                        grepl("csv|xls", filename) ~ "table",
                        grepl("ppt|prezentace", filename) ~ "file-powerpoint",
                        grepl("shrnutí|summary|shrnuti|manag|manaž|manaz", filename) ~ "business-time",
                        grepl("letá|leta|graf", filename) ~ "file-invoice",
                        grepl("případ|pripad", filename) ~ "magnifying-glass",
                        grepl("teorie změn|teorie zmen", filename) ~ "diagram-project",
                        grepl("doporučení|recommend", filename) ~ "list-check",
                        grepl("\\.doc", filename) ~ "file-word",
                        grepl("\\.pdf", filename) ~ "file-pdf",
                        TRUE ~ "file"
  )

  is_english <- is_english(filename_orig)

  is_contract <- grepl("smlouva", filename, ignore.case = TRUE)
  htmltools::tagList(
    # if(is_english) htmltools::tagList(fontawesome::fa_i("globe"), tags$span(" ")) else NULL,
    if(!is.na(iconname)) fa_i(iconname),
    tags$span(" ")
  )
}

eval_for_page <- function(data) {
  dt <- data[1,]

  rtrn <- tagList(
    tags$div(
      class = "popis-evaluace-box",
      tags$div(tags$span("Popis: "), dt$eval_popis, class = "popis-evaluace"),
      tags$div(paste0("Zaměření evaluace: ",
                      if(!is.na(dt$eval_typ_hledisko)) dt$eval_typ_hledisko else "nezadáno"),
               class = "zamereni-evaluace"),
      style = "margin-bottom: 0px")
  )

  return(rtrn)
}

etapa_for_page <- function(data) {

  eval_field <- function(name, ...) {
    if (any(is.na(...))) NULL
    else tagList(tags$div(class = "detail-heading", span(class = "detail-label", name), ...))
  }
  eval_block <- function(name, ...) {
    if (any(is.na(...))) NULL
    else tagList(tags$div(class = "detail-heading", span(class = "detail-label", name)),
                 tags$div(..., class = "popis-etapy-text"))
  }

  dt <- data

  etapa_id <- unique(dt$etapa_kod)

  dt_all <- data

  dt_sub <- dt_all[dt_all$etapa_kod == etapa_id,]

  odkazy <- dt_sub |>
    select(odkazy) |>
    unnest(odkazy)

  if(any(!is.na(dt$etapa_nazev))) {
    rtrn <- tagList(
      tags$section(id = dt$etapa_kod, class = "level3",
                   tags$h3(dt$etapa_nazev, `data-anchor-id` = dt$etapa_kod),
                   tags$div(
                     class = "popis-etapy-box",
                     eval_field("Datum dokončení: ", if(!is.na(dt$etapa_ukon_datum)) format_date_human(dt$etapa_ukon_datum) else NA),
                     eval_block("Popis etapy", dt$etapa_popis),
                     eval_block("Komentář", dt$etapa_komentar),
                     if(nrow(odkazy) > 1) tags$div("Zprávy ke stažení", class = "detail-label") else NULL,
                     tags$div(

                       purrr::pmap(list(odkazy$file_url, odkazy$file_text, odkazy$link), function(x, y, link)
                         if(is.na(x)) NULL else tags$div(tags$a(tags$span(get_icons(y), style = "display: inline-block; width: 20px;"),
                                                                href = x, style = "background-color: white; color: darkblue;"),
                                                         tags$a(tags$span(get_base_name(y)), href = x),
                                                         if(is_english(y)) tagList(tags$span(" "), fa_i("globe")),
                                                         if(is_kraj(y)) tagList(tags$span(" "), fa_i("map-location-dot", class = "link")),
                                                         if(!is.na(link)) tagList(tags$span(" + "),
                                                                                  tags$a("dodatečný odkaz", href = link)),
                                                         class = "link"))
                     )
                   ))
    )

  } else {rtrn <- tagList(NULL)}

  return(rtrn)
}
