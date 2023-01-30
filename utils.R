format_date_human <- function (date = lubridate::now(), month_format = c("long",
                                                    "short", "number"), year_format = c("long", "short"), locale = "cs_CZ.UTF-8",
          day_dot = TRUE)
{
  orig_locale <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale("LC_TIME", locale)
  month_format2 <- match.arg(month_format, c("long", "short",
                                             "number"))
  year_format2 <- match.arg(year_format, c("long", "short"))
  month_format3 <- dplyr::case_when(month_format2 == "long" ~
                                      "%B", month_format2 == "short" ~ "%b", month_format2 ==
                                      "number" ~ "%m.")
  year_format3 <- dplyr::case_when(year_format2 == "long" ~
                                     "%Y", year_format2 == "short" ~ "%y")
  dot <- ifelse(day_dot, ".", "")
  if (month_format2 == "number") {
    date_formatted <- stringr::str_glue("{lubridate::day(date)}{dot} {lubridate::month(date)}. {format(date, year_format3)}")
  }
  else {
    mon_yr_fmt_string <- paste0(month_format3, " ", year_format3)
    mon_yr <- format(date, mon_yr_fmt_string)
    date_formatted <- stringr::str_glue("{lubridate::day(date)}{dot} {mon_yr}")
  }
  Sys.setlocale(category = "LC_TIME", orig_locale)
  return(date_formatted)
}
