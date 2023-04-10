library(quarto)
library(glue)
library(dplyr)
library(stringr)
library(purrr)
# library(furrr)


dat <- readRDS("data-interim/evals_for_table.rds") |>
  mutate(eval_kod = str_sub(etapa_kod, 1, 7) |> str_remove("\\.$"))

render_single_eval <- function(eval_kod, eval_nazev, file) {
  # quarto::quarto_render(file, execute_params = list(kraj = kraj),
  #                       output_file = paste0("kraj_", kraj, ".html"), quiet = TRUE)
  eval_kod_clean <- stringr::str_replace_all(eval_kod, "\\.", "\\-")
  output_file = paste0("eval_", eval_kod_clean, ".html")

  args <- c("render", file,
            "--execute-param", glue("eval_kod:{eval_kod}"),
            "--metadata", glue("title:{eval_nazev}"),
            "--metadata", glue("pagetitle:{eval_nazev}"),
            # "--profile", "batch",
            "--output-dir", ".",
            "--output", glue("{output_file}"))

  print(output_file)

  processx::run("quarto", args = args)
  return(output_file)
}
# evals_out <- map_chr(unique(dat$eval_kod)[1], render_single_eval, "single-eval-template.qmd")

evals_meta <- distinct(dat, eval_kod, eval_nazev)

# fs::dir_delete("evals")
# plan(multisession)
# evals_out <- future_map2_chr(evals_meta$eval_kod, evals_meta$eval_nazev,
evals_out <- map2_chr(evals_meta$eval_kod, evals_meta$eval_nazev,
                             render_single_eval, "single-eval-template.qmd",
                             .progress = TRUE)
# plan(sequential)
# fs::dir_delete("evals/docs")
