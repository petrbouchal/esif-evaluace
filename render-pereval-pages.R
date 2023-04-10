library(quarto)
library(glue)
library(dplyr)
library(stringr)
library(purrr)
library(furrr)

plan(multisession)

dat <- readRDS("data-interim/evals_for_table.rds") |>
  mutate(eval_kod = str_sub(etapa_kod, 1, 7) |> str_remove("\\.$"))

render_single_eval <- function(eval_kod, file) {
  # quarto::quarto_render(file, execute_params = list(kraj = kraj),
  #                       output_file = paste0("kraj_", kraj, ".html"), quiet = TRUE)
  eval_kod_clean <- stringr::str_replace_all(eval_kod, "\\.", "\\-")
  output_file = paste0("eval_", eval_kod_clean, ".html")

  args <- c("render", file,
            "--execute-param", glue("eval_kod:{eval_kod}"),
            # "--metadata", glue("title:{eval_kod}"),
            # "--metadata", glue("pagetitle:{eval_kod}"),
            # "--profile", "batch",
            "--output-dir", ".",
            "--output", glue("{output_file}"))

  print(output_file)

  processx::run("quarto", args = args)
  return(output_file)
}
# evals_out <- map_chr(unique(dat$eval_kod)[1], render_single_eval, "single-eval-template.qmd")

# fs::dir_delete("evals")
evals_out <- map_chr(unique(dat$eval_kod), render_single_eval, "single-eval-template.qmd")
plan(sequential)
# fs::dir_delete("evals/docs")
unique(dat$eval_kod)[5]
