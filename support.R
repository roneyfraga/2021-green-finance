
# excelente tutorial
#
# https://www.rstudio.com/resources/webinars/tidy-evaluation-is-one-of-the-major-feature-of-the-latest-versions-of-dplyr-and-tidyr/

# para pacote caiporar

library(rlang)

get_keywords_tfidf <- function(data, groups, keywords, sep = ';',  n_keywords = 15) {

    group <- rlang::enquo(groups)
    DE <- rlang::enquo(keywords)

    data |>
        tibble::as_tibble() |>
        dplyr::rename(group = !!group, DE = !!DE) |> 
        dplyr::filter(!is.na(.data$group)) %>>% 
        dplyr::filter(!is.na(.data$DE)) %>>% 
        dplyr::select(.data$group, .data$DE) |>
        tidyr::separate_rows(.data$DE, sep = sep) |>
        dplyr::mutate(DE = str_trim(.data$DE)) |>
        dplyr::group_by(.data$group, .data$DE) |> 
        dplyr::tally(sort = T) |> 
        dplyr::ungroup() |>
        dplyr::arrange(.data$group, desc(.data$n)) |> 
        dplyr::mutate(DE = str_trim(.data$DE)) ->
        grupoDEfreq

    grupoDEfreq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::arrange(.data$group, desc(n)) |> 
        dplyr::top_n(n_keywords) |> 
        dplyr::filter(.data$n > 1) |> 
        dplyr::mutate(keywords_freq = paste0(.data$DE, ' (', n, ')')) |> 
        dplyr::select(-.data$n) |> 
        dplyr::ungroup() ->
        keywords_freq

    grupoDEfreq |>
        dplyr::group_by(.data$group) |>
        dplyr::summarise(total = sum(.data$n)) ->
        total_DE

    left_join(grupoDEfreq, total_DE) |>
        tidytext::bind_tf_idf(.data$DE, .data$group, .data$n) ->
        tfidf

    tfidf |>
        dplyr::arrange(.data$group, desc(.data$tf_idf)) |>
        dplyr::group_by(.data$group) |> 
        dplyr::top_n(n_keywords) |> 
        dplyr::filter(.data$n > 1) |> 
        dplyr::mutate(keywords_tfidf = paste0(.data$DE, ' (', n, ')')) |> 
        dplyr::select(-.data$n) |> 
        dplyr::ungroup() |>
        dplyr::select(.data$group, .data$keywords_tfidf) ->
        tfidf_freq

    keywords_freq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(keywords_freq = paste(.data$keywords_freq, collapse = ', ')) -> 
        keywords_freq2

    tfidf_freq |> 
        dplyr::group_by(.data$group) |> 
        dplyr::summarise(keywords_tfidf = paste(.data$keywords_tfidf, collapse = ', ')) -> 
        tfidf_freq2

    dplyr::full_join(keywords_freq2, tfidf_freq2) 

}

net3 |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate(nodes) |>
    get_keywords_tfidf(group, DE, sep = ';', n_keywords = 20) 

get_keywords_tfidf(net3, group, DE, sep = ';', n_keywords = 20) 



# ------------------------------
### rlang tidyverse style
# ------------------------------

# env-variables are “programming” variables that live in an environment. They are usually created with <-.

var_summary <- function(data, var) {
  data %>%
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
}

mtcars %>% 
  group_by(cyl) %>% 
  var_summary(mpg)

mtcars %>% 
  group_by(cyl) %>% 
  var_summary('mpg')

# data-variables are “statistical” variables that live in a data frame. They usually come from data files (e.g. .csv, .xls), or are created manipulating existing variables.

for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()
}


for (var in names(mtcars)) {
  mtcars |> count(.data[[var]]) |> print()
}

soma_var2 <- function(df, var) {
    df |>
    summarise(soma = sum({{ var }}))
}

soma_var2(mtcars, cyl)
soma_var2(mtcars, 'cyl')


soma_var3 <- function(df, var) {
    df |>
    summarise(soma = sum(across({{ var }})))
}

soma_var3(mtcars, cyl)
soma_var3(mtcars, 'cyl')


var_summary <- function(data, var) {
  data |>
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
}

var_summary(mtcars, cyl) 
var_summary(mtcars, 'cyl') 

summarise_mean <- function(data, vars) {
  data %>% summarise(n = n(), across({{ vars }}, mean))
}

mtcars %>% 
  group_by(cyl) %>% 
  summarise_mean(where(is.numeric))

vars <- c("mpg", "vs")
mtcars %>% select(all_of(vars))
mtcars %>% select(!all_of(vars))


