
# Green Finance [directed] {#green-finance-directed}

Query applied on Scopus:

```

3,517 document results

TITLE-ABS-KEY ( ( finance  OR  financial )  W/3  ( green  OR  climate  OR  carbon  OR  sustainable ) )  AND  
( LIMIT-TO ( DOCTYPE ,  "ar" )  OR  LIMIT-TO ( DOCTYPE ,  "ch" )  OR  LIMIT-TO ( DOCTYPE ,  "re" )  OR  
LIMIT-TO ( DOCTYPE ,  "bk" ) )  AND  ( LIMIT-TO ( SRCTYPE ,  "j" ) )  
```

Downloaded in August 2021.

```{r eval = F, echo = F, warning = FALSE, error = TRUE, tidy = FALSE, message = FALSE}

# M <- convert2df(file = c("bibs/green_finance_1.bib", 
#                          "bibs/green_finance_2.bib", 
#                          "bibs/green_finance_3.bib"), 
#                 dbsource = "scopus", format = "bibtex")
#
# export(M, 'rawfiles/M_green_finance.rds')
```

## Growth rate

```{r eval = TRUE, echo = F, warning = FALSE, error = TRUE, tidy = FALSE, message = FALSE}

M <- import('rawfiles/M_green_finance.rds')

M %>>%
    count(PY, sort = F, name = 'Papers') %>>%
    (~ d0) %>>%
    dplyr::filter(PY %in% c(2003:2020)) %>>% 
    dplyr::arrange(PY) %>>% 
    dplyr::mutate(trend = 1:n()) %>>% 
    (. -> d)

d$lnp <- log(d$Papers)

# ajustar parametros via mqo 
m1 <- lm(lnp ~ trend, data = d)
# summary(m1)

beta0 <- m1$coefficients[[1]]
beta1 <- m1$coefficients[[2]]

# modelo não linear
# 2000 é o primeiro ano da série
m2 <- nls(Papers ~ b0 * exp(b1 * (PY - 2003)), start = list(b0 = beta0, b1 = beta1), data = d)
# summary(m2)

# publications estimado
d$predicted <- 3.58205 * exp(0.31068 * (d$PY - 2003))

# taxa de crescimento anual é de 
# (exp(0.31068) - 1) * 100

# o período necessário para dobrar o tempo é 
# log(2) / 0.31068

d %>>% 
    mutate(Year = PY) %>>% 
    mutate(predicted = round(predicted, 0)) %>>% 
    (. -> d)

periodo <- 2003:2024
predicted <- tibble::tibble(PY = periodo, Predicted = round(3.58205 * exp(0.31068 * (periodo - 2003)), 0)) 

dplyr::full_join(d0, predicted) |>
    dplyr::rename(Year = PY) |>
    dplyr::filter(Year >= 2000) ->
    d2

highcharter::hchart(d2, "column", hcaes(x = Year, y = Papers), name = "Publications", showInLegend = TRUE) %>>%
   highcharter::hc_add_series(d2, "line", hcaes(x = Year, y = Predicted), name = "Predicted", showInLegend = TRUE) %>>% 
   highcharter::hc_add_theme(hc_theme_elementary()) %>>%
   highcharter::hc_navigator(enabled = TRUE)  %>>% 
   highcharter::hc_exporting(enabled = TRUE, filename = 'groups_growth') %>>%
   highcharter::hc_xAxis(plotBands = list(list(from = 2020, to = 2020, color = "#330000")))
```

Analysis 2003-2020
<ul>
  <li> Growth Rate 36% </li>
  <li> Doubling time  2.2 Years </li>
</ul>

A série começa em 2003, pois, foi o ano em que inicia publicações sem interrupção. 

## Top 10 authors

```{r eval=TRUE, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
topAU <- authorProdOverTime(M, k = 15, graph = TRUE)
```

## Network

Directed citation network

```{r echo=FALSE, fig.cap="Directed Citation [Shibata et al (2009)]", out.width = '68%'}
knitr::include_graphics("images/met-2009-Shibata-et-al.png")
```

```{r eval=TRUE, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# histResults <- histNetwork(M, min.citations = 1, sep = ";")
# rio::export(histResults, 'rawfiles/histResults.rds')

rio::import('rawfiles/histResults.rds') -> histResults

histResults$NetMatrix |>
    graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) |> 
    simplify() |>
    as_tbl_graph() -> 
    net

net |>
    activate(nodes) |>
    left_join(M |> dplyr::mutate(name = SR) |> dplyr::select(name, PY, AB, DE, TI, DI, SO, TC, AU, SR)) ->
    net
```

## Componets 

Giant component of our network holds 2162 papers.

```{r eval=TRUE, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE, rows.print=5}

comp <- birddog::sniff_components(net)

net2 <- comp$network

comp$components |>
    dplyr::slice(1:20) |>
    rmarkdown::paged_table()
```

## Components description

[Excel file with papers' full info.](rawfiles/green_finance_directed_m_comps.xlsx)

### Keywords tfidf

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# M +  components

# selected_comps
comp$components |>
    dplyr::filter(quantity_publications >= 9) |>
    dplyr::pull(component) ->
    selected_comps

tidygraph::as_tbl_graph(comp$network) |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::filter(component %in% selected_comps) |>
    dplyr::left_join(M |> select(SR, CR)) ->
    m_comps

# rio::export(m_comps, 'rawfiles/green_finance_directed_m_comps.xlsx')

# keywords: frequency and tfidf per group
m_comps |> 
    dplyr::select(.data$component, .data$DE) |> 
    dplyr::filter(!is.na(.data$component)) |> 
    dplyr::filter(!is.na(.data$DE)) -> 
    comps_keywords

keywords_freq_tfidf <- birddog::sniff_tfidf(comps_keywords, 
                                            component, 
                                            DE,
                                            separate_rows = T, 
                                            sep = ';', 
                                            n_terms = 15)

keywords_freq_tfidf |>
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
    dplyr::rename(keyword_freq = term_freq, keyword_tfidf = term_tfidf) |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list( extend = 'collection', 
                                                         buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

Terms generated by Natural Language Processing using noun phrase filters.

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# terms via NLP
m_comps |>
    dplyr::group_by(.data$component) |>
    dplyr::summarise(text = paste(.data$TI, .data$AB, collapse = '. ')) |>
    dplyr::mutate(text = tolower(.data$text)) |>
    dplyr::mutate(text = gsub('elsevier|elsevier b v', '', text)) ->
    comps_texts

# time consuming
# tic()
# comps_texts |>
#     {\(x) split(x, x$component)}() |>
#     purrr::map(~ birddog::sniff_terms(data = ., 
#                                       groups = component, 
#                                       text = text, 
#                                       algorithm = 'phrase', 
#                                       n_cores = 3)) |>
#     purrr::map(~ tibble::as_tibble(.)) ->
#     comps_term_occorrence_phrase
# toc()
## XX minutes
#
# export(comps_term_occorrence_phrase, 'rawfiles/green_finance_comps_term_occorrence_phrase.rds')

import('rawfiles/green_finance_comps_term_occorrence_phrase.rds') -> 
    comps_term_occorrence_phrase 

# tfidf and frequency for NLP terms 
dplyr::bind_rows(comps_term_occorrence_phrase, .id = 'group') |>
    dplyr::filter(.data$ngram > 1 & .data$freq > 2) |>
    dplyr::select(group, keyword, freq) |>
    {\(x) birddog::sniff_tfidf(x, group = group, term = keyword, frequency = freq)}() ->
    terms_freq_tfidf

terms_freq_tfidf |>
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list( extend = 'collection', 
                                                         buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

## Authors per component

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

qtde_authors <- 10

m_comps %>>%
    dplyr::select(component, AU) %>>%
    tidyr::separate_rows(AU, sep = ';') %>>%
    dplyr::group_by(component, AU) %>>% 
    dplyr::tally(sort = T) %>>% 
    dplyr::ungroup() %>>% 
    dplyr::arrange(component, desc(n)) %>>% 
    dplyr::mutate(AU = stri_trim(AU)) %>>% 
    (. -> compAUfreq)

compAUfreq %>>% 
    dplyr::group_by(component) %>>% 
    dplyr::arrange(component, desc(n)) %>>% 
    dplyr::top_n(qtde_authors) %>>% 
    dplyr::filter(n >= 1) %>>% 
    dplyr::mutate(authors_freq = paste0(AU, ' (', n, ')')) %>>% 
    dplyr::select(-n) %>>% 
    dplyr::ungroup() %>>%
    (. -> authors_freq)

authors_freq |>
    dplyr::group_by(component) |>
    dplyr::summarise(authors_freq = paste(authors_freq, collapse = ', ')) |> 
    DT::datatable(extensions = 'Buttons', rownames = F, 
                  options = list(dom = 'Bfrtip', pageLength = 6, 
                                 buttons = list(list(extend = 'collection', 
                                                     buttons = list(list(extend = 'csv', filename = 'data'), 
                                                                    list(extend = 'excel', filename = 'data')), 
                                                 text = 'Download'))))
```

## Journals per component

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

qtde_journals <- 10

m_comps |>
    dplyr::select(component, SO) |>
    dplyr::group_by(component, SO) |> 
    dplyr::tally(sort = TRUE) |>
    dplyr::arrange(component, desc(n)) |> 
    dplyr::top_n(qtde_journals) |> 
    dplyr::filter(n > 1) |> 
    dplyr::mutate(n = paste0('(', n, ')')) |> 
    dplyr::mutate(SO2 = paste(SO, n, sep =' ')) |>
    dplyr::group_by(component) |>
    dplyr::summarise(SourceTitle = paste(SO2, collapse = '; ')) |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list( extend = 'collection', 
                                                         buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

## Groups

```{r eval=TRUE, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

gru <- birddog::sniff_groups(net2, 
                             min_group_size = 15, 
                             keep_component = 'component01', 
                             cluster_component = 'component01', 
                             algorithm = 'louvain')

net3 <- gru$network

gru$aggregate %>>% 
    as_tibble() %>>%
    dplyr::mutate(group = gsub('^.*_', '', group)) %>>% 
    rmarkdown::paged_table()
```

## Groups growth 

```{r eval=TRUE, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# M + groups 
tidygraph::as_tbl_graph(gru$network) |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::left_join(M |> select(SR, CR)) ->
    m_groups

m_groups |>
    dplyr::group_by(group, PY) |>
    dplyr::tally(name = 'Publications') |>
    dplyr::ungroup() |>
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
    dplyr::rename(Group = group, Year = PY) |>
    dplyr::filter(Year < 2021) ->
    groups_growth

highcharter::hchart(groups_growth, "line", hcaes(x = Year, y = Publications, group = Group), fillOpacity = 0.2) %>>% 
    highcharter::hc_add_theme(hc_theme_elementary())  %>>% 
    highcharter::hc_navigator(enabled = TRUE)  %>>% 
    highcharter::hc_exporting(enabled = TRUE, filename = 'groups_growth')
```

## Groups description

### Keywords tfidf

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# M + groups 
tidygraph::as_tbl_graph(gru$network) |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::left_join(M |> select(SR, CR)) ->
    m_groups

# keywords: frequency and tfidf per group
m_groups |> 
    dplyr::select(.data$group, .data$DE) |> 
    dplyr::filter(!is.na(.data$group)) |> 
    dplyr::filter(!is.na(.data$DE)) -> 
    groups_keywords

keywords_freq_tfidf <- birddog::sniff_tfidf(groups_keywords, 
                                            group, 
                                            DE,
                                            separate_rows = T, 
                                            sep = ';', 
                                            n_terms = 15)

keywords_freq_tfidf |>
    dplyr::rename(keyword_freq = term_freq, keyword_tfidf = term_tfidf) |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list( extend = 'collection', 
                                                         buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

### Terms NLP

Terms generated by Natural Language Processing using noun phrase filters.

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# terms via NLP
m_groups |>
    dplyr::filter(!is.na(.data$group)) |> 
    dplyr::group_by(.data$group) |>
    dplyr::summarise(text = paste(.data$TI, .data$AB, collapse = '. ')) |>
    dplyr::mutate(text = tolower(.data$text)) |>
    dplyr::mutate(text = gsub('elsevier|elsevier b v', '', text)) ->
    groups_texts

# time consuming
# tic()
# groups_texts |>
#     {\(x) split(x, x$group)}() |>
#     purrr::map(~ birddog::sniff_terms(data = ., 
#                                       groups = group, 
#                                       text = text, 
#                                       algorithm = 'phrase', 
#                                       n_cores = 3)) |>
#     purrr::map(~ tibble::as_tibble(.)) ->
#     group_term_occorrence_phrase
# toc()
## 5 minutes
#
export(group_term_occorrence_phrase, 'rawfiles/green_finance_group_term_occorrence_phrase_directed.rds')

import('rawfiles/green_finance_group_term_occorrence_phrase_directed.rds') -> 
    group_term_occorrence_phrase 

# tfidf and frequency for NLP terms 
dplyr::bind_rows(group_term_occorrence_phrase, .id = 'group') |>
    dplyr::filter(.data$ngram > 1 & .data$freq > 2) |>
    dplyr::select(group, keyword, freq) |>
    {\(x) birddog::sniff_tfidf(x, group = group, term = keyword, frequency = freq)}() ->
    terms_freq_tfidf

terms_freq_tfidf |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list( extend = 'collection', 
                                                         buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```


## Authors per group

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

qtde_authors <- 10

m_groups %>>%
    dplyr::filter(!is.na(group)) %>>% 
    dplyr::select(group, AU) %>>%
    tidyr::separate_rows(AU, sep = ';') %>>%
    dplyr::group_by(group, AU) %>>% 
    dplyr::tally(sort = T) %>>% 
    dplyr::ungroup() %>>% 
    dplyr::arrange(group, desc(n)) %>>% 
    dplyr::mutate(AU = stri_trim(AU)) %>>% 
    (. -> grupoAUfreq)

grupoAUfreq %>>% 
    dplyr::group_by(group) %>>% 
    dplyr::arrange(group, desc(n)) %>>% 
    dplyr::top_n(qtde_authors) %>>% 
    dplyr::filter(n >= 1) %>>% 
    dplyr::mutate(authors_freq = paste0(AU, ' (', n, ')')) %>>% 
    dplyr::select(-n) %>>% 
    dplyr::ungroup() %>>%
    (. -> authors_freq)

authors_freq |>
    dplyr::group_by(group) |>
    dplyr::summarise(authors_freq = paste(authors_freq, collapse = ', ')) |> 
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
    DT::datatable(extensions = 'Buttons', rownames = F, 
                  options = list(dom = 'Bfrtip', pageLength = 6, 
                                 buttons = list(list(extend = 'collection', 
                                                     buttons = list(list(extend = 'csv', filename = 'data'), 
                                                                    list(extend = 'excel', filename = 'data')), 
                                                 text = 'Download'))))
```

## Journals per group

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

qtde_journals <- 10

m_groups |>
    dplyr::filter(!is.na(group)) |> 
    dplyr::select(group, SO) |>
    dplyr::group_by(group, SO) |> 
    dplyr::tally(sort = TRUE) |>
    dplyr::arrange(group, desc(n)) |> 
    dplyr::top_n(qtde_journals) |> 
    dplyr::filter(n > 1) |> 
    dplyr::mutate(n = paste0('(', n, ')')) |> 
    dplyr::mutate(SO2 = paste(SO, n, sep =' ')) |>
    dplyr::group_by(group) |>
    dplyr::summarise(SourceTitle = paste(SO2, collapse = '; ')) |>
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list( extend = 'collection', 
                                                         buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

