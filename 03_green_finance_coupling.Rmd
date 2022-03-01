
# Green Finance [coupling] {#green-finance-coupling}

Query applied on Scopus:

```

3,517 document results

TITLE-ABS-KEY ( ( finance  OR  financial )  W/3  ( green  OR  climate  OR  carbon  OR  sustainable ) )  AND  
( LIMIT-TO ( DOCTYPE ,  "ar" )  OR  LIMIT-TO ( DOCTYPE ,  "ch" )  OR  LIMIT-TO ( DOCTYPE ,  "re" )  OR  
LIMIT-TO ( DOCTYPE ,  "bk" ) )  AND  ( LIMIT-TO ( SRCTYPE ,  "j" ) )  
```

Downloaded in August 2021.

```{r eval = F, echo = F, warning = FALSE, error = TRUE, tidy = FALSE, message = FALSE}

M <- convert2df(file = c("bibs/green_finance_1.bib", 
                         "bibs/green_finance_2.bib", 
                         "bibs/green_finance_3.bib"), 
                dbsource = "scopus", format = "bibtex")

export(M, 'rawfiles/M_green_finance.rds')
export(M, 'rawfiles/M_green_finance.csv')
```

[Download csv file.](rawfiles/M_green_finance.csv)

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

Bibliographic coupling was chosen, because it focuses on the most recent papers.

```{r echo=FALSE, fig.cap="Bibliographic Coupling [Shibata et al (2009)]", out.width = '68%'}
knitr::include_graphics("images/met-2009-Shibata-et-al.png")
```

```{r eval=TRUE, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

# histResults <- histNetwork(M, min.citations = 1, sep = ";")
#
# histResults$NetMatrix |>
#     graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) |> 
#     simplify() |>
#     as_tbl_graph() -> 
#     net
#
# net |>
#     activate(nodes) |>
#     left_join(M |> dplyr::mutate(name = SR) |> dplyr::select(name, PY, AB, DE, TI, DI, SO, TC, AU, SR)) |>
#     net

M |> 
    biblioNetwork(analysis = "coupling", network = "references", sep = ";") |> 
    graph_from_adjacency_matrix(mode = "undirected", weighted = NULL, diag = FALSE) |> 
    simplify() |>
    as_tbl_graph() -> 
    net

net |>
    activate(nodes) |>
    left_join(M |> dplyr::mutate(name = paste(SR, PY, sep = '. ')) |> dplyr::select(name, PY, AB, DE, TI, DI, SO, TC, AU, SR)) ->
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
    dplyr::filter(Year < 2021) |>
    dplyr::filter(!is.na(Group)) ->
    groups_growth

highcharter::hchart(groups_growth, "line", hcaes(x = Year, y = Publications, group = Group), fillOpacity = 0.2) %>>% 
    highcharter::hc_add_theme(hc_theme_elementary())  %>>% 
    highcharter::hc_navigator(enabled = TRUE)  %>>% 
    highcharter::hc_exporting(enabled = TRUE, filename = 'groups_growth')
```

## Groups growth rate 

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

grupos <- sort(unique(groups_growth$Group))

res <- vector('double', length(grupos))

for (i in seq_along(grupos)) { 

    groups_growth |> 
        dplyr::filter(Group==paste0('g0', i)) |> 
        dplyr::arrange(Year) |>  
        dplyr::filter(Year >= 2010) |>
        dplyr::mutate(trend = 1:n()) |>
        dplyr::mutate(lnp = log(Publications)) ->
        d 

    # ajustar parametros via mqo 
    m1 <- lm(lnp ~ trend, data = d)
    beta0 <- m1$coefficients[[1]]
    beta1 <- m1$coefficients[[2]]
    
    # modelo não linear
    m2 <- nls(Publications ~ b0*exp(b1 * (Year - 2010)), start = list(b0 = beta0, b1 = beta1), data = d)
    res[[i]] <- coef(m2)[2]

}

data.frame(Group = grupos, Coef = res) %>>% 
    tibble::as_tibble() %>>% 
    dplyr::mutate(GrowthRateYear = (exp(Coef) - 1)*100) %>>% 
    dplyr::select(-Coef) %>>% 
    left_join(gru$aggregate |> dplyr::mutate(group = gsub('^.*_', '', group)) |> dplyr::rename(Group = group)) %>>% 
    dplyr::select(Group, quantity_papers, average_age, GrowthRateYear) |> 
    rmarkdown::paged_table()
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
## 20 minutes
#
# export(group_term_occorrence_phrase, 'rawfiles/green_finance_group_term_occorrence_phrase.rds')

import('rawfiles/green_finance_group_term_occorrence_phrase.rds') -> 
    group_term_occorrence_phrase 

# tfidf and frequency for NLP terms 
dplyr::bind_rows(group_term_occorrence_phrase, .id = 'group') |>
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


## Hubs per group

```{r eval=T, echo=F, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

m_groups |>
    dplyr::select(SR, group, CR, TC, PY, TI) |> 
    dplyr::filter(!is.na(group)) -> 
    tt

# birddog::sniff_topological_positions(m_groups, 
#                                      id = SR, 
#                                      group = group, 
#                                      PY = PY, 
#                                      TI = TI, 
#                                      CR = CR, 
#                                      min.citations = 1) ->
#     hubs
#
# rio::export(hubs, 'rawfiles/hubs_green_finance.rds')

rio::import('rawfiles/hubs_green_finance.rds') -> 
    hubs

# hubs full info
hubs |>    
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
    dplyr::mutate(SR = gsub('-c|-b|-a', '', SR)) |>
    dplyr::mutate(SR = gsub('-a', '', SR)) |>
    dplyr::distinct(SR, .keep_all = TRUE) |>
    dplyr::left_join(m_groups %>>% select(SR, AU, PY, TI, DE, DI, AB)) |>    
    dplyr::relocate(group, SR, PY) |>
    dplyr::distinct(SR, .keep_all = T) ->
    hubs_full_info

rio::export(hubs_full_info, 'rawfiles/hubs_full_info_green_finance.xlsx')

# Hubs per group
hubs |> 
    dplyr::mutate(SR = gsub('-c|-b|-a', '', SR)) |>
    dplyr::mutate(SR = gsub('-a', '', SR)) |>
    dplyr::distinct(SR, .keep_all = TRUE) |>
    dplyr::mutate(hub = ifelse(Zi >= 2.5, 'hub', 'nohub')) |> 
    dplyr::count(group, hub) |> 
    tidyr::spread(hub, n) |>
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
    rmarkdown::paged_table()

hubs |>
    dplyr::mutate(SR = gsub('-c|-b|-a', '', SR)) |>
    dplyr::mutate(SR = gsub('-a', '', SR)) |>
    dplyr::distinct(SR, .keep_all = TRUE) |>
    dplyr::filter(zone != 'noHub') |>
    dplyr::mutate(group = gsub('^.*_', '', group)) |>
    dplyr::mutate(Zi = round(Zi, digits = 2), Pi = round(Pi, digits = 2)) |>
        DT::datatable(extensions = 'Buttons', rownames = F, 
                      options = list(
                                     dom = 'Bfrtip', pageLength = 10, 
                                     buttons = list(list(extend = 'collection', 
                                                         buttons = list(list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

- TC = Scopus times cited
- Ki = Network citations
- ki = Group citations
- Zi = The within-group degree $z_i$ measures how ‘well-connected’ article $i$ is to other articles in the group [$z_i \geq 2.5$ Hub]
- Pi = Measures how ‘well-distributed’ the links of article $i$ are among different groups. [higher $=$ citations better distributed between groups]
- zone = $R5$ provincial hubs; $R6$ connector hubs; $R7$ kinless hubs 
- TI = Title
- DI = DOI
- AB = Abstract
- DE = Keywords

[Excel file with papers' full info.](rawfiles/hubs_full_info_green_finance.xlsx)


## Groups immersion

### g01  

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

aut <- get_authors_network(M, 
                           m_groups, 
                           hubs_full_info, 
                           keep_group = 'component01_g01', 
                           internal_citations = 11)

visNetwork(aut$nodes, 
           aut$edges, 
           main = "Authors collaboration - g01",
           height = "700px", 
           width = '800px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  |>
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visInteraction(navigationButtons = TRUE)
```

### g02

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

aut <- get_authors_network(M, 
                           m_groups, 
                           hubs_full_info, 
                           keep_group = 'component01_g02', 
                           internal_citations = 6)

visNetwork(aut$nodes, 
           aut$edges, 
           main = "Authors collaboration - g02",
           height = "700px", 
           width = '800px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  |>
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visInteraction(navigationButtons = TRUE)
```

### g03


```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

aut <- get_authors_network(M, 
                           m_groups, 
                           hubs_full_info, 
                           keep_group = 'component01_g03', 
                           internal_citations = 3)

visNetwork(aut$nodes, 
           aut$edges, 
           main = "Authors collaboration - g03",
           height = "700px", 
           width = '800px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  |>
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visInteraction(navigationButtons = TRUE)
```

### g04


```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

aut <- get_authors_network(M, 
                           m_groups, 
                           hubs_full_info, 
                           keep_group = 'component01_g04', 
                           internal_citations = 2)

visNetwork(aut$nodes, 
           aut$edges, 
           main = "Authors collaboration - g04",
           height = "700px", 
           width = '800px') %>>% 
    visIgraphLayout(layout = "layout_with_kk")  |>
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visInteraction(navigationButtons = TRUE)
```

## Topic Modeling  

Wikipedia description:

* _In machine learning and natural language processing, a topic model is a type of statistical model for discovering the abstract "topics" that occur in a collection of documents. Topic modeling is a frequently used text-mining tool for discovery of hidden semantic structures in a text body._

Tutoriais:

* [A Friendly Introduction to Text Clustering](https://medium.com/swlh/topic-modeling-in-r-with-tidytext-and-textminer-package-latent-dirichlet-allocation-764f4483be73)
* [YT Short Introduction to Topic Modeling](https://www.youtube.com/watch?v=p1I9Sa1lRvk)
* [YT Introduction to Topic Modeling with R](https://www.youtube.com/watch?v=IUAHUEy1V0Q)
* [YT David Blei Palestra](https://www.youtube.com/watch?v=FkckgwMHP2s&t=490s)

### LDA g01

* LDA - Latent Direchlet Allocation
    * Blei (2012) Probabilistic topic models. Communications of the ACM.
    * primeiro método da área 

Cada `t_` significa um tópico de pesquisa dentro de determinado grupo, seguido das princiapis palavras-chave que compõem o tópico.

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

grupo <- 'component01_g01'

m_groups |> 
    dplyr::filter(group == grupo) |> 
    dplyr::group_by(SR) |>
    dplyr::summarise(text = paste(.data$TI, .data$AB, collapse = '. '), TI = TI) |>
    dplyr::select(SR, text, TI) |> 
    dplyr::distinct(TI, .keep_all = TRUE) ->
    g01


g01_clean <- textcleaner_lda(g01$text)

g01_clean |> 
    dplyr::mutate(id = g01$SR) |>
    tibble::as_tibble() ->
    g01_clean

# criar uma DTM a partir de um data.frame()
set.seed(123)
dtm_g01 <- textmineR::CreateDtm(doc_vec = g01_clean$x,
                                doc_names = g01_clean$id,
                                ngram_window = c(1, 2),
                                stopword_vec = stopwords("en"),
                                verbose = F)

dtm_g01 <- dtm_g01[, Matrix::colSums(dtm_g01) > 2]

# cálculo do LDA, com métricas de qualidade por tópico
set.seed(123)
lda_g01 <- textmineR::FitLdaModel(dtm = dtm_g01,
                                  k = 5,              # number of topic
                                  iterations = 500,
                                  burnin = 180,
                                  alpha = 0.1,
                                  beta = 0.05,
                                  optimize_alpha = T,
                                  calc_likelihood = T,
                                  calc_coherence = T,
                                  calc_r2 = T)


# analisar a qualidade dos topics
# lda_g01$r2

# plot(lda_g01$log_likelihood,type = "l")

# Tabela dos principais termos
lda_g01$top_terms <- GetTopTerms(phi = lda_g01$phi, M = 15)

lda_g01$top_terms |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 15, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

Coerência = proximidade das palavas-chave de determinado tópico.

Prevalecência = participação das palavas-chave em todos os documentos. 


```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
# cohrence de cada tópico
# lda_g01$coherence

# Prevalence is the probability of topics distribution in the whole documents
lda_g01$prevalence <- colSums(lda_g01$theta) / sum(lda_g01$theta) * 100
# lda_g01$prevalence

lda_g01$summary <- data.frame(topic = rownames(lda_g01$phi),
                                coherence = round(lda_g01$coherence, 3),
                                prevalence = round(lda_g01$prevalence, 3),
                                top_terms = apply(lda_g01$top_terms, 2, function(x) { paste(x, collapse = ", ")}))

modsum_g01 <- lda_g01$summary %>%
  `rownames<-`(NULL)

modsum_g01 %>% 
    tidyr::pivot_longer(cols = c(coherence, prevalence)) %>%
    ggplot(aes(x = factor(topic, levels = unique(topic)), y = value, group = 1)) +
    geom_point() + 
    geom_line() +
    facet_wrap(~ name, scales = "free_y", nrow = 2) +
    theme_minimal() + 
    labs(title = "Best topics by coherence and prevalence score", 
         subtitle = "Text review with 5 rating", x = "Topics", y = "Value")

# dendograma
# lda_g01$linguistic <- CalcHellingerDist(lda_g01$phi)
# lda_g01$hclust <- hclust(as.dist(lda_g01$linguistic),"ward.D")
# lda_g01$hclust$labels <- paste(lda_g01$hclust$labels, lda_g01$labels[,1])
# plot(lda_g01$hclust)
```

Encontrar os principais trabalhos dentre de cada tópico. Quanto maior o valor de `theta` mais o trabalho pertence a determinado tópico. Lembrando que um trabalho percente a mais de um tópico (soft-clustering).

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
theta_g01 <- as.data.frame(lda_g01$theta)  
theta_g01$SR <- rownames(theta_g01)

tibble::as_tibble(theta_g01) |>
    left_join(g01 |> dplyr::select(- text)) |>
    tidyr::pivot_longer(! c(SR, TI), names_to = "Topic", values_to = "theta") |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 5, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))


```

### LDA g02

Cada `t_` significa um tópico de pesquisa dentro de determinado grupo, seguido das princiapis palavras-chave que compõem o tópico.

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

grupo <- 'component01_g02'

m_groups |> 
    dplyr::filter(group == grupo) |> 
    dplyr::group_by(SR) |>
    dplyr::summarise(text = paste(.data$TI, .data$AB, collapse = '. '), TI = TI) |>
    dplyr::select(SR, text, TI) |> 
    dplyr::distinct(TI, .keep_all = TRUE) ->
    g02


g02_clean <- textcleaner_lda(g02$text)

g02_clean |> 
    dplyr::mutate(id = g02$SR) |>
    tibble::as_tibble() ->
    g02_clean

# criar uma DTM a partir de um data.frame()
set.seed(123)
dtm_g02 <- textmineR::CreateDtm(doc_vec = g02_clean$x,
                                doc_names = g02_clean$id,
                                ngram_window = c(1, 2),
                                stopword_vec = stopwords("en"),
                                verbose = F)

dtm_g02 <- dtm_g02[, Matrix::colSums(dtm_g02) > 2]

# cálculo do LDA, com métricas de qualidade por tópico
set.seed(123)
lda_g02 <- textmineR::FitLdaModel(dtm = dtm_g02,
                                  k = 5,              # number of topic
                                  iterations = 500,
                                  burnin = 180,
                                  alpha = 0.1,
                                  beta = 0.05,
                                  optimize_alpha = T,
                                  calc_likelihood = T,
                                  calc_coherence = T,
                                  calc_r2 = T)


# analisar a qualidade dos topics
# lda_g02$r2

# plot(lda_g02$log_likelihood,type = "l")

# Tabela dos principais termos
lda_g02$top_terms <- GetTopTerms(phi = lda_g02$phi, M = 15)

lda_g02$top_terms |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 15, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

Coerência = proximidade das palavas-chave de determinado tópico.

Prevalecência = participação das palavas-chave em todos os documentos. 


```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
# cohrence de cada tópico
# lda_g02$coherence

# Prevalence is the probability of topics distribution in the whole documents
lda_g02$prevalence <- colSums(lda_g02$theta) / sum(lda_g02$theta) * 100
# lda_g02$prevalence

lda_g02$summary <- data.frame(topic = rownames(lda_g02$phi),
                                coherence = round(lda_g02$coherence, 3),
                                prevalence = round(lda_g02$prevalence, 3),
                                top_terms = apply(lda_g02$top_terms, 2, function(x) { paste(x, collapse = ", ")}))

modsum_g02 <- lda_g02$summary %>%
  `rownames<-`(NULL)

modsum_g02 %>% 
    tidyr::pivot_longer(cols = c(coherence, prevalence)) %>%
    ggplot(aes(x = factor(topic, levels = unique(topic)), y = value, group = 1)) +
    geom_point() + 
    geom_line() +
    facet_wrap(~ name, scales = "free_y", nrow = 2) +
    theme_minimal() + 
    labs(title = "Best topics by coherence and prevalence score", 
         subtitle = "Text review with 5 rating", x = "Topics", y = "Value")

# dendograma
# lda_g02$linguistic <- CalcHellingerDist(lda_g02$phi)
# lda_g02$hclust <- hclust(as.dist(lda_g02$linguistic),"ward.D")
# lda_g02$hclust$labels <- paste(lda_g02$hclust$labels, lda_g02$labels[,1])
# plot(lda_g02$hclust)
```

Encontrar os principais trabalhos dentre de cada tópico. Quanto maior o valor de `theta` mais o trabalho pertence a determinado tópico. Lembrando que um trabalho percente a mais de um tópico (soft-clustering).

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
theta_g02 <- as.data.frame(lda_g02$theta)  
theta_g02$SR <- rownames(theta_g02)

tibble::as_tibble(theta_g02) |>
    left_join(g02 |> dplyr::select(- text)) |>
    tidyr::pivot_longer(! c(SR, TI), names_to = "Topic", values_to = "theta") |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 5, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))


```

### LDA g03

Cada `t_` significa um tópico de pesquisa dentro de determinado grupo, seguido das princiapis palavras-chave que compõem o tópico.

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

grupo <- 'component01_g03'

m_groups |> 
    dplyr::filter(group == grupo) |> 
    dplyr::group_by(SR) |>
    dplyr::summarise(text = paste(.data$TI, .data$AB, collapse = '. '), TI = TI) |>
    dplyr::select(SR, text, TI) |> 
    dplyr::distinct(TI, .keep_all = TRUE) ->
    g03


g03_clean <- textcleaner_lda(g03$text)

g03_clean |> 
    dplyr::mutate(id = g03$SR) |>
    tibble::as_tibble() ->
    g03_clean

# criar uma DTM a partir de um data.frame()
set.seed(123)
dtm_g03 <- textmineR::CreateDtm(doc_vec = g03_clean$x,
                                doc_names = g03_clean$id,
                                ngram_window = c(1, 2),
                                stopword_vec = stopwords("en"),
                                verbose = F)

dtm_g03 <- dtm_g03[, Matrix::colSums(dtm_g03) > 2]

# cálculo do LDA, com métricas de qualidade por tópico
set.seed(123)
lda_g03 <- textmineR::FitLdaModel(dtm = dtm_g03,
                                  k = 5,              # number of topic
                                  iterations = 500,
                                  burnin = 180,
                                  alpha = 0.1,
                                  beta = 0.05,
                                  optimize_alpha = T,
                                  calc_likelihood = T,
                                  calc_coherence = T,
                                  calc_r2 = T)


# analisar a qualidade dos topics
# lda_g03$r2

# plot(lda_g03$log_likelihood,type = "l")

# Tabela dos principais termos
lda_g03$top_terms <- GetTopTerms(phi = lda_g03$phi, M = 15)

lda_g03$top_terms |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 15, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

Coerência = proximidade das palavas-chave de determinado tópico.

Prevalecência = participação das palavas-chave em todos os documentos. 


```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
# cohrence de cada tópico
# lda_g03$coherence

# Prevalence is the probability of topics distribution in the whole documents
lda_g03$prevalence <- colSums(lda_g03$theta) / sum(lda_g03$theta) * 100
# lda_g03$prevalence

lda_g03$summary <- data.frame(topic = rownames(lda_g03$phi),
                                coherence = round(lda_g03$coherence, 3),
                                prevalence = round(lda_g03$prevalence, 3),
                                top_terms = apply(lda_g03$top_terms, 2, function(x) { paste(x, collapse = ", ")}))

modsum_g03 <- lda_g03$summary %>%
  `rownames<-`(NULL)

modsum_g03 %>% 
    tidyr::pivot_longer(cols = c(coherence, prevalence)) %>%
    ggplot(aes(x = factor(topic, levels = unique(topic)), y = value, group = 1)) +
    geom_point() + 
    geom_line() +
    facet_wrap(~ name, scales = "free_y", nrow = 2) +
    theme_minimal() + 
    labs(title = "Best topics by coherence and prevalence score", 
         subtitle = "Text review with 5 rating", x = "Topics", y = "Value")

# dendograma
# lda_g03$linguistic <- CalcHellingerDist(lda_g03$phi)
# lda_g03$hclust <- hclust(as.dist(lda_g03$linguistic),"ward.D")
# lda_g03$hclust$labels <- paste(lda_g03$hclust$labels, lda_g03$labels[,1])
# plot(lda_g03$hclust)
```

Encontrar os principais trabalhos dentre de cada tópico. Quanto maior o valor de `theta` mais o trabalho pertence a determinado tópico. Lembrando que um trabalho percente a mais de um tópico (soft-clustering).

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
theta_g03 <- as.data.frame(lda_g03$theta)  
theta_g03$SR <- rownames(theta_g03)

tibble::as_tibble(theta_g03) |>
    left_join(g03 |> dplyr::select(- text)) |>
    tidyr::pivot_longer(! c(SR, TI), names_to = "Topic", values_to = "theta") |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 5, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))


```

### LDA g04

Cada `t_` significa um tópico de pesquisa dentro de determinado grupo, seguido das princiapis palavras-chave que compõem o tópico.

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}

grupo <- 'component01_g04'

m_groups |> 
    dplyr::filter(group == grupo) |> 
    dplyr::group_by(SR) |>
    dplyr::summarise(text = paste(.data$TI, .data$AB, collapse = '. '), TI = TI) |>
    dplyr::select(SR, text, TI) |> 
    dplyr::distinct(TI, .keep_all = TRUE) ->
    g04


g04_clean <- textcleaner_lda(g04$text)

g04_clean |> 
    dplyr::mutate(id = g04$SR) |>
    tibble::as_tibble() ->
    g04_clean

# criar uma DTM a partir de um data.frame()
set.seed(123)
dtm_g04 <- textmineR::CreateDtm(doc_vec = g04_clean$x,
                                doc_names = g04_clean$id,
                                ngram_window = c(1, 2),
                                stopword_vec = stopwords("en"),
                                verbose = F)

dtm_g04 <- dtm_g04[, Matrix::colSums(dtm_g04) > 2]

# cálculo do LDA, com métricas de qualidade por tópico
set.seed(123)
lda_g04 <- textmineR::FitLdaModel(dtm = dtm_g04,
                                  k = 5,              # number of topic
                                  iterations = 500,
                                  burnin = 180,
                                  alpha = 0.1,
                                  beta = 0.05,
                                  optimize_alpha = T,
                                  calc_likelihood = T,
                                  calc_coherence = T,
                                  calc_r2 = T)


# analisar a qualidade dos topics
# lda_g04$r2

# plot(lda_g04$log_likelihood,type = "l")

# Tabela dos principais termos
lda_g04$top_terms <- GetTopTerms(phi = lda_g04$phi, M = 15)

lda_g04$top_terms |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 15, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list( list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))
```

Coerência = proximidade das palavas-chave de determinado tópico.

Prevalecência = participação das palavas-chave em todos os documentos. 


```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
# cohrence de cada tópico
# lda_g04$coherence

# Prevalence is the probability of topics distribution in the whole documents
lda_g04$prevalence <- colSums(lda_g04$theta) / sum(lda_g04$theta) * 100
# lda_g04$prevalence

lda_g04$summary <- data.frame(topic = rownames(lda_g04$phi),
                                coherence = round(lda_g04$coherence, 3),
                                prevalence = round(lda_g04$prevalence, 3),
                                top_terms = apply(lda_g04$top_terms, 2, function(x) { paste(x, collapse = ", ")}))

modsum_g04 <- lda_g04$summary %>%
  `rownames<-`(NULL)

modsum_g04 %>% 
    tidyr::pivot_longer(cols = c(coherence, prevalence)) %>%
    ggplot(aes(x = factor(topic, levels = unique(topic)), y = value, group = 1)) +
    geom_point() + 
    geom_line() +
    facet_wrap(~ name, scales = "free_y", nrow = 2) +
    theme_minimal() + 
    labs(title = "Best topics by coherence and prevalence score", 
         subtitle = "Text review with 5 rating", x = "Topics", y = "Value")

# dendograma
# lda_g04$linguistic <- CalcHellingerDist(lda_g04$phi)
# lda_g04$hclust <- hclust(as.dist(lda_g04$linguistic),"ward.D")
# lda_g04$hclust$labels <- paste(lda_g04$hclust$labels, lda_g04$labels[,1])
# plot(lda_g04$hclust)
```

Encontrar os principais trabalhos dentre de cada tópico. Quanto maior o valor de `theta` mais o trabalho pertence a determinado tópico. Lembrando que um trabalho percente a mais de um tópico (soft-clustering).

```{r eval=T, echo=F, fig.height = 7, fig.width = 12, warning=FALSE, error=TRUE, tidy=FALSE, message=FALSE}
theta_g04 <- as.data.frame(lda_g04$theta)  
theta_g04$SR <- rownames(theta_g04)

tibble::as_tibble(theta_g04) |>
    left_join(g04 |> dplyr::select(- text)) |>
    tidyr::pivot_longer(! c(SR, TI), names_to = "Topic", values_to = "theta") |>
        datatable(
        extensions = 'Buttons', 
        rownames = F, 
        options = list(
                    dom = 'Bfrtip', 
                    pageLength = 5, 
                    buttons = list(list(
                                        extend = 'collection', 
                                        buttons = list(list(extend = 'csv', filename = 'data'), 
                                                        list(extend = 'excel', filename = 'data')), 
                                                        text = 'Download'))))

```

### STM

Roberts et al (2015).

* STM - Structural Topic Modeling
    * utiliza os metadados dos documentos para melhorar a classificação
        * data da publicação, revista, etc 
