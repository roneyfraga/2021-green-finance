

# write clu or vec files with identification of number of vertices
# *Vertices[n]
# where n is a number of vertices
# x is a vector
writePajek <- function(x, file='file') {
    write.table(x, file = file, row.names = FALSE, quote = FALSE)
    dimx <- length(x)
    f <- readLines(file)
    f[1] <- paste0('*Vertices ', dimx)
    write.table(f, file = file, row.names = FALSE, quote = FALSE, col.names = FALSE)
}

expand.grid.unique <- function(x, y, include.equals = FALSE)
{
    x <- unique(x)
    y <- unique(y)
    g <- function(i)
    {
        z <- setdiff(y, x[seq_len(i - include.equals)])
        if (length(z)) cbind(x[i], z, deparse.level = 0)
    }
    as.data.frame(do.call(rbind, lapply(seq_along(x), g)))
}

get_authors_network <- function(M = M, 
                                m_groups = m_groups, 
                                hubs_full_info = hubs_full_info, 
                                keep_group = 'g01', 
                                internal_citations = 2) {

    m_groups |>
    dplyr::filter(.data$group %in% keep_group) |>
    dplyr::pull(SR) ->
        g0

    M |>
    dplyr::filter(SR %in% g0) |>
    {\(x)(biblioNetwork(x, analysis = "collaboration", network = "authors", sep = ";"))}() ->
    NetMatrix

    igraph::graph_from_adjacency_matrix(NetMatrix) |>
    igraph::simplify() |>
    tidygraph::as_tbl_graph() ->
        net

    M |>
    dplyr::filter(SR %in% g0) |>
    tidyr::separate_rows(AU, sep = ';') |>
    dplyr::count(AU, sort = T, name = 'total_papers') |>
    dplyr::rename(name = AU) ->
        total_papers

    hubs_full_info |>
    dplyr::filter(SR %in% g0) |>
    dplyr::select(AU, ki) |>
    tidyr::separate_rows(AU, sep = ';') |>
    dplyr::group_by(AU) |>
    dplyr::summarise(ki = sum(ki)) |>
    dplyr::arrange(dplyr::desc(ki)) |>
    dplyr::rename(name = AU) ->
        top_cited

    full_join(total_papers, top_cited) |>
    dplyr::mutate(kin = total_papers / ki) |>
    dplyr::mutate(kin = ifelse(kin == Inf, 0, kin)) ->
        tt

    net |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(tt) ->
        net

    net |>
    tidygraph::activate(nodes) |>
    dplyr::filter(ki >= internal_citations) ->
        net2

    V(net2)$group <- igraph::cluster_label_prop(net2)$membership

    net2 |>
    igraph::as_data_frame(what = 'vertices') |>
    dplyr::mutate(label = name, value = ki) |>
    dplyr::mutate(title = paste(paste('Papers ', total_papers, sep = ''), paste('Cited ', ki, sep = ''), sep = '; ')) |>
    dplyr::mutate(title = paste(label, title,  sep = '; ')) |>
    dplyr::rename(id = name) ->
        nodes

    net2 |>
    igraph::as_data_frame(what = 'edges') ->
        edges

    list(nodes = tibble::tibble(nodes), edges = tibble::tibble(edges))

}


# build textcleaner function
textcleaner_lda <- function(x) {

  x <- as.character(x)
  
  x <- x %>%
    stringr::str_to_lower() %>%                                     # convert all the string to low alphabet
    textclean::replace_contraction() %>%                            # replace contraction to their multi-word forms
    # textclean::replace_internet_slang() %>%                       # replace internet slang to normal words
    # textclean::replace_emoji() %>%                                # replace emoji to words
    # textclean::replace_emoticon() %>%                             # replace emoticon to words
    textclean::replace_hash(replacement = "") %>%                   # remove hashtag
    textclean::replace_word_elongation() %>%                        # replace informal writing with known semantic replacements
    textclean::replace_number(remove = T) %>%                       # remove number
    textclean::replace_date(replacement = "") %>%                   # remove date
    textclean::replace_time(replacement = "") %>%                   # remove time
    stringr::str_remove_all(pattern = "[[:punct:]]") %>%            # remove punctuation
    # stringr::str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>%  # remove mixed string n number
    stringr::str_squish() %>%                                       # reduces repeated whitespace inside a string.
    stringr::str_trim()                                             # removes whitespace from start and end of string
  
    return(as.data.frame(x))
}

