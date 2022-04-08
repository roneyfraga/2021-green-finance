
renv::settings$ignored.packages("nvimcom")
install.packages('devtools')
devtools::install("~/.vim/plugged/Nvim-R/R/nvimcom", force = T)
devtools::install_github('jalvesaq/colorout', force = T)
install.packages("languageserver")
install.packages("conflicted")
