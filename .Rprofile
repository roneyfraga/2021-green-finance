source("renv/activate.R")

if (interactive()) {
    suppressMessages(require(colorout))
    conflicted::conflict_prefer("filter", "dplyr")
    conflicted::conflict_prefer("pluck", "purrr")

    # cores do terminal
    setOutputColors256(normal = 39, number = 51, negnum = 183, date = 43, string = 79, const = 75, verbose = FALSE)

    # função para enviar para o R qual a largura do terminal de saída dos dados
    largura <- function() {
        options(width = system("tput cols", intern = TRUE))
    }

}

options(repos = structure(c(CRAN = "https://cran.rstudio.com/")))

options(
        usethis.full_name = "Roney Fraga Souza",
        usethis.description = list(
                                   `Authors@R` = 'person("Roney", "Souza", email = "roneyfraga@gmail.com", role = c("aut", "cre"), 
                                   comment = c(ORCID = "orcid.org/0000-0001-5750-489X"))'
        ),
usethis.protocol  = "ssh"
)

options(browser = "/usr/bin/firefox")
