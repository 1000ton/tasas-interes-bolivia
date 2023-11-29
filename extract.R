# Programa web scraping de los archivos de tasas de interes de la pagina del BCB
# Descarga los archivo excel

library(rvest)
library(dplyr)

base_uri <- "https://www.bcb.gob.bo/?q=tasas_interes_anuales_efectivas&field_fecha_tasa_iae_value%5Bvalue%5D%5Byear%5D=&page="
page <- 0
repeat {
    uri <- paste0(base_uri, page)
    print(paste0("Scraping url: ", uri))
    html <- try(read_html(uri), silent = TRUE)
    container <- html %>% html_nodes("div.view-content > table > tbody")
    if (length(container) == 0) {
        break
    }
    filenames <- container %>% html_nodes("td:nth-child(1)") %>% html_text2()
    for (i in 1:length(filenames)) {
        print(paste0("Getting ", filenames[i]))
        sel <- paste0("tr:nth-child(", i,
          ") > td.views-field.views-field-field-archivos-semanales-excel")
        href <- container %>%
        html_nodes(sel) %>%
        html_elements("a") %>%
        html_attr("href")
        if (length(href) == 0) {
            print(paste0("Skipping, no file for ", filenames[i]))
        } else {
            download.file(href,
              paste0("db/", filenames[i], ".xlsx"), mode = "wb")
        }
    }
    page <- page + 1
}