# Programa para extraer para trasnformar la informacion de archivos excel
# en formato RData


library(readxl)
library(dplyr)
library(lubridate)

getDataFrameACT <- function(year, row, fecha, categoria, categoria1, filename, sheet) {
    entidad <- as.character(row[1])

    mn_empresarial <- as.numeric(row[2])
    mn_pyme <- as.numeric(row[3])
    mn_micro_credito <- as.numeric(row[4])
    mn_consumo <- as.numeric(row[5])
    mn_vivienda <- as.numeric(row[6])

    me_empresarial <- as.numeric(row[7])
    me_pyme <- as.numeric(row[8])
    me_micro_credito <- as.numeric(row[9])
    me_consumo <- as.numeric(row[10])
    me_vivienda <- as.numeric(row[11])

    fechas <- parse_fecha(fecha)
    day <- fechas[1]
    month <- fechas[2]
    start_date <- mdy(paste0(month, "-", day, "-", year))
    end_date <- start_date + days(6)

    data.frame(
        "start_date" = start_date,
        "end_date" = end_date,
        "day" = day,
        "month" = month,
        "year" = year,
        "fecha" = fecha,
        "categoria" = categoria,
        "categoria1" = categoria1,
        "entidad" = entidad,
        "mn_empresarial" = mn_empresarial,
        "mn_pyme" = mn_pyme,
        "mn_micro_credito" = mn_micro_credito,
        "mn_consumo" = mn_consumo,
        "mn_vivienda" = mn_vivienda,
        "me_empresarial" = me_empresarial,
        "me_pyme" = me_pyme,
        "me_micro_credito" = me_micro_credito,
        "me_consumo" = me_consumo,
        "me_vivienda" = me_vivienda,

        "file" = filename,
        "sheet" = sheet
    )
}

getDataFramePAS <- function(year, row, fecha, categoria, categoria1, filename, sheet) {
    entidad <- as.character(row[1])

    mn_caja_ahorro <- as.numeric(row[2])
    mn_dpf_30 <- as.numeric(row[3])
    mn_dpf_60 <- as.numeric(row[4])
    mn_dpf_90 <- as.numeric(row[5])
    mn_dpf_180 <- as.numeric(row[6])
    mn_dpf_360 <- as.numeric(row[7])
    mn_dpf_720 <- as.numeric(row[8])
    mn_dpf_1080 <- as.numeric(row[9])
    mn_dpf_mayor_1080 <- as.numeric(row[10])

    me_caja_ahorro <- as.numeric(row[11])
    me_dpf_30 <- as.numeric(row[12])
    me_dpf_60 <- as.numeric(row[13])
    me_dpf_90 <- as.numeric(row[14])
    me_dpf_180 <- as.numeric(row[15])
    me_dpf_360 <- as.numeric(row[16])
    me_dpf_720 <- as.numeric(row[17])
    me_dpf_1080 <- as.numeric(row[18])
    me_dpf_mayor_1080 <- as.numeric(row[19])

    fechas <- parse_fecha(fecha)
    day <- fechas[1]
    month <- fechas[2]
    start_date <- mdy(paste0(month, "-", day, "-", year))
    end_date <- start_date + days(6)

    data.frame(
        "start_date" = start_date,
        "end_date" = end_date,
        "day" = day,
        "month" = month,
        "year" = year,
        "fecha" = fecha,
        "categoria" = categoria,
        "categoria1" = categoria1,
        "entidad" = entidad,

        "mn_caja_ahorro" = mn_caja_ahorro,
        "mn_dpf_30" = mn_dpf_30,
        "mn_dpf_60" = mn_dpf_60,
        "mn_dpf_90" = mn_dpf_90,
        "mn_dpf_180" = mn_dpf_180,
        "mn_dpf_360" = mn_dpf_360,
        "mn_dpf_720" = mn_dpf_720,
        "mn_dpf_1080" = mn_dpf_1080,
        "mn_dpf_mayor_1080" = mn_dpf_mayor_1080,

        "me_caja_ahorro" = me_caja_ahorro,
        "me_dpf_30" = me_dpf_30,
        "me_dpf_60" = me_dpf_60,
        "me_dpf_90" = me_dpf_90,
        "me_dpf_180" = me_dpf_180,
        "me_dpf_360" = me_dpf_360,
        "me_dpf_720" = me_dpf_720,
        "me_dpf_1080" = me_dpf_1080,
        "me_dpf_mayor_1080" = me_dpf_mayor_1080,

        "file" = filename,
        "sheet" = sheet
    )
}


parse_fecha <- function(fecha) {
    s <- tolower(gsub("\\s{2,}", " ", fecha))
    month <- 0
    dat <- as.numeric(substring(s, 12, 13))
    if (grepl("enero", s)) month <- 1
    if (grepl("febrero", s)) month <- 2
    if (grepl("marzo", s)) month <- 3
    if (grepl("abril", s)) month <- 4
    if (grepl("mayo", s)) month <- 5
    if (grepl("junio", s)) month <- 6
    if (grepl("julio", s)) month <- 7
    if (grepl("agosto", s)) month <- 8
    if (grepl("septiembre", s)) month <- 9
    if (grepl("octubre", s)) month <- 10
    if (grepl("noviembre", s)) month <- 11
    if (grepl("diciembre", s)) month <- 12
    c(dat, month)
}

process_act <- function(df, year, filename, sheet) {
    data <- NULL
    step <- 1
    for (i in 1:nrow(df)) {
        row <- df[i, ]
        col1 <- as.character(row[1])
        col2 <- as.character(row[2])

        if (!is.na(col1) && i > 10 && startsWith(tolower(col1), "tasas")) {
            break
        }

        if (step == 1 && !is.na(col2) && startsWith(col2, "Semana")) {
            fecha <- col2
            step <- 2
            next
        }
        if (step == 2 && !is.na(col1) &&
          (startsWith(col1, "BANCOS MÚLTIPLES") || startsWith(col1, "BANCOS MULTIPLES"))) {
            categoria <- col1
            categoria1 <- col1
            step <- 3
            next
        }

        if (step == 3 && !is.na(col1)) {
            if (startsWith(col1, "ENTIDADES ESPECIALIZADAS EN MICROFINANZAS")) {
                step <- 4
                categoria <- col1
                next
            }
            data <- rbind(data, getDataFrameACT(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 4 && !is.na(col1) && (startsWith(col1, "BANCOS MÚLTIPLES") || startsWith(col1, "BANCOS MULTIPLES"))) {
            categoria1 <- col1
            step <- 5
            next
        }

        if (step == 5 && !is.na(col1)) {
            if (startsWith(col1, "BANCOS PYME")) {
                step <- 6
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFrameACT(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 6 && !is.na(col1)) {
            if (startsWith(col1, "ENTIDADES FINANCIERAS DE VIVIENDA")
                || startsWith(col1, "MUTUALES")) {
                step <- 7
                categoria <- col1
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFrameACT(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 7 && !is.na(col1)) {
            if (startsWith(col1, "COOPERATIVAS")) {
                step <- 8
                categoria <- col1
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFrameACT(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 8 && !is.na(col1)) {
            if (startsWith(col1, "INSTITUCIONES FINANCIERAS DE DESARROLLO")) {
                step <- 9
                categoria <- col1
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFrameACT(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 9 && !is.na(col1)) {
            data <- rbind(data, getDataFrameACT(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }
    }
    print(paste0("Total records: ", nrow(data)))
    data
}

process_pas <- function(df, year, filename, sheet) {
    data <- NULL
    step <- 1
    for (i in 1:nrow(df)) {
        row <- df[i, ]
        col1 <- as.character(row[1])
        col2 <- as.character(row[2])

        if (!is.na(col1) && i > 10 && startsWith(tolower(col1), "tasas")) {
            break
        }

        if (step == 1 && !is.na(col2) && startsWith(col2, "Semana")) {
            fecha <- col2
            step <- 2
            next
        }
        if (step == 2 && !is.na(col1) &&
          (startsWith(col1, "BANCOS MÚLTIPLES") || startsWith(col1, "BANCOS MULTIPLES"))) {
            categoria <- col1
            categoria1 <- col1
            step <- 3
            next
        }

        if (step == 3 && !is.na(col1)) {
            if (startsWith(col1, "ENTIDADES ESPECIALIZADAS EN MICROFINANZAS")) {
                step <- 4
                categoria <- col1
                next
            }
            data <- rbind(data, getDataFramePAS(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 4 && !is.na(col1) && (startsWith(col1, "BANCOS MÚLTIPLES") || startsWith(col1, "BANCOS MULTIPLES"))) {
            categoria1 <- col1
            step <- 5
            next
        }

        if (step == 5 && !is.na(col1)) {
            if (startsWith(col1, "BANCOS PYME")) {
                step <- 6
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFramePAS(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 6 && !is.na(col1)) {
            if (startsWith(col1, "ENTIDADES FINANCIERAS DE VIVIENDA")
                || startsWith(col1, "MUTUALES")) {
                step <- 7
                categoria <- col1
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFramePAS(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 7 && !is.na(col1)) {
            if (startsWith(col1, "COOPERATIVAS")) {
                step <- 8
                categoria <- col1
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFramePAS(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 8 && !is.na(col1)) {
            if (startsWith(col1, "INSTITUCIONES FINANCIERAS DE DESARROLLO")) {
                step <- 9
                categoria <- col1
                categoria1 <- col1
                next
            }
            data <- rbind(data, getDataFramePAS(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }

        if (step == 9 && !is.na(col1)) {
            data <- rbind(data, getDataFramePAS(
                year, row, fecha, categoria, categoria1, filename, sheet))
            next
        }
    }
    print(paste0("Total records: ", nrow(data)))
    data
}

db_tasa_activa <- NULL
db_tasa_pasiva <- NULL
years <- list("2023", "2022", "2021", "2020", "2019",
  "2018", "2017", "2016", "2015")

for (year in years) {
    files <- dir(paste0("db/", year), full.names = TRUE)
    for (i in 1:length(files)) {
        filename <- files[i]
        print(paste0("Parsing file: ", filename))
        for (sheet in excel_sheets(filename)) {
            print(paste0(" sheet: ", sheet))
            suppressMessages({
                data <- read_excel(filename, sheet = sheet)
            })
            if (startsWith(sheet, "ACT")) {
                db_tasa_activa <- rbind(
                    db_tasa_activa, process_act(data, year, filename, sheet))
            }
            if (startsWith(sheet, "PAS")) {
                db_tasa_pasiva <- rbind(
                    db_tasa_pasiva, process_pas(data, year, filename, sheet))
            }
        }
    }
}

print(paste0("Grand Total db_tasa_activa = ", nrow(db_tasa_activa)))
save(db_tasa_activa, file = "db_tasa_activa.RData")
print(paste0("Grand Total db_tasa_pasiva = ", nrow(db_tasa_pasiva)))
save(db_tasa_pasiva, file = "db_tasa_pasiva.RData")