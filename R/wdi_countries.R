library(data.table)
wdi <-
  wbstats::wb_countries()  |>
  as.data.table() |>
  {
    \(.) {

      # clean data

      iso2 <- grep("_iso2c", names(.), value = TRUE)
      x    <- .[, !..iso2]

      iso3 <- grep("_iso3c", names(x), value = TRUE)

      withiso <-
        gsub("_iso3c", "", iso3) |>
        paste0(collapse = "|") |>
        grep(names(x), value = TRUE)

      tokeep <- c("country", "iso3c", withiso)

      x[region != "Aggregates"
      ][,
        ..tokeep]
    }
  }()


# rename iso3c
owdi <- names(wdi)
nwdi <-
  gsub("iso3c", "code", names(wdi))

setnames(wdi, owdi, nwdi)

fwrite(wdi, "wdi_countries.csv")

