library(data.table)


#   ____________________________________________________________________________
#   Read Data from WDI                                                      ####

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


#   ____________________________________________________________________________
#   Read data from CLASS.dta file                                           ####


ind <- fs::path("OutputData/CLASS.dta")

byv <-
  c(
    "code",
    "region_SSA",
    "fcv_current" )


dt <- haven::read_dta(ind) |>
  as.data.table() |>
  unique(by = byv) |>
  (\(.){.[, ..byv]})()  # select just these variables


dt_o <- names(dt)
dt_n <- gsub("_current", "", dt_o)

setnames(dt, dt_o, dt_n)
setnames(dt, "region_SSA", "ewa")

#   ____________________________________________________________________________
#   Merge wdi and CLASS                                                     ####


rg <-
joyn::merge(dt, wdi,
            by = "code",
            match_type = "1:1",
            reportvar = FALSE)


#   ____________________________________________________________________________
#   Clean Data                                                              ####

# East and West Africa

rg[,
   ewa_code := fifelse(ewa == "",
                       admin_region,
                       ewa)
   ][,
     ewa := fcase(
       ewa == "", region,
       ewa == "AFE", "Eastern and Southern Africa",
       ewa == "AFW", "Wetern and Central Africa",
       default = ""
     )]

# Fragile countries

rg[,
   fcv_code := fifelse(fcv == "Yes", "FCVT", "FCVF")
   ][,
     fcv := fifelse(fcv == "Yes", "Fragile", "Not-fragile")]


setnames(rg, c("code", "country"), c("country_code", "country_name") )


# Order columns alphabetically
varn  <- names(rg)[!names(rg) %in% c("country_code", "country_name")] |>
  sort() |>
  {\(.) c("country_code", "country_name", .)}()

setcolorder(rg, varn)

fwrite(rg, "country_list.csv")

