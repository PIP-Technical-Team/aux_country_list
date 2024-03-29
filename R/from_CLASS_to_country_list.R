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
        ..tokeep
        ]
    }
  }()


# rename iso3c
owdi <- names(wdi)
nwdi <-
  gsub("iso3c", "code", names(wdi))

setnames(wdi, owdi, nwdi)


# Add "(excluding high income)" to South Asia
wdi[, admin_region  := fifelse(test = grepl("income", admin_region) | is.na(admin_region),
                               yes  = admin_region ,
                               no   = paste(admin_region , "(excluding high income)"))]

#   ____________________________________________________________________________
#   Read data from CLASS.dta file                                           ####


ind <- fs::path("OutputData/CLASS.dta")

byv <-
  c(
    "code",
    "region_SSA",
    "fcv_current",
    "region_pip")


dt <- haven::read_dta(ind) |>
  as.data.table() |>
  unique(by = byv) |>
  (\(.){.[, ..byv]})()  # select just these variables


dt_o <- names(dt)
dt_n <- gsub("_current", "", dt_o)

setnames(dt, dt_o, dt_n)
setnames(dt,
         old = c("region_SSA", "region_pip"),
         new = c("africa_split_code", "pip_region_code"))

#   ____________________________________________________________________________
#   Merge wdi and CLASS                                                     ####


rg <-
joyn::merge(dt, wdi,
            by = "code",
            match_type = "1:1",
            reportvar = FALSE)


#   ____________________________________________________________________________
#   Clean Data                                                              ####

# PIP region

rg[, pip_region := fifelse(pip_region_code == "OHI",
                           yes = "Other High Income Countries",
                           no  = region)
]



# East and West Africa

rg[,
   africa_split :=  fcase(
     africa_split_code == "", "",
     africa_split_code == "AFE", "Eastern and Southern Africa",
     africa_split_code == "AFW", "Western and Central Africa",
     default = "")
   ][,
     africa_split_code := fifelse(test = africa_split_code == "",
                                  yes  = "",
                                  no   =   africa_split_code)
   ]

# Fragile countries

rg[,
   fcv_code := fifelse(fcv == "Yes", "FCVT", "FCVF")
   ][,
     fcv := fifelse(fcv == "Yes", "Fragile", "Not-fragile")]

## Admin regions

rg[,
   admin_region_code := fifelse(
     admin_region_code == "" | is.na(admin_region_code),
     NA_character_,
     paste0(admin_region_code, "-AD"))]


# Add PCN region temporarilly

rg[,
   `:=`(
       pcn_region = pip_region,
       pcn_region_code = pip_region_code
     )]

# ff <- copy(rg)
# rg <- copy(ff)

# Convert empty strings to NA
vars <- names(rg)
names(vars) <- vars
rg[, (vars) := lapply(.SD,
                      \(x) {
                        fifelse(x == "" | is.na(x), NA_character_, x)
                        }
                      )
   ]


# fix "Not classified"
# ff <- copy(rg)

rg <- copy(ff)

# not_class <- function(x) {
#   y <- deparse(substitute(x))
#   fifelse(test = grepl("classified", x),
#           paste(x, "by",  y),
#           x)
# }
#
# rg[, (vars) := lapply(.SD,not_class), .SDcols = vars]
#
#
# rg[, (vars) := lapply(.SD,
#                       \(x){
#                         y <- deparse(substitute(x))
#                         # y <- ..x
#                         fifelse(test = grepl("classified", x),
#                                 paste(x, "by",  y),
#                                 x)
#                         })]
#
#
# rg[lending_type_code == "LNX", unique(lending_type)]
#



rg[, lending_type := fifelse(grepl("classified", lending_type),
                             paste(lending_type, "by", "lending type"),
                             lending_type)]



rg[, income_level := fifelse(grepl("classified", income_level),
                             paste(income_level, "by", "income level"),
                             income_level)]


# Create the World

rg[, `:=`(
    world      = "World",
    world_code =  "WLD"
  )]



# janitor::tabyl(rg, region_code, admin_region_code)
# janitor::tabyl(rg, region, admin_region)
# janitor::tabyl(rg, region, pip_region)

#   ____________________________________________________________________________
#   Clean and Save                                                    ####


rg[,
   c( "region_code", "region") := NULL]


setnames(x = rg,
         old = c("code", "country", "pip_region", "pip_region_code"),
         new = c("country_code", "country_name", "region", "region_code") )



## Order columns alphabetically ------------
varn <- names(rg)
setcolorder(rg, sort(varn))
setcolorder(rg, c("country_code", "country_name"))


## Remove  categoeries that we don't need ---------

rm_agg <- c("fcv", "income_level", "lending_type", "admin_region")

to_rm <-
  rm_agg |>
  paste0("_code") |>
  c(rm_agg)

rg[, (to_rm) := NULL]


fwrite(rg, "country_list.csv")


