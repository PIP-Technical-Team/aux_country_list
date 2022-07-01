library(data.table)
pcndir <- "//wbntpcifs/povcalnet/01.PovcalNet/00.Master/02.vintage"

# Get list of files
m_files <- list.files(pcndir,
                      pattern = "Master_2021[0-9]{10}.xlsx")

# Find latest Masterfile
pcn_master_path <- m_files %>%
  gsub("Master_|.xlsx", "", .) %>%
  as.POSIXlt(format = "%Y%m%d%H%M%S") %>%
  max(na.rm = TRUE) %>%
  as.character() %>%
  gsub("-|:| ", "", .) %>%
  sprintf("%s/Master_%s.xlsx", pcndir, .)

# Read CountryList sheet from latest Masterfile
df <- readxl::read_xlsx(pcn_master_path, sheet = "CountryList")
df <- setDT(df)
df <- df[, c("CountryCode", "WBRegionCode")]
names(df) <- c("country_code", "pcn_region_code")


# ---- Read country data from WDI ----

country_list <- wbstats::wb_countries()
setDT(country_list)

country_list <- country_list[region != "Aggregates"]

# Recode names
setnames(
  country_list,
  old = c("iso3c", "region_iso3c", "admin_region_iso3c"),
  new = c("country_code", "region_code", "admin_region_code")
)

# Merge w/ PCN data
country_list <- merge(country_list, df,
                      all.x = TRUE, by = "country_code")

# Keep relevant variables
country_list <-
  country_list[,
               .(
                 country_code,
                 country,
                 region,
                 region_code,
                 admin_region_code,
                 pcn_region_code,
                 income_level,
                 lending_type
               )]

readr::write_csv(country_list, "country_list.csv")


