# Meredith Willmott script for analyzing arthropod biodiversity according to 
#environment for B2 and outside pitfall traps 

##libraries -------


##graphics setup------ 



## read the data into R ------

#replace with your file path on your computer

file_path <- "C:\\Users\\mered\\OneDrive\\Desktop\\B2\\B2_pitfalls.csv"

pitfalls_raw <- readr::read_csv(file_path, show_col_types = FALSE)

## basic data cleaning ----

pitfalls <- pitfalls_raw
names(pitfalls) <- trimws(names(pitfalls))
names(pitfalls) <- gsub("\\s+", "_", names(pitfalls))
names(pitfalls) <- tolower(names(pitfalls))

#trim leading/trailing whitespace and collapse internal spaces
squish <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

pitfalls$environment <- squish(pitfalls$environment)
pitfalls$trap_code <- squish(pitfalls$trap_code)
pitfalls$arthropod_identity <- squish(pitfalls$arthropod_identity)
pitfalls$number_of_arthropods <- as.numeric(pitfalls$number_of_arthropods)
pitfalls$date <- as.Date(pitfalls$date, format = "%m-%d-%Y")


# calculating ant abundance both by trap and by biome 
ants <- pitfalls[
  !is.na(pitfalls$arthropod_identity) &
    grepl("\\.", pitfalls$arthropod_identity) &
    !is.na(pitfalls$number_of_arthropods),
]

ant_by_trap <- aggregate(
  number_of_arthropods ~ date + environment + trap_code,
  data = ants,
  FUN = sum,
  na.rm = TRUE
)

names(ant_by_trap)[names(ant_by_trap) == "number_of_arthropods"] <- "ants_total"

ant_by_trap <- ant_by_trap[order(ant_by_trap$environment, ant_by_trap$date, ant_by_trap$trap_code), ]

ant_by_trap


ant_total_by_biome <- aggregate(
  ants_total ~ environment,
  data = ant_by_trap,
  FUN = sum
)

names(ant_total_by_biome)[2] <- "total_ants"

ant_total_by_biome



