
## Functions PC:

clean.main.pathway <- function(pathway_string) {
  if (is.na(pathway_string) || pathway_string == "NA" || pathway_string == "") {
    return("NA")
  }
  
  components <- str_split(pathway_string, ",\\s*")[[1]]
  
  simplified <- sapply(components, function(item) {
    item <- str_trim(item)
    # Use case-insensitive matching with (?i) and check if pattern exists anywhere in the string
    if (str_detect(item, "(?i)contaminant.*stowaway|transport.*contaminant")) return("Contaminant & Stowaway")
    if (str_detect(item, "(?i)release.*nature")) return("Release")
    if (str_detect(item, "(?i)escape.*confinement")) return("Escape")
    if (str_detect(item, "(?i)corridor")) return("Corridor")
    return(NA)  
  })
  
  simplified <- unique(simplified[!is.na(simplified)])
  
  if (length(simplified) == 0) return("Other")
  
  paste(simplified, collapse = ", ")
}


# subpathways

sub.pathways <- function(pathway_string) {
  if (is.na(pathway_string) || pathway_string == "NA" || str_trim(pathway_string) == "") {
    return("NA")
  }
  
  components <- str_split(pathway_string, ",\\s*")[[1]]
  
  simplified <- sapply(components, function(item) {
    item <- str_trim(item)
    
    if (str_detect(item, "Ballast water|Hull fouling")) return("Shipping")
    if (str_detect(item, "Aquaculture|Fishery|Live food & live bait")) return("Food production")
    if (str_detect(item, "Interconnected waterways")) return("Interconnected waterways")
    if (str_detect(item, "Botanical garden/zoo/aquaria|l garden/zoo/aquaria|Botanica|Pet species|Ornamental")) return("Trade")
        if (str_detect(item, "Research (in facilities)")) return("Research")
        if (str_detect(item, "Transportation of habitat material")) return("Other")
        if (str_detect(item, "Conservation introduction")) return("Other")

    return(NA_character_)
  })
  
  simplified <- unique(na.omit(simplified))
  
  if (length(simplified) == 0) return("Other")
  
  paste(simplified, collapse = ", ")
}

path <- path %>% mutate(Sub.Path = map_chr(Sub.pathways, sub.pathways))

all_sub_pathways <- path$Sub.pathways %>%
  na.omit() %>%
  str_split(",\\s*") %>%
  flatten_chr() %>% str_trim() %>% unique() %>% sort()
