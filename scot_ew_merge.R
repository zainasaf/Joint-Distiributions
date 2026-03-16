f <- list.files(recursive=TRUE, pattern = "scot.*\\.csv", full.names = TRUE)
f <- f[1:11]

recode_map <- list()
recode_map[["education"]] <- function(x){
  out <- dplyr::case_when(
    x == "No qualifications" ~ 
      "Incomplete Secondary Education (Below GCSE /O Level)",
    x == "Lower school qualifications" ~ 
      "Secondary Education Completed (GCSE /O Level /CSE or equivalent)",
    x == "Upper school qualifications" ~ 
      "Secondary Education Completed (A Level or equivalent)",
    grepl("Apprentice|Further", x) ~ 
      "Vocational or Technical Qualifications Completed (e.g. HND, NVQ)",
    grepl("Degree level", x) ~ 
      "University Education Completed (First Degree e.g. BA, BSc)",
    TRUE ~ NA_character_)
  out = factor(out, levels = c(
    "Incomplete Secondary Education (Below GCSE /O Level)",
    "Secondary Education Completed (GCSE /O Level /CSE or equivalent)",
    "Secondary Education Completed (A Level or equivalent)",
    "Vocational or Technical Qualifications Completed (e.g. HND, NVQ)",
    "University Education Completed (First Degree e.g. BA, BSc)"
  ))
  out
}
recode_map[["age_group"]] <- function(x){
  out <- dplyr::case_when(
    x %in% c("Aged 18 to 19", "Aged 20 to 24") ~ "18-24",
    x %in% c("Aged 25 to 29", "Aged 30 to 34") ~ "25-34",
    x %in% c("Aged 35 to 39", "Aged 40 to 44") ~ "35-44",
    x %in% c("Aged 45 to 49", "Aged 50 to 54") ~ "45-54",
    x %in% c("Aged 55 to 59", "Aged 60 to 64") ~ "55-64",
    x %in% c("Aged 65 to 69", "Aged 70 to 74") ~ "65-74",
    x %in% c("Aged 75 to 79", "Aged 80 and over") ~ "75+",
    TRUE ~ NA_character_)
  out <- as.factor(out)
  out
}

recode_map[["ethnicity8"]] <- recode_map[["ethnicity6"]] <- 
  function(x){
    out <- dplyr::case_when(
      grepl("White", x) ~ "White",
      grepl("African|Caribbean", x) ~ "Black",
      grepl("Asian", x) ~ "Asian",
      grepl("Mixed", x) ~ "Mixed", 
      grepl("Other", x) ~ "Other", 
      TRUE ~ NA_character_)
    out <- factor(out, levels=c("White", "Black", "Asian", "Mixed", "Other"))
    out
  }

recode_map[["ctry_birth"]] <- function(x){
  out <- dplyr::case_when(
    grepl("United Kingdom", x) ~ "Born in UK",
    grepl("Total", x) ~ NA_character_,
    TRUE ~ "Born Elsewhere") 
  out <- factor(out, levels=c("Born in UK", "Born Elsewhere"))
  out
}
recode_map[["religion12"]] <- function(x){
  out <- dplyr::case_when(
    grepl("Catholic", x) ~ "Christian",
    grepl("Church of Scotland", x) ~ "Christian",
    grepl("Other Christian", x) ~ "Christian",
    grepl("No religion", x) ~ "No religion",
    grepl("Buddhist", x) ~ "Buddhist",
    grepl("Hindu", x) ~ "Hindu",
    grepl("Jewish", x) ~ "Jewish",
    grepl("Muslim", x) ~ "Muslim",
    grepl("Sikh", x) ~ "Sikh",
    x %in% c("Pagan", "Other religion", "Religion not stated") ~ "Other religion", 
    TRUE ~ NA_character_)
  out <- factor(out, levels=c("No religion", "Christian", "Buddhist", "Hindy",
                              "Jewish", "Muslim", "Sikh", "Other religion"))
  out
}

recode_map[["socio_econ"]] <- function(x){
  out <- dplyr::case_when(
    grepl("Full-time students", x) ~ "Full time education/Student",
    grepl("Higher managerial", x) ~ "Higher managerial/ professional/ administrative ",
    grepl("Intermediate occupations", x) ~ "Intermediate managerial/ professional/ administrative",
    grepl("Lower managerial", x) ~ "Supervisory or clerical/ junior managerial/ professional/ administrative",
    grepl("Lower supervisory", x) ~ "Supervisory or clerical/ junior managerial/ professional/ administrative",
    grepl("Never worked|long-term unemployed", x) ~ NA_character_,
    grepl("Routine occupations", x) ~ "Semi or unskilled manual work",
    grepl("Semi-routine occupations", x) ~ "Skilled manual work",
    grepl("Small employers and own account workers", x) ~ NA_character_,
    TRUE ~ NA_character_)
  out <- factor(out, levels=c(
    "Semi or unskilled manual work",
    "Skilled manual work",
    "Supervisory or clerical/ junior managerial/ professional/ administrative",
    "Intermediate managerial/ professional/ administrative",
    "Higher managerial/ professional/ administrative ",
    "Full time education/Student"
  ))
  out
}


dlist <- list()
for(i in 1:11){
  if(i %in% c(1:2, 4, 6:9, 11)){
    dat <- readr::read_csv(f[i])
    v1 <- names(dat)[1]
    v2n <- as.character(dat[1,2])
    v3n <- as.character(dat[1,3])
    v2 <- names(dat)[2]
    v3 <- names(dat)[3]
    v4 <- names(dat)[4]
    vlast <- names(dat)[length(names(dat))]
    names(dat)[2:3] <- c(v2n, v3n)
    dlist[[i]] <-  dat %>%
      select(-1) %>% 
      fill(all_of(v2n), .direction = "down") %>%
      filter(!is.na(!!sym(v3n))) %>%
      filter(!if_all(c(!!sym(v4):!!sym(vlast)), is.na)) %>%
      mutate(across(c(!!sym(v4):!!sym(vlast)), as.numeric)) %>% 
      filter(!`Age - 20 groups, all` %in% c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 14", "Aged 15", "Aged 16 to 17")) %>% 
      pivot_longer(
        cols = c(!!sym(v4):!!sym(vlast)),
        names_to = v1,
        values_to = "n"
      ) 
    
  }else{
    dat <- readr::read_csv(f[i])
    v1 <- names(dat)[1]
    v2n <- as.character(dat[1,2])
    v2 <- names(dat)[2]
    v4 <- names(dat)[3]
    vlast <- names(dat)[length(names(dat))]
    names(dat)[2] <- v2n
    dlist[[i]] <-  dat %>%
      select(-1) %>% 
      filter(!if_all(c(!!sym(v4):!!sym(vlast)), is.na)) %>%
      mutate(across(c(!!sym(v4):!!sym(vlast)), as.numeric)) %>% 
      filter(!`Age - 20 groups, all` %in% c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 14", "Aged 15", "Aged 16 to 17")) %>% 
      pivot_longer(
        cols = c(!!sym(v4):!!sym(vlast)),
        names_to = v1,
        values_to = "n"
      ) 
  }
}


old_names <- unique(c(unlist(sapply(dlist, names))))
new_names <- c("age_group", "education", "ethnicity8", "n", "sex", "ctry_birth", "religion12", "ethnicity6", "socio_econ")
names(old_names) <- new_names
dlist <- lapply(dlist, \(x)setNames(x, names(old_names)[match(names(x), old_names)]))

dlist2 <- dlist
for(i in seq_along(dlist2)){
  nms <- names(dlist2[[i]])
  nms <- intersect(nms, names(recode_map))
  for(n in nms){
    dlist2[[i]][[n]] <- recode_map[[n]](dlist2[[i]][[n]])
  }
  dlist2[[i]] <- na.omit(dlist2[[i]])
}


f <- list.files(recursive=TRUE, pattern = "ew.*\\.csv", full.names = TRUE)
# f <- f[1:11]

young <- c("Aged 2 years and under", "Aged 3 to 4 years",      "Aged 5 to 7 years",
           "Aged 8 to 9 years",      "Aged 10 to 14 years",    "Aged 15 years",         
           "Aged 16 to 17 years", "Aged 15 years and under")


recode_map <- list()
recode_map[["education"]] <- function(x){
  out <- dplyr::case_when(
    x == "No qualifications" ~ 
      "Incomplete Secondary Education (Below GCSE /O Level)",
    grepl("^Level 1", x) ~ 
      "Secondary Education Completed (GCSE /O Level /CSE or equivalent)",
    grepl("^Level 2", x) ~ 
      "Secondary Education Completed (GCSE /O Level /CSE or equivalent)",
    x == "^Level 3" ~ 
      "Secondary Education Completed (A Level or equivalent)",
    grepl("^Other", x) ~ 
      "Vocational or Technical Qualifications Completed (e.g. HND, NVQ)",
    grepl("^Level 4", x) ~ 
      "University Education Completed (First Degree e.g. BA, BSc)",
    TRUE ~ NA_character_)
  out = factor(out, levels = c(
    "Incomplete Secondary Education (Below GCSE /O Level)",
    "Secondary Education Completed (GCSE /O Level /CSE or equivalent)",
    "Secondary Education Completed (A Level or equivalent)",
    "Vocational or Technical Qualifications Completed (e.g. HND, NVQ)",
    "University Education Completed (First Degree e.g. BA, BSc)"
  ))
  out
}
recode_map[["age_group"]] <- function(x){
  out <- dplyr::case_when(
    x %in% c("Aged 18 to 19 years", "Aged 20 to 24 years") ~ "18-24",
    x %in% c("Aged 25 to 29 years", "Aged 30 to 34 years") ~ "25-34",
    x %in% c("Aged 35 to 39 years", "Aged 40 to 44 years") ~ "35-44",
    x %in% c("Aged 45 to 49 years", "Aged 50 to 54 years") ~ "45-54",
    x %in% c("Aged 55 to 59 years", "Aged 60 to 64 years") ~ "55-64",
    x %in% c("Aged 65 years", "Aged 66 to 69 years", "Aged 70 to 74 years") ~ "65-74",
    x %in% c("Aged 75 to 79 years", "Aged 80 to 84 years", "Aged 85 years and over") ~ "75+",
    TRUE ~ NA_character_)
  out <- as.factor(out)
  out
}

recode_map[["ethnicity6"]] <- recode_map[["ethnicity6"]] <- 
  function(x){
    out <- dplyr::case_when(
      grepl("White", x) ~ "White",
      grepl("Black", x) ~ "Black",
      grepl("Asian", x) ~ "Asian",
      grepl("Mixed", x) ~ "Mixed", 
      grepl("Other", x) ~ "Other", 
      TRUE ~ NA_character_)
    out <- factor(out, levels=c("White", "Black", "Asian", "Mixed", "Other"))
    out
  }

recode_map[["ctry_birth"]] <- function(x){
  out <- dplyr::case_when(
    grepl("United Kingdom", x) ~ "Born in UK",
    grepl("Does not apply", x) ~ NA_character_,
    TRUE ~ "Born Elsewhere") 
  out <- factor(out, levels=c("Born in UK", "Born Elsewhere"))
  out
}
recode_map[["religion12"]] <- function(x){
  out <- dplyr::case_when(
    grepl("Christian", x) ~ "Christian",
    grepl("No religion", x) ~ "No religion",
    grepl("Buddhist", x) ~ "Buddhist",
    grepl("Hindu", x) ~ "Hindu",
    grepl("Jewish", x) ~ "Jewish",
    grepl("Muslim", x) ~ "Muslim",
    grepl("Sikh", x) ~ "Sikh",
    x %in% c("Other religion") ~ "Other religion", 
    TRUE ~ NA_character_)
  out <- factor(out, levels=c("No religion", "Christian", "Buddhist", "Hindy",
                              "Jewish", "Muslim", "Sikh", "Other religion"))
  out
}

recode_map[["socio_econ"]] <- function(x){
  out <- dplyr::case_when(
    grepl("L15", x) ~ "Full time education/Student",
    grepl("^L1,", x) ~ "Higher managerial/ professional/ administrative ",
    grepl("^L7", x) ~ "Intermediate managerial/ professional/ administrative",
    grepl("^L4", x) ~ "Supervisory or clerical/ junior managerial/ professional/ administrative",
    grepl("^L10", x) ~ "Supervisory or clerical/ junior managerial/ professional/ administrative",
    grepl("^L14", x) ~ NA_character_,
    grepl("L13", x) ~ "Semi or unskilled manual work",
    grepl("L12", x) ~ "Skilled manual work",
    grepl("L8", x) ~ NA_character_,
    TRUE ~ NA_character_)
  out <- factor(out, levels=c(
    "Semi or unskilled manual work",
    "Skilled manual work",
    "Supervisory or clerical/ junior managerial/ professional/ administrative",
    "Intermediate managerial/ professional/ administrative",
    "Higher managerial/ professional/ administrative ",
    "Full time education/Student"
  ))
  out
}

recode_map[["welshlang"]] <- function(x){
  out <- dplyr::case_when(
    x == "Does not apply" ~ NA_character_, 
    TRUE ~ x
  )
  out
}

ew_dlist <- list()
for(i in 1:12){
  dat <- readr::read_csv(f[i])
  nms <- names(dat)
  nms <- nms[-c(1, length(nms))]
  ew_dlist[[i]] <-  dat %>%
   ## select(-1)
    select(-1, -ends_with("Code"), -any_of(c("Countries", "England and Wales"))) %>% 
    filter(!`Age (23 categories)` %in% young) %>% 
    rename(n="Observation")
}

old_names <- unique(c(unlist(sapply(ew_dlist, names))))
new_names <- c("ethnicity6", "age_group", "education", "n", "sex", "ctry_birth", "religion", "socio_econ", "region", "welshlang")
names(old_names) <- new_names
ew_dlist <- lapply(ew_dlist, \(x)setNames(x, names(old_names)[match(names(x), old_names)]))


ew_dlist2 <- ew_dlist
for(i in seq_along(ew_dlist2)){
  nms <- names(ew_dlist2[[i]])
  nms <- intersect(nms, names(recode_map))
  for(n in nms){
    ew_dlist2[[i]][[n]] <- recode_map[[n]](ew_dlist2[[i]][[n]])
  }
  ew_dlist2[[i]] <- na.omit(ew_dlist2[[i]])
}

dlist2 <- lapply(dlist2, \(x){
  names(x) <- gsub("[0-9]*", "", names(x))
  x
})

ew_dlist2 <- lapply(ew_dlist2, \(x){
  names(x) <- gsub("[0-9]*", "", names(x))
  x
})

tmp <- bind_rows(ew_dlist2[[1]] %>% mutate(nation = "England & Wales"), 
                 dlist2[[1]] %>% mutate(nation = "Scotland"))
vbls <- list(
  c("age_group", "ethnicity", "degree"), 
  c("age_group", "sex", "education"), 
  c("age_group", "ctry_birth"), 
  c("religion", "ctry_birth"), 
  c("age_group", "ethnicity"),
  c("age_group", "ethnicity", "education"), 
  c("ethnicity", "age_group", "socio_econ"), 
  c("ethnicity", "education"), 
  c("sex", "ethnicity"), 
  c("region", "socio_econ"), 
  c("religion", "education"),
  c("age_group", "welshlang")
)  




both_list <- lapply(seq_along(dlist2), \(i){
 tmp <- bind_rows(ew_dlist2[[i]] %>% mutate(nation = "England & Wales"), 
           dlist2[[i]] %>% mutate(nation = "Scotland")) 
 if(i == 1){
   tmp <- tmp %>% 
     rename(degree=education) %>% 
     mutate(degree = ifelse(grepl("Degree", degree), "Degree", "No Degree"))
 } 
 tmp %>% 
  group_by(across(all_of(vbls[[i]]))) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(percent = n/sum(n)*100)
})

both_list[[12]] <- ew_dlist2[[12]] %>% 
  group_by(across(all_of(vbls[[12]]))) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(percent = n/sum(n)*100)


new_f <- gsub("ew_", "both_", f)
for(i in seq_along(new_f)){
  readr::write_csv(both_list[[i]], new_f[i])
}


