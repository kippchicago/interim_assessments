##Knit IA reports##
library(googledrive)

##Create Folders
# ia_1920_folder <- googledrive::drive_mkdir("19-20 Interim Assessments", overwrite = T) Only need to do once a year
ia_folder <- googledrive::drive_mkdir("IA #1", ia_1920_folder) #Change to current IA
subject_names <- c("DL", "ELA", "Math", "Science")
subject_folders <- subject_names %>% 
  purrr::map(~googledrive::drive_mkdir(.x, ia_folder))

sub_folders <- list(c("Math", "ELA"), #DL
                    c("3rd Grade", map_chr(c(4:8), ~sprintf("%sth Grade", .x))),#ELA
                    c("3rd Grade", map_chr(c(4:8), ~sprintf("%sth Grade", .x))),#Math
                    map_chr(c(5:8), ~sprintf("%sth Grade", .x))#Sci
)
grade_folder_ls <- list()
make_sub_folders <- function(folder_names, parent_folders){
  for (i in 1:length(folder_names)){
    grade_folder_ls[[i]] <- folder_names[[i]] %>% 
      purrr::map(~googledrive::drive_mkdir(.x, parent_folders[[i]], overwrite = TRUE))
  }
  return(grade_folder_ls)
}

grade_folders <- make_sub_folders(sub_folders, subject_folders)

names(grade_folders) <- c("DL", "ELA", "Math", "Science") 

#Creating DL ELA & Math sub folders by grade level
dl_math <- sub_folders[[2]] %>% 
  purrr::map(~googledrive::drive_mkdir(.x, grade_folders[[1]][[1]], overwrite = TRUE))

dl_ela <- sub_folders[[2]] %>% 
  purrr::map(~googledrive::drive_mkdir(.x, grade_folders[[1]][[2]], overwrite = TRUE))


load(here::here("data/1920_IA_1.Rda"))
ge_list <- regional_school_rollup %>% 
  select(-reg_pct_correct) %>% 
  tidyr::gather(school, pct_correct, -local_assessment_id) %>% 
  mutate(subject = str_extract(local_assessment_id, "ELA|Math|Science"),
         grade = as.integer(str_extract(local_assessment_id, "0\\d"))) %>% 
  filter(!is.na(pct_correct)) %>% 
  select(-c(local_assessment_id,
            pct_correct)) 


render_ge_math_sci <- function(school, subject, grade) {
  file_name <- sprintf("19-20 IA 1 %s %s %i.pdf", 
                       school, 
                       subject, 
                       grade)
  parent_folder_name <- grade_folders[[subject]]
  if (subject == "Science"){
    sub_folder_id <- parent_folder_name[[grade-4]]$id
  } else{
    sub_folder_id <- parent_folder_name[[grade-2]]$id
  }
  
  rmarkdown::render(here("reports/SY 18-19 IA 03-8 Math Report.Rmd"), params = list(
    school = school,
    subject = subject,
    grade = grade
  ),
  output_file = file_name,
  output_dir = here("1920 IA 1"))
  
  drive_upload(here::here(paste0("1920 IA 1/",file_name)), 
               path = as_id(sub_folder_id),
               overwrite = TRUE)
}


ge_list %>% 
  filter(!subject %in% "ELA") %>% 
  pmap(~render_ge_math_sci(..1, ..2, ..3))

render_ge_ela <- function(school, subject, grade) {
  file_name <- sprintf("19-20 IA 1 %s %s %i.pdf", 
                       school, 
                       subject, 
                       grade)
  parent_folder_name <- grade_folders[[subject]]
  sub_folder_id <- parent_folder_name[[grade-2]]$id
  rmarkdown::render(here("/reports/SY 18-19 IA 03-8 ELA Report.Rmd"), params = list(
    school = school,
    subject = subject,
    grade = grade
  ),
  output_file = file_name,
  output_dir = here("1920 IA 1"))
  
  drive_upload(here::here(paste0("1920 IA 1/",file_name)), 
               path = as_id(sub_folder_id),
               overwrite = TRUE)
}

ge_list %>% 
  filter(subject %in% "ELA") %>% 
  pmap(~render_ge_ela(..1, ..2, ..3))

render_dl <- function(test_id, test, teacher, school) {
  subject <- str_extract(test_id, "ELA|Math")
  grade <- as.double(str_extract(test_id, "0\\d"))
  file_name <- sprintf("19-20 DL IA 1 %s %s %s %s.pdf",
                       school,
                       subject,
                       grade,
                       teacher)
  
  sub_folder_id <- if_else(subject == "ELA",
                           dl_ela[[grade-2]]$id,
                           dl_math[[grade-2]]$id)
  rmarkdown::render(here("/reports/SY 19-20 DL IA Report.Rmd"), params = list(
    test_id = test_id,
    test = test,
    teacher = teacher,
    school = school
  ),
  output_file = file_name,
  output_dir = here("1920 IA 1"))
  
  drive_upload(here::here(paste0("1920 IA 1/",file_name)), 
               path = as_id(sub_folder_id),
               overwrite = TRUE)
}
render_dl("IA.1920.07.ELA.1DL", "19-20 G7 ELA Interim Assessment 1 - DL Modified",
          "Asbury", "KAMS")
load(here::here("data/1920_dl_IA_1.Rda"))
dl_list <- regional_school_rollup_sped %>% 
  select(-Region) %>% 
  tidyr::gather(teacher_school, pct_correct, -c(local_assessment_id, title)) %>% 
  filter(!is.na(pct_correct)) %>%
  tidyr::separate(teacher_school, c("teacher", "school")) %>% 
  mutate(school = if_else(is.na(school), teacher, school)) %>% 
  select(-pct_correct)

dl_list %>% 
  pmap(~render_dl(..1, ..2, ..3, ..4))

