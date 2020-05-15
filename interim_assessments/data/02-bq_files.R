
# ------------------ ## Students Tables ## -------------------------------------

students <- get_illuminate("students", "public") %>%
  select(student_id,
         local_student_id) %>%
  collect()

ps_stus <- get_powerschool("students") %>% 
  drop_fivetran_cols %>%
  select(
    studentid = id,
    first_name,
    last_name,
    grade_level,
    schoolid,
    home_room,
    student_number) %>%
  collect()

enrollment <- get_powerschool("ps_enrollment_all") %>%
  filter(yearid %in% year_ids) %>% 
  select(studentid,
         schoolid,
         entrydate,
         exitdate,
         grade_level) %>% 
  collect()

# ------------------ ## Configurations for Reading in Files ## -----------------

con <- dbplyr::simulate_dbi()

# ------------------------------ ## BQ Assessment Tables ## -----------------------------

assessments_current <- get_illuminate("assessments") %>% 
  filter(dbplyr::build_sql("REGEXP_CONTAINS(local_assessment_id,'IA.[0-9]{4}.(0[3-8]).(ELA|Math|Science).[1-3]')", con = con)) %>% 
  collect()

# NOTE ALGEBRA ISN'T IN HERE!

assessments_current_round <- assessments_current %>%
  filter(str_detect(local_assessment_id, sy_abbreviation),
         str_detect(local_assessment_id, paste0('(ELA|Math|Science)','\\.', ia_round)))

gened_current <- assessments_current %>% 
  filter(!grepl("DL", local_assessment_id))

dl_current <- assessments_current %>% 
  filter(grepl("DL", local_assessment_id))

# ---------------------------- ## Past and Current IAs ## ----------------------

gened_ias <- c(ia_1_1718, 
             ia_2_1718,
             ia_3_1718,
             ia_1_1819_assess_ids,
             ia_2_1819_assess_ids,
             ia_3_1819_assess_ids,
             gened_current$assessment_id)

dl_ids <- c(dl_ia_1_1718,
            dl_ia_2_1718,
            dl_ia_3_1718,
            dl_ia_1_1819,
            dl_ia_2_1819,
            dl_ia_3_1819,
            dl_current$assessment_id)

dl_gen_ed <- c(dl_ids,
               gened_ias)

# ---------------------------- ## BQ Assessment Tables Continued ## ----------------------
assessments <- get_illuminate("assessments") %>% 
  filter(assessment_id %in% dl_gen_ed) %>% 
  collect()


agg_student_responses_table <- get_illuminate("agg_student_responses") %>%
  dplyr::filter(assessment_id %in% dl_gen_ed) %>%
  collect(n = Inf)

agg_student_responses <- agg_student_responses_table %>%
  group_by(student_assessment_id, 
           student_id, 
           assessment_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()

archived_assess <- get_illuminate("students_assessments_archive") %>% 
  collect()

agg_student_responses_standard_table <- get_illuminate("agg_student_responses_standard") %>%
  dplyr::filter(assessment_id %in% dl_gen_ed) %>%
  collect()

agg_student_responses_standard <- agg_student_responses_standard_table %>%
  group_by(student_assessment_id, 
           student_id, 
           assessment_id, 
           standard_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()

standards <- get_illuminate("standards", schema = "standards") %>%
  drop_fivetran_cols() %>%
  collect()




