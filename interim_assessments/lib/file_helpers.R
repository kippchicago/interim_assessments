
#----------------------- ## Functions to Help Read and Clean Data from BQ ## ---------------------

# Drop _fivetran column that's in most BQ tables
drop_fivetran_cols <- function(data){
  data %>% 
    select(-starts_with("_fivetran"))
}

# Clean BQ column names
clean_bq_col_names <- function(.data) {
  old_names <- names(.data)
  new_names <- old_names %>% str_replace("^.{10}_", "")
  names(.data) <- new_names
  
  # return
  .data
}

# Creates list of the year ids tables should be filtered on

year_id_list_function <- function(end_year) {
  
  yearid_begin_range <- calc_ps_termid(2017)   
  # 2017 as beginning is not going to change
  
  yearid_end_range <- calc_ps_termid(end_year)
  # when range should end, input should be current_first_year 
  
  range <- (yearid_end_range - yearid_begin_range)/100
  
  year_ids_full <- c(yearid_begin_range)
  
  for (i in 1:range) {
    year_ids_full[length(year_ids_full) + 1] <- yearid_begin_range + (i*100)
  }
  
  year_ids <- year_ids_full / 100
  
  return(year_ids)
  
}

#----------------------- ## Functions to Help Read and Clean Data from Illuminate ## ---------------------

full_name_subj <-  function(abbrv){
  full_name <- if_else(abbrv %in% "SCI", "Science", abbrv)
  full_name
}


#----------------------- ## Functions to Help Read and Clean Data from Powerschool ## ---------------------

drop_duplicate_enrollments <- function(s_y){
  assessment_stus %>% 
    filter(sy == s_y) %>% 
    select(-schoolid,
           -grade_level) %>% 
    left_join(enrollment,
              by = c("ps_studentid" = "studentid")) %>% 
    mutate(keep = date_taken >= entrydate & 
             date_taken < exitdate) %>% 
    filter(keep)
}  



