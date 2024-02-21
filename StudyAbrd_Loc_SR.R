# Enrollment: Study Abroad by Location Sound Report

input_name <- "UP_IR_ABRD_SourceData_2226-2234.csv"
input_classes <- "UP_SR_TRAVEL_CLASS_STUDENTS_2226-2234.csv"
output_name <- "Enrl_StudyAbrd_by_Location_output_2226-2234.csv"



library(tidyverse)
library(magrittr)
library(janitor)

# Term codes for report use (one year per run please)
summer <- 2226
fall <- 2228
spring <- 2234

acad_year <- "AY 2022-23"

abrd <- read_csv(paste0("Source Data/", input_name), col_names = TRUE, cols(ID=col_character())) %>% 
	clean_names(., case="parsed") %>% 
	filter(Acad_Load!="N") %$%
	add_column(., Location = 
	                if_else(Study_Agreement=="UPSPACRIM", "Pacific Rim Program",
									if_else(Study_Agreement=="SITHEALTH", "Intl Honors Program/Health & Community",
									if_else(Study_Agreement=="AUGSBURGCA", "CGE Central America", 
									if_else(Study_Agreement=="AUGSBURGSA", "CGE Southern Africa", 
											  Country_Desc))))) %$%
	add_column(., Acad_Year = 
	                if_else(Term==summer|Term==fall|Term==spring, acad_year,
	                        NA_character_)) %$% 
  add_column(., Term_Type = 
               if_else(substr(Term,4,4)==6, "Summer",
                       if_else(substr(Term,4,4)==4, "Spring",
                               if_else(substr(Term,4,4)==8, "Fall", NA_character_)))) %>% 
	left_join(., read_csv("R helper files/Location_to_Region_mapping.csv"), byall=TRUE)
	
# Selecting needed vars into reduced abrd df 
abrd_ <- abrd %>% select(ID, Acad_Year, Term_Type, Term, Region, Location)

# adding class based faculty led students
abrd_classes <- read_csv(paste0("Source Data/", input_classes))


# ----------------------------------------------
# SPOT CHECK: Do any of these look domestic and need to be removed?
View(abrd_classes %>% group_by(Subject, Catalog, Descr...3, Instructors, Term) %>% summarize(n()))

# If no, proceed. If yes, filter from abrd_classes prior to proceeding.
#-----------------------------------------------


abrd_classes_prep <- abrd_classes %>%
   select(ID, Term) %>%
   mutate(ID = as.character(ID),
          Acad_Year = acad_year,
          Term_Type = if_else(substr(Term,4,4)==6, "Summer",
                              if_else(substr(Term,4,4)==4, "Spring",
                                      if_else(substr(Term,4,4)==8, "Fall", NA_character_))),
          Region = "zzFaculty Led",
          Location = "Faculty Led")

abrd_ <- full_join(abrd_, abrd_classes_prep)

# Grouping by ID + Acad_Year + Region + Location, then spreading wide by Term + Term-Type ----
abrd_wide <- abrd_ %>% 
  group_by(ID, Acad_Year, Region, Location) %>% 
  spread(., key = Term_Type, value = Term) %>% 
  select(1:4,7,5:6) %>% 
  ungroup(.)

abrd_wide %<>% 
  mutate(., Summer_only =   if_else(!is.na(Summer) &  is.na(Fall) &  is.na(Spring), 1, 0)) %>% 
  mutate(., Fall_only =     if_else( is.na(Summer) & !is.na(Fall) &  is.na(Spring), 1, 0)) %>% 
  mutate(., Spring_only =   if_else( is.na(Summer) &  is.na(Fall) & !is.na(Spring), 1, 0)) %>% 
  mutate(., Summer_Fall =   if_else(!is.na(Summer) & !is.na(Fall) &  is.na(Spring), 1, 0)) %>%
  mutate(., Fall_Spring =   if_else( is.na(Summer) & !is.na(Fall) & !is.na(Spring), 1, 0)) %>%
  mutate(., Summer_Spring = if_else(!is.na(Summer) &  is.na(Fall) & !is.na(Spring), 1, 0)) %>%
  mutate(., All_terms =     if_else(!is.na(Summer) & !is.na(Fall) & !is.na(Spring), 1, 0))

if_else((abrd_wide %>% filter(All_terms==1) %>% nrow(.)) > 0, 
        "Warning: one or more students studied abroad in all 3 acad terms in the same country. They will be included in Total-Distinct column for that country but not in any of the term-specific columns for that country.",
        "data ok. there were no instances of any students studying abroad in all 3 terms in the same country.")

abrd_wide %<>% 
  mutate(., Total_distinct = 
           if_else(sum(Summer_only,Fall_only,Spring_only,Summer_Fall,Fall_Spring,Summer_Spring,All_terms) > 0, 1, 0))

# Check to ensure mutates executed properly
if_else((abrd_wide %>% filter(Total_distinct==0) %>% nrow(.)) == 0, 
                      "data mutated properly", 
                      "errors in mutate() calculation(s), rows exist with Total_distinct = 0. Total_distinct should = 1 for all rows")


# Grouping and Summarizing for report by Acad Year, Region, Location for each term and pairs of terms ----
abrd_summed <- abrd_wide %>% 
  group_by(Acad_Year, Region, Location) %>% 
  summarise(Summer_only = sum(Summer_only, na.rm = TRUE),
            Fall_only = sum(Fall_only, na.rm = TRUE),
            Spring_only = sum(Spring_only, na.rm = TRUE),
            Summer_Fall = sum(Summer_Fall, na.rm = TRUE),
            Summer_Spring = sum(Summer_Spring, na.rm = TRUE),
            Fall_Spring = sum(Fall_Spring, na.rm = TRUE),
            All_terms = sum(All_terms, na.rm = TRUE),
            Total_distinct = sum(Total_distinct, na.rm = TRUE)) %>% 
  ungroup(.)

# Check to ensure grouping and summarizing executed properly
if_else( (sum(abrd_wide$Summer_only) == sum(abrd_summed$Summer_only))==TRUE &
         (sum(abrd_wide$Fall_only) == sum(abrd_summed$Fall_only))==TRUE &
         (sum(abrd_wide$Spring_only) == sum(abrd_summed$Spring_only))==TRUE &
         (sum(abrd_wide$Summer_Fall) == sum(abrd_summed$Summer_Fall))==TRUE &
         (sum(abrd_wide$Fall_Spring) == sum(abrd_summed$Fall_Spring))==TRUE &
         (sum(abrd_wide$Summer_Spring) == sum(abrd_summed$Summer_Spring))==TRUE &
         (sum(abrd_wide$Total_distinct) == sum(abrd_summed$Total_distinct))==TRUE, 
         "grouping and summarizing executed properly",
         "summing of abrd_wide into abrd_summed did not execute properly")


# Generating unduplicated total of students for each academic year and term/pair of terms ----
  # this is not a simple column sum since it's possible for a student to study abroad in multiple terms & multiple countries

# Grouping and spreading to wide format
abrd_wide_undupe <- abrd_ %>% 
  select(-Region, -Location) %>% 
  group_by(ID, Acad_Year) %>% 
  spread(., key = Term_Type, value = Term) %>% 
  ungroup(.)

# Creating Term vars for report
abrd_wide_undupe %<>% 
  mutate(., Summer_only =   if_else(!is.na(Summer) &  is.na(Fall) &  is.na(Spring), 1, 0)) %>% 
  mutate(., Fall_only =     if_else( is.na(Summer) & !is.na(Fall) &  is.na(Spring), 1, 0)) %>% 
  mutate(., Spring_only =   if_else( is.na(Summer) &  is.na(Fall) & !is.na(Spring), 1, 0)) %>% 
  mutate(., Summer_Fall =   if_else(!is.na(Summer) & !is.na(Fall) &  is.na(Spring), 1, 0)) %>%
  mutate(., Fall_Spring =   if_else( is.na(Summer) & !is.na(Fall) & !is.na(Spring), 1, 0)) %>%
  mutate(., Summer_Spring = if_else(!is.na(Summer) &  is.na(Fall) & !is.na(Spring), 1, 0)) %>%
  mutate(., All_terms =     if_else(!is.na(Summer) & !is.na(Fall) & !is.na(Spring), 1, 0))

if_else((abrd_wide_undupe %>% filter(All_terms==1) %>% nrow(.)) > 0, 
        "Warning: one or more students studied abroad in all 3 acad terms across multiple countries. They will be included in unduplicated total's Total-Distinct column but not in any of the term-specific columns.",
        "data ok. there were no instances of any students studying abroad in all 3 terms across multiple countries.")

abrd_wide_undupe %<>% 
  mutate(., Total_distinct = 
           if_else(sum(Summer_only,Fall_only,Spring_only,Summer_Fall,Fall_Spring,Summer_Spring,All_terms) > 0, 1, 0)) 

# Check to ensure mutates executed properly
if_else((abrd_wide_undupe %>% filter(Total_distinct==0) %>% nrow(.)) == 0, 
                      "data mutated properly", 
                      "errors in mutate() calculation(s), rows exist with Total_distinct = 0. Total_distinct should = 1 for all rows")

# Groups by Acad Year and summarizes to Unduplicated Total row for each year
abrd_undupe <- abrd_wide_undupe %>% 
  group_by(Acad_Year) %>% 
  summarise(Summer_only = sum(Summer_only, na.rm = TRUE),
            Fall_only = sum(Fall_only, na.rm = TRUE),
            Spring_only = sum(Spring_only, na.rm = TRUE),
            Summer_Fall = sum(Summer_Fall, na.rm = TRUE),
            Summer_Spring = sum(Summer_Spring, na.rm = TRUE),
            Fall_Spring = sum(Fall_Spring, na.rm = TRUE),
            All_terms = sum(All_terms, na.rm = TRUE),
            Total_distinct = sum(Total_distinct, na.rm = TRUE)) %>% 
  ungroup(.) %>% 
  mutate(., Location = "Total (Unduplicated)")


# Nests df by Acad_Year then joins Mapping file and unnests ----
  # This provides the same Region-Location rows for each Acad_Year, 
  # even for Reg-Loc that had no students for a given Acad_Year
abrd_x <- 
  abrd_summed %>% 
  group_by(Acad_Year) %>% 
  nest() %>% 
  mutate(., data = map(data,
    .f = function(.x) full_join(.x, read_csv("R helper files/Location_to_Region_mapping.csv"), byall=TRUE))
    ) %>% 
  unnest() %>% 
  ungroup()

# Check to ensure nested joins executed properly
if_else( (sum(abrd_x$Summer_only, na.rm = TRUE) == sum(abrd_summed$Summer_only))==TRUE &
         (sum(abrd_x$Fall_only, na.rm = TRUE) == sum(abrd_summed$Fall_only))==TRUE &
         (sum(abrd_x$Spring_only, na.rm = TRUE) == sum(abrd_summed$Spring_only))==TRUE &
         (sum(abrd_x$Summer_Fall, na.rm = TRUE) == sum(abrd_summed$Summer_Fall))==TRUE &
         (sum(abrd_x$Fall_Spring, na.rm = TRUE) == sum(abrd_summed$Fall_Spring))==TRUE &
         (sum(abrd_x$Summer_Spring, na.rm = TRUE) == sum(abrd_summed$Summer_Spring))==TRUE, 
         "nested joins executed properly",
         "nested joins of abrd_summed into abrd_x did not execute properly")

# Joining Unduplicated Totals (by Acad Year)
abrd_x %<>% 
  full_join(., abrd_undupe, byall=TRUE)

# Arranging then exporting report to CSV
abrd_x %>% 
  arrange(Acad_Year, Region, Location) %>% 
  select(Acad_Year, Region, Location, Summer_only, Fall_only, Spring_only, 
         Summer_Fall, Summer_Spring, Fall_Spring, All_terms, Total_distinct) %$% 
  rename_all(., function(.x) snakecase::to_any_case(.x, case = "parsed", sep_in = "_", sep_out = " ")) %>%
  write_excel_csv(., paste0("Source Data/", output_name), na = "--")


# Not for Sound Report - data export for Snowflake----
   # save(abrd, file = "Source Data/Study-Abroad-SR-data-for-Snowflake_by-Student-Term.RData")
   # save(abrd_wide, file = "Source Data/Study-Abroad-SR-data-for-Snowflake.RData")



### Initial Sound Report code - deprecated ----
#a1 <- 
#list(
## TERM ---
#	full_join( 
#	# TERM - By Location
#			abrd  %>% 
#			group_by(Term, Location) %>% 
#			summarise(N=n()) %>% 
#			spread(., key = Term, value = N), 
#	# TERM - Unduplicated Grand Total
#		abrd  %>% 
#			group_by(Term) %>% 
#			summarise(N=n()) %>% 
#			spread(., key = Term, value = N) %$% 
#			add_column(., Location="Total Undup"), byall=TRUE),
## ACAD YEAR ---
#	full_join(
#	# ACAD YEAR - By Location
#		abrd %>% 
#			select(ID, Acad_Year, Location) %>% 
#			distinct(.) %>% 
#			group_by(Location, Acad_Year) %>% 
#			summarise(N=n()) %>% 
#			spread(., key = Acad_Year, value = N),
#		abrd %>% 
#			select(ID, Acad_Year) %>% 
#			distinct(.) %>% 
#			group_by(Acad_Year) %>% 
#			summarise(N=n()) %>% 
#			spread(., key = Acad_Year, value = N) %$% 
#			add_column(., Location="Total Undup"), byall=TRUE)
#	) %>% 
#
## Joining TERM + ACAD YEAR ---
#Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2, byall=TRUE), .) %>% 
#
## Joining Location-to-Region Mapping ---
#left_join(., read_csv("R helper files/Location_to_Region_mapping.csv"), byall=TRUE) %>% 
#select(24,1:23) %>% 
#	
## REGION Totals ---
#	full_join(.,
#	# Unduplicated Region Totals
#		full_join(
#			abrd  %>% 
#				group_by(Region, Term) %>% 
#				summarise(N=n()) %>% 
#				spread(., key = Term, value = N) %$% 
#				add_column(., Location="Region Total Undup"), 
#			abrd %>% 
#				select(Region, ID, Acad_Year) %>% 
#				distinct(.) %>% 
#				group_by(Region, Acad_Year) %>% 
#				summarise(N=n()) %>% 
#				spread(., key = Acad_Year, value = N) %$% 
#			add_column(., Location="Region Total Undup"), 
#		byall=TRUE), 
#	byall=TRUE) %>% 
#arrange(Region, Location) %>% 
#write_excel_csv(., "Source Data/Enrl_StudyAbrd_by_Location_output.csv", na = "")
#
#
#
#
#	
#	
#	#write_excel_csv(., "Source Data/Enrl_StudyAbrd_by_Location_output.csv", na = "0")
