library(tidyverse)
library(lubridate)
library(dbplyr)
library(stringr)
library(class)
library(reader)
library(tidytext)
library(tidyr)
library(ROCR)
library(ranger)
library(randomForest) #note that random forest masks importance in ranger

NPBD_OG <- readr::read_csv('OG_Data_Download_4_1_22/NPDB2210.csv')
NPBD <- NPBD_OG #this way if we need to go to the original raw file, we don't have to read in the whole file again
#View(NPBD)
#str(NPBD)

## Filtering our data down to years/reports we want to look at/clean

# Filtering out to only years 2010 and beyond
NPBD <- NPBD %>% filter(ORIGYEAR >= 2010)

# Creating state variable
NPBD  <- NPBD  %>% mutate(STATE = case_when( #changed this to caps
  is.na(WORKSTAT) ~ HOMESTAT,
  !is.na(WORKSTAT) & !is.na(HOMESTAT) ~ WORKSTAT,
  TRUE ~ WORKSTAT),
  STATE = case_when(
    is.na(STATE) ~ LICNSTAT,
    TRUE ~ STATE
  ))
#sum(is.na(NPBD$STATE)) #number of cases that have no data for state after function. and that's for C and P


# Filtering out to just malpractice reports...
NPBD <-NPBD %>% filter(RECTYPE == "P") # Filtering out to just malpractice reports...


#MissingState <- NPBD %>% filter(is.na(NPBD$STATE))
#View(MissingState)
sum(is.na(NPBD$STATE)) #should be 0

# Removing all columns that have all (or a majority are) NAs after filtering
NA.cols <- c("WORKCTRY","HOMECTRY", "AAYEAR", "AACLASS1", "AACLASS2", "AACLASS3", 
             "AACLASS4", "AACLASS5", "BASISCD1", "BASISCD2", "BASISCD3", "BASISCD4",
             "BASISCD5", "AALENTYP", "AALENGTH", "AAEFYEAR", "AASIGYR", 
             "HOMESTAT", "WORKSTAT") # need ALEGATN2 for feature engineering

NPBD <- NPBD %>% select(-all_of(NA.cols))

removept2 <- c("ACCRRPTS", "NPMALRPT", "NPLICRPT", "NPCLPRPT", "NPPSMRPT", 
               "NPDEARPT", "NPEXCRPT","NPGARPT","NPCTMRPT", "FUNDPYMT")

NPBD <- NPBD %>% select(-all_of(removept2))

## Turning variables into factors

To_Factor_List <- c("RECTYPE", "REPTYPE", "LICNSTAT", "LICNFELD", "ALGNNATR", "ALEGATN1",
                    "PAYNUMBR", "PAYTYPE", "PYRRLTNS", "PTGENDER", "PTTYPE", "OUTCOME", "STATE", 
                    "PRACTAGE")

NPBD[To_Factor_List] <- lapply(NPBD[To_Factor_List], factor)

#shows a list of all the factors and all their levels
lapply(NPBD[ , sapply(NPBD, is.factor)], function(f) {
  data.frame(Levels=levels(unique(sort(f))),
             Values=as.numeric(unique(sort(f))))
})

### Releveling some Factor variables
#PYRRLTNS - some of the prior 2004 factors still present
  #combine 1 and P (assuming 1 is for the primary insurer), and 3 and M (since they are both self insured)
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="1"] <- "P"
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="3"] <- "M"
class(NPBD$PYRRLTNS)
#levels(NPBD$PYRRLTNS)
NPBD$PYRRLTNS <- factor(NPBD$PYRRLTNS, levels = c("E", "G", "M", "O", "P", "S"))

levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="E"] <- "insurncCo_excess"
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="G"] <- "insurnc_guaranty_fund"
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="M"] <- "state_mdmp_fund_prmry"
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="O"] <- "state_mdmp_fund_scdry"
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="P"] <- "insurncCo_prmry"
levels(NPBD$PYRRLTNS)[levels(NPBD$PYRRLTNS)=="S"] <- "self_insrd_org"

#PAYNUMBER - makings single payments the lower level
NPBD$PAYNUMBR <- relevel(NPBD$PAYNUMBR, ref = "S")
NPBD$PAYNUMBR <- factor(NPBD$PAYNUMBR, levels = c("S","M"))
#levels(NPBD$PAYNUMBR)

###ALGNNATR
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="1"] <- "diagnosis_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="10"] <- "anesthesia_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="20"] <- "surgery_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="30"] <- "medication_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="40"] <- "ivblood_pdct_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="50"] <- "obs_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="60"] <- "treatment_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="70"] <- "monitoring_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="80"] <- "equip_pdct_rel"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="90"] <- "other_misc"
levels(NPBD$ALGNNATR)[levels(NPBD$ALGNNATR)=="100"] <- "behavhlth_rel"

#OUTCOME
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="1"] <- "emot_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="2"] <- "insignificant_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="3"] <- "minortemp_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="4"] <- "majortemp_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="5"] <- "minorperm_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="6"] <- "significant_perm_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="7"] <- "majorperm_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="8"] <- "quad_brain_lifelongcare_inj"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="9"] <- "death"
levels(NPBD$OUTCOME)[levels(NPBD$OUTCOME)=="10"] <- "unable_tbd"

#REPTYPE
levels(NPBD$REPTYPE)[levels(NPBD$REPTYPE)=="101"] <- "insurnc_mp_indiv"
levels(NPBD$REPTYPE)[levels(NPBD$REPTYPE)=="102"] <- "non-insurnc_mp_indiv"

#PAYTYPE
levels(NPBD$PAYTYPE)[levels(NPBD$PAYTYPE)=="B"] <- "b4_sttlmt"
levels(NPBD$PAYTYPE)[levels(NPBD$PAYTYPE)=="J"] <- "judgement"
levels(NPBD$PAYTYPE)[levels(NPBD$PAYTYPE)=="O"] <- "other"
levels(NPBD$PAYTYPE)[levels(NPBD$PAYTYPE)=="S"] <- "sttlmt"
levels(NPBD$PAYTYPE)[levels(NPBD$PAYTYPE)=="U"] <- "unk_b4_sttlmt"

#PTTYPE
levels(NPBD$PTTYPE)[levels(NPBD$PTTYPE)=="B"] <- "both_in_out"
levels(NPBD$PTTYPE)[levels(NPBD$PTTYPE)=="I"] <- "inpatient"
levels(NPBD$PTTYPE)[levels(NPBD$PTTYPE)=="O"] <- "outpatient"
levels(NPBD$PTTYPE)[levels(NPBD$PTTYPE)=="U"] <- "unk"


#feature engineering - how long a case lasted
NPBD <- NPBD %>% mutate(ADD_YEARS = case_when((MALYEAR1 != MALYEAR2) & (!is.na(MALYEAR2)) ~ (MALYEAR2 - MALYEAR1),
                                                  is.na(MALYEAR2) ~ 0,
                                                    TRUE ~ 0),
                            DURATION = 1 ,
                            DURATION = case_when((MALYEAR1 != MALYEAR2) & (!is.na(MALYEAR2)) ~ (DURATION + (MALYEAR2 - MALYEAR1)),
                                                 TRUE ~ DURATION))

#NPBDtest <- NPBD %>% filter(MALYEAR1 != MALYEAR2) - above formula worked
  #CHECK OUT PRACTNUM 128602

#feature engineering - More than 1 Allegation
NPBD <- NPBD %>% mutate(ADDT_ALLEGNT = ifelse(is.na(ALEGATN2), F, T))

#removing ALEGATN2"
NPBD <- NPBD %>% select(-ALEGATN2)

#Removing $ signs from TOTALPMT and PAYMENT
NPBD <- NPBD %>% 
  mutate(across(c("PAYMENT", "TOTALPMT"), ~gsub("\\$", "", .) %>% as.numeric))

class(NPBD$PAYMENT)

#### - Reclassifying LICENFLED VARS

Code <- c(10, 15, 1365)
OCC1<- as_tibble(Code)
OCC1$FIELD <- c("general_MD")

Code <- c(20, 25)
OCC2<- as_tibble(Code)
OCC2$FIELD <- c("general_DO")

OCC_FULL <- rbind(OCC1,OCC2)

Code <- c(30, 35,  606, 607,
          609,
          612,
          613,
          1362
)
OCC3<- as_tibble(Code)
OCC3$FIELD <- c("dentistry")

OCC_FULL <- rbind(OCC_FULL, OCC3)


Code <- c(50,
          55,
          60,
          70,
          75,
          76,
          1345,
          1346
)
OCC4<- as_tibble(Code)
OCC4$FIELD <- c("pharmacy")


Code <- c(1382,
          1398
)
OCC5<- as_tibble(Code)
OCC5$FIELD <- c("end_of_life")


Code <- c(100,
          110,
          120
          
)
OCC6<- as_tibble(Code)
OCC6$FIELD <- c("Nurse")


Code <- c(130,
          134,
          135,
          141,
          642,
          645
)
OCC7<- as_tibble(Code)
OCC7$FIELD <- c("non_md_practioners")


Code <- c(140,
          142
          
)
OCC8<- as_tibble(Code)
OCC8$FIELD <- c("other_nurse")


Code <- c(148,
          150,
          160,
          165,
          618,
          699,
          175,
          176,
          1353,
          1393
          
)
OCC9<- as_tibble(Code)
OCC9$FIELD <- c("asstmed_caregivers")


Code <- c(170,
          370,
          371,
          372,
          373,
          374
          
)
OCC10<- as_tibble(Code)
OCC10$FIELD <- c("psychology")


Code <- c(200,
          210,
          211
          
)
OCC11<- as_tibble(Code)
OCC11$FIELD <- c("dietians")


Code <- c(240,
          250,
          260,
          270,
          280,
          281,
          1390
          
)
OCC12<- as_tibble(Code)
OCC12$FIELD <- c("emt")


Code <- c(300,
          621,
          651,
          652,
          653,
          654,
          657,
          660,
          661,
          662,
          668,
          1366,
          1383,
          1386,
          1388,
          1303,
          1308,
          1395
          
)
OCC13<- as_tibble(Code)
OCC13$FIELD <- c("mental_health")


Code <- c(350,
          639,
          648,
          649,
          1364
          
)
OCC14<- as_tibble(Code)
OCC14$FIELD <- c("podiatry")


Code <- c(400,
          460,
          470,
          471
          
)
OCC15<- as_tibble(Code)
OCC15$FIELD <- c("hearing")


Code <- c(402,
          665,
          667,
          664
          
)
OCC16<- as_tibble(Code)
OCC16$FIELD <- c("hearing")


Code <- c(405,
          430,
          440,
          1367
          
)
OCC17<- as_tibble(Code)
OCC17$FIELD <- c("pt")


Code <- c(410,
          420,
          450,
          658,
          663,
          666
          
)
OCC18<- as_tibble(Code)
OCC18$FIELD <- c("occp_therapy")


Code <- c(500,
          501,
          502,
          503,
          504,
          505,
          510,
          520,
          530,
          540,
          550,
          551,
          647,
          1397,
          1399
          
)
OCC19<- as_tibble(Code)
OCC19$FIELD <- c("med_tech")


Code <- c(600,
          601,
          603,
          604,
          605,
          1361
          
)
OCC20<- as_tibble(Code)
OCC20$FIELD <- c("licensed_specialist")



Code <- c(615,
          624,
          627
          
)
OCC21<- as_tibble(Code)
OCC21$FIELD <- c("non_licensed_specialist")


Code <- c(630,
          633,
          636,
          637,
          1363
          
)
OCC22<- as_tibble(Code)
OCC22$FIELD <- c("optometry")


Code <- c(755,
          758,
          800,
          1370,
          752,
          759,
          1381,
          1389
)
OCC23<- as_tibble(Code)
OCC23$FIELD <- c("med_admin")


Code <- c(810,
          812,
          820,
          822,
          830,
          840,
          850,
          853,
          1320,
          1331,
          1351,
          1352
          
)
OCC24<- as_tibble(Code)
OCC24$FIELD <- c("biz_ops")


Code <- c(998,
          999,
          899
)
OCC25<- as_tibble(Code)
OCC25$FIELD <- c("uk_solo_pract")


Code <- c(1301,
          1304,
          1391,
          1392
          
)
OCC26<- as_tibble(Code)
OCC26$FIELD <- c("gen_hospital")


Code <- c(1302,
          1307
)
OCC27<- as_tibble(Code)
OCC27$FIELD <- c("psych_facility")


Code <- c(1310
)
OCC28<- as_tibble(Code)
OCC28$FIELD <- c("lab")


Code <- c(1335,
          1336,
          1338
          
)
OCC29<- as_tibble(Code)
OCC29$FIELD <- c("3rd_party_org")


Code <- c(1342,
          1347,
          1348,
          1343,
          1344,
          1349
          
)
OCC30<- as_tibble(Code)
OCC30$FIELD <- c("supplier_manfact")


Code <- c(1394,
          1396
          
)
OCC31<- as_tibble(Code)
OCC31$FIELD <- c("community")


Code <- c(1999,
          9999
          
)
OCC32<- as_tibble(Code)
OCC32$FIELD <- c("other_nonspec_org")


OCC_FULL <- rbind(OCC_FULL, OCC4, OCC5, OCC6, OCC7, OCC8, OCC9, OCC10, OCC11, OCC12, OCC13, OCC14, OCC15, OCC16, OCC17,
                  OCC18, OCC19, OCC20, OCC21, OCC22, OCC23, OCC24, OCC25, OCC26, OCC27, OCC28, OCC29, OCC30, OCC31, OCC32)

OCC_FULL <- OCC_FULL %>%  rename(Code = value)

NPBD$FIELD2 <- OCC_FULL[match(NPBD$LICNFELD, OCC_FULL$Code), "FIELD"]
NPBD$FIELD2$FIELD <-as.factor(NPBD$FIELD2$FIELD)
NPBD_newfield <- NPBD$FIELD2
NPBD_newfield$FIELD <- as.factor(NPBD_newfield$FIELD)

NPBD <-  NPBD %>% select(-FIELD2)
NPBD <- bind_cols(NPBD, NPBD_newfield)
View(NPBD)
#USE sum(is.na(NPBD$FIELD)) to make sure none are missing.
#levels(NPBD$FIELD)
#releveling to follow order of our grouping, which is based on NPBD file
NPBD$FIELD <- factor(NPBD$FIELD, levels = c("general_MD", "general_DO", "dentistry", "pharmacy", "Nurse", "non_md_practioners",
                                            "other_nurse", "asstmed_caregivers", "psychology", "dietians", "emt", "mental_health", "podiatry",
                                            "hearing", "pt", "occp_therapy", "med_tech", "licensed_specialist", "non_licensed_specialist", "optometry"))

NPBD <- NPBD %>% rename(LCNFIELD_GRP = FIELD)
#should be 20 unique factors in data set 

#filtering out low states
NPBD <- NPBD %>% filter(STATE != "AS" & STATE != "GU" & STATE != "MP" & STATE != "AP" & STATE != "VI" & STATE != "AE")

#changing NAs for persons involved in report to 1
NPBD$NUMBPRSN <- ifelse(is.na(NPBD$NUMBPRSN), 1, NPBD$NUMBPRSN)


### Feature Engineering Section ###
## Create variable for counts of previous malpractice payment records (prior to 2010)
historical.data <- NPBD_OG %>% filter(RECTYPE == "P" | RECTYPE == "M") %>% 
  filter(ORIGYEAR < 2010)

c <- count(historical.data, PRACTNUM)

# Adding counts to our working dataset
NPBD <- left_join(NPBD, c, by = "PRACTNUM")
NPBD <- NPBD %>% rename(previous_mp = n)

# Imputing 0s where we have NAs
NPBD$previous_mp <- ifelse(is.na(NPBD$previous_mp), 0, NPBD$previous_mp)


## Create indicator if practitioner also had/has Adverse Action report against them prior to 2010
historical.AAdata <- NPBD_OG %>% filter(RECTYPE == "C" | RECTYPE == "A") %>% 
  filter(ORIGYEAR < 2010)

historical.AAdata <- historical.AAdata %>% distinct(PRACTNUM)
historical.AAdata$AA_indicator <- 1

NPBD <- left_join(NPBD, historical.AAdata, by = "PRACTNUM")

# Imputing 0s where we have NAs
NPBD$AA_indicator <- ifelse(is.na(NPBD$AA_indicator), 0, NPBD$AA_indicator)

## Creating outcome variable for total payments that are greater than half a million
NPBD <- NPBD %>%
  mutate( outcome = (TOTALPMT >= 500000))

## Changing outcome/severity so name isn't confused with the outcome variable
NPBD <- NPBD %>% rename(SEVERITY = OUTCOME)

# Fixing some column names so everything is consistent
NPBD <- NPBD %>% rename(OUTCOME = outcome)
NPBD <- NPBD %>% rename(PREVIOUS_CASES = previous_mp)
NPBD <- NPBD %>% rename(AA_INDICATOR = AA_indicator)
NPBD <- NPBD %>% rename(REPRT_ENTITY = TYPE) #in another life, probably should factor this too, but it comes after the settlement payment probably, so it probably can't be used in our model

NPBD <- NPBD %>% select(-LICNFELD, -MALYEAR2)
NPBD <- NPBD %>% select(-LICNSTAT)
