# ---------------------------------------------------
# Datenextraktion für das Projekt Hygienebrandmelder
# ---------------------------------------------------
install.packages("fhircrackr")
install.packages("data.table")


library(fhircrackr)
library(data.table)

# Basis-URL FHIR Server
base_url <- ""

ssl_cert_path <- "certificate.crt"
#Authentifizierung
auth_user <- ""
args <- commandArgs(trailingOnly = TRUE)

if (length(args)==0) {
        stop("i want a auth pw ")
}
auth_pw <- args[1]
set_config(config(ssl_verifypeer = 0L))


outfile_encounter <- "encounter.csv"
outfile_patient   <- "patient.csv"
outfile_condition <- "condition.csv"
outfile_procedure <- "procedure.csv"

all_patient_ids <- character()

# Design
encounter_design <- fhir_table_description(
  resource = "Encounter",
  cols = c(
    encounter_id   = "id",
    kontaktklasse  = "class/code",
    kontaktebene   = "type/coding/code",
    patient_id     = "subject/reference",
    aufnahmenummer = "identifier/value",
    aufnahmeanlass = "hospitalization/admitSource/coding/code",
    beginndatum    = "period/start",
    enddatum       = "period/end",
    fachabteilung  = "serviceType/coding/code"
  )
)

patient_design <- fhir_table_description(
  resource = "Patient",
  cols = c(
    patient_id  = "id",
    gender      = "gender",
    birth_date  = "birthDate"
  )
)

condition_design <- fhir_table_description(
  resource = "Condition",
  cols = c(
    condition_id        = "id",
    patient_id          = "subject/reference",
    icd_code            = "code/coding/code",
    recordedDate        = "recordedDate"
  )
)

procedure_design <- fhir_table_description(
  resource = "Procedure",
  cols = c(
    procedure_id   = "id",
    patient_id     = "subject/reference",
    prozedur_code  = "code/coding/code",
    datum          = "performedDateTime"
  )
)

# -----------------------------
# Zeitraum & Flags
# -----------------------------
start_year <- 2009
end_year   <- 2025
first_write_encounter <- TRUE
first_write_patient   <- TRUE
first_write_condition <- TRUE
first_write_procedure <- TRUE

# -----------------------------
#Encounter abrufen (quartalsweise)
# -----------------------------
for(year in start_year:end_year){
  quarters <- list(
    c(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-03-31"))),
    c(as.Date(paste0(year,"-04-01")), as.Date(paste0(year,"-06-30"))),
    c(as.Date(paste0(year,"-07-01")), as.Date(paste0(year,"-09-30"))),
    c(as.Date(paste0(year,"-10-01")), as.Date(paste0(year+1,"-12-31")))
  )
  
  for(q in quarters){
    enc_url <- paste0(base_url, "Encounter?",
                      "date=ge", q[1],
                      "&date=lt", q[2],
                      "&class=IMP&_count=200")
    
    bundles <- fhir_search(request=enc_url, username = auth_user,
  password = auth_pw, max_bundles=Inf)
    
    if(length(bundles) > 0){
      df_enc <- fhir_crack(bundles, design=encounter_design)
      
      if(nrow(df_enc) > 0){
        # Datum formatieren
        df_enc$beginndatum <- format(as.Date(df_enc$beginndatum), "%d-%m-%Y")
        df_enc$enddatum    <- format(as.Date(df_enc$enddatum), "%d-%m-%Y")
        
        # Dedup Encounter
        if(file.exists(outfile_encounter)){
          dt_existing <- fread(outfile_encounter, select="encounter_id")
          new_enc <- df_enc[!df_enc$encounter_id %in% dt_existing$encounter_id,]
        } else {
          new_enc <- df_enc
        }
        
        if(nrow(new_enc) > 0){
          all_patient_ids <- c(all_patient_ids, new_enc$patient_id)
          write.table(new_enc, outfile_encounter, sep=",",
                      row.names=FALSE,
                      col.names=first_write_encounter,
                      append=!first_write_encounter)
          first_write_encounter <- FALSE
        }
      }
    }
  }
}

# -----------------------------
# Patienten aus Encounter ziehen
# -----------------------------
patient_ids <- unique(sub("Patient/", "", all_patient_ids))
chunk_size <- 50
split_ids <- split(patient_ids, ceiling(seq_along(patient_ids)/chunk_size))

first_write_patient <- !file.exists(outfile_patient)

for (id_chunk in split_ids) {
  
  # Patienten-Abfrage
  patient_request <- fhir_url(
    url = base_url,
    resource = "Patient",
    parameters = list(`_id` = paste(id_chunk, collapse = ","))
  )
  
  patient_bundles <- fhir_search(request = patient_request,  username = auth_user,
  password = auth_pw, max_bundles = Inf)
  
  if (length(patient_bundles) > 0) {
    df_pat <- fhir_crack(patient_bundles, design = patient_design)
    
    if (nrow(df_pat) > 0) {
      # Duplikate entfernen
      df_pat <- df_pat[!duplicated(df_pat$patient_id), ]
      
      # Bereits gespeicherte Patienten aus der CSV entfernen
      if (file.exists(outfile_patient)) {
        existing_ids <- fread(outfile_patient, select = "patient_id")
        df_pat <- df_pat[!df_pat$patient_id %in% existing_ids$patient_id, ]
      }
      
      # Schreiben, wenn noch neue Patienten da sind
      if (nrow(df_pat) > 0) {
        write.table(df_pat, outfile_patient, sep = ",",
                    row.names = FALSE,
                    col.names = first_write_patient,
                    append = !first_write_patient)
        first_write_patient <- FALSE
      }
    }
  }
}

# -----------------------------
# Conditions pro Patient abrufen
# -----------------------------

condition_chunk_size <- 80
#patient_ids <- patient_df$patient_id
swplit_ids <- split(patient_ids, ceiling(seq_along(patient_ids)/condition_chunk_size))

for(id_chunk in split_ids){
  # Abfrage aller Patienten im Chunk
  condition_request <- fhir_url(
    url = base_url,
    resource = "Condition",
    parameters = list(`subject` = paste(id_chunk, collapse = ","))
  )
  
  condition_bundles <- fhir_search(request=condition_request, username = auth_user,
  password = auth_pw,  max_bundles=Inf)
  
  if(length(condition_bundles) > 0){
    df_cond <- fhir_crack(condition_bundles, design=condition_design)
    
    if(nrow(df_cond) > 0){
      # Duplikate entfernen
      df_cond <- df_cond[!duplicated(df_cond$condition_id), ]
      df_cond$recordedDate <- format(as.Date(df_cond$recordedDate), "%d-%m-%Y")
      write.table(df_cond, outfile_condition, sep=",",
                  row.names=FALSE,
                  col.names=first_write_condition,
                  append=!first_write_condition)
      first_write_condition <- FALSE
    }
  }
}

# -----------------------------
# Procedures pro Patient abrufen
# -----------------------------
procedure_chunk_size <-100
split_ids <- split(patient_ids, ceiling(seq_along(patient_ids)/procedure_chunk_size))

first_write_procedure <- !file.exists(outfile_procedure)
seen_procedure_ids <- character()  # globale Deduplication über alle Chunks

for(id_chunk in split_ids){
  
  # Procedure-Abfrage für alle Patienten im Chunk
  procedure_request <- fhir_url(
    url = base_url,
    resource = "Procedure",
    parameters = list(`subject` = paste(id_chunk, collapse = ","))
  )
  
  procedure_bundles <- fhir_search(request = procedure_request,username = auth_user,
  password = auth_pw, max_bundles = Inf)
  
  if(length(procedure_bundles) > 0){
    df_pro <- fhir_crack(procedure_bundles, design = procedure_design)
    
    if(nrow(df_pro) > 0){
      # Duplikate im Chunk und global entfernen
      df_pro <- df_pro[!duplicated(df_pro$procedure_id), ]
      df_pro <- df_pro[!df_pro$procedure_id %in% seen_procedure_ids, ]
      seen_procedure_ids <- c(seen_procedure_ids, df_pro$procedure_id)
      
      if(nrow(df_pro) > 0){
        df_pro$datum <- format(as.Date(df_pro$datum), "%d-%m-%Y")
        fwrite(df_pro, outfile_procedure, append = !first_write_procedure)
        first_write_procedure <- FALSE
      }
    }
  }
}





