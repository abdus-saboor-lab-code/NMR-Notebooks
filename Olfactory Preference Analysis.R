library(rhdf5)
library(tidyverse)

parent_dir <- "/Users/Simon/Desktop/Olfactory preference test - SLEAP analysis/"
files <- list.files(parent_dir, pattern = "*.h5")

nmr_ids <- c("009-895-085", "600-585-804")
body_part_to_track <- 4 #centroid

for (nmr_id in 1:length(nmr_ids)) {
  nmr_id_files <- files[which(grepl(nmr_ids[nmr_id], files) == TRUE)]
  
  baseline_parameters <- H5Fopen(paste0(parent_dir, nmr_id_files[1]))
  baseline_tracking <- H5Fopen(paste0(parent_dir, nmr_id_files[3]))
  trim_parameters <- H5Fopen(paste0(parent_dir, nmr_id_files[2]))
  trim_tracking <- H5Fopen(paste0(parent_dir, nmr_id_files[4]))
  
  baseline_parameters_tracks <- baseline_parameters$tracks[,,,1]
  baseline_tracking_tracks <- baseline_tracking$tracks[,body_part_to_track,,1]
  
  trim_parameters_tracks <- trim_parameters$tracks[,,,1]
  trim_tracking_tracks <- trim_tracking$tracks[,body_part_to_track,,1]
  
  baseline_tracking_tracks <- as.data.frame(baseline_tracking_tracks) %>%
    mutate("In Left Chamber?" = ifelse(V1 > baseline_parameters_tracks[1,1] &
                                         V1 < baseline_parameters_tracks[2,1] &
                                         V2 > baseline_parameters_tracks[1,2] &
                                         V2 < baseline_parameters_tracks[3,2], yes = TRUE, FALSE)) %>%
    mutate("In Right Chamber?" = ifelse(V1 > baseline_parameters_tracks[5,1] &
                                         V1 < baseline_parameters_tracks[6,1] &
                                         V2 > baseline_parameters_tracks[5,2] &
                                         V2 < baseline_parameters_tracks[7,2], yes = TRUE, FALSE))
  
  trim_tracking_tracks <- as.data.frame(trim_tracking_tracks) %>%
    mutate("In Left Chamber?" = ifelse(V1 > trim_parameters_tracks[1,1] &
                                         V1 < trim_parameters_tracks[2,1] &
                                         V2 > trim_parameters_tracks[1,2] &
                                         V2 < trim_parameters_tracks[3,2], yes = TRUE, FALSE)) %>%
    mutate("In Right Chamber?" = ifelse(V1 > trim_parameters_tracks[5,1] &
                                          V1 < trim_parameters_tracks[6,1] &
                                          V2 > trim_parameters_tracks[5,2] &
                                          V2 < trim_parameters_tracks[7,2], yes = TRUE, FALSE))
  
  summary_df <- data.frame(matrix(nrow = 0, ncol = 4))
  names(summary_df) <- c("NMR_ID", "Condition", "Time in Left Chamber", "Time in Right Chamber")
  
  summary_df <- rbind(summary_df, list("NMR_ID" = nmr_ids[nmr_id], "Condition" = "Baseline",
                                       "Time.in.Left.Chamber" = sum(baseline_tracking_tracks$`In Left Chamber?`, na.rm = TRUE),
                                       "Time.in.Right.Chamber" = sum(baseline_tracking_tracks$`In Right Chamber?`, na.rm = TRUE)))
  
  summary_df <- rbind(summary_df, list("NMR_ID" = nmr_ids[nmr_id], "Condition" = "Trimmed",
                                       "Time.in.Left.Chamber" = sum(trim_tracking_tracks$`In Left Chamber?`, na.rm = TRUE),
                                       "Time.in.Right.Chamber" = sum(trim_tracking_tracks$`In Right Chamber?`, na.rm = TRUE)))
  
  write.csv(summary_df, paste0("/Users/Simon/Desktop/Olf_Pref/summary_data_", nmr_ids[nmr_id], ".csv"))
  write.csv(baseline_tracking_tracks, paste0("/Users/Simon/Desktop/Olf_Pref/baseline_tracking_", nmr_ids[nmr_id], ".csv"))
  write.csv(trim_tracking_tracks, paste0("/Users/Simon/Desktop/Olf_Pref/baseline_tracking_", nmr_ids[nmr_id], ".csv"))
  

}
