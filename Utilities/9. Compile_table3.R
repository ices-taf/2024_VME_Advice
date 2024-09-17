################################################
#### Compile Table 4   #########################
################################################

# Current lists of VME indicators and VME habitats in Annex III of EU deepwater access regulations
VMEindic <- c('Black coral','Cup coral','Gorgonian','Soft coral','Sponge','Sea-pen','Stylasterids','Stony coral')
VMEhabs <- c('Bryozoan patches','Cold-water coral reef','Coral Garden','Deep-sea Sponge Aggregations','Mud and sand emergent fauna','Seapen fields','Tube-dwelling anemone aggregations')


# Tabulate habitat observation percentage in each polygon 
habtab <- s_vmedb_sf %>%
            st_drop_geometry() %>%
            filter(HabitatType %in% VMEhabs) %>%
            mutate(All = 1) %>%
            mutate(All = as.numeric(All)) %>%
            relocate(All, .after = EU_closures) %>%
            tidyr::pivot_longer(scen11ol:All,names_to = 'Scenario',values_to = 'PA') %>%
            filter(PA==1) %>%
            group_by(HabitatType,Scenario) %>%
            tally() %>%
            tidyr::spread(Scenario, n) %>%
            ungroup() %>%
            mutate(across(EU_closures:scen23prol,~ round((./All)*100,1)))

nhab <- nrow(habtab)

#habtab[,ncol(habtab)] <- "n/a" # set old S2/O3 to NA
habtab <- habtab[,c(1:4,6,8,10,12,5,7,9,11,13)] # restructure for table

indtab <- s_vmedb_sf %>%
            st_drop_geometry() %>%
            filter(VME_Indicator %in% VMEindic) %>%
            mutate(All = 1) %>%
            mutate(All = as.numeric(All)) %>%
            relocate(All, .after = EU_closures) %>%
            tidyr::pivot_longer(scen11ol:All,names_to = 'Scenario',values_to = 'PA') %>%
            filter(PA==1) %>%
            group_by(VME_Indicator,Scenario) %>%
            tally() %>%
            tidyr::spread(Scenario, n) %>%
            ungroup() %>%
            mutate(across(EU_closures:scen23prol,~ round((./All)*100,1)))
nind <- nrow(indtab)

#indtab[,ncol(indtab)] <- "n/a" # set old S2/O3 to NA
indtab <- indtab[,c(1:4,6,8,10,12,5,7,9,11,13)] # restructure for table

tab4 <-  c('VME Habitats','','','','','','','','','','','','') %>%
          rbind(habtab) %>%
          rbind(c('VME Indicators','','','','','','','','','','','','')) %>%
          rbind(setNames(indtab, names(habtab)))

# # Remove the last column - this option was not included in previous advice
# tab4 <- tab4 %>%
#           dplyr::select(-last_col())

colnames(tab4) <- c("","Total number of records","% in EU Closures",
                    paste("% in Scen. A - ",datacallyear),
                    paste("% in Scen. B - ",datacallyear),
                    paste("% in Scen. C - ",datacallyear),
                    paste("% in Scen. D - ",datacallyear),
                    paste("% in Scen. E - ",datacallyear),
                    paste("% in Scen. A - ",assyearprev),
                    paste("% in Scen. B - ",assyearprev),
                    paste("% in Scen. C - ",assyearprev),
                    paste("% in Scen. D - ",assyearprev),
                    paste("% in Scen. E - ",assyearprev))

