options("warn"=0) # default
rm(list = ls()) # empty work space
closeAllConnections()

library(xlsx)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(plyr)
library(gridExtra)
library(RColorBrewer)
library(colorRamps)

# copy paste in the cmd prompt to start PK-Sim in developer mode:
#  cd "C:\Program Files\Open Systems Pharmacology\PK-Sim 11.2"
#  PKSim /dev 


# Load auxiliary functions and specify the working directory + database
workingDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingDir)

source(file.path("Auxiliary functions","JSONauxiliaryFunctionsInVitroDb.R"))

sharePoint <- getwd()
obsDataFile <- "Input in vitro database/In vitro db template.xlsx"  # file name

#### Load database ####
studies    <- read.xlsx(obsDataFile, sheetName='Studies' , startRow=2)
df         <- read.xlsx(obsDataFile, sheetName='ReleaseProfiles' , startRow=2)
MW         <- read.xlsx(obsDataFile, sheetName='Analyte' , startRow=1)
proj       <- read.xlsx(obsDataFile, sheetName='Projects' , startRow=1)
solubility <- read.xlsx(obsDataFile, sheetName='Solubility' , startRow=2)
released   <- df

# remove white space (left and right) and NA's
studies <- data.frame(apply(studies,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
df      <- data.frame(apply(df,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
MW      <- data.frame(apply(MW,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
proj    <- data.frame(apply(proj,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)

# Create output directory
if (!dir.exists(file.path(getwd(),"OutputDbPlots"))){
  dir.create(file.path(getwd(),"OutputDbPlots"))
}

### Which compound included in the vitro database do you want to plot? ###
print(unique(studies$Compound)) # Select from console
compound <- "Dipyridamole"
outputPath <- paste0(workingDir,"/OutputDbPlots")
fileName <-  paste0(compound,"_plots",".pdf")

#############################################
##### GENERATE PLOTS ########################
#############################################

{
  # remove white space (left and right) & convert relevant columns to numeric
  studies <- data.frame(apply(studies,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
  cols.num.st <- c("Trans.Rate","Dose..mg.","Medium.V.S1..ml.","Medium.V.S2..ml.")
  studies[cols.num.st] <- sapply(studies[cols.num.st],as.numeric)
  
  solubility <- data.frame(apply(solubility,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
  cols.num.sol <- c("Med.S","S.Var","LOQ.D")
  solubility[cols.num.sol] <- sapply(solubility[cols.num.sol],as.numeric)
  
  released <- data.frame(apply(released,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
  cols.num.rel <- c("Time","Released","Variability")
  released[cols.num.rel] <- sapply(released[cols.num.rel],as.numeric)
  
  
  # Create unit conversion data-frame
  join_studies <- c("Study.ID","Dose..mg.","Formulation","Replicates","Medium.V.S1..ml.",
                    "Medium.V.S2..ml.","Trans.Rate","Trans.Unit","Medium","RPM")
  ToMergeSt <- subset(studies, select =  join_studies)
  join_solubility <- c("Study.ID","Sample.name","Medium","Med.S","S.Unit","S.Var","S.Var.Unit","LOQ.D","LOQ.D.Unit")
  ToMergeSol <- subset(solubility, select =  join_solubility)
  
  drops <- c("Remarks","In.projects.")
  df.plot <- data.frame(released)
  df.plot <- df.plot[, !(names(df.plot) %in% drops)]
  df.plot <- (merge(df.plot, ToMergeSt, by = "Study.ID"))
  df.plot <- (merge(df.plot, ToMergeSol, by = "Study.ID", all.x=T))
  df.plot$Trans.Unit <- replace_na(df.plot$Trans.Unit, "No")
  df.plot$S.Unit <- replace_na(df.plot$S.Unit, "mg/l")
  df.plot$Med.S <- replace_na(df.plot$Med.S, 0)
  
  sapply(df.plot, class)
  
  
  # Run functions returning Time in min, giving medium volume at Time point and determine drug release + solubility in in mg/l
  df.plot <- minute.f(df.plot)
  df.plot <- current.volume.f(df.plot)
  df.plot <- factor.f(df.plot)
  df.plot <- solubility.f(df.plot)
  df.plot$Concentration.mg.l <- df.plot$factor*df.plot$Released
  df.plot$Variability.mg.l<- df.plot$factor*df.plot$Variability
  
  
  # Prepare df.plot for plotting
  cpd.df <- subset(df.plot, Compound == compound)
  cpd.df <- group.f(cpd.df)
  
  # Create plots
  cpd.plots <- dlply(cpd.df , c("Study.ID") , function(x) plot(plot.f(x)))
  overlay.plots <- dlply(cpd.df , c("Group") , function(x) plot(overlay.plot.f(x)))
  plots <- append(cpd.plots,overlay.plots)
  
  # Save plots as .pdf
  # fileName <-  paste0(compound,"_plots",".pdf")
  ggsave(filename = paste0(outputPath,"/",fileName), 
         plot = marrangeGrob(plots, nrow=1, ncol=1), 
         width = 15, height = 9)
}



#############################################
##### ANALYZE PLOTS #########################
#############################################

# Open with Acrobat Reader
browseURL(paste0(outputPath,"/",fileName))


#############################################
##### BUILD JSON FILES ######################
#############################################

# remove NAs
studies[is.na(studies)] <- ""
df[is.na(df)] <- ""
MW[is.na(MW)] <- ""
proj[is.na(proj)] <- ""

# create vector with unique ID's
ids <- unique(df$Study.ID)
PROJECTS <- unique(trimws(strsplit(paste(proj$Projects[is.element(proj$Study.ID,ids)],collapse = ","),split = ",", fixed = TRUE)[[1]],which = "both"))
mapProjToIDs <- lapply(strsplit(paste(proj$Projects[is.element(proj$Study.ID,ids)]),split = ",", fixed = TRUE),trimws,"both")
names(mapProjToIDs) <- proj$Study.ID[is.element(proj$Study.ID,ids)]

# Create sub directories
if (!dir.exists(file.path(getwd(),"buildingBlocks"))){
  dir.create(file.path(getwd(),"buildingBlocks"))
}


# Run conversion
{
  BB_all <- data.frame(stringsAsFactors = FALSE)
  eachID_collector <- NULL
  
  for(eachProject in PROJECTS[PROJECTS!=""]) {
    
    counter <- 0
    BB <- data.frame(stringsAsFactors = FALSE)
    idsInProject <- as.numeric(names(mapProjToIDs[unlist(lapply(mapProjToIDs,FUN = function(x) any(is.element(x,eachProject))))]))
    
    for(eachID in idsInProject){
      counter <- counter+1
      SELECT_study <- studies$Study.ID == eachID
      SELECT_df    <- df$Study.ID == eachID
      SELECT_projects <- proj$Study.ID == eachID
      if(!(!any(studies$Study[SELECT_study] != c(df$Study[SELECT_df][1],proj$Study[SELECT_projects])) &
           !any(studies$Reference[SELECT_study] != c(df$Reference[SELECT_df][1],proj$Reference[SELECT_projects])) &
           !any(studies$Sample.name[SELECT_study] != c(df$Sample.name[SELECT_df][1],proj$Sample.name[SELECT_projects])) &
           !any(studies$Compound[SELECT_study] != c(df$Compound[SELECT_df][1],proj$Compound[SELECT_projects])))){
        stop("Inconsistency over sheets. No matching for ",eachID)
      }
      
      # get x and y
      rawAvgUnit <- unique(tolower(df$Rel.Unit[SELECT_df & df$Released!=""]))
      if(length(rawAvgUnit)>1) stop("Multiple Avg units found within one data series for ",eachID)
      rawAvgType <- unique(tolower(df$Avg.type[SELECT_df & df$Released!=""]))
      if(length(rawAvgType)>1) stop("Multiple Avg types found within one data series for ",eachID)
      rawVarUnit <- unique(tolower(df$Var.Unit[SELECT_df & df$Variability!=""]))
      if(length(rawVarUnit)>1) stop("Multiple Var units found within one data series for ",eachID)
      rawVarType <- unique(tolower(df$Var.type[SELECT_df  & df$Variability!=""]))
      if(length(rawVarType)>1) stop("Multiple Var types found within one data series for ",eachID)
      
      avgColumn <- getAverage(avgValue=df$Released[SELECT_df],avgUnit=rawAvgUnit,avgType=rawAvgType,timeValue=df$Time[SELECT_df],timeUnit=df$Time.unit[SELECT_df],
                              CurStage=df$Current.stage[SELECT_df],VolOne=studies$Medium.V.S1..ml.[SELECT_study],VolTwo=studies$Medium.V.S2..ml.[SELECT_study],
                              DoseMg=studies$Dose..mg.[SELECT_study],TrRate=studies$Trans.Rate[SELECT_study],TrUnit=studies$Trans.Unit[SELECT_study],eachID)
 
      timeColumn <- getTime(timeValue = df$Time[SELECT_df],timeUnit = df$Time.unit[SELECT_df][1],avgValue = df$Released[SELECT_df],eachID)
      
      varColumn <- getVariance(varValue = df$Variability[SELECT_df],varUnit =rawVarUnit,varType = rawVarType,avgValue = df$Released[SELECT_df],avgUnit = rawAvgUnit,
                               timeValue = df$Time[SELECT_df],timeUnit=df$Time.unit[SELECT_df],n = as.numeric(studies$Replicates[SELECT_study]),
                               CurStage=df$Current.stage[SELECT_df],VolOne=studies$Medium.V.S1..ml.[SELECT_study],VolTwo=studies$Medium.V.S2..ml.[SELECT_study],
                               DoseMg=studies$Dose..mg.[SELECT_study],TrRate=studies$Trans.Rate[SELECT_study],TrUnit=studies$Trans.Unit[SELECT_study],eachID)
      
      if(tolower(studies$Data.type[SELECT_study])=="aggregated"){
        dataType <- "aggregated"
        dataTypeNaming <- paste0("agg.",ifelse(as.character(studies$Replicates[SELECT_study])!="",paste0(" (n=",as.character(studies$Replicates[SELECT_study]),")"),""))
      } else if(tolower(studies$Data.type[SELECT_study])=="typical"){
        dataType <- "typical representative"
        dataTypeNaming <- paste0("typical",ifelse(as.character(studies$Replicates[SELECT_study])!="",paste0(" (n=",as.character(studies$Replicates[SELECT_study]),")"),""))
      } else if(tolower(studies$Data.type[SELECT_study])=="individual"){
        dataType <- "individual"
        dataTypeNaming <- "indiv."
      } else {
        stop("Unknown data type (neither aggregated nor individual) specified for ",eachID)
      }
      
      datasetName <- paste(studies$Study.ID[SELECT_study],
                           ifelse(studies$No.of.stages[SELECT_study] == "1","One-Stage",
                                  ifelse(studies$No.of.stages[SELECT_study] == "2" & studies$Trans.Unit[SELECT_study] == "","Two-Stage",
                                         ifelse(studies$No.of.stages[SELECT_study] == "2" & studies$Trans.Unit[SELECT_study] != "","Transfer","Unknown"))),
                           studies$Medium[SELECT_study],
                           paste0(studies$Medium.V.S1..ml.[SELECT_study],
                                  ifelse(studies$Medium.V.S2..ml.[SELECT_study]!="",paste0("/",studies$Medium.V.S2..ml.[SELECT_study]),"")," mL"),
                           paste0(studies$Dose..mg.[SELECT_study]," mg"),
                           studies$Batch[SELECT_study],
                           studies$Compound[SELECT_study],
                           paste0(studies$RPM[SELECT_study]," RPM"),
                           studies$Study[SELECT_study],
                           sep=" - ")
      
      # Name
      {
        BB[counter,"Name"] <- datasetName
      }
      # ExtendedProperties
      {
        extProp <- as.data.frame(matrix(c("DB Version","IN VITRO DATABASE",
                                          "ID",as.character(studies$Study.ID[SELECT_study]),
                                          "Study Id",studies$Study[SELECT_study],
                                          "Reference",as.character(studies$Reference[SELECT_study]),
                                          "Source",studies$Source[SELECT_study],
                                          "Prandial state", ifelse(grepl("fa",studies$Medium[SELECT_study], ignore.case = T),"Fasted",
                                                            ifelse(grepl("fe",studies$Medium[SELECT_study], ignore.case = T),"Fed","Other")),
                                          "Grouping", ifelse(studies$No.of.stages[SELECT_study] == "1","One-Stage",
                                                      ifelse(studies$No.of.stages[SELECT_study] == "2" & studies$Trans.Unit[SELECT_study] == "","Two-Stage",
                                                      ifelse(studies$No.of.stages[SELECT_study] == "2" & studies$Trans.Unit[SELECT_study] != "","Transfer","Unknown"))),
                                          "Data type", dataType,
                                          "N",ifelse(as.character(studies$Replicates[SELECT_study])!="",as.character(studies$Replicates[SELECT_study]),"n/a"),
                                          "Molecule",studies$Compound[SELECT_study],
                                          "Top container","Organism",
                                          "Compartment","Dissolved",
                                          "Dose",paste0(studies$Dose..mg.[SELECT_study]," mg"),
                                          "Formulation",ifelse(studies$Formulation[SELECT_study]!="",studies$Formulation[SELECT_study],"."),
                                          "Medium",studies$Medium[SELECT_study],
                                          "Final target pH",studies$pH[SELECT_study],
                                          "Volume one",studies$Medium.V.S1..ml.[SELECT_study],
                                          "Volume two",ifelse(studies$Medium.V.S2..ml.[SELECT_study]!="",studies$Medium.V.S2..ml.[SELECT_study],"0"),
                                          "Transfer rate", ifelse(studies$Trans.Rate[SELECT_study]!="",studies$Trans.Rate[SELECT_study],"."),
                                          "Transfer unit", ifelse(studies$Trans.Unit[SELECT_study]!="",studies$Trans.Unit[SELECT_study],"."),
                                          "Comment",varColumn$comment),
                                        ncol=2,byrow = TRUE),stringsAsFactors = FALSE)
        colnames(extProp) <- c("Name","Value")
        BB$ExtendedProperties[[counter]] <- extProp
      }
      
      # Columns
      {
        BB$Columns[[counter]] <- data.frame(stringsAsFactors = FALSE)
        # Name
        {
          BB$Columns[[counter]][1,"Name"] <- "Avg"
        }
        # QuantityInfo
        {
          BB$Columns[[counter]]$QuantityInfo <- data.frame(
            Name="Avg",
            Path=paste(datasetName,
                       "ObservedData",
                       "Organism",
                       "Dissolved",
                       studies$Compound[SELECT_study],
                       avgColumn$type,sep="|"),
            stringsAsFactors = FALSE)
        }
        # DataInfo
        {
          curMolWeight <- as.numeric(MW[tolower(MW[,1])==tolower(BB$ExtendedProperties[[counter]]$Value[BB$ExtendedProperties[[counter]]$Name == "Molecule"]),3])
          if(length(curMolWeight)!=1){
            stop("Molecular weight not specified for ",BB$ExtendedProperties[[counter]]$Value[BB$ExtendedProperties[[counter]]$Name == "Molecule"])
          }
          BB$Columns[[counter]]$DataInfo <- data.frame(
            Origin="Observation",
            AuxiliaryType=getAuxiliaryType(avgColumn$type,eachID),
            Source="",
            MolWeight=curMolWeight,
            stringsAsFactors = FALSE)
        }
        # Dimension
        {
          BB$Columns[[counter]]$Dimension <- avgColumn$dim
        }
        # Unit
        {
          BB$Columns[[counter]]$Unit <- avgColumn$unit
        }
        # Values
        {
          if(any(is.na(avgColumn$value))) avgColumn$value[is.na(avgColumn$value)] <- "NaN"
          BB$Columns[[counter]]$Values <- list(I(avgColumn$value))
        }
        # RelatedColumns
        {
          if(varColumn$dim!=""){
            BB$Columns[[counter]]$RelatedColumns[[1]] <- list()
            BB$Columns[[counter]]$RelatedColumns[[1]] <- data.frame(stringsAsFactors = FALSE)
            # Name
            {
              BB$Columns[[counter]]$RelatedColumns[[1]][1,"Name"] <- "Var"
            }
            # QuantityInfo
            {
              BB$Columns[[counter]]$RelatedColumns[[1]]$QuantityInfo <- data.frame(
                Name="Var",
                Path=paste(datasetName,
                           "ObservedData",
                           "Organism",
                           "Dissolved",
                           studies$Compound[SELECT_study],
                           varColumn$type,sep="|"),
                stringsAsFactors = FALSE)
            }
            # DataInfo
            {
              BB$Columns[[counter]]$RelatedColumns[[1]]$DataInfo <- data.frame(
                Origin="ObservationAuxiliary",
                AuxiliaryType=getAuxiliaryType(varColumn$type,eachID),
                Source="",
                MolWeight=curMolWeight,
                stringsAsFactors = FALSE)
            }
            # Dimension
            {
              BB$Columns[[counter]]$RelatedColumns[[1]]$Dimension <- varColumn$dim
            }
            # Unit
            {
              if(varColumn$unit!=""){
                BB$Columns[[counter]]$RelatedColumns[[1]]$Unit <- varColumn$unit
              }
            }
            # Values
            {
              if(any(is.na(varColumn$value))) varColumn$value[is.na(varColumn$value)] <- "NaN"
              BB$Columns[[counter]]$RelatedColumns[[1]]$Values <- list(I(varColumn$value))
            }
          }
        }
      }
      # BaseGrid
      {
        BB$BaseGrid[[counter]] <- list()
        # Name
        {
          #BB$BaseGrid[[counter]][1,"Name"] <- "Time"
          BB$BaseGrid[[counter]]$Name <- "Time"
        }
        # QuantityInfo
        {
          BB$BaseGrid[[counter]]$QuantityInfo <- list(
            Name="Time",
            Path=paste(datasetName,"Time",sep="|"),
            Type="Time")
        }
        # DataInfo
        {
          BB$BaseGrid[[counter]]$DataInfo <- list(
            Origin="BaseGrid",
            AuxiliaryType="Undefined",
            Source="")
        }
        # Dimension
        {
          BB$BaseGrid[[counter]]$Dimension <- timeColumn$dim
        }
        # Unit
        {
          BB$BaseGrid[[counter]]$Unit <- timeColumn$unit
        }
        # Values
        {
          if(any(is.na(timeColumn$value))) timeColumn$value[is.na(timeColumn$value)] <- "NaN"
          BB$BaseGrid[[counter]]$Values <- I(timeColumn$value)
        }
      }
      # add to BB_all
      {
        if(!(eachID %in% eachID_collector)){
          idx <- nrow(BB_all)+1
          for(jj in 1:ncol(BB)){
            BB_all[idx,jj] <- ifelse(is.list(BB[nrow(BB),jj]),list(BB[nrow(BB),jj]),BB[nrow(BB),jj])
          }
          if(idx == 1) colnames(BB_all) <- colnames(BB)
        }
        eachID_collector <- unique(c(eachID_collector,eachID))
      }
    }
    cat("Writing: ",paste0("ObsData_",eachProject,".json"),"\n")
    curFile <- file(file.path(getwd(),paste0("ObsData_",eachProject,".json")),encoding="UTF-8")
    write(toJSON(BB, digits=I(10), pretty=T, auto_unbox=T),file = curFile)
    close(curFile)
  }
  cat("Writing: ",paste0("ObsData_","ALL",".json"),"\n")
  curFile <- file(file.path(getwd(),paste0("ObsData_","ALL",".json")),encoding="UTF-8")
  write(toJSON(BB_all, digits=I(10), pretty=T, auto_unbox=T),file = curFile)
  close(curFile)
}

for(eachBB in 1:dim(BB_all)[1]){
  fileName <- paste(BB_all[eachBB,]$Name,sep=" - ") # BB_all[eachBB,]$ExtendedProperties[[1]]$Value[2],
  fileName <- gsub("[?*\"<>|\\/]", "_", fileName)
  curFile <- file(file.path(getwd(),"buildingBlocks",paste0(fileName,".json")),encoding="UTF-8")
  write(toJSON(BB_all[eachBB,], digits=I(10), pretty=T, auto_unbox=T),file = curFile)
  close(curFile)
}

