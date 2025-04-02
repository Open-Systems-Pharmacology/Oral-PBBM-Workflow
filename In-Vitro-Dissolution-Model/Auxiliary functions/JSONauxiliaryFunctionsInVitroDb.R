#############################################
##### DISSOLUTION DB PLOTTING FUNCTIONS #####
#############################################

# Convert all times to min
minute.f <- function(Time) {
  for (i in 1:dim(df.plot)[1]) {
    if (tolower(df.plot$Time.unit[i]) == "min") {
      df.plot$Time.unit[i] <- df.plot$Time.unit[i]
    } else if (tolower(df.plot$Time.unit[i]) == "h") {
      df.plot$Time[i] <- df.plot$Time[i] * 60
      df.plot$Time.unit[i] <- "min"
    } else {
      stop("Unknown Time.unit in Study with ID: ", df.plot$Study.ID[i])
    }
  }
  return(df.plot)
}

# Calculate the fluid volume in the vessel (for 2-stage assays)
current.volume.f <- function(Current.stage) {
  for (i in 1:dim(df.plot)[1]) {
    if (df.plot$Current.stage[i] == "1") {
      df.plot$`Current.Volume`[i] <- df.plot$Medium.V.S1..ml.[i]
    } else if (df.plot$Current.stage[i] == "2" & tolower(df.plot$Trans.Unit[i]) == "no") {
      df.plot$`Current.Volume`[i] <- df.plot$Medium.V.S2..ml.[i]
    } else if (df.plot$Current.stage[i] == "2" & tolower(df.plot$Trans.Unit[i]) == "min") {
      df.plot$`Current.Volume`[i] <- df.plot$Medium.V.S2..ml.[i] + 
        (df.plot$Medium.V.S1..ml.[i] - df.plot$Medium.V.S1..ml.[i] * (0.5)^(df.plot$Time[i] / df.plot$Trans.Rate[i]))
    } else if (df.plot$Current.stage[i] == "2" & tolower(df.plot$Trans.Unit[i]) == "h") {
      df.plot$`Current.Volume`[i] <- df.plot$Medium.V.S2..ml.[i] + 
        (df.plot$Medium.V.S1..ml.[i] - df.plot$Medium.V.S1..ml.[i] * (0.5)^(df.plot$Time[i] / (df.plot$Trans.Rate[i] / 60)))
    } else if (df.plot$Current.stage[i] == "2" & tolower(df.plot$Trans.Unit[i]) == "ml/min") {
      df.plot$`Current.Volume`[i] <- ifelse(
        df.plot$Trans.Rate[i] * df.plot$Time[i] <= df.plot$Medium.V.S1..ml.[i], 
        df.plot$Medium.V.S2..ml.[i] + df.plot$Trans.Rate[i] * df.plot$Time[i], 
        df.plot$Medium.V.S2..ml.[i] + df.plot$Medium.V.S1..ml.[i]
      )
    } else {
      stop("Unknown value for Current.stage or Trans.Unit in Study with ID: ", df.plot$Study.ID[i])
    }
  }
  return(df.plot)
}

# Get unit conversion factor
factor.f <- function(Rel.Unit) { 
  for (i in 1:dim(df.plot)[1]) { 
    if (tolower(df.plot$Rel.Unit[i]) == "%") {
      df.plot$`factor`[i] <- (df.plot$Dose..mg.[i] / (df.plot$Current.Volume[i] / 1000)) * 1e-2
    } else if (tolower(df.plot$Rel.Unit[i]) == "mg/l") {
      df.plot$`factor`[i] <- 1e0
    } else if (tolower(df.plot$Rel.Unit[i]) == "µg/l") {
      df.plot$`factor`[i] <- 1e-3
    } else if (tolower(df.plot$Rel.Unit[i]) == "ng/l") {
      df.plot$`factor`[i] <- 1e-6
    } else if (tolower(df.plot$Rel.Unit[i]) == "pg/l") {
      df.plot$`factor`[i] <- 1e-9
    } else if (tolower(df.plot$Rel.Unit[i]) == "mg/dl") {
      df.plot$`factor`[i] <- 1e1
    } else if (tolower(df.plot$Rel.Unit[i]) == "µg/dl") {
      df.plot$`factor`[i] <- 1e-2
    } else if (tolower(df.plot$Rel.Unit[i]) == "ng/dl") {
      df.plot$`factor`[i] <- 1e-5
    } else if (tolower(df.plot$Rel.Unit[i]) == "pg/dl") {
      df.plot$`factor`[i] <- 1e-8
    } else if (tolower(df.plot$Rel.Unit[i]) == "mg/ml") {
      df.plot$`factor`[i] <- 1e3
    } else if (tolower(df.plot$Rel.Unit[i]) == "µg/ml") {
      df.plot$`factor`[i] <- 1e0
    } else if (tolower(df.plot$Rel.Unit[i]) == "ng/ml") {
      df.plot$`factor`[i] <- 1e-3
    } else if (tolower(df.plot$Rel.Unit[i]) == "pg/ml") {
      df.plot$`factor`[i] <- 1e-6
    } else {
      stop("Unknown Rel.unit in Study with ID: ", df.plot$Study.ID[i])
    }
  }
  return(df.plot)
}

# Convert solubilities to mg/L
solubility.f <- function(S.Unit) {
  for (i in 1:dim(df.plot)[1]) { 
    if (tolower(df.plot$S.Unit[i]) == "mg/l") {
      df.plot$Med.S[i] <- 1e0 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "µg/l") {
      df.plot$Med.S[i] <- 1e-3 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "ng/l") {
      df.plot$Med.S[i] <- 1e-6 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "pg/l") {
      df.plot$Med.S[i] <- 1e-9 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "mg/dl") {
      df.plot$Med.S[i] <- 1e1 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "µg/dl") {
      df.plot$Med.S[i] <- 1e-2 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "ng/dl") {
      df.plot$Med.S[i] <- 1e-5 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "pg/dl") {
      df.plot$Med.S[i] <- 1e-8 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "mg/ml") {
      df.plot$Med.S[i] <- 1e3 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "µg/ml") {
      df.plot$Med.S[i] <- 1e0 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "ng/ml") {
      df.plot$Med.S[i] <- 1e-3 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else if (tolower(df.plot$S.Unit[i]) == "pg/ml") {
      df.plot$Med.S[i] <- 1e-6 * df.plot$Med.S[i]
      df.plot$S.Unit[i] <- "mg/l"
    } else {
      stop("Unknown S.Unit in Study with ID: ", df.plot$Study.ID[i])
    }
  }
  return(df.plot)
}

# Divide groups for overlay plot
group.f <- function(Medium.x) {
  for (i in 1:dim(cpd.df)[1]) {
    if (cpd.df$No.of.stages[i] == "1" & grepl("sif", cpd.df$Medium.x[i], ignore.case = TRUE)) {
      cpd.df$`Group`[i] <- "Intestinal"
    } else if (cpd.df$No.of.stages[i] == "1" & grepl("sgf", cpd.df$Medium.x[i], ignore.case = TRUE)) {
      cpd.df$`Group`[i] <- "Gastric"
    } else if (cpd.df$No.of.stages[i] == "2") {
      cpd.df$`Group`[i] <- "Both"
    } else {
      stop("Unknown No.of.stages or Medium.x in Study with ID: ", cpd.df$Study.ID[i])
    }
  }
  return(cpd.df)
}


plot.f <- function(cpd.df) {
  ggplot(data=cpd.df, aes(x=Time, y=Concentration.mg.l)) +
    theme(
      axis.line = element_line(colour = "black", 
                               linewidth = 1, linetype = "solid"),
      rect = element_rect(fill = "white", colour = "black",
                          linewidth = 0.5, linetype = 1),
      panel.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
      
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.x = element_text(size=18, face = "bold"),
      axis.title.y = element_text(size=18, face = "bold"),
      plot.title = element_text(face = "bold", size=25, hjust = 0.5),
      plot.subtitle = element_text(size=18, hjust = 0.5),
      plot.caption = element_text(face = "italic", size=14),
      
      plot.margin=unit(c(.4,.4,.2,.5), 'cm')
    ) +
    geom_blank() +
    {if (cpd.df$Med.S[1] > 0 & cpd.df$Med.S[1]/tail(cpd.df$Concentration.mg.l,1) < 20)
      geom_hline(yintercept = cpd.df$Med.S[1], linetype = "longdash", color = "darkblue")} + 
    geom_line(linetype = "dotdash", color = "blue") +
    geom_errorbar(aes(ymax=Concentration.mg.l+Variability.mg.l, ymin=Concentration.mg.l-Variability.mg.l), width=1,
                  size=.8, color="darkblue") +
    geom_point(shape=23, size=3, stroke =1.5, fill="lightblue", color="darkblue") +
    
    labs(title = compound,
         subtitle = paste0(cpd.df$Dose..mg., " mg ", cpd.df$Formulation, " in ", cpd.df$Medium.V.S1..ml., " ml ",
                           cpd.df$Medium.x, " at ", cpd.df$RPM, " RPM (N=", cpd.df$Replicates, ")"),
         caption = paste0("Source: ", cpd.df$Study, ", ", cpd.df$Reference, " (ID: ", cpd.df$Study.ID, ")"),
         x = "Time [min]", 
         y = "Concentration [mg/l]") +
    expand_limits(x = 0, y = 0)
}

overlay.plot.f <- function(cpd.df) {
  ggplot(data=cpd.df, aes(x=Time, y=Concentration.mg.l, color=paste0("ID ",Study.ID,": ",Dose..mg.,"mg ",Formulation, " in ", "\n",
                                                                     Medium.V.S1..ml., "ml ",Medium.x," @",RPM))) +
    theme(
      axis.line = element_line(colour = "black", 
                               linewidth = 1, linetype = "solid"),
      rect = element_rect(fill = "white", colour = "black",
                          linewidth = 0.5, linetype = 1),
      panel.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
      
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.x = element_text(size=18, face = "bold"),
      axis.title.y = element_text(size=18, face = "bold"),
      legend.text = element_text(size=14),
      plot.title = element_text(face = "bold", size=25, hjust = 0.5),
      plot.subtitle = element_text(size=18, hjust = 0.5),
      legend.title = element_text(size=18, face = "bold"),
      
      legend.spacing.y = unit(0.15, 'cm'),
      plot.margin=unit(c(.4,.4,.2,.5), 'cm')
    ) +
    
    scale_color_manual(values = colorRampPalette(brewer.pal(12,"Paired"))(length(unique(cpd.df$Study.ID))))+
    guides(color = guide_legend(byrow = TRUE)) +
    geom_line() +
    geom_errorbar(aes(ymax=Concentration.mg.l+Variability.mg.l, ymin=Concentration.mg.l), width=.8, size=.75) +
    geom_point(shape=15, size=3.5) +
    
    labs(title = compound,
         subtitle = paste0(cpd.df$Group, " fluid(s)"),
         x = "Time [min]", 
         y = "Concentration [mg/l]",
         color = "Study protocol:") +
    expand_limits(x = 0, y = 0) 
}



#############################################
##### JSON BUILDING FUNCTIONS ###############
#############################################


getTime <- function(timeValue,timeUnit,avgValue,eachID){
  timeUnit <- tolower(timeUnit)
  if(length(timeValue) != length(avgValue)) stop("Unequal length of time and avg values"," for ",eachID)
  avgValue <- as.numeric(avgValue)
  timeValue <- as.numeric(timeValue)
  timeValue <- timeValue[!is.na(avgValue)] 
  timeUnit <- trimws(timeUnit,which = "both")
  timeDim <- getDimension(timeUnit)
  if(timeDim != "Time") stop("Incorrect unit for time for ",eachID)
  return(list(value=timeValue,unit=timeUnit,dim=timeDim))
}

getAverage <- function(avgValue, avgUnit, avgType, timeValue, timeUnit, CurStage, VolOne, VolTwo, DoseMg, TrRate, TrUnit, eachID) {
  avgUnit <- tolower(avgUnit)
  if (length(timeValue) != length(avgValue)) 
    stop("Unequal length of time and avg values", " for ", eachID)
  
  if (length(avgType) == 0 || tolower(avgType) == "") {
    avgType <- ""
    avgDim <- ""
    avgUnit <- ""
    avgValue <- rep(NaN, length(varValue))
  } else {
    if (avgUnit == "%" & VolTwo == "") {
      DoseMg <- as.numeric(DoseMg)
      VolOne <- as.numeric(VolOne)
      avgFrac <- as.numeric(avgValue)
      avgValue <- ifelse(avgFrac <= 0, NaN, DoseMg / (VolOne * 1e-3) * 1e-2 * avgFrac)
      avgUnit <- "mg/l"
      avgType <- trimws(avgType, which = "both")
      avgDim <- "Concentration (mass)"
    } else if (avgUnit == "%" & VolTwo != "" & tolower(TrUnit) == "") {
      DoseMg <- as.numeric(DoseMg)
      VolOne <- as.numeric(VolOne)
      VolTwo <- as.numeric(VolTwo)
      CurStage <- as.numeric(CurStage)
      avgFrac <- as.numeric(avgValue)
      avgValue <- ifelse(avgFrac <= 0, NaN, DoseMg / (((CurStage - 2) * -VolOne + (CurStage - 1) * VolTwo) * 1e-3) * 1e-2 * avgFrac)
      avgUnit <- "mg/l"
      avgType <- trimws(avgType, which = "both")
      avgDim <- "Concentration (mass)"
    } else if (avgUnit == "%" & tolower(TrUnit) == "ml/min" & tolower(timeUnit)[1] == "min") {
      timeValue <- as.numeric(timeValue)
      DoseMg <- as.numeric(DoseMg)
      VolOne <- as.numeric(VolOne)
      VolTwo <- as.numeric(VolTwo)
      TrRate <- as.numeric(TrRate)
      CurVol <- ifelse(TrRate * timeValue <= VolOne, VolTwo + TrRate * timeValue, VolTwo + VolOne)
      avgFrac <- as.numeric(avgValue)
      avgValue <- ifelse(avgFrac <= 0, NaN, DoseMg / (CurVol * 1e-3) * 1e-2 * avgFrac)
      avgUnit <- "mg/l"
      avgType <- trimws(avgType, which = "both")
      avgDim <- "Concentration (mass)"
    } else if (avgUnit == "%" & tolower(TrUnit) == tolower(timeUnit)[1]) {
      timeValue <- as.numeric(timeValue)
      DoseMg <- as.numeric(DoseMg)
      VolOne <- as.numeric(VolOne)
      VolTwo <- as.numeric(VolTwo)
      TrRate <- as.numeric(TrRate)
      CurVol <- VolTwo + (VolOne - VolOne * 0.5^(timeValue / TrRate))
      avgFrac <- as.numeric(avgValue)
      avgValue <- ifelse(avgFrac <= 0, NaN, DoseMg / (CurVol * 1e-3) * 1e-2 * avgFrac)
      avgUnit <- "mg/l"
      avgType <- trimws(avgType, which = "both")
      avgDim <- "Concentration (mass)"
    } else {
      timeValue <- as.numeric(timeValue)
      avgDimRep <- rep(getDimension(avgUnit), length(timeValue))
      avgValue <- ifelse(avgDimRep == "Concentration (mass)", as.numeric(avgValue), avgValue[!is.na(avgValue) & !is.na(timeValue) & avgValue > 0])
      avgUnit <- trimws(avgUnit, which = "both")
      avgType <- trimws(avgType, which = "both")
      avgDim <- getDimension(avgUnit)
    }
    
    if (tolower(avgType) == "arith. mean") {
      avgType <- "ArithmeticMean"
    } else if (tolower(avgType) %in% c("geo. mean", "geom. mean")) {
      avgType <- "GeometricMean"
    } else if (tolower(avgType) == "individual") {
      avgType <- "Individual"
    } else if (tolower(avgType) == "median") {
      avgType <- "Median"
    } else {
      stop("Unknown average type: ", avgType, " for ", eachID)
    }
  }
  
  return(list(value = avgValue, unit = avgUnit, type = avgType, dim = avgDim))
}


getVariance <- function(varValue, varUnit, varType, avgValue, avgUnit, timeValue, timeUnit, n, CurStage, VolOne, VolTwo, DoseMg, TrRate, TrUnit, eachID) {
  varUnit <- tolower(varUnit)
  avgUnit <- tolower(avgUnit)
  
  if (length(varValue) != length(avgValue)) 
    stop("Unequal length of variance and avg values", " for ", eachID)
  
  if (length(varType) == 0 || tolower(varType) == "") {
    varType <- ""
    varDim <- ""
    varUnit <- ""
    varValue <- rep(NaN, length(varValue))
    varComment <- "."
  } else {
    if (varUnit == "%" & tolower(varType) == "arith. sd") {
      varUnit <- "mg/l"
      varDim <- "Concentration (mass)"
      varType <- "ArithmeticStdDev"
      
      DoseMg <- as.numeric(DoseMg)
      VolOne <- as.numeric(VolOne)
      varFrac <- as.numeric(varValue)
      timeValue <- as.numeric(timeValue)
      
      if (VolTwo == "") {
        varValue <- DoseMg / (VolOne * 1e-3) * 1e-2 * varFrac
        varComment <- "."
      } else if (VolTwo != "" & tolower(TrUnit) == "") {
        VolTwo <- as.numeric(VolTwo)
        CurStage <- as.numeric(CurStage)
        switchTime <- sort(timeValue)[(length(CurStage) - match(unique(CurStage), rev(CurStage)) + 1)[1]]
        varValue <- DoseMg / (((CurStage - 2) * -VolOne + (CurStage - 1) * VolTwo) * 1e-3) * 1e-2 * varFrac
        varComment <- paste(sep = " ", "Start time stage-two:", switchTime, timeUnit[1])
      } else if (tolower(TrUnit) == "ml/min" & tolower(timeUnit)[1] == "min") {
        VolTwo <- as.numeric(VolTwo)
        TrRate <- as.numeric(TrRate)
        CurVol <- ifelse(TrRate * timeValue <= VolOne, VolTwo + TrRate * timeValue, VolTwo + VolOne)
        varValue <- DoseMg / (CurVol * 1e-3) * 1e-2 * varFrac
        varComment <- "Vessel volume (t) = Transfer rate*timeValue <= Volume one ? Volume two + Transfer rate*time : Volume two + Volume one)"
      } else if (tolower(TrUnit) == tolower(timeUnit)[1]) {
        VolTwo <- as.numeric(VolTwo)
        TrRate <- as.numeric(TrRate)
        CurVol <- VolTwo + (VolOne - VolOne * 0.5^(timeValue / TrRate))
        varValue <- DoseMg / (CurVol * 1e-3) * 1e-2 * varFrac
        varComment <- "Vessel volume (t) = Volume two + (Volume one - Volume one*0.5^(time/Transfer rate))"
      } else {
        stop("Unable to convert ArithmeticStdDev varValues from % to Concentration (mass) for ", eachID)
      }
    } else {
      timeValue <- as.numeric(timeValue)
      varValue <- varValue[!is.na(avgValue) & !is.na(timeValue) & avgValue > 0]
      avgValue <- avgValue[!is.na(avgValue) & !is.na(timeValue) & avgValue > 0]
      varValue <- as.numeric(varValue)
      varUnit <- trimws(varUnit, which = "both")
      varType <- trimws(varType, which = "both")
      
      if (tolower(varType) == "arith. sd") {
        varType <- "ArithmeticStdDev"
        varDim <- getDimension(varUnit)
        if (varDim != getDimension(avgUnit)) 
          stop("Wrong dimension for ", varType, " for ", eachID)
        
        varValue[is.na(varValue) | varValue < 0] <- NaN
        varComment <- ifelse(tolower(TrUnit) == "ml/min", 
                             "Vessel volume (t) = Transfer rate*timeValue <= Volume one ? Volume two + Transfer rate*time : Volume two + Volume one)",
                             ifelse(tolower(TrUnit) == "min" | tolower(TrUnit) == "h", 
                                    "Vessel volume (t) = Volume two + (Volume one - Volume one*0.5^(time/Transfer rate))", 
                                    "."))
      } else if (tolower(varType) == "geo. sd") {
        varType <- "GeometricStdDev"
        if (varUnit == "") {
          varDim <- "Dimensionless"
        } else {
          stop("Unknown unit for ", varType, " for ", eachID)
        }
        varValue[is.na(varValue) | varValue < 1] <- NaN
        varComment <- "."
      } else if (tolower(varType) == "arith. sem") {
        if (!is.na(as.numeric(n)) & as.numeric(n) > 1) {
          varType <- "ArithmeticStdDev"
          varDim <- getDimension(varUnit)
          if (varDim != getDimension(avgUnit)) 
            stop("Wrong dimension for ", varType, " for ", eachID)
          
          varValue <- varValue * sqrt(as.numeric(n))
          varValue[is.na(varValue) | varValue < 0] <- NaN
          varComment <- "Arith. SEM converted to arith. SD"
        } else {
          varType <- ""
          varDim <- ""
          varValue[1:length(varValue)] <- NaN
          varComment <- "Arith. SEM not converted to arith. SD (N not specified)"
        }
      } else if (tolower(varType) == "geo. cv") {
        varType <- "GeometricStdDev"
        if (varUnit == "%") {
          varDim <- "Dimensionless"
          varValue <- varValue / 100
        } else if (varUnit == "") {
          varDim <- "Dimensionless"
        } else {
          stop("Unknown unit for ", varType, " for ", eachID)
        }
        varUnit <- ""
        varValue <- exp(sqrt(log(varValue^2 + 1)))
        varValue[is.na(varValue) | varValue < 0] <- NaN
        varComment <- "Geo. CV converted to geo. SD"
      } else if (tolower(varType) == "arith. cv") {
        varType <- "ArithmeticStdDev"
        if (varUnit == "%") {
          varDim <- getDimension(avgUnit)
          varValue <- varValue / 100
        } else if (varUnit == "") {
          varDim <- getDimension(avgUnit)
        } else {
          stop("Unknown unit for ", varType, " for ", eachID)
        }
        varUnit <- avgUnit
        varValue <- avgValue[!is.na(avgValue)] * varValue
        varValue[is.na(varValue) | varValue < 0] <- NaN
        varComment <- "Arith. CV converted to arith. SD"
      } else {
        varType <- ""
        varDim <- ""
        varUnit <- ""
        varValue <- rep(NaN, length(varValue))
        varComment <- "."
        warning("Unknown variance type: ", varType, " for ", eachID, ". Values ignored.")
      }
      
      varValue[is.na(avgValue) | is.na(varValue)] <- NaN
    }
  }
  
  return(list(value = varValue, unit = varUnit, dim = varDim, type = varType, comment = varComment))
}


getDimension <- function(unit,eachID){
  unit <- tolower(unit)
  if(is.element(trimws(tolower(unit),which = "both"),tolower(c("mg/l","µg/l","ng/l","pg/l","mg/dl","µg/dl","ng/dl","pg/dl","mg/ml","µg/ml","ng/ml","pg/ml")))){
    return("Concentration (mass)")
  } else if(is.element(trimws(tolower(unit),which = "both"),tolower(c("mmol/l","µmol/l","nmol/l","pmol/l","mmol/dl","µmol/dl","nmol/dl","pmol/dl","mmol/ml","µmol/ml","nmol/ml","pmol/ml")))){
    return("Concentration (molar)")
  } else if(is.element(trimws(tolower(unit),which = "both"),tolower(c("","%")))){
    return("Fraction")
  } else if(is.element(trimws(tolower(unit),which = "both"),tolower(c("h","min","s","day(s)","week(s)","month(s)","year(s)")))){
    return("Time")
  } else if(is.element(trimws(tolower(unit),which = "both"),tolower(c("mmol","µmol","nmol","pmol")))){
    return("Amount")
  }else if(is.element(trimws(tolower(unit),which = "both"),tolower(c("mg","µg","ng","pg")))){
    return("Mass")
  } else {
    stop("Unknown Dimension"," for ",eachID)
  }
}

getAuxiliaryType <- function(type,eachID){
  if(tolower(type)=="arithmeticmean"){
    return("Undefined")
  } else if(tolower(type)=="geometricmean"){
    return("Undefined")
  } else if(tolower(type)=="arithmeticstddev"){
    return("ArithmeticStdDev")
  } else if(tolower(type)=="geometricstddev"){
    return("GeometricStdDev")
  } else if(tolower(type)=="individual"){
    return("Undefined")
  } else if(tolower(type)=="median"){
    return("Undefined")
  } else{
    warning("Unknown type: no auxilary type could be determined for ",type," for ",eachID)
    return("Undefined")
  }
}
