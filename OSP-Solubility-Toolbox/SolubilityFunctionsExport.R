#############################################
##### SOLUBILITY FUNCTIONS ##################
#############################################

#### Convert reference and observed solubility data to mg/ml ####
getUnitFactor.ref.f <- function(ref_unit) {
  unit <- tolower(ref_unit)
  ref_factor <- switch(unit,
                       "m"={1e0*MW.API},"mm"={1e-3*MW.API},"um"={1e-6*MW.API},"nm"={1e-9*MW.API},
                       "mol/l"={1e0*MW.API},"mmol/l"={1e-3*MW.API},"umol/l"={1e-6*MW.API},"nmol/l"={1e-9*MW.API},
                       "g/l"={1e0},"mg/l"={1e-3},"ug/l"={1e-6},"ng/l"={1e-9},"pg/l"={1e-12},
                       "mg/dl"={1e-2},"ug/dl"={1e-5},"ng/dl"={1e-8},"pg/dl"={1e-11},
                       "mg/ml"={1e0},"ug/ml"={1e-3},"ng/ml"={1e-6},"pg/ml"={1e-9},
                       stop("Unknown reference solubility unit"))
  return(ref_factor)
}

getUnitFactor.int.f <- function(int_unit) {
  unit <- tolower(int_unit)
  int_factor <- switch(unit,
                       "m"={1e0*MW.API},"mm"={1e-3*MW.API},"um"={1e-6*MW.API},"nm"={1e-9*MW.API},
                       "mol/l"={1e0*MW.API},"mmol/l"={1e-3*MW.API},"umol/l"={1e-6*MW.API},"nmol/l"={1e-9*MW.API},
                       "g/l"={1e0},"mg/l"={1e-3},"ug/l"={1e-6},"ng/l"={1e-9},"pg/l"={1e-12},
                       "mg/dl"={1e-2},"ug/dl"={1e-5},"ng/dl"={1e-8},"pg/dl"={1e-11},
                       "mg/ml"={1e0},"ug/ml"={1e-3},"ng/ml"={1e-6},"pg/ml"={1e-9},
                       stop("Unknown reference solubility unit"))
  return(int_factor)
}

getUnitFactor.aq.f <- function(unit) {
  for (i in 1:dim(obs.aq)[1]) {
    unit <- tolower(obs.aq$Obs.aq_unit[i])
    obs.aq$`factor`[i] <- switch(unit,
                                 "m"={1e0*MW.API},"mm"={1e-3*MW.API},"um"={1e-6*MW.API},"nm"={1e-9*MW.API},
                                 "mol/l"={1e0*MW.API},"mmol/l"={1e-3*MW.API},"umol/l"={1e-6*MW.API},"nmol/l"={1e-9*MW.API},
                                 "g/l"={1e0},"mg/l"={1e-3},"ug/l"={1e-6},"ng/l"={1e-9},"pg/l"={1e-12},
                                 "mg/dl"={1e-2},"ug/dl"={1e-5},"ng/dl"={1e-8},"pg/dl"={1e-11},
                                 "mg/ml"={1e0},"ug/ml"={1e-3},"ng/ml"={1e-6},"pg/ml"={1e-9},
                                 stop("Unknown Obs.aq_unit in Study with ID: ", obs.aq$ID.aq[i]))
  }
  return(obs.aq)
}

getUnitFactor.br.f <- function(unit) {
  for (i in 1:dim(obs.br)[1]) {
    unit <- tolower(obs.br$Obs.BR_unit[i])
    obs.br$`factor`[i] <- switch(unit,
                                 "m"={1e0*MW.API},"mm"={1e-3*MW.API},"um"={1e-6*MW.API},"nm"={1e-9*MW.API},
                                 "mol/l"={1e0*MW.API},"mmol/l"={1e-3*MW.API},"umol/l"={1e-6*MW.API},"nmol/l"={1e-9*MW.API},
                                 "g/l"={1e0},"mg/l"={1e-3},"ug/l"={1e-6},"ng/l"={1e-9},"pg/l"={1e-12},
                                 "mg/dl"={1e-2},"ug/dl"={1e-5},"ng/dl"={1e-8},"pg/dl"={1e-11},
                                 "mg/ml"={1e0},"ug/ml"={1e-3},"ng/ml"={1e-6},"pg/ml"={1e-9},
                                 stop("Unknown Obs.aq_unit in Study with ID: ", obs.br$ID.BR[i]))
  }
  return(obs.br)
}

Input.tab.f <- function() {
  Paremeter <- c("API Name", "MW", "MW Unit", "LogP","Type 1", "pKa1", "Type 2", "pKa2", "Type 3", "pKa3",
                 "Reference pH","Ref. Sol.","Ref. unit","Sol. Gain","Intrinsic pH","Int. Sol.","Int. unit",
                 "LogK m:w neutral","LogK m:w ionized","Conc. pure water")
  Value     <- c(API,MW.API,MW.unit,LogP,CT0,pKa0,CT1,pKa1,CT2,pKa2,ref_pH,ref_sol,ref_unit,base,int_pH,
                 int_sol,int_unit,IE.Kn,IE.Ki,C.H2O)
  df <- cbind(Paremeter,Value)
  return(df)
}


#### Calculate Solubility-pH profile ####
# Solubility plot functions
I.Sol.f <- function(pH.range) {
  (S_ref * (((ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))+
               ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT0^2)+
               (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT1^2)+
               (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^(CT2^2)+
               ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^max(CT0+CT1,-CT0-CT1)+
               ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT2,-CT0-CT2)+
               (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT1+CT2,-CT1-CT2)+
               ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))
            /
              (
                (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1}) +
                  (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^(CT0^2) +
                  (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^(CT1^2) +
                  (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^(CT2^2) +
                  (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^max(CT0+CT1,-CT0-CT1)+
                  (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^max(CT0+CT2,-CT0-CT2)+
                  (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^max(CT1+CT2,-CT1-CT2)+
                  (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.range)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.range)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.range)))}else{1})/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))))
}

aq.polt.f <- function(df,pH.range) {
  p <- ggplot(df,aes(pH.range)) +
    theme(
      axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
      rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
      
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      
      panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
      
      legend.key = element_rect(fill = "white"),
      
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16,hjust = 0.5),
      plot.margin=unit(c(.4,.4,.2,.5), 'cm')) +
    stat_function(fun=I.Sol.f)+
    geom_point(data = obs.aq, aes(x=pH.final,y=aq.S_mg.ml),stroke = 1.5, color= "blue") +
    scale_x_continuous(breaks = pH.range, labels = pH.range) +
    scale_y_log10()+
    labs(title = paste0("Observed vs. predicted Aqueous Solubility"),
         subtitle = paste0(API),
         x = "Medium pH",
         y = "Log10(Solubility [mg/mL])")
  return(p)
}

# intrinsic pH and solubility calculation functions
pH.int.f <- function(CT0,CT1,CT2,pKa0,pKa1,pKa2) {
  if (CT0 < 0 & CT1 == 0 & CT2 == 0) { # A N N
    pH.int <- 1
  } else if (CT0 == 0 & CT1 < 0 & CT2 == 0) { # N A N
    pH.int <- 1
  } else if (CT0 == 0 & CT1 == 0 & CT2 < 0) { # N N A
    pH.int <- 1
  } else if (CT0 < 0 & CT1 < 0 & CT2 == 0) { # A A N
    pH.int <- 1
  } else if (CT0 == 0 & CT1 < 0 & CT2 < 0) { # N A A
    pH.int <- 1
  } else if (CT0 < 0 & CT1 == 0 & CT2 < 0) { # A N A
    pH.int <- 1
  } else if (CT0 < 0 & CT1 < 0 & CT2 < 0) { # A A A
    pH.int <- 1
  } else if (CT0 > 0 & CT1 == 0 & CT2 == 0) { # B N N
    pH.int <- 14
  } else if (CT0 == 0 & CT1 > 0 & CT2 == 0) { # N B N
    pH.int <- 14
  } else if (CT0 == 0 & CT1 == 0 & CT2 > 0) { # N N B
    pH.int <- 14
  } else if (CT0 > 0 & CT1 > 0 & CT2 == 0) { # B B N
    pH.int <- 14
  } else if (CT0 > 0 & CT1 == 0 & CT2 > 0) { # B N B
    pH.int <- 14
  } else if (CT0 == 0 & CT1 > 0 & CT2 > 0) { # N B B
    pH.int <- 14
  } else if (CT0 > 0 & CT1 > 0 & CT2 > 0) { # B B B
    pH.int <- 14
  } else if (CT0 < 0 & CT1 > 0 & CT2 == 0) { # A B N
    pH.int <- (pKa0+pKa1)/2
  } else if (CT0 == 0 & CT1 < 0 & CT2 > 0) { # N A B
    pH.int <- (pKa1+pKa2)/2
  } else if (CT0 > 0 & CT1 < 0 & CT2 == 0) { # B A N
    pH.int <- (pKa0+pKa1)/2
  } else if (CT0 == 0 & CT1 > 0 & CT2 < 0) { # N B A
    pH.int <- (pKa1+pKa2)/2
  } else if (CT0 < 0 & CT1 < 0 & CT2 > 0) { # A A B
    pH.int <- (min(pKa0,pKa1)+pKa2)/2
  } else if (CT0 > 0 & CT1 < 0 & CT2 < 0) { # B A A
    pH.int <- (min(pKa1,pKa2)+pKa0)/2
  } else if (CT0 < 0 & CT1 > 0 & CT2 < 0) { # A B A
    pH.int <- (min(pKa0,pKa2)+pKa1)/2
  } else if (CT0 > 0 & CT1 > 0 & CT2 < 0) { # B B A
    pH.int <- (max(pKa0,pKa1)+pKa2)/2
  } else if (CT0 < 0 & CT1 > 0 & CT2 > 0) { # A B B
    pH.int <- (max(pKa1,pKa2)+pKa0)/2
  } else if (CT0 > 0 & CT1 < 0 & CT2 > 0) { # B A B
    pH.int <- (max(pKa0,pKa2)+pKa1)/2
  } else if (CT0 == 0 & CT1 == 0 & CT2 == 0) { # N N N
    pH.int <- 0
  } else {
    stop("Incompatible API type, are all pKa values and types configured correctly?")
  }
  return(pH.int)
}

S.int.f <- function(CT0,CT1,CT2,pKa0,pKa1,pKa2,S_ref,ref_pH) {
  pH.int  <- pH.int.f(CT0,CT1,CT2,pKa0,pKa1,pKa2)
  S.int.r <- (S_ref *
                (((ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))+
                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT0^2)+
                    (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT1^2)+
                    (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^(CT2^2)+
                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^max(CT0+CT1,-CT0-CT1)+
                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT2,-CT0-CT2)+
                    (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT1+CT2,-CT1-CT2)+
                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))
                 /
                   (
                     (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1}) +
                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^(CT0^2) +
                       (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^(CT1^2) +
                       (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^(CT2^2) +
                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^max(CT0+CT1,-CT0-CT1)+
                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^max(CT0+CT2,-CT0-CT2)+
                       (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^max(CT1+CT2,-CT1-CT2)+
                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH.int)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH.int)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH.int)))}else{1})/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))))
  S.int <- signif(S.int.r, digits = 4)
  return(S.int)
}

# SG-estimation algorithm
base.nlm.f <- function(base.nlm, fit_scale = "linear") {
  S.aq.Fit <- obs.aq[, 'aq.S_mg.ml']
  pH.aqf <- obs.aq[, 'pH.final']
  
  # Model formula
  model_formula <- function(pH, base) {
    S_ref * (
      ((ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))+
         ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT0^2)+
         (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT1^2)+
         (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^(CT2^2)+
         ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^max(CT0+CT1,-CT0-CT1)+
         ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT2,-CT0-CT2)+
         (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT1+CT2,-CT1-CT2)+
         ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))
      /
        ((if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1}) +
           (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^(CT0^2) +
           (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^(CT1^2) +
           (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^(CT2^2) +
           (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^max(CT0+CT1,-CT0-CT1)+
           (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^max(CT0+CT2,-CT0-CT2)+
           (if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^max(CT1+CT2,-CT1-CT2)+
           (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-pH)))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-pH)))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-pH)))}else{1})/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))
    )
  }
  
  if (fit_scale == "log") {
    # Fit on log scale
    base.nlm <- nls(log(S.aq.Fit) ~ log(model_formula(pH.aqf, base)),
                    start = list(base=base), 
                    algorithm = "port", 
                    lower = 1)
  } else {
    # Fit on linear scale (original)
    base.nlm <- nls(S.aq.Fit ~ model_formula(pH.aqf, base),
                    start = list(base=base), 
                    algorithm = "port", 
                    lower = 1)
  }
  
  return(base.nlm)
}


# Calculate solubilities based on estimated SG
S.aq.PKSim.calc.f <- function(obs.aq) {
  for (i in 1:dim(obs.aq)[1]) {
    obs.aq$`Saq.PKSim`[i] <- (S_ref *
                                (((ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))+
                                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT0^2)+
                                    (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^(CT1^2)+
                                    (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^(CT2^2)+
                                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1))/base^max(CT0+CT1,-CT0-CT1)+
                                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT2,-CT0-CT2)+
                                    (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT1+CT2,-CT1-CT2)+
                                    ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-ref_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-ref_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-ref_pH))), 1)))/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))
                                 /
                                   (
                                     (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1}) +
                                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^(CT0^2) +
                                       (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^(CT1^2) +
                                       (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^(CT2^2) +
                                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^max(CT0+CT1,-CT0-CT1)+
                                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^max(CT0+CT2,-CT0-CT2)+
                                       (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^max(CT1+CT2,-CT1-CT2)+
                                       (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.aq$pH.final[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.aq$pH.final[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.aq$pH.final[i])))}else{1})/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))))
  }
  return(obs.aq)
}

SG.Est.f <- eventReactive(input$SGfit, {
  base.nlm <- base.nlm.f(base.nlm, fit_scale = input$sg_fit_scale)
  
  # Calculate performance for the aqueous solubility model
  coeff  <- signif(summary(base.nlm)$coeff, digits = 6)
  est.sg <- coeff[1]
  st.e   <- coeff[2]
  t.stat <- signif(coeff[3],3)
  p.val  <- signif(coeff[4],3)
  CI.SG  <- as.data.frame(confint(base.nlm))
  CI.95  <- paste0(signif(CI.SG[1,],6)," - ",signif(CI.SG[2,] ,digits = 6))
  RSE    <- signif(summary(base.nlm)$sigma, digits = 3)
  obs.aq <- S.aq.PKSim.calc.f(obs.aq)
  obs.aq$Ln.PKSim <- log(obs.aq$Saq.PKSim)-log(obs.aq$aq.S_mg.ml)
  AFE    <- signif(exp(1/dim(obs.aq)[1]*sum(obs.aq$Ln.PKSim)), digits = 3)
  AAFE   <- signif(exp(1/dim(obs.aq)[1]*sum(abs(obs.aq$Ln.PKSim))), digits = 3)
  
  # Create output table
  out <- cbind(est.sg,st.e,t.stat,p.val,CI.95,RSE,AFE,AAFE)
  colnames(out) <- c("Estimated SG","Std. Error","t-statistic","p-value (t)","CI (95%)","RSE","AFE","AAFE")
  return(list(t = out, obs.aq = obs.aq))
})


# Plot residuals
plot.RES.pH.aq.f <- function (df,obs.aq) {
  p <- ggplot(obs.aq, aes(x=as.numeric(pH.final), y=Ln.PKSim)) +
    theme(
      axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
      rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
      
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      
      panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
      
      legend.key = element_rect(fill = "white"),
      
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16,hjust = 0.5),
      plot.margin=unit(c(.4,.4,.2,.5), 'cm')) +
    
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -log(2), linetype='dashed') +
    geom_hline(yintercept = log(2), linetype='dashed') +
    geom_point(size=3, stroke = 0.5, color = "blue") +
    
    scale_x_continuous() +
    scale_y_continuous(limits=c(-4,4)) +
    labs(title = paste0("Residuals vs. medium pH"),
         subtitle = paste0("PK-Sim predicted Aqueous Solubility"),
         x = "Medium pH",
         y = expression(bold("Residuals"["Ln(Predicted)-Ln(Observed)"])))
  return(p)
}

#### Biorelevant solubility calculation ####
Kmwn.f <- function (LogP) {
  IE.Log.K.mw.n.r <- 0.74*LogP+2.291
  IE.Log.K.mw.n   <- signif(IE.Log.K.mw.n.r, digits = 4)
  return(IE.Log.K.mw.n)
}

Penalty.f <- function (CT0,CT1,CT2) {
  if (CT0 == 0 & CT1 == 0 & CT2 == 0) {
    Penalty <- 0
  } else if (CT0 > 0 & CT1 == 0 & CT2 == 0) {
    Penalty <- 1
  } else {
    Penalty <- 2
  }
  return(Penalty)
}

Kmwi.f <- function (CT0,CT1,CT2,LogP) {
  if (CT0 == 0 & CT1 == 0 & CT2 == 0) {
    IE.Log.K.mw.i   <- 0
  } else {
    Penalty <- Penalty.f(CT0,CT1,CT2)
    IE.Log.K.mw.i.r <- 0.74*LogP+2.291-Penalty
    IE.Log.K.mw.i   <- signif(IE.Log.K.mw.i.r, digits = 4)
  }
  return(IE.Log.K.mw.i)
}

S.ion.f <- function(obs.br) {
  for (i in 1:dim(obs.br)[1]) {
    obs.br$`S_ion`[i] <- (S_int *
                            (((ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1))+
                                ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1)*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1))/base^(CT0^2)+
                                (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1))/base^(CT1^2)+
                                (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1)*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1)))/base^(CT2^2)+
                                ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1))*ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1))/base^max(CT0+CT1,-CT0-CT1)+
                                ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1))*ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1)*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1)))/base^max(CT0+CT2,-CT0-CT2)+
                                (ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1)*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1)))/base^max(CT1+CT2,-CT1-CT2)+
                                ((1-ifelse(CT0 != 0, 1/(1+10^(CT0*(pKa0-int_pH))), 1))*(1-ifelse(CT1 != 0, 1/(1+10^(CT1*(pKa1-int_pH))), 1))*(1-ifelse(CT2 != 0, 1/(1+10^(CT2*(pKa2-int_pH))), 1)))/base^max(CT0+CT1+CT2,-CT0-CT1-CT2))
                             /
                               (
                                 (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1}) +
                                   (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^(CT0^2) +
                                   (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^(CT1^2) +
                                   (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^(CT2^2) +
                                   (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^max(CT0+CT1,-CT0-CT1)+
                                   (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^max(CT0+CT2,-CT0-CT2)+
                                   (if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^max(CT1+CT2,-CT1-CT2)+
                                   (1-if(CT0 != 0){1/(1+10^(CT0*(pKa0-obs.br$pH.BR[i])))}else{1}) * (1-if(CT1 != 0){1/(1+10^(CT1*(pKa1-obs.br$pH.BR[i])))}else{1}) * (1-if(CT2 != 0){1/(1+10^(CT2*(pKa2-obs.br$pH.BR[i])))}else{1})/base^max(CT0+CT1+CT2,-CT0-CT1-CT2)))
                          - S_int)
  }
  return(obs.br)
}


LogK.nlm.f <- function(obs.br, fit_scale = "linear") {
  # Subset input data for NLM functions and estimate the Bile Salt affinity constants
  obs.br <- S.ion.f(obs.br)
  BR.Fit <- obs.br[, 'BR.S_mg.ml']
  pH.Fit <- obs.br[, 'pH.BR']
  BS.Fit <- obs.br[, 'BS_mM']
  Si.Fit <- obs.br[, 'S_ion']
  
  # Define the fitting function based on fitting scale
  if (fit_scale == "log") {
    # For neutral compounds
    if (CT0 == 0 & CT1 == 0 & CT2 == 0) {
      LogK <- nls(log(BR.Fit) ~ log((BS.Fit*(S_int/(1e3*C.H2O))*10^Log.K.mw.n+S_int)),
                  start = list(Log.K.mw.n = IE.Kn), 
                  algorithm = "port", 
                  lower = list(Log.K.mw.n = 0))
      Log.K.mw.i <- 0
    } else {
      # For ionizable compounds
      LogK <- nls(log(BR.Fit) ~ log((BS.Fit*(S_int/(1e3*C.H2O))*10^Log.K.mw.n+S_int) +
                                      (BS.Fit*(Si.Fit/(1e3*C.H2O))*10^Log.K.mw.i+Si.Fit)),
                  start = list(Log.K.mw.n = IE.Kn, Log.K.mw.i = IE.Ki),
                  algorithm = "port", 
                  lower = list(Log.K.mw.n = 0, Log.K.mw.i = 0))
      Log.K.mw.i <- environment(LogK[["m"]][["fitted"]])[["env"]][["Log.K.mw.i"]]
    }
  } else {
    # Original linear weighting
    if (CT0 == 0 & CT1 == 0 & CT2 == 0) {
      LogK <- nls(BR.Fit ~ ((BS.Fit*(S_int/(1e3*C.H2O))*10^Log.K.mw.n+S_int)),
                  start = list(Log.K.mw.n = IE.Kn), 
                  algorithm = "port", 
                  lower = list(Log.K.mw.n = 0))
      Log.K.mw.i <- 0
    } else {
      LogK <- nls(BR.Fit ~ ((BS.Fit*(S_int/(1e3*C.H2O))*10^Log.K.mw.n+S_int) +
                              (BS.Fit*(Si.Fit/(1e3*C.H2O))*10^Log.K.mw.i+Si.Fit)),
                  start = list(Log.K.mw.n = IE.Kn, Log.K.mw.i = IE.Ki),
                  algorithm = "port", 
                  lower = list(Log.K.mw.n = 0, Log.K.mw.i = 0))
      Log.K.mw.i <- environment(LogK[["m"]][["fitted"]])[["env"]][["Log.K.mw.i"]]
    }
  }
  Log.K.mw.n <- environment(LogK[["m"]][["fitted"]])[["env"]][["Log.K.mw.n"]]
  
  return(list(obs.br=obs.br, LogK=LogK))
}

Kmw.est.f <- eventReactive(input$Kfit, {
  LogK    <- LogK.nlm.f(obs.br, fit_scale = input$fit_scale)$LogK
  coeff   <- if (CT0 == 0 & CT1 == 0 & CT2 == 0) {
    temp  <- as.data.frame(signif(summary(LogK)$coeff, digits = 6))
    empty <- c(0,"NA","NA","NA")
    rbind(temp,empty)
  } else {
    as.data.frame(signif(summary(LogK)$coeff, digits = 6))
  }
  obs.br <- S.ion.f(obs.br)
  obs.br <- Pred.br.f(obs.br)
  obs.br$Ln.RES <- log(obs.br$Pred.br)-log(obs.br$BR.S_mg.ml)
  AFE    <- rep(signif(exp(1/dim(obs.br)[1]*sum(obs.br$Ln.RES)), digits = 3),2)
  AAFE   <- rep(signif(exp(1/dim(obs.br)[1]*sum(abs(obs.br$Ln.RES))), digits = 3),2)
  Parameter <- c("Log Km:w (neutral)","Log Km:w (ionized)")
  coeffK    <- cbind(Parameter,coeff,AFE,AAFE)
  return(list(t = coeffK)) #, obs.br = obs.br, v = K.bm.n
})

Pred.br.f <- function(obs.br) {
  for (i in 1:dim(obs.br)[1]) {
    obs.br$`Pred.br`[i] <- ((obs.br$BS_mM[i]*(S_int/(1e3*C.H2O))*10^IE.Kn+S_int) +
                              (obs.br$BS_mM[i]*(obs.br$S_ion[i]/(1e3*C.H2O))*10^IE.Ki+obs.br$S_ion[i]))
  }
  return(obs.br)
}

plot.RES.pH.br.f <- function (obs.br) {
  p <- ggplot(obs.br, aes(x=as.numeric(pH.BR), y=Ln.RES)) +
    theme(
      axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
      rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
      
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      
      panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
      
      legend.key = element_rect(fill = "white"),
      
      plot.title = element_text(size = 18,face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16,hjust = 0.5),
      plot.margin=unit(c(.4,.4,.2,.5), 'cm')) +
    
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -log(2), linetype='dashed') +
    geom_hline(yintercept = log(2), linetype='dashed') +
    geom_point(size=3, stroke = 0.5, color = "blue") +
    
    scale_x_continuous() +
    scale_y_continuous(limits=c(-4,4)) +
    labs(title = paste0("Residuals vs. medium pH"),
         subtitle = paste0("PK-Sim predicted Biorelevant Solubility"),
         x = "Medium pH",
         y = expression(bold("Residuals"["Ln(Predicted)-Ln(Observed)"])))
  return(p)
}


plot.RES.bs.br.f <- function (obs.br) {
  p <- ggplot(obs.br, aes(x=as.numeric(BS_mM), y=Ln.RES)) +
    theme(
      axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
      rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
      
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      
      panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
      
      legend.key = element_rect(fill = "white"),
      
      plot.title = element_text(size = 18,face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16,hjust = 0.5),
      plot.margin=unit(c(.4,.4,.2,.5), 'cm')) +
    
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = -log(2), linetype='dashed') +
    geom_hline(yintercept = log(2), linetype='dashed') +
    geom_point(size=3, stroke = 0.5, color = "blue") +
    
    scale_x_continuous() +
    scale_y_continuous(limits=c(-4,4)) +
    labs(title = paste0("Residuals vs. Bile Salt conc. medium"),
         subtitle = paste0("PK-Sim predicted Biorelevant Solubility"),
         x = "Bile salt conc. (mM)",
         y = expression(bold("Residuals"["Ln(Predicted)-Ln(Observed)"])))
  return(p)
}


