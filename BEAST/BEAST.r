rm(list = ls())
gc()
library(lubridate)       #lubricate???ĺ???date_decimalС??ת??Ϊ???ڣ?decimal_date????תΪС??
library(stringr)
library(Rbeast)
library(data.table)
setDTthreads(threads = 0)
library(doParallel)
library(foreach)
library(dplyr)
library(imputeTS)
# options(digits=10)
cl = makeCluster(40L)#makeForkcluster适用于Linux系统
registerDoParallel(cl)
CCD_result = fread(
  "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/CCD_ALL_LAST.csv",
  header = TRUE
)[, c("X", "Y") := NULL]
CCD_result$X = as.double(str_extract_all(CCD_result$F_geo, "\\d+\\.*\\d*", simplify = TRUE)[, 1])
CCD_result$Y = as.double(str_extract_all(CCD_result$F_geo, "\\d+\\.*\\d*", simplify = TRUE)[, 2])
system.time(for (n in 1:100) {
  #BEAST算法处理
  PAsel <-
    fread(
      paste(
        "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/CCB",
        n,
        ".csv",
        sep = ""
      ),
      header = TRUE ,
      key = ".geo"
    )
  PAsel$Landsat = str_extract_all(PAsel$`system:index`, '[A-Z]+\\d+', simplify = TRUE)
  PAsel = subset(PAsel, Landsat != "LE07")[, c("system:index", "Landsat") :=
                                             NULL]
  PApointnum = data.table:::unique.data.table(PAsel, by = ".geo")[, .(.geo)]
  PApointnum$point <- seq(1, nrow(PApointnum))
  colnames(PApointnum) <- c(".geo", "point")
  setkey(PApointnum, ".geo")
  PAfinal <-
    data.table:::merge.data.table(PAsel, PApointnum, by = ".geo")
  rm(PAsel)
  PApointnum$X = as.double(str_extract_all(PApointnum$.geo, "\\d+\\.*\\d*", simplify = TRUE)[, 1])
  PApointnum$Y = as.double(str_extract_all(PApointnum$.geo, "\\d+\\.*\\d*", simplify = TRUE)[, 2])
  PApointnum[, c(".geo") := NULL]
  setkey(PApointnum, "point")
  PAfinal$Date <- as.Date(PAfinal$imgDate)
  PAfinal[, c(".geo", "imgDate") := NULL]
  setkey(PAfinal, "point")
  
  CCDfinal = data.table:::merge.data.table(CCD_result, PApointnum, by =
                                             c("X", "Y"))
  main <- function(i) {
    omityear = 7
    num      = 0
    CCD_break = 0
    trendall  = data.table()
    seasonall = data.table()
    staicall  = data.table()
    ccdcall_abrupt = data.table()
    ccdcall_gradual = data.table()
    ccdcacc_abrupt = 0
    ccdcacc_gradual = 0
    for (s in 1:10) {
      PAfinal_sub    = subset(PAfinal,  point == i)
      CCD_result_sub = subset(CCDfinal, point == i)
      CCDS = list(
        "S1_tStart",
        "S2_tStart",
        "S3_tStart",
        "S4_tStart",
        "S5_tStart",
        "S6_tStart",
        "S7_tStart",
        "S8_tStart",
        "S9_tStart",
        "S10_tStart"
      )[[s]]
      CCDE = list(
        "S1_tEnd",
        "S2_tEnd",
        "S3_tEnd",
        "S4_tEnd",
        "S5_tEnd",
        "S6_tEnd",
        "S7_tEnd",
        "S8_tEnd",
        "S9_tEnd",
        "S10_tEnd"
      )[[s]]
      CCDSEG = list('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10')[[s]]
      CCDtbreak = list(
        "S1_tBreak",
        "S2_tBreak",
        "S3_tBreak",
        "S4_tBreak",
        "S5_tBreak",
        "S6_tBreak",
        "S7_tBreak",
        "S8_tBreak",
        "S9_tBreak",
        "S10_tBreak"
      )[[s]]
      CCDnumObs = list(
        "S1_numObs",
        "S2_numObs",
        "S3_numObs",
        "S4_numObs",
        "S5_numObs",
        "S6_numObs",
        "S7_numObs",
        "S8_numObs",
        "S9_numObs",
        "S10_numObs"
      )[[s]]
      
      CCD_RED_SLP = list(
        "S1_RED_coef_SLP",
        "S2_RED_coef_SLP",
        "S3_RED_coef_SLP",
        "S4_RED_coef_SLP",
        "S5_RED_coef_SLP",
        "S6_RED_coef_SLP",
        "S7_RED_coef_SLP",
        "S8_RED_coef_SLP",
        "S9_RED_coef_SLP",
        "S10_RED_coef_SLP"
      )[[s]]
      CCD_RED_INTP = list(
        "S1_RED_coef_INTP",
        "S2_RED_coef_INTP",
        "S3_RED_coef_INTP",
        "S4_RED_coef_INTP",
        "S5_RED_coef_INTP",
        "S6_RED_coef_INTP",
        "S7_RED_coef_INTP",
        "S8_RED_coef_INTP",
        "S9_RED_coef_INTP",
        "S10_RED_coef_INTP"
      )[[s]]
      CCD_NIR_SLP = list(
        "S1_NIR_coef_SLP",
        "S2_NIR_coef_SLP",
        "S3_NIR_coef_SLP",
        "S4_NIR_coef_SLP",
        "S5_NIR_coef_SLP",
        "S6_NIR_coef_SLP",
        "S7_NIR_coef_SLP",
        "S8_NIR_coef_SLP",
        "S9_NIR_coef_SLP",
        "S10_NIR_coef_SLP"
      )[[s]]
      CCD_NIR_INTP = list(
        "S1_NIR_coef_INTP",
        "S2_NIR_coef_INTP",
        "S3_NIR_coef_INTP",
        "S4_NIR_coef_INTP",
        "S5_NIR_coef_INTP",
        "S6_NIR_coef_INTP",
        "S7_NIR_coef_INTP",
        "S8_NIR_coef_INTP",
        "S9_NIR_coef_INTP",
        "S10_NIR_coef_INTP"
      )[[s]]
      
      #######################################???ļ???????ʱ???Σ???Ϊʵ??????ʱ??Ҫ??һ?꣨2000-2020)###################################
      if (is.na(setDF(CCD_result_sub[, .SD, .SDcols = c(CCDS)])[, 1])) {
        next
      }
      if (is.na(setDF(CCD_result_sub[, .SD, .SDcols = c(CCDE)])[, 1])) {
        next
      }
      if (setDF(CCD_result_sub[, .SD, .SDcols = c(CCDS)])[, 1] < 2000) {
        startday = as.Date(lubridate:::date_decimal(2000))
      } else{
        startday = as.Date(lubridate:::date_decimal(setDF(CCD_result_sub[, .SD, .SDcols = c(CCDS)])[, 1]))
      }
      if (setDF(CCD_result_sub[, .SD, .SDcols = c(CCDE)])[, 1] > 2021) {
        endday = as.Date(lubridate:::date_decimal(2021))
      } else{
        endday = as.Date(lubridate:::date_decimal(setDF(CCD_result_sub[, .SD, .SDcols = c(CCDE)])[, 1]))
      }
      PAfinal_sub = PAfinal_sub[which(PAfinal_sub$Date >= startday &
                                        PAfinal_sub$Date <= endday), ]
      if (nrow(PAfinal_sub) == 0) {
        next
      }
      
      ###########################################????CCDC??abrupt gradual change#############################################
      if (is.na(CCD_result_sub[, .SD, .SDcols = c(CCDtbreak)][, 1]) ==
          FALSE) {
        if (CCD_result_sub[, .SD, .SDcols = c(CCDtbreak)][, 1] != 0) {
          CCD_break = CCD_break + 1
        }
      }
      abrupt  = NULL
      gradual = NULL
      if (s <= 9) {
        CCDS1 = list(
          "S1_tStart",
          "S2_tStart",
          "S3_tStart",
          "S4_tStart",
          "S5_tStart",
          "S6_tStart",
          "S7_tStart",
          "S8_tStart",
          "S9_tStart",
          "S10_tStart"
        )[[s + 1]]
        startday1 = as.Date(lubridate:::date_decimal(setDF(CCD_result_sub[, .SD, .SDcols = c(CCDS1)])[, 1]))
        CCD_RED_SLP1 = list(
          "S1_RED_coef_SLP",
          "S2_RED_coef_SLP",
          "S3_RED_coef_SLP",
          "S4_RED_coef_SLP",
          "S5_RED_coef_SLP",
          "S6_RED_coef_SLP",
          "S7_RED_coef_SLP",
          "S8_RED_coef_SLP",
          "S9_RED_coef_SLP",
          "S10_RED_coef_SLP"
        )[[s + 1]]
        CCD_RED_INTP1 = list(
          "S1_RED_coef_INTP",
          "S2_RED_coef_INTP",
          "S3_RED_coef_INTP",
          "S4_RED_coef_INTP",
          "S5_RED_coef_INTP",
          "S6_RED_coef_INTP",
          "S7_RED_coef_INTP",
          "S8_RED_coef_INTP",
          "S9_RED_coef_INTP",
          "S10_RED_coef_INTP"
        )[[s + 1]]
        CCD_NIR_SLP1 = list(
          "S1_NIR_coef_SLP",
          "S2_NIR_coef_SLP",
          "S3_NIR_coef_SLP",
          "S4_NIR_coef_SLP",
          "S5_NIR_coef_SLP",
          "S6_NIR_coef_SLP",
          "S7_NIR_coef_SLP",
          "S8_NIR_coef_SLP",
          "S9_NIR_coef_SLP",
          "S10_NIR_coef_SLP"
        )[[s + 1]]
        CCD_NIR_INTP1 = list(
          "S1_NIR_coef_INTP",
          "S2_NIR_coef_INTP",
          "S3_NIR_coef_INTP",
          "S4_NIR_coef_INTP",
          "S5_NIR_coef_INTP",
          "S6_NIR_coef_INTP",
          "S7_NIR_coef_INTP",
          "S8_NIR_coef_INTP",
          "S9_NIR_coef_INTP",
          "S10_NIR_coef_INTP"
        )[[s + 1]]
        
        RED_start = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_SLP)])[, 1] *
          (decimal_date(startday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_INTP)])[, 1]
        NIR_start = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_SLP)])[, 1] *
          (decimal_date(startday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_INTP)])[, 1]
        
        RED_end = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_SLP)])[, 1] *
          (decimal_date(endday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_INTP)])[, 1]
        NIR_end = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_SLP)])[, 1] *
          (decimal_date(endday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_INTP)])[, 1]
        
        RED_start1 = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_SLP1)])[, 1] *
          (decimal_date(startday1)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_INTP1)])[, 1]
        NIR_start1 = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_SLP1)])[, 1] *
          (decimal_date(startday1)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_INTP1)])[, 1]
        
        if (is.na(RED_start)) {
          RED_start = 0
        }
        if (is.na(NIR_start)) {
          NIR_start = 0
        }
        if (is.na(RED_end)) {
          RED_end = 0
        }
        if (is.na(NIR_end)) {
          NIR_end = 0
        }
        if (is.na(RED_start1)) {
          RED_start1 = 0
        }
        if (is.na(NIR_start1)) {
          NIR_start1 = 0
        }
        
        NDVI_start = (NIR_start - RED_start) / (NIR_start + RED_start)
        NDVI_end = (NIR_end - RED_end) / (NIR_end + RED_end)
        NDVI_start1 = (NIR_start1 - RED_start1) / (NIR_start1 + RED_start1)
        
        if (is.na(NDVI_start)) {
          NDVI_start = 0
        }
        if (is.na(NDVI_end)) {
          NDVI_end = 0
        }
        if (is.na(NDVI_start1)) {
          NDVI_start1 = 0
        }
        
        if ((decimal_date(endday) - decimal_date(startday)) != 21) {
          if (decimal_date(endday) == 2021) {
            gradual = NDVI_end - NDVI_start
          } else{
            abrupt  = NDVI_start1 - NDVI_end
            gradual = NDVI_end - NDVI_start
          }
        } else{
          gradual = NDVI_end - NDVI_start
        }
      } else{
        RED_start = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_SLP)])[, 1] *
          (decimal_date(startday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_INTP)])[, 1]
        NIR_start = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_SLP)])[, 1] *
          (decimal_date(startday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_INTP)])[, 1]
        
        RED_end = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_SLP)])[, 1] *
          (decimal_date(endday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_RED_INTP)])[, 1]
        NIR_end = setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_SLP)])[, 1] *
          (decimal_date(endday)) + setDF(CCD_result_sub[, .SD, .SDcols = c(CCD_NIR_INTP)])[, 1]
        
        NDVI_start = (NIR_start - RED_start) / (NIR_start + RED_start)
        NDVI_end = (NIR_end - RED_end) / (NIR_end + RED_end)
        
        gradual = NDVI_end - NDVI_start
      }
      
      if (is.null(abrupt)) {
        ccdc_abrupt = data.table()
        abrupt = 0
      } else{
        ccdc_abrupt = data.table(
          format(lubridate:::date_decimal(as.numeric(
            CCD_result_sub[, .SD, .SDcols = c(CCDtbreak)][, 1]
          )), "%Y-%m-%d"),
          as.numeric(CCD_result_sub[, .SD, .SDcols = c(CCDtbreak)][, 1]),
          abrupt
        )
      }
      if (is.null(gradual)) {
        ccdc_gradual = data.table()
        gradual = 0
      } else{
        if (s < 9) {
          ccdc_gradual = data.table(endday, lubridate:::decimal_date(endday), gradual)
        } else{
          ccdc_gradual = data.table(endday, lubridate:::decimal_date(endday), gradual)
        }
      }
      
      ccdcall_abrupt  = rbind(ccdcall_abrupt, ccdc_abrupt)
      ccdcall_gradual = rbind(ccdcall_gradual, ccdc_gradual)
      ccdcacc_abrupt  = ccdcacc_abrupt + abrupt
      ccdcacc_gradual = ccdcacc_gradual + gradual
      
      ########################################################???˺???????###################################################
      if ((decimal_date(endday) - decimal_date(startday)) < omityear) {
        next
      }
      num = num + 1
      if (nrow(PAfinal_sub) < 15) {
        next
      }
      #####################################################################################
      ###############################BEAST MODEL###########################################
      #####################################################################################
      #beast123() is an all-inclusive function that duplicates the functionalities of beast
      # and beast.irreg. It can handle a single, multiple, or 3D of stacked time series, being
      # either regular or irregular. It allows for customization through four LIST arguments:
      #   metadata -- additional info about the input Y
      #   prior    -- prior parameters for the beast model
      #   mcmc     -- MCMC simulation setting
      #   extra    -- misc parameters turning on/off outputs and setting up parallel computation
      
      # metadata is NOT part of BEAST itself, but some extra info to describe the input
      # time series Y. Below, the input Y is the 'Yellowstone' ts.
      metadata                  = list()
      metadata$isRegularOrdered = FALSE        # Regular input
      # metadata$whichDimIsTime   = 1            # Which dim of the input refer to time for
      # 2D/3D inputs? Ignored for a single time
      # series input.
      metadata$time             = PAfinal_sub$Date  # Or startTime=1981.5137
      #    startTime=as.Date('1981-7-7')
      metadata$deltaTime        = 1 / 24         # Half-monthly regular ts: 0.5/12=1/24
      metadata$period           = 1.0          # The period is 1 year:
      # freq x deltaTime = period
      # 24    x  1/24    = 1.0
      metadata$omissionValue    = NaN          # By default, NaNs are ignored
      metadata$maxMissingRateAllowed = 0.80 # If missingness is higher than .75, the ts
      #  is  skipped and not fitted
      metadata$deseasonalize    = FALSE        # Do not remove the global seasonal pattern
      #  before fitting the beast model
      metadata$detrend          = FALSE        # Do not remove the global trend before
      # the fitting
      
      # prior is the ONLY true parameters of the beast model,used to specify the priors
      # in the Bayesian formulation
      prior = list()
      prior$seasonMinOrder   = 1         #min harmonic order allowed to fit seasonal cmpnt
      prior$seasonMaxOrder   = 5         #max harmonic order allowed to fit seasonal cmpnt
      prior$seasonMinKnotNum = 0         #min number of changepnts in seasonal cmpnt
      prior$seasonMaxKnotNum = 3         #max number of changepnts in seasonal cmpnt
      prior$seasonMinSepDist = 24        #min inter-chngpts seperation for seasonal cmpnt
      prior$trendMinOrder	= 0         #min polynomial order allowed to fit trend cmpnt
      prior$trendMaxOrder	= 1         #max polynomial order allowed to fit trend cmpnt
      prior$trendMinKnotNum  = 0         #min number of changepnts in trend cmpnt
      prior$trendMaxKnotNum  = 3        #max number of changepnts in trend cmpnt
      prior$trendMinSepDist  = 24         #min inter-chngpts seperation for trend cmpnt
      prior$precValue        = 1.50      #Initial value of the precision parameter (no
      # need to change it unless for precPrioType='const')
      prior$precPriorType    = 'uniform' # Possible values: const, uniform, and componentwise
      
      # mcmc is NOT part of the beast model itself, but some parameters to configure the
      # MCMC inference.
      mcmc = list()
      
      mcmc$seed                      = 20220# an arbitray seed for random number generator
      mcmc$samples                   = 8000   # samples collected per chain
      mcmc$thinningFactor            = 7      # take every 3rd sample and discard others
      mcmc$burnin                    = 4000    # discard the initial 150 samples per chain
      mcmc$chainNumber               = 5      # number of chains
      mcmc$maxMoveStepSize           = 24      # max random jump step when proposing new chngpts
      mcmc$trendResamplingOrderProb  = 0.100  # prob of choosing to resample polynomial order
      mcmc$seasonResamplingOrderProb = 0.100  # prob of choosing to resample harmonic order
      mcmc$credIntervalAlphaLevel    = 0.950  # the significance level for credible interval
      
      # extra is NOT part of the beast model itself, but some parameters to configure the
      # output and computation process
      extra = list()
      extra$dumpInputData        = TRUE #If true, a copy of input time series is outputted
      extra$whichOutputDimIsTime = 1     #For 2D or 3D inputs, which dim of the output refers to
      # time? Ignored if the input is a single time series
      extra$computeCredible      = TRUE #If true, compute CI: computing CI is time-intensive.
      extra$fastCIComputation    = TRUE  #If true, a faster way is used to get CI, but it is
      # still time-intensive. That is why the function beast()
      # is slow because it always compute CI.
      extra$computeSeasonOrder   = TRUE #If true, dump the estimated harmonic order over time
      extra$computeTrendOrder    = TRUE #If true, dump the estimated polynomial order over time
      extra$computeSeasonChngpt  = TRUE  #If true, get the most likely locations of s chgnpts
      extra$computeTrendChngpt   = TRUE  #If true, get the most likely locations of t chgnpts
      extra$computeSeasonAmp     = TRUE #If true, get time-varying amplitude of seasonality
      extra$computeTrendSlope    = TRUE #If true, get time-varying slope of trend
      extra$tallyPosNegSeasonJump = FALSE #If true, get those changpts with +/- jumps in season
      extra$tallyPosNegTrendJump = TRUE #If true, get those changpts with +/- jumps in trend
      extra$tallyIncDecTrendJump = TRUE #If true, get those changpts with increasing/
      # decreasing trend slopes
      extra$printProgressBar     = TRUE
      extra$printOptions         = FALSE
      extra$consoleWidth         = 0     # If 0, the console width is from the current console
      # extra$numThreadsPerCPU     = 16     # 'numThreadsPerCPU' and 'numParThreads' are used to
      extra$numParThreads        = 0     # configure multithreading runs; they're used only if
      # Y has multiple time series (e.g.,stacked images)
      beast_result = beast123(PAfinal_sub[[3]], metadata, prior, mcmc, extra, season =
                                'harmonic')
      
      # plot(
      #   beast_result,
      #   index = 1,
      #   vars  = c('y','s','scp','sorder','t','tcp','torder','slpsgn','o','ocp','error'),
      #   col         = NULL,
      #   main        = "BEAST decomposition and changepoint detection",
      #   xlab        = 'Time',
      #   ylab        = NULL,
      #   cex.main    = 1,
      #   cex.lab     = 1,
      #   relative.heights = NULL,
      #   interactive = FALSE,
      #   ncpStat     = c('median','mode','mean','pct90','max'),
      # )
      # print(beast_result)
      ################################################################################
      # rescaled = as.numeric(PAfinal_sub$Date)-as.numeric(startday)
      #
      # pi_val_simple = (2 * pi) / 365.25
      # pi_val_advanced = (4 * pi) / 365.25
      # pi_val_full = (6 * pi) / 365.25
      # x = data.table()
      # x = data.table(rescaled,cos(pi_val_simple * rescaled),
      #                sin(pi_val_simple * rescaled))
      #
      # # 18 or more observations. Fit two harmonic terms (advanced model, six coefficients)
      # if(setDF(CCD_result_sub[,.SD,.SDcols = c(CCDnumObs)])[,1] >= 18){
      #   x = cbind(x, data.table(cos(pi_val_advanced * rescaled),
      #                           sin(pi_val_advanced * rescaled)))}
      #
      # # 24 or more observations. Fit three harmonic terms (full model, eight coefficients)
      # if(setDF(CCD_result_sub[,.SD,.SDcols = c(CCDnumObs)])[,1] >= 24){
      #   x = cbind(x, data.table(cos(pi_val_full * rescaled),
      #                           sin(pi_val_full * rescaled)))}
      #
      # fitlasso = glmnet(as.matrix(x), as.matrix(PAfinal_sub[[ba]]),family="mgaussian", lambda=0.0002, alpha=1) #????alpha=1ΪLASSO?ع飬????????0???????ع?
      # glmnet:::coef.glmnet(fitlasso,s=0.0002)
      #
      # a = lars(as.matrix(x), as.matrix(PAfinal_sub[[ba]]),type = "lasso")
      
      if (is.nan(beast_result[["R2"]])) {
        next
      }
      
      #if (imputeTS:::statsNA(beast_result[["data"]],print_only = FALSE)$longest_na_gap>=8){
      #  next
      #}
      # #beast ??��ָ????????һ??R2???Ǽ???׼ȷ??
      # if (beast_result[["R2"]]<=0.7){
      #   next
      # }
      #????ncp probility??????Ϊ??ֵ
      tncp = data.table(beast_result[["trend"]][["ncpPr"]], seq(
        from = 0,
        to = length(beast_result[["trend"]][["ncpPr"]]) - 1,
        by = 1
      ))
      maxtncp = subset(tncp, V1 == max(tncp[, 1]))[[2]]
      sncp = data.table(beast_result[["season"]][["ncpPr"]], seq(
        from = 0,
        to = length(beast_result[["season"]][["ncpPr"]]) - 1,
        by = 1
      ))
      maxsncp = subset(sncp, V1 == max(sncp[, 1]))[[2]]
      
      if (maxtncp == 1) {
        trend = data.table(beast_result[["trend"]][["cp"]],
                           beast_result[["trend"]][["cpPr"]],
                           beast_result[["trend"]][["cpAbruptChange"]],
                           beast_result[["trend"]][["cpCI"]])[c(1)]
      } else if (maxtncp == 2) {
        trend = data.table(beast_result[["trend"]][["cp"]],
                           beast_result[["trend"]][["cpPr"]],
                           beast_result[["trend"]][["cpAbruptChange"]],
                           beast_result[["trend"]][["cpCI"]])[c(1, 2)]
      } else if (maxtncp == 3) {
        trend = data.table(beast_result[["trend"]][["cp"]],
                           beast_result[["trend"]][["cpPr"]],
                           beast_result[["trend"]][["cpAbruptChange"]],
                           beast_result[["trend"]][["cpCI"]])[c(1, 2, 3)]
      } else{
        trend = data.table()
      }
      
      if (maxsncp == 1) {
        season = data.table(beast_result[["season"]][["cp"]],
                            beast_result[["season"]][["cpPr"]],
                            beast_result[["season"]][["cpAbruptChange"]],
                            beast_result[["season"]][["cpCI"]])[c(1)]
      } else if (maxsncp == 2) {
        season = data.table(beast_result[["season"]][["cp"]],
                            beast_result[["season"]][["cpPr"]],
                            beast_result[["season"]][["cpAbruptChange"]],
                            beast_result[["season"]][["cpCI"]])[c(1, 2)]
      } else if (maxsncp == 3) {
        season = data.table(beast_result[["season"]][["cp"]],
                            beast_result[["season"]][["cpPr"]],
                            beast_result[["season"]][["cpAbruptChange"]],
                            beast_result[["season"]][["cpCI"]])[c(1, 2, 3)]
      } else{
        season = data.table()
      }
      trend = na.omit(trend)
      season = na.omit(season)
      if (nrow(trend) != 0) {
        stat1 = data.table(
          mean(beast_result[["trend"]][["order"]], na.rm = TRUE),
          mean(beast_result[["trend"]][["cpOccPr"]], na.rm = TRUE),
          mean(beast_result[["trend"]][["SD"]], na.rm =
                 TRUE)
        )
        stat11 = data.table(beast_result[["time"]], beast_result[["trend"]][["slp"]], beast_result[["trend"]][["Y"]])
        colnames(trend) = c("cpyear",
                            "cpprob",
                            "cpabruptchange",
                            "trendCI1",
                            "trendCI2")
        trend$meanorder  = stat1$V1
        trend$meancpoccpr = stat1$V2
        trend$meanSD     = stat1$V3
        trend$startday    = startday
        trend$endday      = endday
        trend$CCDSEG      = CCDSEG
        #######################################################################
        ####################### ???˽׶Σ?????RSE??????#######################
        ######################################################################
        if (nrow(trend) == 2) {
          setkey(trend, cpyear)
          if ((trend[, 1][2] - trend[, 1][1]) <= 2) {
            trend = trend[which(trend$cpprob == max(trend[, 2])), ]#��?????ڵ?changeȡprob?????ĵ㣬ȥ????ʼ????��???ڵĵ?
          }
        }
        if (nrow(trend) == 3) {
          setkey(trend, cpyear)
          if ((trend[, 1][2] - trend[, 1][1]) <= 2) {
            trend = trend[which(trend$cpprob != min(trend[c(1, 2)][, 2])), ]
          } else if ((trend[, 1][3] - trend[, 1][2]) <= 2) {
            trend = trend[which(trend$cpprob != min(trend[c(2, 3)][, 2])), ]
          }
        }
        trend = trend[which(trend$cpyear >= (decimal_date(startday) +
                                               2) & trend$cpyear <= (decimal_date(endday) - 2)), ]
        
        if (nrow(trend) != 0) {
          stat111 = data.table()
          for (slp_t in 1:nrow(trend)) {
            #slopeǰ???Ƚ?
            tr_timea = as.numeric(trend[, 4][slp_t])
            tr_timeb = as.numeric(trend[, 5][slp_t])
            slopeA = mean(stat11[which(stat11$V1 >= (tr_timeb) &
                                         stat11$V1 <= (tr_timeb + 1)), ]$V2, na.rm = TRUE)
            slopeB = mean(stat11[which(stat11$V1 >= (tr_timea - 1) &
                                         stat11$V1 <= (tr_timea)), ]$V2, na.rm = TRUE)
            slope = abs(slopeA - slopeB)
            YA = mean(stat11[which(stat11$V1 >= (tr_timeb) &
                                     stat11$V1 <= (tr_timeb + 1)), ]$V3, na.rm = TRUE)
            YB = mean(stat11[which(stat11$V1 >= (tr_timea - 1) &
                                     stat11$V1 <= (tr_timea)), ]$V3, na.rm = TRUE)
            Ydata = YA - YB
            stat111_sub = data.table(slope, Ydata)
            stat111 = rbind(stat111, stat111_sub)
          }
          trend = cbind(trend, stat111)
          
          trend = trend[which(trend$cpprob >= 0.3), ]##cp prob
          trend = trend[which(abs(trend$cpabruptchange) >= 0.005), ]##abrupt change
          if (nrow(trend) == 0) {
            trend = data.table()
          }
          
        } else{
          trend = data.table()
        }
      } else{
        trend = data.table()
      }
      
      if (nrow(season) != 0) {
        stat2 = data.table(
          mean(beast_result[["season"]][["order"]], na.rm = TRUE),
          mean(beast_result[["season"]][["cpOccPr"]], na.rm = TRUE),
          mean(beast_result[["season"]][["SD"]], na.rm =
                 TRUE)
        )
        stat22 = data.table(beast_result[["time"]], beast_result[["season"]][["amp"]], beast_result[["season"]][["Y"]])
        colnames(season) = c("cpyear",
                             "cpprob",
                             "cpabruptchange",
                             "seasonCI1",
                             "seasonCI2")
        season$meanorder  = stat2$V1
        season$meancpoccpr = stat2$V2
        season$meanSD     = stat2$V3
        season$startday    = startday
        season$endday      = endday
        season$CCDSEG      = CCDSEG
        #######################################################################
        ####################### ???˽׶Σ?????RSE??????######################
        ######################################################################
        if (nrow(season) == 2) {
          setkey(season, cpyear)
          if ((season[, 1][2] - season[, 1][1]) <= 2) {
            season = season[which(season$cpprob != min(season[, 2])), ]
          }
        }
        if (nrow(season) == 3) {
          setkey(season, cpyear)
          if ((season[, 1][2] - season[, 1][1]) <= 2) {
            season = season[which(season$cpprob != min(season[c(1, 2)][, 2])), ]
          } else if ((season[, 1][3] - season[, 1][2]) <= 2) {
            season = season[which(season$cpprob != min(season[c(2, 3)][, 2])), ]
          }
        }
        season = season[which(season$cpyear >= (decimal_date(startday) +
                                                  2) & season$cpyear <= (decimal_date(endday) - 2)), ]
        
        if (nrow(season) != 0) {
          stat222 = data.table()
          for (slp_s in 1:nrow(season)) {
            se_timea = as.numeric(season[, 4][slp_s])
            se_timeb = as.numeric(season[, 5][slp_s])
            ampA = mean(stat22[which(stat22$V1 >= (se_timeb) &
                                       stat22$V1 <= (se_timeb + 1)), ]$V2, na.rm = TRUE)
            ampB = mean(stat22[which(stat22$V1 >= (se_timea - 1) &
                                       stat22$V1 <= (se_timea)), ]$V2, na.rm = TRUE)
            amp = ampA - ampB
            stat222_sub = data.table(amp)
            stat222 = rbind(stat222, stat222_sub)
          }
          season = cbind(season, stat222)
          
          season = season[which(season$cpprob >= 0.3), ]
          season = season[which(abs(season$cpabruptchange) >= 0.005), ]
          if (nrow(season) == 0) {
            season = data.table()
          }
        } else{
          season = data.table()
        }
      } else{
        season = data.table()
      }
      
      staic    = data.table(
        beast_result[["R2"]],
        beast_result[["RMSE"]],
        beast_result[["sig2"]],
        beast_result[["marg_lik"]],
        (decimal_date(endday) - decimal_date(startday))
      )
      colnames(staic) = c("R2", "RMSE", "sig2", "marg_lik", "interval_year")
      
      trendall = rbind(trendall, trend)
      seasonall = rbind(seasonall, season)
      staicall = rbind(staicall, staic)
    }
    ######################################################################################################################
    ###########################################   DATA ?????? ###############################################################
    ######################################################################################################################
    
    ############################################################BEAST############################
    if (nrow(trendall) != 0) {
      trendall$point = i
      trendall$trueyear = format(lubridate:::date_decimal(trendall$cpyear), "%Y-%m-%d")
    } else{
      trendall = data.table()
    }
    #######seasonall
    if (nrow(seasonall) != 0) {
      seasonall$point = i
      seasonall$trueyear = format(lubridate:::date_decimal(seasonall$cpyear),
                                  "%Y-%m-%d")
    } else{
      seasonall = data.table()
    }
    
    #######staicall
    if (nrow(staicall) != 0) {
      staicall = data.table(
        staicall[, .(mean(R2))],
        staicall[, .(mean(RMSE))],
        staicall[, .(mean(sig2))],
        staicall[, .(mean(marg_lik))],
        ccdcacc_abrupt,
        ccdcacc_gradual,
        ccdcacc_abrupt + ccdcacc_gradual,
        sum(trendall$cpabruptchange),
        sum(seasonall$cpabruptchange),
        sum(trendall$cpabruptchange) + sum(seasonall$cpabruptchange),
        num,
        CCD_break
      )
      colnames(staicall) = c(
        "R2",
        "RMSE",
        "sig2",
        "marg_link",
        "ccdc_totabrupt",
        "ccdc_totgradual",
        "ccdc_tot",
        "beast_tottrendabrupt",
        "beast_totseasonabrupt",
        "beast_totabrupt",
        "numforrun",
        "ccdcbreaks"
      )
      staicall$numtrend  = nrow(trendall)
      staicall$numseason = nrow(seasonall)
      staicall$point = i
    } else{
      staicall = data.table()
    }
    ###################################################################CCDC#############################################
    if (nrow(ccdcall_abrupt) != 0) {
      colnames(ccdcall_abrupt) = c("trueyear", "cpyear", "abrupt")
      ccdcall_abrupt$point = i
    } else{
      ccdcall_abrupt = data.table()
    }
    
    if (nrow(ccdcall_gradual) != 0) {
      colnames(ccdcall_gradual) = c("trueyear", "cpyear", "gradual")
      ccdcall_gradual$point = i
    } else{
      ccdcall_gradual = data.table()
    }
    
    CCDC_ALL = ccdcall_abrupt
    listpart = list(trendall, seasonall, staicall, ccdcall_gradual, CCDC_ALL)
    return(listpart)
  }
  listall = foreach(
    i = 1:nrow(PApointnum),
    .combine = rbind,
    .packages = c("data.table", "Rbeast", "lubridate", "dplyr", "imputeTS"),
    .verbose = TRUE
  ) %dopar% main(i)
  rm(PAfinal)
  TR = rbindlist(listall[, 1], use.names = TRUE)
  SE = rbindlist(listall[, 2], use.names = TRUE)
  ST = rbindlist(listall[, 3], use.names = TRUE)
  CG = rbindlist(listall[, 4], use.names = TRUE)
  CA = rbindlist(listall[, 5], use.names = TRUE)
  if (nrow(TR) != 0) {
    trendchp = data.table:::merge.data.table(TR, PApointnum, by = "point")
    fwrite(
      trendchp,
      paste(
        "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/RESULT_A/BEAST_trendchp",
        n,
        ".csv",
        sep = ""
      )
    )
  }
  if (nrow(SE) != 0) {
    seasonchp = data.table:::merge.data.table(SE, PApointnum, by = "point")
    fwrite(
      seasonchp,
      paste(
        "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/RESULT_A/BEAST_seasonchp",
        n,
        ".csv",
        sep = ""
      )
    )
  }
  if (nrow(ST) != 0) {
    staicall = data.table:::merge.data.table(ST, PApointnum, by = "point")
    fwrite(
      staicall,
      paste(
        "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/RESULT_A/CBD_ALL",
        n,
        ".csv",
        sep = ""
      )
    )
  }
  if (nrow(CG) != 0) {
    ccdcall_gradual = data.table:::merge.data.table(CG, PApointnum, by = "point")
    fwrite(
      ccdcall_gradual,
      paste(
        "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/RESULT_A/CCDC_gradual",
        n,
        ".csv",
        sep = ""
      )
    )
  }
  if (nrow(CA) != 0) {
    CCDC_ALL = data.table:::merge.data.table(CA, PApointnum, by = "point")
    fwrite(
      CCDC_ALL,
      paste(
        "/group_homes/PA_disturb/home/zhaowp20/CCDC_BEAST_DATA/RESULT_A/CCDC_abrupt",
        n,
        ".csv",
        sep = ""
      )
    )
  }
  
  print(n)
})