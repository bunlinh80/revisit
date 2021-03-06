# overview:

# original code is in file named origcodenm; makebranch0() converts that
# to the master branch, named origcodenm.0; further branches will be
# origcodenm.1.R, origcodenm.2.R etc.

# a branch is merely a .R code file, but with the first line being a
# comment holding a brief description of this particular branch

# see README.md for more

# initialize rvisit; the R environment rvenv will contain the relevant
# information about the currently-in-use branch

library(readxl)

rvinit <- function(smalleffect=0.05) {
   rvenv <<- new.env()
   rvenv$currb <<- NULL
   rvenv$currbasenm <<- NULL
   rvenv$username <- NULL
   rvenv$currcode <<- NULL
   rvenv$smalleffect <<- smalleffect
   rvenv$pc <<- NULL
   rvenv$pcount <<- 0
}


# load original code, a .R file, and make the first branch from it
makebranch0 <- function(origcodenm) {
   code <- readLines(con = origcodenm)
   desclines <-
      c('# RV history start','# original code','# RV history end')
   code <- c(desclines,code)
   rvenv$currbasenm <<- tools::file_path_sans_ext(origcodenm)
   br0filenm <- paste(rvenv$currbasenm,'.0.R',sep='')
   writeLines(code,br0filenm)
}

# create new branch, with "midfix" ("middle suffix") midfix, in a file
# whose name is the concatenization of our original prefix
# rvenv$currbasenm; the midfix; and '.R'; the note desc describes
# the branch
saveb <- function(midfix, desc, username) {
   username <- NULL
   code <- rvenv$currcode
   # add lines at top of file with the description of the branch,
   # consisting of the change history
   #
   # find end of description
   g <- grep('# RV history end',code)
   if (length(g) > 0){
      endline <- g[1]
      toplines <- code[1:(endline-1)]
      toplines <- c(toplines,paste('# Time:',Sys.time(), '\n# Revisited by: ', '\n#',desc))
      code <- c(toplines,code[endline:length(code)])
   } else {
      toplines <-
         c('# RV history start',
           '# WARNING: RV history missing and recreated',
           paste('# Time:',Sys.time(),'\n# Revisited by: ','\n#',desc),'# RV history end')
      code <- c(toplines,code)
   }
   branchname <- paste(rvenv$currbasenm,'.',midfix,'.R',sep='')
   # should add code asking user if OK to overwrite
   writeLines(code,branchname)
}


# set current branch to br, a filename
loadb <- function(br) {
   rvenv$currb <<- br
   tmp <- tools::file_path_sans_ext(br)  # remove '.R'
   tmp <- tools::file_path_sans_ext(tmp)  # remove branch number
   rvenv$currbasenm <<- tmp
   rvenv$currcode <<- readLines(br)
   rvenv$pc <<- 1
   rvenv$desc <<- ""
   g <- grep('# RV history end',rvenv$currcode)
   if (length(g) > 0 & g[1] > 1){
      rvenv$desc <<- rvenv$currcode[g[1]-1]
   }
   # rvenv$firstrunafteredit <<- FALSE
}


# run the code from lines startline through throughline; neither of
# those can be inside a function call or function definition, including
# loops, if(); startline is 1 by default, use 'f' to finish the run from
# the present line, or use 'n' to step just one line


runb <- function(
   startline = 1,
   throughline=length(rvenv$currcode))  {
   if (startline == 'f' || startline == 'n') {
      if (startline == 'n') throughline <- rvenv$pc
      startline <- rvenv$pc
   }
   lcode <- length(rvenv$currcode)
   if (startline < 1 || startline > lcode ||
       throughline > lcode)
      stop('line number out of range')
   execrange <- startline:throughline
   writeLines(rvenv$currcode[execrange],'tmprv.R')
   source('tmprv.R')
   rvenv$pc <- throughline + 1
   # rvenv$firstrunafteredit <<- FALSE
}


# single-step, as with debuggers
nxt <- function() runb('n')


# resume execution from the current line; execution will finish the
# remaining lines, unless throughline is specified
go <- function(throughline=length(rvenv$currcode))
   runb(startline=rvenv$pc,throughline)


# list current code
lcc <- function() {
   code <- rvenv$currcode
   print('next line to execute indicated by ***')
   for (i in 1:length(code)) {
      if (i == rvenv$pc) code[i] <- paste('***',code[i])
      catn(i,code[i])
   }
}


# edit current code; edits the current code file, not an R object; if
# 'listresult', then new code will be printed to the scree
#
# current implementation rather kludgy, repeatedly going back and forthe
# to disk


edt <- function(listresult=FALSE) {
   # rvenv$firstrunafteredit <<- TRUE
   code <- rvenv$currcode
   code <- c('if (FALSE) {',code,'}')
   writeLines(code,'tmprv.R')
   tmprv <- edit(file='tmprv.R')  # tmprv just a dummy to prevent execution
   tmp <<- readLines('tmprv.R')
   rvenv$currcode <<- tmp[c(-1,-length(tmp))]
   # make sure not to return the edited code itself, as it would be executed
   if (listresult) lcc() else return(0)
}


# do one line of code from a branch
docmd <- function(toexec)
   eval(parse(text=toexec),envir=.GlobalEnv)


# to be inserted after each app line that does a plot
pause <- function() {
   readline('hit Enter ')
}


catn <- function(...) {
   cat(...,'\n')
}


# replace t.test() to check for misleadingly low p-value, and also
# adjust for for multiple comparisons; 2-sample only; bonf
# ("Bonferroni") is the number of anticipated comparisons multiple
# inference is desired (many other possibilities for that, e.g.
# p.adjust() in base R)


t.test.rv <- function(x,y,alpha=0.05,bonf=1) {
   alpha <- alpha / bonf
   tout <- t.test(x,y,conf.level=1-alpha)
   muhat1 <- tout$estimate[1]
   muhat2 <- tout$estimate[2]
   tout$p.value <- tout$p.value * bonf
   rvenv$pcount <<- rvenv$pcount + 1
   if (tout$p.value < alpha && muhat1 != 0) {
      if (abs(muhat1 - muhat2)/ abs(muhat1) < rvenv$smalleffect)
         warning(paste('small p-value but effect size',
                       'could be of little practical interest'))
   }
   tout
}


# When user calls lm.rv, we still print lm and everything along with it
# but we check if their y-vals are only made up of 2 distinguished values.
# If it is, we print a warning as well, referring them to logit instead.
lm.rv <- function(formula, user.data){
   lmout <- lm(formula, data = user.data) # call lm for lm.rv
   rqout <- rq(formula,data=user.data) 
   lmc <- coef(lmout)
   rqc <- coef(rqout)
   lmout$rqc <- rqc
   cat('max. prop. difference, linear median regression:',
      max(abs((rqc-lmc)/lmc)),'\n')
   cat('larger values, may indicate outlier or model fit issues\n')
   # check for binary Y
   yval <- lmout$model[[1]]               # extract y-vals from lmout's model
   uyval <- unique(yval)
   class(lmout) <- c("lm.rv", "lm")
   if (length(uyval) <= 1){
      lmout$binaryYval <- TRUE
      print(lmout)
   }
   else if (length(uyval) == 2){
      lmout$binaryYval <- TRUE
      print(lmout)
      warning('only 2 distinct Y values; consider a logistic model')
   }
   else{
      lmout$binaryYval <- FALSE
      print(lmout)
   }
   lmout
}


# finds CIs and p-values, and warns of misleadingly small p-values;
# optionally applies a Bonferroni adjustment; lmobj is an object of
# class 'lm' (including 'glm')
coef.rv <- function(lmobj,alpha=0.05,usebonf=TRUE) {
   cfs <- coef(lmobj)
   lc <- length(cfs)
   if (usebonf) alpha <- alpha / lc
   rvenv$pcount <<- rvenv$pcount + lc
   vc <- vcov(lmobj)
   ses <- sqrt(diag(vc))
   zcut <- qnorm(1-alpha/2)
   sdyhat <- sd(lmobj$fitted.values)
   output <- matrix(nrow=lc,ncol=5)
   output <- data.frame(output)
   for (i in 1:lc) {
      rad <- zcut*ses[i]
      cfi <- cfs[i]
      ci1 <- cfi - rad
      ci2 <- cfi + rad
      tmp <- pnorm(abs(cfi) / ses[i])
      pval <- (2 * (1 - tmp))
      if (usebonf) pval <- lc * pval
      pval <- min(1.0,pval)
      warn <- ''
      if (i > 1 && pval < alpha &&
          cfi * sd(lmobj$model[[i]]) < sdyhat)
         warn <- 'X'
      output[i,1:4] <- c(cfi,ci1,ci2,pval)
      output[i,5] <- warn
   }
   names(output) <- c('est.','left','right','p-val','warning')
   output
}


# copies the given file from the CaseStudies/ directory to the current
# directory
getexample <- function(exname) {
   fullexname <- paste('CaseStudies/',exname,sep='')
   loc <- system.file(fullexname,package='revisit')
   file.copy(loc,'.')
}


# gets GDP data from the Bureau of Economic Analysis web site;
# span="year" (default) or "quarter" - get annual or quarterly GDP data;
# overwrite=FALSE (default) or TRUE  - overwrite local data if already gotten;
# year fields: year, gdp_curr, gdp_const, change=real growth of year (percent);
# quarter fields: quarter, gdp_curr, gdp_const, change=real annualized growth of quarter,
#  change4q=real growth of last 4 quarters; iyear, qtr, & year - calculated from quarter

getGDP <- function(span="year",overwrite=FALSE) {
   datapath <- system.file("data", package="revisit")
   filepath <- paste0(datapath, "/gdplev.xlsx")
   if (!file.exists(filepath) | overwrite == TRUE) {
      download.file("https://www.bea.gov/national/xls/gdplev.xlsx", "gdplev.xlsx", mode = "wb")
      file.rename("gdplev.xlsx", filepath)
   }
   xx <- read_excel(filepath, col_names = FALSE, skip = 8)
   if (span == "year") {
      gg <- as.data.frame(xx[!is.na(xx[,1]),1:3])
      colnames(gg) <- c("year", "gdp_curr", "gdp_const")
      change <- 100 * diff(gg$gdp_const, 1) / gg$gdp_const[1:(length(gg$gdp_const)-1)]
      gg$change <- NA
      gg$change[2:(length(gg$change))] <- change
      gg$year <- as.numeric(gg$year)
   }
   else {
      gg <- as.data.frame(xx[!is.na(xx[,5]),5:7])
      colnames(gg) <- c("quarter", "gdp_curr", "gdp_const")
      change <- 100 * (diff(gg$gdp_const, 1) / gg$gdp_const[1:(length(gg$gdp_const)-1)] + 1)^4 - 100
      gg$change <- NA
      gg$change[2:(length(gg$change))] <- change
      change4q <- 100 * diff(gg$gdp_const, 4) / gg$gdp_const[1:(length(gg$gdp_const)-4)]
      gg$change4q <- NA
      gg$change4q[5:(length(gg$change4q))] <- change4q
      
      gg$iyear <- as.numeric(substr(gg$quarter,1,4))
      gg$qtr   <- as.numeric(substr(gg$quarter,6,6))
      gg$year  <- gg$iyear + (gg$qtr-1)/4
   }
   gdp <- gg
}
