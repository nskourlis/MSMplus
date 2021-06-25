#' EBMT platelet recovery data
#'
#' @description A data frame of 2204 patients transplanted at the EBMT between 1995 and 1998.
#'  These data were used in Section 4 of the tutorial on competing risks and multi-state models 
#'  (Putter, Fiocco & Geskus, 2007). The included variables are
#' @format This data frame has 2204 rows and 10 columns.
#' \describe{
#'   \item{id}{Patient identification number}
#'   \item{prtime}{Time in days from transplantation to platelet recovery or last follow-up}
#'   \item{prstat}{Platelet recovery status; 1 = platelet recovery, 0 = censored}
#'   \item{rfstime}{Time in days from transplantation to relapse or death or last follow-up (relapse-free survival time)}
#'   \item{rfsstat}{Relapse-free survival status; 1 = relapsed or dead, 0 = censored}
#'   \item{dissub}{Disease subclassification; factor with levels "AML", "ALL", "CML"}
#'   \item{age}{Patient age at transplant; factor with levels "<=20", "20-40", ">40"}
#'   \item{drmatch}{Donor-recipient gender match; factor with levels "No gender mismatch", "Gender mismatch"}
#'   \item{tcd}{T-cell depletion; factor with levels "No TCD", "TCD"}
#'  
#' }
#' @source We acknowledge the European Society for Blood and Marrow Transplantation (EBMT) for making available these data.
#'  Disclaimer: these data are used simplified and were taken from the mstate package.
#'  
"ebmt"


#' Heart transplant monitoring data
#'
#' @description A series of approximately yearly angiographic examinations of heart transplant
#'  recipients. The state at each time is a grade of cardiac allograft vasculopathy (CAV), 
#'  a deterioration of the arterial walls.
#' @format This data frame has 2846 rows and 10 columns.There are 622 patients, the rows 
#' are grouped by patient number and ordered by years after transplant, with each row representing
#'  an examination and containing additional covariates.
#' \describe{
#'   \item{PTNUM}{Patient identification number}
#'   \item{age}{Recipient age at examination (years)}
#'   \item{years}{Examination time (years after transplant)}
#'   \item{dage}{Age of heart donor (years)}
#'   \item{sex}{sex (0=male, 1=female)}
#'   \item{pdiag}{Primary diagnosis (reason for transplant)HD=ischaemic heart disease, IDC=idiopathic dilated cardiomyopathy.}
#'   \item{cumrej}{Cumulative number of acute rejection episodes}
#'   \item{state}{State at the examination.State 1 represents no CAV, state 2 is mild/moderate CAV and state 3 is severe CAV. State 4 indicates death.}
#'   \item{firstobs}{0 = record represents an angiogram or date of death. 1 = record represents transplant (patient's first observation)}
#'   \item{statemax}{Maximum observed state so far for this patient (added in version 1.5.1)}

#' }
#' @source Papworth Hospital, U.K.
#' @references Sharples, L.D. and Jackson, C.H. and Parameshwar, J. and Wallwork, J. and Large, 
#' S.R. (2003). Diagnostic accuracy of coronary angiopathy and risk factors for 
#' post-heart-transplant cardiac allograft vasculopathy. Transplantation 76(4):679-82
#'  
"cav"

