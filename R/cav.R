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
