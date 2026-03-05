#' MJMS data
#'
#' A dataset 500 simulated patients containing health-related variables.
#'
#' @format A data frame with 500 observations and 16 variables:
#' \describe{
#'   \item{PatientID}{Integer. A unique identifier for each patient (1 to 500).}
#'   \item{Age}{Numeric. Age of the patient in years (ranging from 11 to 91).}
#'   \item{BMI}{Numeric. Body Mass Index (BMI) of the patient (ranging from 12.4 to 41.1).}
#'   \item{Cholesterol}{Numeric. Total cholesterol level (ranging from 107.5 to 292.6).}
#'   \item{Sex}{Factor. Biological sex of the patient, with 2 levels: \code{"Female"} or \code{"Male"}.}
#'   \item{Smoker}{Factor. Smoking status, with 2 levels: \code{"No"} or \code{"Yes"}.}
#'   \item{Treatment_Group}{Factor. Treatment assigned, with 3 levels: \code{"Drug_A"}, \code{"Drug_B"}, and \code{"Placebo"}.}
#'   \item{SBP_Left_Arm}{Numeric. Systolic blood pressure measured on the left arm (mmHg, ranging from 107 to 181).}
#'   \item{SBP_Right_Arm}{Numeric. Systolic blood pressure measured on the right arm (mmHg, ranging from 111 to 181).}
#'   \item{Heart_Disease}{Factor. History or presence of heart disease, with 2 levels: \code{"No"} or \code{"Yes"}.}
#'   \item{Diagnosis_Doc_A}{Factor. Diagnosis result from Doctor A, with 2 levels: \code{"Negative"} or \code{"Positive"}.}
#'   \item{Diagnosis_Doc_B}{Factor. Diagnosis result from Doctor B, with 2 levels: \code{"Negative"} or \code{"Positive"}.}
#'   \item{Gold_Standard}{Factor. True disease status used as the benchmark, with 2 levels: \code{"Disease"} or \code{"Healthy"}.}
#'   \item{New_Test_Result}{Factor. Outcome of a newly developed diagnostic test, with 2 levels: \code{"Negative"} or \code{"Positive"}.}
#'   \item{Symptom_Pre}{Factor. Presence of symptoms prior to treatment, with 2 levels: \code{"No"} or \code{"Yes"}.}
#'   \item{Symptom_Post}{Factor. Presence of symptoms following treatment, with 2 levels: \code{"No"} or \code{"Yes"}.}
#' }
#' @source Simulated data for the \code{mjmstable} package.
"mjms_data"
