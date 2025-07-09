# Run script

# First change the dates in attendance-tidify.R to reflect the current week number
# If the previous release number was not 2 weeks ago, i.e. it was 1 or 3 weeks ago, then change the number in the PA function

# Source script with functions in
source("attendance-tidify.R")

# Run functions
create_reasons_tidy()
create_persistent_absence_tidy()
create_school_returns_tidy()
create_enrol_tidy()