# Never executed — this file exists so renv's dependency discovery locks
# packages the pipeline needs at runtime but never names in code.

# purrr::in_parallel() requires carrier (a Suggests of purrr, so renv would
# otherwise not record it) on both the dispatching session and the daemons.
library(carrier)
