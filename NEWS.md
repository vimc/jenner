### jenner 0.0.9
 * (VIMC-1302): Remove the work done for (VIMC-1266). Because of mr_measles migration, no longer need to treat mr_measles as a special case.
 * (VIMC-1302): Add a function to generate year of introduction in summary outputs. Avoided touching existing functions. Instead, this function is exported and can be called for reports.
 
### jenner 0.0.8
 * (VIMC-1074) re-value Inf/NaN impact_new 
 * (VIMC-1043) remove references to tr_vaccine in jenner - for relevant reports, can safely remove tr_vaccine.csv from meta/ and orderly.yml
 * (VIMC-1266) dealing with the problem of missing sdf7 PSU Measles campaign impact
 
### jenner 0.0.7 (2017-12-14)
 * In `create_touchstone.R`, correct responsibility status from 'approved' to 'incomplete'. 'incompelte' is the default status when a new touchstone is open.

### jenner 0.0.6 (2017-11-22)
 * create synthetic fvps for situations where - fvps is blank but given coverage 

### jenner 0.0.5 (2017-11-21)
 * start work on touchstone import
 * export `database_connection`

### jenner 0.0.4 (2017-11-16)
 * correct impact rate and impact rate type in output files
 * eliminate warning message caused by updating multiple touchstones in one report
 * remove unused codes
 * export mu_scale

### jenner 0.0.3 (2017-11-09)
 * fix suffix in summary_output
 * fix migrate coverage.sql to allow 'bestminus' type of activity (SDF6/7)
