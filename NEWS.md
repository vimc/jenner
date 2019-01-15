### jenner 0.0.24
 * VIMC-2492. Expand fix_coverage_fvps to allow specified gavi_support_levels

### jenner 0.0.22
 * VIMC-2382. Updating future projection constrain.

### jenner 0.0.21
 * Required by VIMC-2255. Constrain Rubella routine total_fvps to be RCV1+RCV2 for 201710gavi/PHE model only; scenarios in later touchstones should have RCV1 and RCV2 evaluated seperately.
 
### jenner 0.0.20
 * Required by VIMC-2172. Fixing native impact rates (routine) where there are fvps given, but NA deaths averted, which casued under-estimation of impact rates.
 
### jenner 0.0.19
 * Required by VIMC-1892.Allows impact calcualation to include maxium age of interest.
 
### jenner 0.0.18
 * VIMC-1976 allow differnt coverage touchstone and population touchstone, so that fix_coverage_fvps function can be flexibly used for interim update
 
### jenner 0.0.17
 * VIMC-1758 constrain gavi_support_level to bestminus and gavi, so that fix_coverage_fvps function can be flexibly used for interim update

### jenner 0.0.16
 * VIMC-1831 ignore touchstone version -42, so that fix_coverage_fvps can be reusable for other touchstones
 
### jenner 0.0.15
 * VIMC-1684 impact_method2 sql modification post Wes' touchstone fast forward

### jenner 0.0.13
 * VIMC-1611 constrain the number of countries for HepB impact calculation - because different scenarios have different number of countries 

### jenner 0.0.12
 * VIMC-1591 add more routine vaccine age 
 * VIMC-1591 undo the constrain on coverage_old when touchstone_new = touchstone_old
 * VIMC-1591 add three columns in the output for method2 impact calculation - population, fvps and coverage
 
### jenner 0.0.11
 * VIMC-1380 method 2 impact calculation  
 * Currently impact calculation is assisted by .csv recipe; this will be changed when recipe is imported montagu
 * Both direct impact caculation and method2 (re-allocate impact by fvps * da_rate) are provided

### jenner 0.0.10
 * Fix bug with `insert_values_into` where `key` was given - this probably broke with the RPostgres upgrade (VIMC-1467)

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
