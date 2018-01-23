# Documentation for the outputs generated in the modified update, method 2

## Files created:

`modified_update_<touchstone_target_date>.rds`
`modified_update_<touchstone_target_date>_summary.rds`
`modified_update_<touchstone_target_date>_summary.csv`

### `modified_update_<touchstone_target_date>.rds`

A structured list with 5 elements: 
- `group`: meta data relating to individual `impact_estimate_sets`
- `impacts`: relating the different impacts (`deaths_averted`, `cases_averted`, `fvps_added`) to impact estimate recipes.
- `touchstone_mod`: the touchstone giving the new coverage to be used in this modified update. Xiang calls this the `source touchstone`. (Can this only be a single touchstone at once?)
- `touchstone_use`: vector of touchstones describing the source touchstones to be updated with new coverage.
- `data`: main dataset, each line defined by:
  * `index`: this can be matched to `group`, to retrieve the meta data.
  * `country`
  * `year`
  * then there are a large number of impact, population, coverage, and impact rate variables:
    - `deaths_averted`, `cases_averted`, `fvps`: all imported from the source touchstone. `fvps` have been post-processed to some extent.
	- `coverage_old`, `coverage_new`: imported from the coverage table relating to the source and new touchstones.
	- `coverage_target`, `coverage_target_new`: these are the target populations as given in the coverage table relating to the source and new touchstones. Note that the target populations given for campaigns are real, taking into account age range and geographic extent of the campaign, for routine they are set to NA, and will be filled in by UNWPP national population size of the target age group. This may well differ from the assumptions taken in the original model runs.
	- `pop_total`, `pop_routine`: These are the population sizes (all ages, and for the age group targeted in routine vaccination, for campaigns and routine, respectively) from the UNWPP version associated with the source touchstone (or rather the slightly amended version on Montagu). 
	- A host of impact rates, including c(`deaths`, `cases`), `_averted_rate_`, c(`inst`, `avg`, `tot`, ` `).  
	  $deaths_averted_rate_inst = \frac{deaths_averted_{index, country, year}}{fvps_{index, country, year}}$  
	  $deaths_averted_rate_avg = \frac{\sum_{year = y - 4}^{y + 4} deaths_averted_{index, country, year}}{\sum_{year = y-4}^{y+4}fvps_{index, country, year}}$  
	  $deaths_averted_rate_tot = \frac{\sum_{all years} deaths_averted_{index, country, year}}{\sum_{all years}fvps_{index, country, year}}$  
	  $deaths_averted_rate = deaths_averted_rate_tot$. In method 2 for the modified update we always use the total rate, no exceptions.  
	  deaths_averted_rate_type specifies which of the rates above is the default in each line, but this should now always be tot (or missing). The various different rates are a legacy from the deprecated method 1 for the modified update. 
    - `target_pop_estimated`, `target_pop_estimated_avg`: we reconstructed the target populations that might have been used in the original model runs:  
	  $target_pop_estimated = \frac{fvps}{coverage_old}$, and similarly averaged across the 9-year window for the averaged version.
	- `target_pop_given`, `target_pop_given_new`: For campaigns, the target population sizes specified in the coverage table (so these should match `coverage_target` and `coverage_target_new`. For routine, which was wholly specified through coverage, asssuming that the target population is the national population in the target age group, we filled this in with the relevant UNWPP data (Montagu version). Therefore, for routine the old and new should match, while there may be substantial changes between old and new for campaigns, in particular one or the other being zero where campaings were only recorded for one of the coverage versions. 
	- `fvps_new`, calculated as $fvps_{new} = target_pop_given_{new} * coverage_{new}$. 
	- `deaths_averted_new`, `cases_averted_new`, calculated as $deaths_averted_new = fvps_new * deaths_averted_rate$ etc.
	
###### To do:
- document the repairs done on fvps.
- apparently MHL, TUV and XK are excluded from the modup because we didn't used to have population data from UNWPP, but I believe that Wes has synthesized that, so we should be able to include that now. 
	
	
### `modified_update_<touchstone_target_date>_summary.rds`

A single dataframe where the group and data information from the above list have been merged in, and the source and target touchstones generated in the modified update are placed into a long dataset. Not all variables have been retained, and some have been renamed:

```
cols <- c(country = "country",
            year = "year",
            coverage = "coverage_old",
            population = "target_pop_given",
            population_estimated = "target_pop_estimated",
            deaths_averted_rate = "deaths_averted_rate",
            rate_type = "deaths_averted_rate_type",
            deaths_averted = "deaths_averted",
            fvps = "fvps",
            cases_averted = "cases_averted",
            cases_averted_rate = "cases_averted_rate")
  cols_update <- c(coverage = "coverage_new",
                   deaths_averted = "deaths_averted_new",
                   cases_averted = "cases_averted_new",
                   fvps = "fvps_new",
                   population = "target_pop_given_new")
```



### `modified_update_<touchstone_target_date>_summary.csv`

This is very similar to the summary.rds output, with slight changes in
columns to make them easier to understand maybe? Anything relating to
China is deleted from this file as information in Montagu on China is
sketchy between models and touchstones, beyond China being included in
HepB (and should be counted at 40% - not sure if the recorded impacts
are actually 40% of total impacts or not?)
