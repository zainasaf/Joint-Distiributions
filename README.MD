Survey Weighting Diagnostics Overview This repository contains the R scripts used to evaluate how well the weighting scheme applied to our polling data reproduces the joint demographic distributions observed in the UK Census. The analysis uses the Resegmentation survey (April 2025) as the polling source and compares weighted and unweighted sample distributions against 2021 Census benchmarks across 14 joint demographic distributions. The goal is to diagnose where the current weighting scheme performs well, where it falls short, and where refinements may be needed.

Many of the sharpest political divisions in the UK run along intersections of demographics — not single characteristics in isolation. A sample that matches census marginals for age, ethnicity, and education individually can still misrepresent the population if it gets the combinations wrong. For example, younger and older people within the same ethnic group often hold very different political views, and ethnic minority populations are concentrated in specific age bands and regions. This analysis checks how well the survey captures these joint distributions.

Census Data The census benchmark files are not included in this repository and must be downloaded before running the scripts. The raw files come from two separate sources

🏴 **Scotland: Download joint distribution tables from Scotland's Census. File names should follow the pattern scot*.csv
🏴󠁧󠁢󠁥󠁮󠁧󠁿 England & Wales:** Download joint distribution tables from the ONS Census. File names should follow the pattern ew*.csv
The following joint demographic tables are required from both Scotland and England & Wales: age by education, age by ethnicity, age by sex, age by country of birth, age by religion, age by socio-economic status, ethnicity by socio-economic status, and religion by country of birth. An additional age by Welsh language ability table is required from the England & Wales census only.

You will need to download the following joint distribution tables from each source:

Joint Distribution Variables

|Age × Place of Birth
|Age × Ethnicity × Degree | Age × Ethnicity × Occupation | Born UK × Religion | Age × Ethnicity | Sex × Ethnicity | Welsh Language × Age | Welsh language ability, age (England & Wales only) | Region × Socio-economic Status | Religion × Degree
| Ethnicity × Degree | Age × Sex × Degree | Religion × Place of Birth | religion, country | Born UK × Age | Age × Degree

Once downloaded, save the raw files into the all_census_data/ folder before running scot_ew_merge.R. That script will process and merge the Scotland and England & Wales files into combined both_*.csv files which are then used by all subsequent scripts.

Scripts scot_ew_merge.R Prepares and harmonises census data from Scotland and England & Wales into a unified set of joint distribution files used as benchmarks throughout the analysis.

Reads raw census CSV files for both nations Applies consistent recoding maps for age, education, ethnicity, country of birth, religion, and socio-economic status across the two different census formats Merges Scotland and England & Wales data, aggregating counts and computing population percentages Outputs a set of both_*.csv files — one per joint distribution — used as census benchmarks in subsequent scripts

poll_census_combine_Dec_13.Rmd The main diagnostic script that compares survey distributions against census benchmarks and computes bias reduction statistics.

Harmonises poll and census variables into a common coding scheme For each joint distribution, computes cell-level comparisons between the census, the unweighted poll, and the weighted poll Calculates the following metrics for every cell:

diff_unw = Unweighted poll minus census (signed bias); diff_wt= Weighted poll minus census (remaining bias); abs_diff_unw / abs_diff_wt = Absolute versions of the previous; bias_reduction = Reduction in absolute bias from weighting bias; bias_red_pct = Percentage reduction in absolute bias

Computes Mean Absolute Bias (MAB) before and after weighting, both unweighted across cells and census-weighted (so larger population cells contribute more) Outputs joined comparison files and a summary bias table

Joint Distributions Examined The analysis covers the following 14 joint distributions:

Age × Place of Birth (born UK / born outside UK) Age × Ethnicity × Degree Age × Ethnicity × Occupation Born UK × Religion Age × Ethnicity Sex × Ethnicity Welsh Language Speaking Ability × Age Region × Socio-economic Status Religion × Degree Ethnicity × Degree Age × Gender × Degree Religion × Place of Birth Born UK × Age Age × Degree
