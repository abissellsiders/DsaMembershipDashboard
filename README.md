# DSA Membership Dashboard

Check out [the interactive dashboard](https://aidanb-dsa-membership-dashboard.share.connect.posit.cloud/)!

The Democratic Socialists of America (DSA) [publicly releases membership data by chapter, by month](https://docs.google.com/spreadsheets/d/1qWH39PiSmFrj8cGtVwMVVZSM9BarvCakKRSKIStcHE0/edit). This level of detail is very rare among social movement organizations (SMOs).

DSA defines its chapter boundaries using zipcodes. Many government and academic sources also produce zipcode-level datasets, which allows direct comparison against DSA membership.

This project compares DSA membership over time, over space, and against social variables in its chapter zipcodes: Demography (incl. education, income, housing, transit, race-ethnicity) from the [American Community Survey](https://www.census.gov/programs-surveys/acs.html); ideology (1-axis left-right) from the [American Ideology Project](https://americanideologyproject.com/); and election results (2016, 2020, 2024) from [Fekrazad 2025](https://www.nature.com/articles/s41597-025-05140-3). This project is inspired by UWash prof Gregory James' "[Mapping American Social Movements](https://depts.washington.edu/moves/)", which graphs the shape of American socialist and civil rights movements over time and across space.

# Want to run locally?

1. Note: Several essential files in `1 Raw/` are not public. Ask me for a copy.
2. Edit `.Renviron_fake` to `.Renviron` and obtain (1) a CARTO basemaps API key and (2) a Census API key.
3. Run `0 Run.R`.