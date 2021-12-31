# covid19_analyses

I plot COVID19 data from Austria provided by the government.

## Antibody Titer

ll antibody_values_plots/

antibody_values_plots/ABvalues_output_2021-12-30.pdf
I plot values for IgG RBD antibody titers over time to show the effect of antibodies after three vaccines. I do not control for variation between measurements due to lack of reference values.

## COVID19 Data Austria

ll covid_data_austria

I plot and overlay distinct datasets showing the
1) number of new cases
2) number of hospital beds and ICU beds taken up by COVID19 patients
3) deaths caused by COVID19
4) number of people vaccinated
per day.

covid_data_austria/vaccinations_dose.resolved_2021-12-31.pdf
I plot the number of people who received the first, second and third vaccine.
The percentages show the absolute numbers relative to the population >= 12 years. This threshold was set by Austrian authorities and includes everybody that is old enough to receive the vaccination ("impfbare Bevölkerung").

covid_data_austria/vaccinations_newcases.deaths.hospitalizations.overtime_2021-12-31.pdf
I plot all data available as described above.

covid_data_austria/vaccinations_ratioof.severecases.over.newcases.perwave_2021-12-31.pdf
Same as before, but with waves in the last two years marked in gray. The labels contains
ratio = peak.severe.sickness / sum.positive.cases
per wave.
with
peak.severe.sickness ... max(number of deaths + hospital beds + ICU beds taken by COVID19 patients per day)
sum.positive.cases ... sum(new positive cases per day)

covid_data_austria/vaccinations_newcases.overtime_2021-12-31.pdf
I plot all new positive cases identified per day and the percentage of people vaccinated over time. Cases rise even though >70% of people got at least two doses.

covid_data_austria/vaccinations_newcases.vs.severecases.overtime_2021-12-31.pdf
I plot the percentage of people that received at least one dose and the number of deaths per day + hospital beds and ICU beds taken by COVID19 patients over time. Vaccines do not protect from getting infected, but from having severe symptoms.
