# Sex differences in COVID-19 mortality in the United States : A state-level analysis

Male mortality due to COVID-19 has been found to be higher than women's due to a combination of biological and behavioural factors. The United States has been one of the countries most affected by the COVID-19 pandemic, with large variations at the state and county levels in infection and mortality rates. In this paper, we assess sex differentials in COVID-19 mortality across US states and compare them with sex differentials in all-cause mortality, as well as six leading causes of death. We establish the male disadvantage in COVID-19 mortality, which is systematically higher than for all-cause mortality. We also describe the trajectory of male-to-female COVID-19 mortality ratios across ages, showing that male disadvantage decreases significantly in the 85+ age group. Although there is some convergence between states as age increases, Western states consistently exhibit the largest male excess mortality, and Southern States, the lowest.

## Where we are now

Using the COVerage database, we estimated the number of COVID-19 related deaths in every state and age group by month. By using the population per age group provided by the Census Bureau, we calculated the monthly mortality rate and determined the male-excess mortality rate by month. A first preview of the problem can be observed through heatmaps.

First, with linear-intrapolated deaths :

![Heatmap 1](https://github.com/PietroViolo/covid_sexdiff/blob/main/Graphs/Graphs_linear/Texas_excess.png)

Second, with monotonic splines :

![Heatmap 1](https://github.com/PietroViolo/covid_sexdiff/blob/main/Graphs/Texas_excess.png)
