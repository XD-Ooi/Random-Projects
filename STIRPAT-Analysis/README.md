For this project, I ran a multivariate linear panel model to study the factors affecting environmental impact under the STIRPAT model, covering the affluence, population, and technology factor.
This project focuses on studying how economic freedom and political freedom relates to environmental impact. The study also employed a different observation segmentation method to provide new insights.

Specifically, I ran the following regression model:
* co2pc = 𝛽0 + 𝛽1gdppc + 𝛽2popk + 𝛽3urbanpop + 𝛽4industry + 𝛽5econfree + 𝛽6polifree + TimeFixedEffects + e (for KPAIC countries); and
* co2pc = 𝛽0 + 𝛽1gdppc + 𝛽2popk + 𝛽3urbanpop + 𝛽4industry + 𝛽5econfree + 𝛽6polifree + e (for non-KPAIC countries)

where
* co2pc: carbon emission per capita
* gdppc: GDP per capita
* urbanpop: urbanization rate
* industry: value added of secondary industry (% of gdp)
* econfree：economic freedom index
* polifree: political freedom index
* KPAIC countries: Kyoto Protocol Annex-I countries that have completed their first committment target


## License
This work is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/). 
