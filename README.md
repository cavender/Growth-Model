# Growth-Model
Predictions for growth of perennial plants with dormant and non-dormant seeds with variation in seed size and growing season length

This is a conceptual model to show the logic of our predictions about how seed dormancy is likely to vary with the length of the unfavorable season for a given seed size. The assumptions are formalized mathematically. 

Total biomass (Y) of a population cohort of a perennial plant species, classified as either having dormant or non-dormant seeds, is tracked for two years. The year is divided into favorable (L) and unfavorable (Lmax – L) seasons, where Lmax represents 365 days. The average seed mass (ms) for both dormant and non-dormant seeds is inversely proportional to the number of seeds (n), such that 

		ms = Y0/n, 												(Eq.1)

where Y0 is the total cohort seed mass at the time of seed maturation.

Plants with dormant and non-dormant seeds differ as follows:


Dormant seeds (strategy 1 in the exponent of Y):

In the dormant seed strategy, seeds remain dormant during the first season and do not germinate. During the unfavorable season, a seed of mass ms has a daily mortality rate (μs).

		μ_s=s-  1/(s+ m_s),									            (Eq.2)

where s is a mortality parameter for seeds. Thus, if n is the number of seeds, the new cohort biomass at the beginning of the second year is

		Y_1^1=n m_s (1-μ_s  (L_max-L)/L_max   ).				(Eq.3)

The seeds will germinate with a probability p and will grow exponentially at a constant rate r. The total biomass at the end of the second favorable season is
	
		Y_final^1=n Y_1^1  p e^rL.								      (Eq.4)



 
Non-dormant seeds (strategy 2 in the exponent of Y):

A non-dormant seed (ms) matures for τ days and germinates with the same probability as the dormant seeds (p). The total biomass of the emergent seedlings at the end of the favorable season (Y_g^2) is:

		Y_g^2=n m_s  p e^(r(L-τ)).								      (Eq.5)


During the unfavorable season, a seedling of mass m_g is subject to an (external) daily mortality (μ_g) 


		μ_g=1-(w- 1/(1+Exp(m_g ) )),							      (Eq.6)


where w is a mortality parameter for seedlings. The seedling mass (mg) at the start of the unfavorable season will be determined by the cohort growth (Y_g^2) in Eq. 5 as a function of seed mass (ms), which is inversely proportional to the number of seeds produced (n) such that mg = Y_g^2/n. 

The total biomass at the end of the first year is thus:


		Y_1^2= Y_g^2 (1-μ_g  (L_max-L)/L_max).					(Eq.7)


The seedlings continue to grow during the second favorable season. The final cohort biomass is thus:


		Y_final^2=Y_1^2 (1+e^rL).								      (Eq.8)




Parameters	Definition										                  Units		    Values
Y0	initial cohort seed biomass at time of seed maturation	g			      100
r	intrinsic growth rate									                    g g-1 d-1	  0.001
p	probability of germination											                      0 to 1
L	favorable season length									                  d			      0 to 365
Lmax	number of  days in the year; maximum season length	  d			      365
ms	seed mass												                        g			      0.01 to 1 
s	seed mortality constant									                  1
τ	seed maturation time 									                    d			      0 to 100
w	seedling mortality constant											                      0.1 – 1.0
			
Variables			
Ytx cohort yield; seed or plant biomass at time t for dormancy model (x=1) or non-dormancy model (x=2)	g	
n	total number of seeds in cohort;  n = Y0/ ms  		
μs 	seed mortality rate	d-1	
mg	mass of germinated seedling	g	
μg	seedling daily mortality during the unfavorable period	g d-1	

