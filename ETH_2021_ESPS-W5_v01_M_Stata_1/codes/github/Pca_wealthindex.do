
cd"/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1"
use "/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect11_hh_w5.dta", replace


replace s11q01 = 0 if missing(s11q01)

tabulate asset_cd, generate(dummy_asset)

// creating the dummy variable

* Generate dummy variables for each unique asset_cd

forvalues i = 1/37  {
    gen asset_`i' = s11q01 if asset_cd == `i' & s11q00 == 1
} 

forvalues i = 1/37 {
    replace asset_`i' = 0 if missing(asset_`i')
}


forvalues i = 1/37 {
    bysort household_id: egen sum_asset_`i' = total(asset_`i')
}


duplicates drop household_id, force


drop asset*
drop dummy_asset*


corr sum_asset*  //correlation is not very high

// Principal component analysis
pca sum_asset*

screeplot, yline(1)

predict pca1, score

estat kmo 
//the value (0.8175) is greater than 0.5 thus the sample adeqacy requirement is fulfilled

// for rural part
* Initialize the wealth index variable
gen wealth_index = .

* Loop to perform PCA separately for rural (0) and urban (1) households
forvalues saq14 = 1/2 {
    * Perform PCA on the asset variables for rural (0) and urban (1) households separately
    factor sum_asset_1-sum_asset_37 if saq14 == `saq14', pcf factors(12)
    
    * Predict the scores for the first principal component
    predict temp_index if saq14 == `saq14'
    
    * Store the predicted values in the wealth_index variable
    replace wealth_index = temp_index if saq14 == `saq14'
    
    * Drop the temporary variable
    drop temp_index
}


corr pca1 wealth_index

estat kmo 

/// the value (0.6473) is greater than 0.5 thus the sample adeqacy requirement is fulfilled

drop sum_asset*
drop pw_w5-s11q02_2

save hh11_w5_pca.dta, replace



