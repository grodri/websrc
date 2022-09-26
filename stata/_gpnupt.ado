program define _gpnupt
*! Coale-McNeil cumulative nuptiality schedule v1 GR 24-Feb-06
	version 9.1
	syntax newvarname=/exp [, Mean(real 25) Stdev(real 5) Pem(real 1)]
	if `mean' <= 0 | `stdev' <= 0 | `pem' <= 0 | `pem' > 1 {
		display as error "invalid parameters"
		exit 198
	}
	tempname z g
	gen `z' = (`exp' - `mean')/`stdev'
	gen `g' = gammap(0.604, exp(-1.896 * (`z' + 0.805)))
	gen `typlist' `varlist'  = `pem' * (1 - `g') 
end	
