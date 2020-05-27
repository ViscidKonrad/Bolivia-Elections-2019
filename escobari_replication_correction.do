set more off
pause off
clear all
set type double
set matsize 4096
*	This program will create files in the current working directory
*	Please adjust as desired
*cd ~/Bolivia

capture: program drop getSummary
program define getSummary

	*	This program simply gets the Excel results by acta based on the source
	*		and timestamp provided

	args source timestamp

	local filename acta.`timestamp'
	capture: use Summaries/`source'/`filename', clear
	if (_rc~=0) {
		capture: confirm file Summaries/`source'/`filename'.xlsx
		if (_rc~=0) {
			di as error "NOTE: WEBSITE HAS BEEN ERASED. PLEASE MANUALLY SUPPLY DATA FILE"
			/*
			local URL https://`source'.oep.org.bo/PubResul/`filename'.xlsx
			capture: mkdir Summaries
			capture: mkdir Summaries/`source'
			copy `"`URL'"' Summaries/`source'/`filename'.xlsx, replace
			*/
		}
		import excel using Summaries/`source'/`filename'.xlsx, clear firstrow case(preserve)
		compress
		save Summaries/`source'/`filename', replace
	}

end

*
*	Produce data set
*
capture: use escobari_data, clear
if (_rc~=0) {

	*
	*	Get the OEP data
	*

	tempfile tf
	*	84% TREP
	getSummary trep 2019.10.20.19.40.57
	keep if regexm(Ele,"Pres")
	save `tf'
	*	Final computo
	getSummary computo 2019.10.25.21.09.30
	keep if regexm(Ele,"Pres")
	merge 1:1 NúmeroMesa using `tf'

	*
	*	Additional variables
	*
	*	Actual sum of valid votes for each acta
	egen sumV = rowtotal(CC-PANBOL)
	gen Others = sumV-(MASIPSP+CC)
	*	inEarlyTREP = 1 if in first 84%
	gen inEarlyTREP = _merge==3
	*	Original geocodes
	egen mcode0 = group(Muni)
	egen rcode0 = group(Rec)
	*	Corrected geocodes
	*egen mcode1 = group(Pa Dep Pro Mun)
	*egen rcode1 = group(mcode1 Loc Rec)
	egen mcode1 = group(Pa Númerodepartamento Departamento Provincia Númeromunicipio Municipio)
	egen rcode1 = group(mcode1 Localidad Recinto)
	
	*
	*	Preliminaries
	*

	egen amtag = tag(mcode1)
	egen artag = tag(rcode1)
	bys mcode1: egen anp = total(artag) 
	count if amtag
	count if artag
	tab anp if amtag, m

	*	Example muni duplication errors in original
	egen mtag = tag(mcode0 mcode1)
	bys mcode0: egen nmtag = total(mtag)
	table Muni if nmtag>1 & mtag, row col
	table Dep Pro if Muni=="Santa Rosa" & mtag, row col
	*	Example precinct duplication errors in original
	egen ptag = tag(rcode0 rcode1)
	bys rcode0: egen nptag = total(ptag)
	table Rec ptag if nptag>10 & ptag, row col
	table Pa if Rec=="Embajada" & ptag, row col

	*	Reshape for easier tabulation
	foreach var of varlist CC-Nulos sumV {
		*	Votes
		gen V_`var' = `var'
		*	Shares by acta (original denominator)
		gen s_`var' = 100*`var'/VotosV
		*	Shares by acta (correct denominator)
		gen c_`var' = 100*`var'/sumV
	}
	gen margin =s_MAS-s_CC
	gen cmargin =c_MAS-c_CC
	save escobari_data, replace
}


*	Marking capital cities
local dlist Beni Chuquisaca Cochabamba Paz Oruro Pando Potosí Cruz Tarija
local mlist Trinidad Sucre Cochabamba Señora Oruro Cobija Potosí Sierra Tarija
gen isCap = 0
forvalues ii=1/`:list sizeof dlist' {
	di `"replace isCap = regexm(Dep,"`:word `ii' of `dlist''") & regexm(Muni,"`:word `ii' of `mlist''") if isCap==0"'
	replace isCap = regexm(Dep,"`:word `ii' of `dlist''") & regexm(Muni,"`:word `ii' of `mlist''") if isCap==0
}
*replace isCap = 1 if regexm(Dep,"Paz") & regexm(Mun,"El Alto")
*replace isCap = 1 if Pa~="Bolivia"

*	Capital cities and El Alto
gen grp = 2*isCap
replace grp = -1 if Pa~="Bolivia"
replace grp = 1 if regexm(Dep,"Paz") & regexm(Mun,"El Alto")

*	shares and margins
gen mass = 100*MAS/sumV
gen ccs = 100*CC/sumV
gen mar = mass-ccs
gen isLate = ~inEarly

table grp [w=sumV], c(mean isLate mean mass mean ccs mean mar)
tab grp if isLate [w=sumV] //, format(%20.0fc)

*
*	Results
*

*	Table 1
foreach var of varlist V_* {
	table inEarly, c(sum `var') format(%15.0fc) row col
}

*	Table 2 (original)
scalar sums = 0
scalar sums2 = 0
scalar sums3 = 0
qui foreach var of varlist MAS CC PDC F MTS MNR PAN UCS FPV {
	reg s_`var' ib(1).inEarly, r
	estimates store os`var'
	lincom _cons
	scalar sums = `r(estimate)'+sums
	lincom 0.inEarly
	scalar sums2 = `r(estimate)'+sums2
	lincom 0.inEarly+_cons
	scalar sums3 = `r(estimate)'+sums3
}
qui foreach var of varlist Bla Nul {
	reg s_`var' ib(1).inEarly, r
	estimates store os`var'
}
scalar sums4 = 0
qui foreach var of varlist MAS CC PDC F MTS MNR PAN UCS FPV {
	reg `var' ib(1).inEarly, r
	estimates store oc`var'
	lincom _cons*34555+0.inEarly*5580
	scalar sums4 = `r(estimate)'+sums4
}
qui foreach var of varlist Bla Nul {
	reg `var' ib(1).inEarly, r
	estimates store oc`var'
}
estimates table os*, b(%6.4fc) se(%6.4fc) stats(N r2)
estimates table oc*, b(%6.4fc) se(%6.4fc) stats(N r2)
qui est restore osMASIPSP
margins inEarly, grand
noi di sums
noi di sums2
noi di sums3
noi di %20.0fc sums4

*	Table 2 (correctly denominated)
scalar sums = 0
scalar sums2 = 0
scalar sums3 = 0
qui foreach var of varlist MAS CC PDC F MTS MNR PAN UCS FPV {
	reg c_`var' ib(1).inEarly, r
	estimates store ms`var'
	lincom _cons
	scalar sums = `r(estimate)'+sums
	lincom 0.inEarly
	scalar sums2 = `r(estimate)'+sums2
	lincom 0.inEarly+_cons
	scalar sums3 = `r(estimate)'+sums3
}
qui foreach var of varlist Bla Nul {
	reg c_`var' ib(1).inEarly, r
	estimates store ms`var'
}
estimates table ms*, b(%6.4fc) se(%6.4fc) stats(N r2)
qui est restore msMASIPSP
margins inEarly, grand
noi di sums
noi di sums2
noi di sums3

*	Table 2 (correctly denominated and weighted)
scalar sums = 0
scalar sums2 = 0
scalar sums3 = 0
qui foreach var of varlist MAS CC PDC F MTS MNR PAN UCS FPV {
	reg c_`var' ib(1).inEarly [w=sumV], r
	estimates store cs`var'
	lincom _cons
	scalar sums = `r(estimate)'+sums
	lincom 0.inEarly
	scalar sums2 = `r(estimate)'+sums2
	lincom 0.inEarly+_cons
	scalar sums3 = `r(estimate)'+sums3
}
qui foreach var of varlist Bla Nul {
	reg c_`var' ib(1).inEarly [w=sumV], r
	estimates store cs`var'
}
estimates table cs*, b(%6.4fc) se(%6.4fc) stats(N r2)
qui est restore csMASIPSP
margins inEarly, grand
lincom 0.inEarly*9817.13
lincom 0.inEarly*981713/6137671
noi di sums
noi di sums2
noi di sums3

*	Table 3 (original)
reg margin ib(1).inEarly, r
lincom 0.inEarly*981713/6137671
areg margin ib(1).inEarly, a(mcode0) vce(robust)
areg margin ib(1).inEarly, a(rcode0) vce(robust)
lincom 0.inEarly*981713/6137671
margins inEarly, grand

*	Table 3 (correctly weighted)
reg cmargin ib(1).inEarly [w=sumV], r
lincom 0.inEarly*981713/6137671
areg cmargin ib(1).inEarly [w=sumV], a(mcode0) vce(robust)
areg cmargin ib(1).inEarly [w=sumV], a(rcode0) vce(robust)
margins inEarly, grand
lincom 0.inEarly*981713/6137671

*	Table 3 (corrected FE)
reg margin ib(1).inEarly, r
est store noFE
lincom 0.inEarly*981713/6137671
areg margin ib(1).inEarly, a(mcode1) vce(robust)
est store muniFE
areg margin ib(1).inEarly, a(rcode1) vce(robust)
est store precinctFE
margins inEarly, grand
lincom 0.inEarly*981713/6137671

gen mcc = MAS-CC
reg mcc ib(1).inEarly, r
est store noFEc
areg mcc ib(1).inEarly, a(mcode1) vce(robust)
est store muniFEc
areg mcc ib(1).inEarly, a(rcode1) vce(robust)
est store precinctFEc
margins inEarly, grand

estimates table *FE*, b(%6.3fc) se(%6.4fc) stats(N r2)


*	Table 3 (correctly weighted and corrected FE)
reg cmargin ib(1).inEarly [w=sumV], r
est store noFEf
lincom 0.inEarly*981713/6137671
areg cmargin ib(1).inEarly [w=sumV], a(mcode1) vce(robust)
est store muniFEf
areg cmargin ib(1).inEarly [w=sumV], a(rcode1) vce(robust)
est store precinctFEf
margins inEarly, grand
lincom 0.inEarly*981713/6137671

estimates table *FEf *FEc, b(%6.3fc) se(%6.4fc) stats(N r2)
