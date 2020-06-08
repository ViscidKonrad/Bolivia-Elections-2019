set more off
clear all
set type double

set seed 20200603
*	Set working directory
*cd OAS

*
*	This code updates the earlier CEPR analysis of the 2019 Bolivian Elections
*	to make use of data provided by the TSE
*

*	INPUT is the directory where the OAS RAR file has been unpacked.
*		datos_trep_computo_eg2019 should be a subdirectory
*		the RAR files within that subdirectory should also be unpacked
global INPUT ../Data/TSE
global DATA Data
global OUTPUT ${DATA}/montecarlo/results

*
*	PROBLEM WITH -IMPORT EXCEL-
*		any timestamp data imported as strings comes in with a 12 hour clock and no AM/PM indicator
*		any timestamp data imported as numeric are fine, but not necessarily formatted usefully
*	WHY THIS IS A PROBLEM
*		information may be lost if -IMPORT EXCEL- imports as string, say if misisng values are listed as "null"
*	SOLUTION
*		reach into the underlying XML files and pull data directly as string
*
*	CONSEQUENCE
*		this fix is probably NOT something Nooruddin did for the OAS, so some timestamps may differ
*

capture: program drop unpackXLSX
program define unpackXLSX

	syntax anything(name=xlsxfile), [To(string asis)] [REPLACE]
	
	*
	*	Note that XLSX files are just other stuff packaged in a zip
	*
		
	capture: confirm file `to'
	if (_rc~=0 | "`replace'"=="replace") {
		! unzip `xlsxfile' -d `to'
	}

end

capture: program drop getWorksheet
program define getWorksheet

	syntax [anything(name=sheetname)], From(string asis)

	*
	*	This program reads in an Excel worksheet (in XML)
	*
	
	if ("`sheetname'"=="") {
		local sheetname sheet1
	}
	
	*
	*	First, get any shared strings
	*
	tempfile ss
	*	Add newlines after each cell for easier parsing
	filefilter `from'/xl/sharedStrings.xml `ss', from("</si>") to("</si>\U") replace
	*	Read in the data
	import delimited using `ss', delim("</si>\n", asstring) enc("utf-8") clear stringcols(_all)
	gen sharedString = regexs(1) if regexm(v1,"<si><t[^>]*>([^<]*)")
	drop v1
	drop if mi(sharedString)
	gen stringCode = _n-1
	compress
	save `ss', replace

	*
	*	Read in the sheet XML
	*
	tempfile fft xml
	*	Add newlines after each cell for easier parsing
	filefilter `from'/xl/worksheets/`sheetname'.xml `fft', from("\Q/><c r=\Q") to("\Q/>\U<c r=\Q") replace
	filefilter `fft' `xml', from("</c>") to("</c>\U") replace
	*	Read in the data
	import delimited using `xml', delim("</c>\n", asstring) enc("utf-8") clear stringcols(_all)

	*	Parse out the basics
	gen cell = regexs(1) if regexm(v1,`"c r="([^"]*)""')
	gen row = regexs(1) if regexm(cell,"[A-Z]+([0-9]+)")
	gen col = regexs(1) if regexm(cell,"([A-Z]+)[0-9]+")
	keep if ~mi(row) & ~mi(col)
	gen value = regexs(1) if regexm(v1,"<v>([^<]*)</v>")
	gen isSString = regexm(v1,`"t="s""')
	gen isIString = regexm(v1,`"t="inlineStr""')

	*	Handle shared strings
	gen stringCode = value if isSString
	destring stringCode, force replace
	compress
	merge n:1 stringCode using `ss', keep(match master)
	li if ~mi(stringCode) & _merge==1
	replace sharedString = "p21F" if sharedString=="21F"
	replace sharedString = "PANBOL" if sharedString=="PAN-BOL"
	replace value = sharedString if isSString

	*	Handle inline strings
	gen inlineString = regexs(1) if regexm(v1,"<is><t>([^<]*)</t></is?")
	replace inlineString = "p21F" if inlineString=="21F"
	replace inlineString = "PANBOL" if inlineString=="PAN-BOL"
	replace value = inlineString if isIString

	*	Reshape into rows
	drop v1 cell _merge
	drop isSString-inlineString
	destring row, force replace
	replace col = strlower(col) if length(col)>1
	reshape wide value, i(row) j(col) string
	drop row

	*
	*	Cleanup
	*
	*	Set variable names from first row
	foreach var of varlist value* {
		capture: ren `var' `=substr(subinstr(`var'[1]," ","",.),1,32)'
	}
	drop in 1
	compress

end

capture: program drop getTSE
program define getTSE

	syntax, [TREP] [COMPUTO] [REPLACE]
	
	local count `trep'`computo'
	if ("`count'"=="trepcomputo") {
		local count
	}
	if ("`count'"=="") {
		di as error "Must choose one of TREP or COMPUTO"
		local count computo
	}
	if ("`count'"=="trep") {
		local upfile ${INPUT}/datos_trep_computo_eg2019/trep/OEA_TREP_LogCompleto_19G_2019.11.03.06.53.xlsx
		local sheetname sheet1
	}
	else {
		local upfile ${INPUT}/datos_trep_computo_eg2019/computo/2.RecepcionSobres_final.xlsx
		local sheetname sheet1
	}
	
	capture: use ${DATA}/`count', clear
	if (_rc~=0 | "`replace'"=="replace") {
	
		*	Unpack XLSX to a working directory
		capture: mkdir ${DATA}
		unpackXLSX `upfile', to(${DATA}/`count')

		*	Read in the data
		getWorksheet, from(${DATA}/`count')
		
		*
		*	Specific cleaning
		*

		*	Detect and decode any timestamps
		foreach var of varlist * {
			if (regexm("`var'","Date|Fecha") | regexm(`var'[1],"^[0-9][0-9][0-9][0-9][0-9]\.[0-9]+$")) {
				destring `var', force replace
				replace `var' = round((`var'+td(30dec1899))*86400)*1000
				format %tcDDmonCCYY_HH:MM:SS.sss `var'
			}
		}
		
		*	Destring election numbers
		destring NumMesa CodVer CC-Ins, force replace
		
		*
		*	SKIP THIS NEXT STEP. Nooruddin does NOT assign zero to missing vote data.
		*	That's obviously a bad thing, but he did what he did
		/*
		foreach var of varlist CC-PANBOL Bla Nul {
			replace `var' = 0 if mi(`var')
		}
		*/
		
		*	Remove variable labels (formerly adjusted cell columns)
		foreach var of varlist * {
			capture: label var `var' ""
		}
		
		compress
		save ${DATA}/`count', replace
		
	}

end

capture: program drop doDataPrep
program define doDataPrep

	syntax anything(name=mcf), [IF(string asis)] [REPLACE]
	
	if ("`if'"=="") {
		local if 1
	}
	
	capture: use ${DATA}/montecarlo/base/`mcf', clear
	if (_rc~=0 | "`replace'"=="replace") {

		capture: mkdir ${DATA}/montecarlo
		capture: mkdir ${DATA}/montecarlo/base
		
		*
		*	Fetch the data
		*
		*	TREP
		tempfile tf
		getTSE, trep
		keep if regexm(Ele,"Pres")
		keep if regexm(Est,"Ver") | regexm(Est,"Computada")
		*	filter
		keep if `if'
		*	drop duplicate verification
		egen sumVálidos = rowtotal(CC-PANBOL)
		drop if sumVálidos==0 & NumMesa==2433
		*	other data maintenance
		order NumMesa
		keep NumMesa CC MAS sumV
		gen others = sumV-(CC+MAS)
		compress
		save `tf'
		
		*	merge with computo (to get entire list of actas)
		getTSE, computo
		keep if regexm(Ele,"Pres")
		keep if regexm(Est,"Ver") | regexm(Est,"Computada")
		keep NumMesa Pa-Reci Ins
		compress
		merge 1:1 NumMesa using `tf', nogen
				
		*	Encode the geographies
		gen foreign = Pais~="Bolivia"
		egen fcode = group(foreign)
		egen pcode = group(fcode Pais), missing
		egen dcode = group(pcode Dep), missing
		egen pdcode = group(dcode Prov), missing
		egen mcode = group(pdcode Muni), missing
		egen lcode = group(mcode Loc), missing
		egen rcode = group(lcode Reci), missing
		egen scode = group(rcode NumMesa), missing
				
		save ${DATA}/montecarlo/base/`mcf', replace
		
	}
	
end

capture: program drop doBoliviaMCprep
program define doBoliviaMCprep

	syntax anything(name=mcf), [IF(passthru)] [REPLACE] [NEWBASE]
	
	if ("`newbase'"=="newbase") {
		local newbase replace
	}

	capture: use ${DATA}/montecarlo/prepped/`mcf', clear
	if (_rc~=0 | "`replace'"=="replace") {

		capture: mkdir ${DATA}/montecarlo
		capture: mkdir ${DATA}/montecarlo/prepped

		doDataPrep `mcf', `if' `newbase'

		*	sample tags one counted acta for each geography
		gen ssample = ~mi(CC)
		gen scodex = scode if ssample
		local lg s
		foreach g in r l m pd d p f {
			*	Tag representative acta for each geography
			egen `g'sample = tag(`g'code) if `lg'sample
			*
			*	Key MC variables here:
			*		codex holds the replacement geography at each level
			*		rcodex will be the code for the replacement precinct
			*		scodex will be the code for the replacement acta if necessary
			*
			bys `g'code: egen `g'codex = total(`g'sample), missing
			replace `g'codex = cond(`g'codex,`g'code,.,.)	
			local lg `g'
		}
		
		*	generate lists of possible replacement geographies for MC sampling
		*	 - we want to connect these to each geography, so every acta in each
		*		(country) has a list of possible (departments) from which to draw
		*	 - when we set a replacement (country) for an acta, we will update the
		*		replacement (department) list as well
		local lg f
		qui foreach gc in p d pd m l r s {
			gen `gc'list = ""
			levelsof `lg'code, local(`lg'l) clean
			foreach g of local `lg'l {
				noi di "`lg'=`g' `gc''s"
				levelsof `gc'code if `lg'code==`g' & `gc'sample, local(gl) clean
				replace `gc'list = "`gl'" if `lg'code==`g'
			}
			gen `gc'len = wordcount(`gc'list)
			local lg `gc'
		}
		
		save ${DATA}/montecarlo/prepped/`mcf', replace
	}


end


capture: program drop doMCiter
program define doMCiter

	syntax [, Beta Gamma Sheet]

	*
	*	Single Iteration
	*
	*	Assign replacement geographies
	*		If method is sheet, need to assign all the way down to acta
	local gprefs p d pd m l r
	if ("`sheet'"=="sheet") {
		local gprefs `gprefs' s
	}
	local lg f
	foreach g in `gprefs' {
		*	Update to available replacement geographies
		bys `lg'codex (`lg'sample): replace `g'list = `g'list[_N]
		by `lg'codex (`lg'sample): replace `g'len = `g'len[_N]
		*	Generic geographic indicator for replacement
		tostring `g'codex, generate(g_i)
		*
		*	Key MC operation right here:
		*		randomly pick a replacement geography if codex is missing
		*
		replace g_i = word(`g'list,runiformint(1,`g'len)) if mi(`g'codex)
		destring g_i, replace
		bys `g'code (`g'sample): replace `g'codex = g_i[_N]
		capture: drop g_i
		local lg `g'
	}
	*	Assign replacement parameters and simulate votes
	bys `lg'codex (`lg'sample): egen nx = total(cond(~mi(CC),1,.))
	bys `lg'codex (`lg'sample): egen Ix = total(cond(~mi(CC),Ins,.))
	foreach p in CC MAS others {
		bys `lg'codex (`lg'sample): egen V`p'x = total(cond(~mi(CC),`p',.))
	}
	*	Simulate votes
	if ("`sheet'"=="sheet") {
		foreach p in CC MAS others {
			gen `p'sheet = V`p'x*Ins/Ix
		}
		order CCs MASs otherss, last
	}
	if ("`gamma'"=="gamma") {
		*
		*	Note that Gamma-Poisson is not quite right in that
		*		there is no guarantee we wont have more votes than eligible voters
		*	 - Additional robustness test
		*
		foreach p in CC MAS others {
			if (1) {
				*	Poisson rate per eligible voter is based on an improper
				*		Bayesian prior
				gen par_`p'gamma = rgamma(V`p'x,1/(0+Ix))
				replace par_`p'gamma = 0 if mi(par_`p'gamma)
				bys rcode: gen `p'gamma = rpoisson(Ins*par_`p'gamma[1])
				replace `p'gamma = 0 if mi(`p'gamma)
			}
			else {
				*	Poisson rate per acta in te precinct
				gen par_`p'gamma = rgamma(0.001+V`p'x,1/(1/1000+nx))
				bys rcode: gen `p'gamma = rpoisson(par_`p'gamma[1])
			}
		}
		order CCg MASg othersg, last
	}
	if ("`beta'"=="beta") {
		*
		*	For Beta-Binomial, we are going to make sure that ordering of the parties
		*	does not matter by randomly ordering with every iteration
		*
		*	 - Primary robustness test
		*
		local betalast CC MAS others
		local betafirst = word(`"`betalast'"',runiformint(1,3))
		local betalast: list betalast - betafirst
		local betanext = word(`"`betalast'"',runiformint(1,2))
		local betalast: list betalast - betanext
		macro li _betafirst _betanext _betalast
		gen first = "`betafirst'"
		gen next = "`betanext'"
		gen last = "`betalast'"

		*	For the first party, the binomial probability of an eligible voter
		*		is beta(V,I-V) where V is the total votes for the party in the
		*		representative precinct and I is the eligible voters in the precinct
		*		 - effectively Bayesian with an improper Haldane prior
		gen par_`betafirst'beta = rbeta(V`betafirst'x, Ix                              -V`betafirst'x)
		*	For the second party, the binomial probability is computed similarly,
		*		but based on the eligible voters who did NOT vote for the first party
		gen par_`betanext'beta  = rbeta(V`betanext'x ,(Ix- V`betafirst'x)              -V`betanext'x)
		*	And likewise the third, based on eligible voters who did not vote for the first two
		gen par_`betalast'beta  = rbeta(V`betalast'x ,(Ix-(V`betafirst'x+V`betanext'x))-V`betalast'x)
		
		*	We want the same draw binomial probability draw across all actas
		foreach p in CC MAS others {
			*	clean up where the draw is missing. Should be zero in all cases
			replace par_`p'beta = 0 if mi(par_`p'beta)
			bys rcode: replace par_`p'beta = par_`p'beta[1]
		}
		
		*	Now assign the actual votes based on the actual base of eligible voters
		*		remembering to reduce the base with each party according to the votes
		*		already assigned
		*	Also, clean up any missings as we proceed
		gen `betafirst'beta = rbinomial(Ins                                 ,par_`betafirst'beta)
		replace `betafirst'beta = 0 if mi(`betafirst'beta)
		gen `betanext'beta =  rbinomial(Ins- `betafirst'beta                ,par_`betanext'beta)
		replace `betanext'beta = 0 if mi(`betanext'beta)
		gen `betalast'beta =  rbinomial(Ins-(`betafirst'beta+`betanext'beta),par_`betalast'beta)
		replace `betalast'beta = 0 if mi(`betalast'beta)

	}


end


capture: program drop doBoliviaMC
program define doBoliviaMC

	syntax [anything(name=mcf)], [Beta Gamma Sheet] [REPLACE] [IF(passthru)] [Inner(integer 50)] [Outer(integer 10)]

	*
	*	Monte Carlo projections based on TREP
	*
	*	Options:
	*		run is an identifier; results will be saved in Summaries`run'
	*
	*		beta (Beta-Binomial based on representative precinct)
	*		gamma (Gamma-Poisson based on representative precinct)
	*		sheet (MC sampling of available actas in representative precinct)
	*		<none of above> (NOT MC) Impute votes per eligible voter based on
	*			average across smallest geography with counted actas
	*
	*		replace overwrites the run with new simulation;
	*			otherwise if run has been performed, the program simply loads 
	*			the previous results of the run
	*
	*		inner is the number of MC iterations in each batch to be saved
	*		outer is the number of MC batches to be saved
	*			(inner x outer = total number of iterations)
	*
	
	*	test must imply replace so that the iteration will be performed
	*		it will break before replacing any saved simulations

	if ("`beta'`gamma'`sheet'"=="") {
		doDataPrep `mcf', `if'
		*	Do a simple point estimate
		foreach p in CC MASIPSP others {
			*	Simulated values
			gen `p'x = `p'
		}
		foreach g in r l m pd d p f {
			*	In each geography, total eligible votes on quick-counted actas
			bys `g'code: egen `g'I = total(cond(~mi(CC),Inscritos,.)), missing
			foreach p in CC MAS others {
				*	In each geography, compute party vote rate on quick-counted actas
				bys `g'code: egen `g'`p'r = total(`p'/`g'I), missing
				by `g'code: replace `p'x = Ins*`g'`p'r if mi(`p'x)
			}
		}

		gen counted = ~mi(CC)
		collapse (sum) *x CC MAS others, by(counted)
		gen margin = 100*(MAS-CC)/(MAS+CC+others)
		gen marginx = 100*(MASx-CCx)/(MASx+CCx+othersx)
		li
		
	}
	else {
		capture: use ${OUTPUT}/`mcf', clear
		if (_rc~=0 | "`replace'"=="replace") {
			capture: mkdir ${DATA}
			capture: mkdir ${DATA}/montecarlo
			capture: mkdir ${DATA}/montecarlo/results
			*
			*	Monte Carlo
			*
			tempfile ti
			qui forvalues ii=1/`outer' {
				*	We might have already done this batch...
				capture: use ${OUTPUT}/`mcf'/batches/`ii', clear
				if (_rc~=0 | "`replace'"=="replace") {
					capture: mkdir ${OUTPUT}/`mcf'
					capture: mkdir ${OUTPUT}/`mcf'/batches
					tempfile tj
					forvalues jj=1/`inner' {
					
						noi di as text "Imputation number: " as result (`ii'-1)*`inner'+`jj'
						
						doBoliviaMCprep `mcf', `if'
						doMCiter, `beta' `gamma' `sheet'
						
						local byvars
						if ("`beta'"=="beta") {
							local byvars by(first next last)
						}
						collapse (sum) CC* MAS* others*, `byvars'
						noi li
						if (`jj'>1) {
							append using `tj'
						}
						save `tj', replace
					}
					*	save this batch
					save ${OUTPUT}/`mcf'/batches/`ii', replace
				}
				*	Add this batch of to the full batch
				if (`ii'>1) {
					append using `ti'
				}
				save `ti', replace
				*	Calculate the margins and report on the distribution
				local s
				gen margin`s' = 100*(MAS`s'-CC`s')/(MAS`s'+CC`s'+others`s')
				foreach s in `beta' `gamma' `sheet' {
					gen margin`s' = 100*(MAS`s'-CC`s')/(MAS`s'+CC`s'+others`s')
				}
				*	Histogram
				discard
				local otypes CC
				unab type: CC*
				local type: list type-otypes
				local type = substr("`type'",3,.)
				sum margin`type', meanonly
				hist margin`type', caption(`"`=_N' iterations of imputation type `type'"') ///
					xti("MAS-IPSP - CC (% of valid votes)") yti("") ///
					freq xlab(`=ceil(10*`r(min)')/10'(0.1)`=floor(10*`r(max)')/10')
			}
			capture: drop margin*
			*	Save the full batch of simulations
			save ${OUTPUT}/`mcf'/`mcf', replace
		}
		local otypes CC
		unab type: CC*
		local type: list type-otypes
		local type = substr("`type'",3,.)
		*	Calculate MAS margins
		gen margin = 100*(MAS`type'-CC`type')/(MAS`type'+CC`type'+others`type')
		*	Histogram
		sum margin, meanonly
		hist margin, /*caption(`"`=_N' iterations of imputation type `type'"')*/ ///
			xti("MAS-IPSP - CC (% of valid votes)") yti("") ///
			freq xlab(`=ceil(10*`r(min)')/10'(0.1)`=floor(10*`r(max)')/10')
		graph export ${OUTPUT}/`mcf'/`mcf'.png, replace
		*	Reporting
		if ("`type'"=="beta") {
			* Check that Beta-Binomial party ordering doesn't matter for beta by
			*	disaggregating results by ordering
			table first next, c(mean margin)
		}
			codebook margin
	}
	
end

*	Options are:
*		sheet (MC sampling of available actas in representative precinct)
*		beta (Beta-Binomial based on representative precinct)
*		gamma (Gamma-Poisson based on representative precinct)
*		<empty> (NOT MC) Impute votes per eligible voter based on average across
*				smallest geography with counted actas
local type sheet
*	May be just about anything here. Used to save simulations in case
*		you want to come back to them
local run 2

doBoliviaMC `run', if(dofc(VerificadorDate)<td(21oct2019)) `type' i(50) o(10)
