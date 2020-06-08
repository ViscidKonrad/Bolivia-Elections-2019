set more off
clear all
set type double

set seed 20200603
*	Set working directory
*cd ~/OAS 

*
*	This code updates the earlier CEPR analysis of the 2019 Bolivian Elections
*	to make use of data provided by the TSE
*

*	INPUT is the directory where the OAS RAR file has been unpacked.
*		datos_trep_computo_eg2019 should be a subdirectory
*		the RAR files within that subdirectory should also be unpacked
global INPUT ../Data/TSE
global DATA Data
global OUTPUT ${DATA}/reanalysis/results

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

	syntax anything(name=mcf), [REPLACE] [KEEPZero] [Degree(integer 0)]
	
	if ("`if'"=="") {
		local if 1
	}
	
	capture: use ${DATA}/reanalysis/base/`mcf', clear
	if (_rc~=0 | "`replace'"=="replace") {

		capture: mkdir ${DATA}/reanalysis
		capture: mkdir ${DATA}/reanalysis/base
		capture: mkdir ${DATA}/reanalysis/graph
		
		*
		*	Fetch the data
		*
		*	TREP
		tempfile tf
		getTSE, trep
		keep if regexm(Ele,"Pres")
		keep if regexm(Est,"Ver") | regexm(Est,"Computada")
		*	drop duplicate verification
		egen sumVálidos = rowtotal(CC-PANBOL)
		drop if sumVálidos==0 & NumMesa==2433
		*	other data maintenance
		keep NumMesa VerificadorDate FechaReg
		compress
		save `tf'
		
		*	merge with computo (to get entire list of actas)
		getTSE, computo
		keep if regexm(Ele,"Pres")
		keep if regexm(Est,"Ver") | regexm(Est,"Computada")
		if ("`keepzero'"=="keepzero") {
			replace MAS = 0 if mi(MAS)
			replace CC = 0 if mi(CC)
		}
		replace MAS = 100*MAS/Emit
		replace CC = 100*CC/Emit
		keep NumMesa Pa-Reci CC MAS Emit
		compress
		merge 1:1 NumMesa using `tf', nogen

		keep if ~mi(VerificadorDate)
		sort VerificadorDate NumMesa
		sort FechaReg NumMesa
		gen pV = sum(Emit)
		replace pV = 100*pV/pV[_N]
		gen inSample = inlist(NumMesa,28177,24862,31056) | VerificadorDate<tc(20oct2019 20:03:59)
		replace inSample = dofc(FechaReg)<td(21oct2019)
		
		preserve
		lpoly CC pV if inSample, degree(`degree') bwidth(4.75) nogra gen(cx0 cs0)
		lpoly CC pV if ~inSample, degree(`degree') bwidth(0.5) nogra gen(cx1 cs1)
		lpoly MAS pV if inSample, degree(`degree') bwidth(6) nogra gen(mx0 ms0)
		lpoly MAS pV if ~inSample, degree(`degree') bwidth(0.5) nogra gen(mx1 ms1)
		keep NumMesa CC MAS pV inSample cx0 cs0 cx1 cs1 mx0 ms0 mx1 ms1
		twoway (scatter MAS pV, msize(vtiny)) (line ms0 mx0, lw(vthick)) (line ms1 mx1, lw(vthick)) ///
			, yline(50) xline(95) xti(" " "Share of Emitidos Counted in TREP") yti("Morales Share of Emitidos On Acta") legend(off)
		gen sort_order = _n
		save ${DATA}/reanalysis/graph/`mcf', replace
		restore

		keep NumMesa Pa-Reci CC MAS pV inSample
		
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
		
		compress
		save ${DATA}/reanalysis/base/`mcf', replace
		
	}
	
end

capture: program drop doBoliviaMCprep
program define doBoliviaMCprep

	syntax anything(name=mcf), [REPLACE] [NEWBASE] [Degree(passthru)]
	
	if ("`newbase'"=="newbase") {
		local newbase replace
		local replace replace
	}

	capture: use ${DATA}/reanalysis/prepped/`mcf', clear
	if (_rc~=0 | "`replace'"=="replace") {

		capture: mkdir ${DATA}/reanalysis
		capture: mkdir ${DATA}/reanalysis/prepped

		doDataPrep `mcf', `newbase' `degree'

		*	sample tags one counted acta for each geography
		gen ssample = inSample
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
		
		compress
		save ${DATA}/reanalysis/prepped/`mcf', replace
	}


end


capture: program drop doMCiter
program define doMCiter

	*
	*	Single Iteration
	*
	*	Assign replacement geographies
	*		If method is sheet, need to assign all the way down to acta
	local gprefs p d pd m l r s
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
	*	Simulate vote shares
	foreach p in CC MAS {
		bys `lg'codex (`lg'sample): egen `p'sheet = total(cond(inSample,`p',.))
	}

end


capture: program drop doBoliviaMC
program define doBoliviaMC

	syntax [anything(name=mcf)], [REPLACE] [Inner(integer 50)] [Outer(integer 10)] [Degree(integer 0)]

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


	capture: use ${OUTPUT}/`mcf'/`mcf', clear
	if (_rc~=0 | "`replace'"=="replace") {
		capture: mkdir ${DATA}
		capture: mkdir ${DATA}/reanalysis
		capture: mkdir ${DATA}/reanalysis/results
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

					doBoliviaMCprep `mcf', degree(`degree')
					doMCiter

					lpoly MASsheet pV if inSample==0, degree(`degree') bwidth(0.5) nogra gen(xmx xms)
					lpoly CCsheet pV if inSample==0, degree(`degree') bwidth(0.5) nogra gen(xcx xcs)
					keep if ~mi(xmx) | ~mi(xcx)
					keep x??
					gen iter  = (`ii'-1)*`inner'+`jj'
					gen sort_order = _n
					reshape wide xmx xms xcx xcs, i(sort_order) j(iter)
					if (`jj'>1) {
						merge 1:1 sort_order using `tj', nogen
					}
					save `tj', replace
				}
				*	save this batch
				save ${OUTPUT}/`mcf'/batches/`ii', replace
			}
			*	Add this batch of to the full batch
			if (`ii'>1) {
				merge 1:1 sort_order using `ti', nogen
			}
			save `ti', replace
			preserve
			merge 1:1 sort_order using ${DATA}/reanalysis/graph/`mcf'
			egen numiters = rownonmiss(xmx*)
			local mcom
			forvalues iter=1/`=numiters[1]' {
				local mcom `mcom' (line xms`iter' xmx`iter', pstyle(p2) lw(vthin))
			}
			twoway (scatter MAS pV, msize(vtiny)) (line ms0 mx0) `mcom' (line ms1 mx1, lw(vthin)) ///
				, legend(off)
			restore
		}
		*	Save the full batch of simulations
		save ${OUTPUT}/`mcf'/`mcf', replace
	}
	
	merge 1:1 sort_order using ${DATA}/reanalysis/graph/`mcf'
	egen numiters = rownonmiss(xmx*)
	sum pV if inSample, meanonly
	local pVm = r(max)
	di `pVm'
	local mcom
	forvalues iter=1/`=numiters[1]' {
		local mcom `mcom' (line xms`iter' xmx`iter', pstyle(p2) lw(thin))
	}
	twoway (scatter MAS pV, msize(vtiny)) (line ms0 mx0, pstyle(p2) lw(thin)) `mcom' ///
		(line ms1 mx1, pstyle(p3) lw(thin)) ///
		, legend(off) yline(50, lc(ceprgraymedium)) xline(`pVm', lc(ceprgraymedium)) ///
		xti(" " "Share of Emitidos Counted in TREP") yti("Morales Share of Emitidos on Acta")
	graph export ${OUTPUT}/`mcf'/`mcf'.png, replace

end

*	May be just about anything here. Used to save simulations in case
*		you want to come back to them
local run 0

doBoliviaMC `run', i(10) o(10)
