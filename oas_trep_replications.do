set more off
clear all
set type double
*	Set working directory, if desired
*cd ~/OAS

*
*	This code reproduces Nooruddin's analysis for the OAS final report
*		TREP only; it's still unclear how Nooruddin sorted the computo data
*

global INPUT ../Data/TSE
global DATA Data
global OUTPUT Output

*
*	PROBLEM WITH -IMPORT EXCEL-
*		any timestamp data imported as strings comes in with a 12 hour clock and no AM/PM indicator
*		any timestamp data imported as numeric are fine, but not necessarily formatted usefully
*	WHY THIS IS A PROBLEM
*		information may be lost if -IMPORT EXCEL- imports as string, say if misisng values are listed as "null"
*	SOLUTION
*		reach into the underlying XML files and pull data directly as string literals
*
*	CONSEQUENCE
*		this fix is probably NOT something Nooruddin did for the OAS, so some timestamps may differ
*

capture: program drop getNaiveTSE
program define getNaiveTSE

	syntax [anything(name=count)] [, REPLACE] [ASSTRINGS] [FORCE]
	
	*
	*	This program imports xlsx election data about as naively as possible
	*
	*	ASSTRINGS imports everthing as string (numerics as millisecond timestamps),
	*	FORCE takes the imported data and then converts any numeric data to string
	*		(does NOT imply ASSTRINGS but is superfluous if ASSTRINGS is specified)
	*
	
	if ("`count'"~="trep") {
		local count computo
	}
	local acom
	if ("`asstrings'"=="asstrings") {
		local acom allstring(%tcDDmonCCYY_HH:MM:SS.sss)
	}

	capture: use ${DATA}/naive/raw/`count', clear
	if (_rc~=0 | "`replace'"=="replace") {
		capture: mkdir ${DATA}
		capture: mkdir ${DATA}/naive
		capture: mkdir ${DATA}/naive/raw
		
		*	import data
		if ("`count'"=="trep") {
			local filename ${INPUT}/datos_trep_computo_eg2019/trep/OEA_TREP_LogCompleto_19G_2019.11.03.06.53.xlsx
		}
		else {
			local filename ${INPUT}/datos_trep_computo_eg2019/computo/2.RecepcionSobres_final.xlsx
		}
		import excel using `filename', firstrow case(preserve) clear `acom'
		
		*	convert any numeric to strings, if desired
		if ("`force'"=="force") {
			ds, has(type numeric)
			if ("`r(varlist)'"~="") {
				format %tcDDmonCCYY_HH:MM:SS.sss `r(varlist)'
				tostring `r(varlist)', force replace u
			}
		}
		save ${DATA}/naive/raw/`count', replace
	}
	
end

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

*
*	Data
*

*	TREP, want timestamps only
tempfile tf
getTSE, trep
keep if regexm(Ele,"Pres")
keep if regexm(Est,"Ver") | regexm(Est,"Computada")
egen sumV = rowtotal(CC-PANBOL)
drop if sumV==0 & NumMesa==2433
keep NumMesa *Fecha* *Date*
ren * trep_*
ren *NumMesa NumMesa
save `tf'

*	Computo
getTSE, computo
keep if regexm(Ele,"Pres")
keep if regexm(Est,"Ver") | regexm(Est,"Computada")

*	merge with TREP timestamps
merge 1:1 NumMesa using `tf', nogen

*	check valid votes -- reported (not actual) on presidential actas -- 6,137,778
sum Validosen, meanonly
di %20.0fc r(sum)

*	actual valid votes
egen sumV = rowtotal(CC-PANBOL)
*	MAS margin (share of valid votes is the official measure)
gen margin = 100*(MAS-CC)/sumV
*	MAS/CC share of emitidos (dropping blank entries, which OUGHT to be zero)
gen cshare = 100*CC/Emit
gen mshare = 100*MAS/Emit
*	MAS share of emitidos (including implied zeros)
gen msharef = mshare
replace msharef = 0 if mi(MAS) & ~mi(Emit)
*	indicator for localidad of Santa Cruz (not needed for replication)
gen isSC = 100*(regexm(Loc,"Santa Cruz") & regexm(Muni,"Santa Cruz"))

*	Nooruddin order
sort trep_VerificadorDate NumMesa
gen isIn = inlist(NumMesa,28177,24862,31056)
gen isLate = ~inlist(NumMesa,28177,24862,31056) & trep_VerificadorDate>=tc(20oct2019 20:03:59)
gen isLate2 = isLate*(1+mi(trep_VerificadorDate))

*	Nooruddin's progress variable
gen pV = sum(Emit*(~mi(trep_VerificadorDate)))
replace pV = 100*pV/pV[_N]
table isLate2, c(min pV max pV)

*	Nooruddin's select departments for reporting
gen isDep = 0
foreach dep in Beni Chuqui Cocha Paz Potos Santa Tar {
	replace isDep = 1 if Pa=="Bolivia" & regexm(Dep,"`dep'")
}

*
*	TREP
*
*
*	Top of p.87
*
lpoly mshare pV if isLate2<2, nogra degree(1) bwidth(4.75) gen(mx ms)
lpoly cshare pV if isLate2<2, nogra degree(1) bwidth(4.75) gen(cx cs)
twoway (line ms mx, lw(vthick)) (line cs cx, lw(vthick)), yline(50) xline(84) xline(95) 

*
*	Top of p.88
*
lpoly cshare pV if isLate2==0, degree(0) bwidth(4.75) nogra gen(cx0 cs0)
lpoly cshare pV if isLate2==1, degree(0) bwidth(0.5) nogra gen(cx1 cs1)
twoway (scatter cshare pV if isLate2<2, msize(vtiny)) (line cs0 cx0, lw(vthick)) (line cs1 cx1, lw(vthick)) ///
	, yline(50) xline(95)  xti(" " "Share of Emitidos Counted in TREP") yti("Mesa Share of Emitidos on Acta") legend(off)

*
*	Bottom of p.88
*
lpoly mshare pV if isLate2==0, degree(0) bwidth(6) nogra gen(mx0 ms0)
lpoly mshare pV if isLate2==1, degree(0) bwidth(0.5) nogra gen(mx1 ms1)
twoway (scatter mshare pV if isLate2<2, msize(vtiny)) (line ms0 mx0, lw(vthick)) (line ms1 mx1, lw(vthick)) ///
	, yline(50) xline(95) xti(" " "Share of Emitidos Counted in TREP") yti("Morales Share of Emitidos On Acta") legend(off)

*
*	Top of p.90
*
table isLate, c(sum Validosen sum MAS sum CC) row col format(%20.0fc)
table Dep isLate if isDep, c(sum Validosen sum MAS sum CC) format(%20.0fc)

*
*	Bottom of p.90
*
table isLate2, c(mean mshare mean cshare) row col format(%6.3fc)
table Dep isLate2 if isDep, c(mean mshare mean cshare) format(%6.3fc)
