set more off
clear all
set type double
set scheme ceprrebrand
*	This program will seek and create files in the current working directory
*	Please adjust as desired
*cd ~/Bolivia/2019/Analysis

global INPUT_TSE ../Data/TSE
global INPUT_OEP ../Data/public/XLSX
global DATA Data
global OUTPUT Output

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
		local upfile ${INPUT_TSE}/datos_trep_computo_eg2019/trep/OEA_TREP_LogCompleto_19G_2019.11.03.06.53.xlsx
		local sheetname sheet1
	}
	else {
		local upfile ${INPUT_TSE}/datos_trep_computo_eg2019/computo/2.RecepcionSobres_final.xlsx
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

capture: program drop get2016data
program define get2016data

	syntax [, REPLACE]

	capture: use ${DATA}/referendum, clear
	if (_rc~=0 | "`replace'"=="replace") {
		tempfile td
		local basedir ../../2016/Data/OEP
		capture: confirm file `basedir'/votos_totales_exterior.csv
		if (_rc~=0) {
			capture: mkdir ../../2016/
			capture: mkdir ../../2016/Data
			capture: mkdir ../../2016/Data/OEP
			tempfile tf
			copy https://atlaselectoral.oep.org.bo/descarga/83/votos_totales.csv `basedir'/votos_totales_exterior.csv, replace
		}
		import delimited using `basedir'/votos_totales_exterior.csv, delim("|") clear enc("utf-8") case(preserve)
		save `td'
		capture: confirm file `basedir'/votos_totales.csv
		if (_rc~=0) {
			tempfile tf
			copy https://atlaselectoral.oep.org.bo/descarga/38/votos_totales.csv `tf'
			filefilter `tf' `basedir'/votos_totales.csv, replace from("LA PALCA") to("LA PALCA\Q")
		}
		import delimited using `basedir'/votos_totales.csv, delim("|") clear enc("utf-8") case(preserve)
		append using `td'
		compress
		save ${DATA}/referendum, replace
	}

end

capture: program drop get2014data
program define get2014data

	syntax [, REPLACE]

	capture: use ${DATA}/2014, clear
	if (_rc~=0 | "`replace'"=="replace") {
		tempfile td
		local basedir ../../2014/Data/OEP
		capture: confirm file `basedir'/votos_totales_exterior.csv
		if (_rc~=0) {
			capture: mkdir ../../2014/
			capture: mkdir ../../2014/Data
			capture: mkdir ../../2014/Data/OEP
			tempfile tf
			copy https://atlaselectoral.oep.org.bo/descarga/85/votos_totales.csv `tf'
			filefilter `tf' `basedir'/votos_totales_exterior.csv, replace from("MANZONI\Q") to("MANZONI")
		}
		import delimited using `basedir'/votos_totales_exterior.csv, delim("|") clear enc("utf-8") case(preserve)
		save `td'
		capture: confirm file `basedir'/votos_totales.csv
		if (_rc~=0) {
			tempfile tf
			copy https://atlaselectoral.oep.org.bo/descarga/17/votos_totales.csv `tf'
			filefilter `tf' `basedir'/votos_totales.csv, replace from("LA PALCA") to("LA PALCA\Q")
		}
		import delimited using `basedir'/votos_totales.csv, delim("|") clear enc("utf-8") case(preserve)
		append using `td'
		compress
		save ${DATA}/2014, replace
	}

end

capture: program drop addPA
program define addPA

	capture: mkdir crosswalks
	capture: mkdir crosswalks/raw
	capture: mkdir crosswalks/clean

	getTSE, computo
	gen codPAIS = trim(Pais)
	egen utag = tag(codPAIS)
	keep if utag
	sort codPAIS
	export excel codPAIS using crosswalks/raw/pais.xlsx, sheet(Pais) firstrow(var) replace
	
	get2016data
	gen codPAIS_2016 = trim(NombrePA)
	egen utag = tag(codPAIS_2016)
	keep if utag
	sort codPAIS_2016
	export excel codPAIS_2016 using crosswalks/raw/pais.xlsx, sheet(Pais) firstrow(var) cell(b1) sheetmodify

end

capture: program drop addExterior
program define addExterior

	tempfile tf
	import excel using crosswalks/clean/pais, firstrow case(preserve) clear
	save `tf', replace

	getTSE, computo
	drop if Pais=="Bolivia"
	gen codPAIS = trim(Pais)
	merge n:1 codPAIS using `tf', keep(match) nogen
	gen codRECINTO = codPAIS+" > "+trim(Dep)+" > "+trim(Prov)+" > "+trim(Muni)+" > "+trim(Loc)+" > "+trim(Reci)
	gen codRECINTOshort = codPAIS+" > "+trim(Reci)
	egen utag = tag(codRECINTO)
	keep if utag
	sort codRECINTOshort
	export excel codRECINTO using crosswalks/raw/exterior.xlsx, sheet(Reci) firstrow(var) replace
	
	get2016data
	drop if NombrePA=="BOLIVIA"
	gen codPAIS_2016 = trim(NombrePA)
	gen codRECINTO_2016 = codPAIS_2016+" > "+trim(NombreREC)
	egen utag = tag(codREC)
	keep if utag
	sort codREC
	export excel codREC using crosswalks/raw/exterior.xlsx, sheet(Reci) firstrow(var) cell(b1) sheetmodify

end

capture: program drop addDEP
program define addDEP

	getTSE, computo
	drop if Pais~="Bolivia"
	gen codDEP = trim(Dep)
	egen utag = tag(codDEP)
	keep if utag
	sort codDEP
	export excel codDEP using crosswalks/raw/departamento.xlsx, sheet(Dep) firstrow(var) replace

	get2016data
	drop if NombrePA~="BOLIVIA"
	gen codDEP_2016 = trim(NombreDEP)
	egen utag = tag(codDEP)
	keep if utag
	sort codDEP
	export excel codDEP using crosswalks/raw/departamento.xlsx, sheet(Dep) firstrow(var) cell(b1) sheetmodify
	
end

capture: program drop addPRO
program define addPRO

	tempfile tf
	import excel using crosswalks/clean/departamento, firstrow case(preserve) clear
	save `tf'
	
	getTSE, computo
	drop if Pa~="Bolivia"
	gen codDEP = trim(Dep)
	merge n:1 codDEP using `tf', keep(match) nogen
	gen codPRO = codDEP_2016+" > "+trim(Pro)
	egen utag = tag(codPRO)
	keep if utag
	sort codPRO
	export excel codPRO using crosswalks/raw/provincia.xlsx, sheet(Pro) firstrow(var) replace
	
	get2016data
	drop if NombrePA~="BOLIVIA"
	gen codPRO_2016 = trim(NombreDEP)+" > "+trim(NombrePRO)
	egen utag = tag(codPRO)
	keep if utag
	sort codPRO
	export excel codPRO using crosswalks/raw/provincia.xlsx, sheet(Pro) firstrow(var) cell(b1) sheetmodify
	
end

capture: program drop addMUNI
program define addMUNI

	*
	*	Note: failed to match Raqaypampa... possibly formerly part of Misque
	*

	tempfile td
	import excel using crosswalks/clean/departamento, firstrow case(preserve) clear
	save `td'
	tempfile tp
	import excel using crosswalks/clean/provincia, firstrow case(preserve) clear
	save `tp'
	
	getTSE, computo
	drop if Pa~="Bolivia"
	gen codDEP = trim(Dep)
	merge n:1 codDEP using `td', keep(match) nogen
	gen codPRO = codDEP_2016+" > "+trim(Pro)
	merge n:1 codPRO using `tp', keep(match) nogen
	gen codMUN = codPRO_2016+" > "+trim(Muni)
	egen utag = tag(codMUN)
	keep if utag
	sort codMUN
	export excel codMUN using crosswalks/raw/municipio.xlsx, sheet(Muni) firstrow(var) replace
	
	get2016data
	drop if NombrePA~="BOLIVIA"
	gen codMUN_2016 = trim(NombreDEP)+" > "+trim(NombrePRO)+" > "+trim(NombreMUN)
	egen utag = tag(codMUN)
	keep if utag
	sort codMUN
	export excel codMUN using crosswalks/raw/municipio.xlsx, sheet(Muni) firstrow(var) cell(b1) sheetmodify
	
end

capture: program drop addREC
program define addREC


	tempfile td
	import excel using crosswalks/clean/departamento, firstrow case(preserve) clear
	save `td'
	tempfile tp
	import excel using crosswalks/clean/provincia, firstrow case(preserve) clear
	save `tp'
	tempfile tm
	import excel using crosswalks/clean/municipio, firstrow case(preserve) clear
	save `tm'
	
	getTSE, computo
	drop if Pa~="Bolivia"
	gen codDEP = trim(Dep)
	merge n:1 codDEP using `td', keep(match) nogen
	gen codPRO = codDEP_2016+" > "+trim(Pro)
	merge n:1 codPRO using `tp', keep(match) nogen
	gen codMUN = codPRO_2016+" > "+trim(Muni)
	*
	*	Note: failed to match Raqaypampa MUNI... possibly formerly part of Misque
	*
	replace codMUN = codPRO_2016+" > Mizque" if Dep=="Cochabamba" & Pro=="Mizque" & Muni=="Raqaypampa"
	merge n:1 codMUN using `tm', keep(match) nogen
	gen codREC = codMUN_2016+" > "+trim(Rec)
	egen utag = tag(codREC)
	keep if utag
	sort codREC
	export excel codREC using crosswalks/raw/recinto.xlsx, sheet(Rec) firstrow(var) replace
	
	get2016data
	drop if NombrePA~="BOLIVIA"
	gen codREC_2016 = trim(NombreDEP)+" > "+trim(NombrePRO)+" > "+trim(NombreMUN)+" > "+trim(NombreREC)
	egen utag = tag(codREC)
	keep if utag
	sort codREC
	export excel codREC using crosswalks/raw/recinto.xlsx, sheet(Rec) firstrow(var) cell(b1) sheetmodify
	
end

capture: program drop getInterruption
program getInterruption

	capture: use interruption_data, clear
	if (_rc~=0) {
		import excel using ${INPUT_OEP}/reports_trep/acta.2019.10.20.19.40.57.xlsx, clear firstrow case(preserve)
		compress
		save interruption_data, replace
	}

end

capture: program drop getAllTREP
program getAllTREP

	capture: use alltrep_data, clear
	if (_rc~=0) {
		import excel using ${INPUT_OEP}/reports_trep/acta.2019.10.25.06.49.40.xlsx, clear firstrow case(preserve)
		compress
		save alltrep_data, replace
	}

end

capture program drop assemble2016data
program define assemble2016data

	*
	*	2016 data
	*
	tempfile d16
	get2016data
	gen codREC_2016 = trim(NombreDEP)+" > "+trim(NombrePRO)+" > "+trim(NombreMUN)+" > "+trim(NombreREC)
	replace codREC_2016 = trim(NombrePA)+" > "+trim(NombreREC) if NombrePA~="BOLIVIA"
	collapse (sum) S NO, by(codREC_2016)
	save `d16'

	*
	*	2016 crosswalks
	*
	*	External
	tempfile tx
	import excel using crosswalks/clean/exterior, firstrow case(preserve) clear
	ren codRECINTO codREC
	ren codRECI codREC_2016
	keep if ~mi(codREC)
	save `tx'
	*	Bolivia
	tempfile td
	import excel using crosswalks/clean/departamento, firstrow case(preserve) clear
	save `td'
	tempfile tv
	import excel using crosswalks/clean/provincia, firstrow case(preserve) clear
	save `tv'
	tempfile tm
	import excel using crosswalks/clean/municipio, firstrow case(preserve) clear
	save `tm'
	tempfile tr
	import excel codREC=A codREC_2016=B using crosswalks/clean/recinto, cellrange(a2) clear
	keep if ~mi(codREC)
	save `tr'
	
	getTSE, computo
	keep if regexm(Ele,"Pres")
	collapse (firstnm) Est, by(Pa-Rec)
	keep Pa-Rec
	*	Bolivia
	preserve
	tempfile dom
	keep if Pa=="Bolivia"
	gen codDEP = trim(Dep)
	merge n:1 codDEP using `td', keep(match) nogen
	gen codPRO = codDEP_2016+" > "+trim(Pro)
	merge n:1 codPRO using `tv', keep(match) nogen
	gen codMUN = codPRO_2016+" > "+trim(Muni)
	*	Note: failed to match Raqaypampa MUNI... possibly formerly part of Misque
	replace codMUN = codPRO_2016+" > Mizque" if Dep=="Cochabamba" & Pro=="Mizque" & Muni=="Raqaypampa"
	merge n:1 codMUN using `tm', keep(match) nogen
	gen codREC = codMUN_2016+" > "+trim(Rec)
	merge n:1 codREC using `tr', keep(match) nogen
	keep Pa-Rec codREC*
	save `dom'
	*	External
	restore
	keep if Pa~="Bolivia"
	gen codREC = trim(Pa)+" > "+trim(Dep)+" > "+trim(Pro)+" > "+trim(Mun)+" > "+trim(Loc)+" > "+trim(Rec)
	merge 1:1 codREC using `tx', keep(match) nogen
	append using `dom'
	drop codREC
	merge n:1 codREC_2016 using `d16', keep(match) nogen

end

capture: program drop addNewmanExtended
program define addNewmanExtended

	*	Newman extended variables by geographic area
	foreach g in r l m {
		local glab Precinct
		if ("`g'"=="l") {
			local glab Locality
		}
		if ("`g'"=="m") {
			local glab Municipality
		}
		*	Newman mesa categories
		tempvar total_mesas total_mesas_before total_valid total_valid_before total_valid_after
		bys `g'code: egen `total_mesas' = total(1)
		bys `g'code: egen `total_mesas_before' = total(after_cutoff==0)
		gen newman`glab'Type = (after_cutoff)+2*(`total_mesas_before'>0 & `total_mesas_before'<`total_mesas')
		lab var newman`glab'Type "Newman: `glab' acta classification"
		egen newman`glab'Tag = tag(`g'code newman`glab'Type)
		lab var newman`glab'Tag "Newman: Tag of one acta in each `glab' within each class"
		*	Newman category recode (to get correct signs on tests)
		recode newman`glab'Type (3=0) (2=1) (1=2) (0=3), generate(newman`glab'Code)
		lab var newman`glab'Code "Newman: `glab' acta classification (recode)"
		*	Margins by geography and time
		bys `g'code after_cutoff: egen `total_valid' = total(sumV)
		bys `g'code after_cutoff: egen newman`glab'Margin = total(sumV*margin/`total_valid'), missing
		lab var newman`glab'Margin "Newman: `glab' margin aggregate by early/late"
		*	Early margins by geography
		bys `g'code: egen `total_valid_before' = total(cond(after_cutoff==0,sumV,.))
		bys `g'code: egen newman`glab'EarlyMargin = total(cond(after_cutoff==0,sumV*margin/`total_valid_before',.)), missing
		lab var newman`glab'EarlyMargin "Newman: `glab' margin early"
		*	Classification of support

		bys `g'code: egen newman`glab'MAS = total(cond(after_cutoff==0, MAS-CC, .)), missing
		lab var newman`glab'MAS "Newman: `glab' favored MAS early"
		gen newman`glab'MAS3 = newman`glab'MAS>=-3
		lab var newman`glab'MAS3 "Newman: `glab' favored MAS early by at least -3"
		gen newman`glab'MAS6 = newman`glab'MAS>=6
		lab var newman`glab'MAS6 "Newman: `glab' favored MAS early by at least 6"
		replace newman`glab'MAS = newman`glab'MAS>=0

		bys `g'code: egen `total_valid_after' = total(cond(after_cutoff==1,sumV,.))
		bys `g'code: egen soft`glab'MAS = total(cond(after_cutoff==1,sumV*margin/`total_valid_after',.)), missing
		replace soft`glab'MAS = soft`glab'MAS+newman`glab'EarlyMargin>=0
		lab var soft`glab'MAS "(Alternative) soft cutoff"

		*	Counterfactual
		gen newman`glab'CF = cond(newman`glab'Type==3 & ~newman`glab'MAS, newman`glab'EarlyMargin, margin)
		lab var newman`glab'CF "Newman: `glab' counterfactual (adjusting late vote in split Mesa geographies)"
		gen newman`glab'CFall = cond(newman`glab'Type==3, newman`glab'EarlyMargin, margin)
		lab var newman`glab'CFall "Newman: `glab' counterfactual (adjusting late vote in all split geographies)"
		drop _*
	}
	lab def newmanType 0 "All before" 1 "All after" 2 "(split) Before" 3 "(split) After"
	lab def newmanCode 3 "All before" 2 "All after" 1 "(split) Before" 0 "(split) After"
	lab def newmanMAS 0 "MAS-CC<0 early" 1 "MAS-CC>=0 early"
	lab def newmanMAS3 0 "MAS-CC<-3 early" 1 "MAS-CC>=-3 early"
	lab def newmanMAS6 0 "MAS-CC<6 early" 1 "MAS-CC>=6 early"
	lab def softMAS 0 "early+late margins<0" 1 "early+late margins>=0"
	lab val newman*Type newmanType
	lab val newman*Code newmanCode
	lab val newman*MAS newmanMAS
	lab val newman*MAS3 newmanMAS3
	lab val newman*MAS6 newmanMAS6
	lab val soft*MAS softMAS

end

capture: program drop addCounterfactual
program define addCounterfactual

	syntax newvarname [, Prefix(string asis) Levels(string asis) TREP]
	
	if ("`levels'"=="") {
		local levels cf cp cd cv cm l r n
	}	
	local tp
	if ("`trep'"=="trep") {
		local tp TREP_
	}

	gen `varlist' = `tp'margin
	lab var `varlist' "`prefix': Counterfactual (see notes)"
	local lstring
	foreach g of local levels {
	
		local gname Nearest
		local ml 6
		if (substr("`g'",-1,.)=="r") {
			local gname Precinct
			local ml 5
		}
		if (substr("`g'",-1,.)=="l") {
			local gname Locality
			local ml 4
		}
		if (substr("`g'",-1,.)=="m") {
			local gname Municipality
			local ml 3
		}
		if (substr("`g'",-1,.)=="v") {
			local gname Province
			local ml 2
		}
		if (substr("`g'",-1,.)=="d") {
			local gname Department
			local ml 1
		}
		if (substr("`g'",-1,.)=="p") {
			local gname Country
			local ml 0
		}
		if (substr("`g'",-1,.)=="f") {
			local gname Foreign
			if ("`g'"=="cf") {
				local gname RUE
			}
			if ("`g'"=="sf") {
				local gname ceprSix
			}
			local ml -1
		}

		replace `varlist' = `prefix'`gname'Margin if `prefix'Matchlevel>=`ml' & ~mi(`prefix'`gname'Margin)
		local lstring `lstring' `gname'
	
	}
	replace `varlist' = `tp'margin if `prefix'Matchlevel>6
	notes `varlist': `lstring'

	
end

capture: program drop addChumaceroMargins
program define addChumaceroMargins

	syntax [if] [in] [, Prefix(string asis) Levels(string asis) REPLACE TREP]
	
	marksample impute
	
	*	if/in says which to impute
	
	if ("`levels'"=="") {
		local levels cf cp cd cv cm l r n
		*local levels n r l cm cv cd cp cf
	}
	if ("`replace'"=="replace") {
		capture: drop `prefix'*Margin* `prefix'*Type `prefix'Matchlevel
	}
	local tp
	if ("`trep'"=="trep") {
		local tp TREP_
	}
		
	*
	*	Calculate mean margins for all geographic areas at all levels
	*
	local ml -1
	gen `prefix'Matchlevel = 7
	foreach g of local levels {

		if ("`g'"=="n") {
			local gname Nearest
			tempvar np1 nm1 n_nearest n_imputed n_ins iweight usemargin
			*	iweight is weight of EARLY observations
			gen `usemargin' = cond(mi(`tp'margin),0,`tp'margin)
			gen `iweight' = cond(`impute',0,`tp'sumV)
			*	Neighbor, first
			sort NumM
			*	code mesa numbers for nearest neighbors
			gen `np1' = NumM+1 if rcode[_n+1]==rcode & NumM[_n+1]==NumM+1
			gen `nm1' = NumM-1 if rcode[_n-1]==rcode & NumM[_n-1]==NumM-1

			*	count the number of nearest neighbors (including self)
			gen `n_nearest' = 1 + ~mi(`np1') + ~mi(`nm1')
			*		of which, in need of imputation
			gen `n_imputed' = 100*(`impute'+cond(~mi(`np1'),`impute'[_n+1],0) + cond(~mi(`nm1'),`impute'[_n-1],0))/`n_nearest'
			*	total weight of available observations
			gen `n_ins' = `iweight' + cond(mi(`np1'),0,`iweight'[_n+1]) + cond(mi(`nm1'),0,`iweight'[_n-1])
			*	weighted total margins
			gen `prefix'NearestMargins = `iweight'*`usemargin' + cond(mi(`np1'),0,`iweight'[_n+1])*`usemargin'[_n+1] + cond(mi(`nm1'),0,`iweight'[_n-1])*`usemargin'[_n-1] if `n_nearest'
			*	convert to weighted averages
			replace `prefix'NearestMargins = `prefix'NearestMargin/`n_ins'
			lab var `prefix'NearestMargins "`prefix': Nearest-neighbors margin"
			gen `prefix'NearestType = (`impute')+2*(`n_imputed'>0 & `n_imputed'<100)
			lab var `prefix'NearestType "`prefix': Nearest acta type"
		}
		else {
			local gname Precinct
			if (substr("`g'",-1,.)=="l") {
				local gname Locality
			}
			if (substr("`g'",-1,.)=="m") {
				local gname Municipality
			}
			if (substr("`g'",-1,.)=="v") {
				local gname Province
			}
			if (substr("`g'",-1,.)=="d") {
				local gname Department
			}
			if (substr("`g'",-1,.)=="p") {
				local gname Country
			}
			if (substr("`g'",-1,.)=="f") {
				local gname Foreign
				if ("`g'"=="cf") {
					local gname RUE
				}
				if ("`g'"=="sf") {
					local gname ceprSix
				}
			}
			local glab `gname'
			if (inlist("`g'","cp","cd","cv","cm")) {
				local glab "RUE/`gname'"
			}
			if (inlist("`g'","sp","sd","sv")) {
				local glab "ceprSix/`gname'"
			}
				
			tempvar total_votes total_votes_early
			bys `g'code: egen `total_votes' = total(`tp'sumV)
			bys `g'code: egen `total_votes_early' = total(cond(`impute',.,`tp'sumV))

			*	Mean margin (early) on those actas
			bys `g'code: egen `prefix'`gname'Margin = total(cond(`impute',.,`tp'sumV*`tp'margin/`total_votes_early')), missing
			lab var `prefix'`gname'Margin "`prefix': `glab' margin early"

			gen `prefix'`gname'Type = (`impute')+2*(`total_votes_early'>0 & `total_votes_early'<`total_votes')
			lab var `prefix'`gname'Type "`prefix': `glab' acta classification"
			
		}
		
		replace `prefix'Matchlevel = `ml' if `prefix'`gname'Type==3
		local ++ml
		
	}
	capture: lab def `prefix'Matchlevel 7 "Self" 6 "Nearest" 5 "Precinct" 4 "Locality" 3 "RUE/Municipality" 2 "RUE/Province" 1 "RUE/Department" 0 "RUE/Country" -1 "RUE"
	capture: lab def `prefix'Type 0 "All before" 1 "All after" 2 "(split) Before" 3 "(split) After"
	lab val `prefix'*Type `prefix'Type
	lab val `prefix'Matchlevel `prefix'Matchlevel		
end

capture: program drop buildFullDataSet
program define buildFullDataSet

	syntax [, REPLACE]

	capture: use fullData, clear
	if (_rc~=0 | "`replace'"=="replace") {

		*
		*	2016 data
		*
		tempfile d16
		assemble2016data
		save `d16'
		
		*
		*	TSE TREP data
		*
		tempfile gt
		getTSE, trep	
		sort NumM FechaReg Est
		keep if regexm(Ele,"Pres")
		
		*	Newman's definition for pre/post-interruption
		gen after_cutoff = PriTransmisionDate > tc(20oct2019 19:40:57) | ~regexm(Est,"Veri")
		replace after_cutoff = 1 if FechaReg > tc(20oct2019 19:40:57)
		replace after_cutoff = 1 if NumMesa==2433 & MAS==0
		lab def after_cutoff 0 "Pre-interruption" 1 "Post-interruption"
		lab val after_cutoff after_cutoff
		lab var after_cutoff "Newman's definition for post-interruption actas"
		notes after_cutoff: There are six actas Newman counts as post-interruption despite their inclusion in the last pre-interruption report
		notes after_cutoff: 2433 and 61373 as noted under VerificadorDate
		notes after_cutoff: 2444 and 2497-99 are actas from outside Bolivia with no PriTransmisionDate
		
		*	TREP timestamps
		lab var EstadoActa			"TREP: Last action taken on acta"
		lab var FechaRegistroenLog	"TREP: (EstadoActa) timestamp"	
		lab var PriRegistroDate		"TREP: First time on phone when taken/sent"
		lab var UltRegistroDate		"TREP: Last time on phone when taken/sent"
		lab var PriTransmisionDate	"TREP: First time image and transcription received"
		lab var UltTransmisionDate	"TREP: Last time image and transcription received"
		lab var VerificadorDate		"TREP: Transcribed by SERECI"
		notes VerificadorDate: Two actas listed in the 2019.10.20.19.40.57 report were Verified after that time
		notes VerificadorDate: 2433  was (originally) verified at 13:21:05
		notes VerificadorDate: 61373 was verified at 19:40:59 but was included in the 19:40:57 report
		lab var AprobadorDate		"TREP: Reviewed by SERCEI"
		notes: This data only includes the LAST entry for each acta
		notes: The 2019.10.20.19.40.57 report includes all most recently verified no later than 20oct2019 19:40:57, but votes differ on 2433
		notes: 2433 was originally APPROVED at 18:32:26 but with zero valid votes (though registered at 12:05:42 with votes, restored on the 22nd at 09:16:16)

		*	TREP voting
		foreach var of varlist CC-PANB Bla Nul Emit {
			*	Count 2433 as entered at time of interruption
			*replace `var' = 0 if NumM==2433
		}
		egen sumVálidos = rowtotal(CC-PANB)
		gen others = sumV-(MAS+CC)
		gen margin = 100*(MAS-CC)/sumV
		foreach var of varlist CC MAS others Bla Nul Emit sumV margin {
			ren `var' TREP_`var'
		}
		
		*	Timestamp and nature of last entry before interruption
		*	 - must sort by EstadoActa as well, because 60856 was (re)Registerd and Verified in the same second
		tempvar isPreEvent
		gen `isPreEvent'= FechaReg<=tc(20oct2019 19:40:57)
		bys NumM (FechaReg Est): egen lastPreEvent = max(cond(`isPreEvent',FechaReg,.))
		lab var lastPre "TREP: Time of last action taken on acta no later than 20oct2019 19:40:57"
		format %tc lastPre
		gen lastPreEventType = Est if FechaReg==lastPreEvent
		bys NumM (FechaReg Est): replace lastPreEventType = lastPreEventType[_n-1] if mi(lastPreEventType)
		lab var lastPreEventType "TREP: Last action taken on acta no later than 20oct2019 19:40:57"
		
		*	Keep only the latest entry
		bys NumM (FechaReg Est): keep if _n==_N

		*	Actas included in a report had last event prior to report as VERIFIED
		gen preInt = regexm(lastPreEventType,"Veri")
		*	Include 61373 in pre-interruption as it was included in the 2019.10.20.19.40.57 report
		*		(presumably a timing issue: see notes for VerificadorDate
		replace preInt = 1 if NumM==61373
		lab def preInt 0 "Post-interruption" 1 "Pre-interruption"
		lab val preInt preInt
		lab var preInt "Included in 20oct2019 19:40:57 TREP report"
		save `gt'
		
		if (0) {
			*
			*	Check that mesa numbers match those listed in the 2019.10.20.19.40.57 report
			*
			getInterruption
			keep if regexm(Ele,"Pres")
			ren N*a NumMesa
			keep NumMesa
			merge 1:1 NumMesa using `gt'
			tab _merge preInt, m
			mata: CCC
		}

		*
		*	TSE computo data
		*
		getTSE, computo
		keep if regexm(Elec,"Pres")
		drop Est
		
		*	Computo timestamps
		lab var Fecharecepción				"Computo: Envelope received at TED"
		lab var Fechaapertura				"Computo: Envelope opened at TED"
		lab var PrimeraTransmisiónImagen	"Computo: First time image scanned"
		lab var UltimaTransmisiónImagen		"Computo: Last time image scanned"
		lab var Fechacustodia				"Computo: Archived"
		lab var RegComputoDate				"Computo: First transcription"
		lab var ComputoDate					"Computo: Second transcription"
		lab var AprobComputoDate			"Computo: Review if transcriptions do not match"

		*	Extended acta data
		egen sumVálidos = rowtotal(CC-PANB)
		lab var sumV "Sum of valid votes counted on acta (valid votes cast)"
		notes sumV: Not universally equal to the total as reported on the acta (see ValidosenActa)
		lab var Validosen "Total valid votes as reported on the acta (valid vote total declared)"
		notes Validosen: Not universally equal sum of valid votes case (see sumVálidos)
		*	Note: Computo data is missing when zero, so fix...
		foreach var of varlist CC-PANB Bla Nul Validosen Emiti {
			*	saving missings as nonzero for Nooruddin
			gen computo`var' = `var'
			lab var computo`var' "Nooruddin: `var' with nonzero 'missing' values"
			replace `var' = 0 if mi(`var')
		}
		gen margin = 100*(MAS-CC)/sumV
		lab var margin "Net votes for MAS over CC on acta (% of valid votes)"
		gen shareMAS = 100*MAS/sumV
		lab var shareMAS "Votes for MAS on acta (% of valid votes)"
		gen shareCC = 100*CC/sumV
		lab var shareCC "Votes for CC on acta (% of valid votes)"
		*	Timing: computo
		gen computoTime = max(RegComputoDate, ComputoDate)
		replace computoTime = max(computoTime, AprobComputoDate) if ~mi(AprobComputoDate)
		format %tc computoTime
		lab var computoTime "Latest Computo activity"
		sort computoTime NumM
		gen computoOrder = _n
		lab var computoOrder "Computo ordering by computoTime"
		notes computoOrder: Ties broken by NumMesa
		gen computoComplete = sum(sumV)
		replace computoComplete = 100*computoComplete/computoComplete[_N]
		lab var computoComplete "Cómputo progress (% of all valid votes counted)"
			
		*	Add TREP data
		merge 1:1 NumMesa using `gt', nogen
		replace preInt = 0 if mi(preInt)
		replace after_cutoff = 1 if mi(after_cutoff)
		
		*	How far did the acta actually progress through the TREP?
		gen progress = (~preInt)+(~regexm(Est,"Ver"))+mi(Est)
		lab def progress 0 "Pre-interruption" 1 "Post-interruption, verified" 2 "Unverified" 3 "Computo-only"
		lab val progress progress
		lab var progress "Progress of acta through TREP"
		notes progress: 61373 is considered pre-interruption though not verified until two seconds past the last TREP report before the interruption
		tab Est progress, m
		
		*	Timing: TREP
		gen trepTime = max(VerificadorDate,AprobadorDate) if ~regexm(Est,"Esper")
		replace trepTime = AprobadorDate if NumM==2433
		format %tc trepTime
		lab var trepTime "Latest TREP approval"
		notes trepTime: Tally sheets with no AprobadorDate are implicitly approved when verified
		sort trepTime NumMesa FechaReg NumMesa
		gen trepOrder = _n
		lab var trepOrder "TREP ordering by trepTime"
		notes trepOrder: Non-TREP sorted at end by FechaRegistroenLog
		notes trepOrder: Ties broken by NumMesa
		gen trepComplete = sum(sumV)
		replace trepComplete = 100*trepComplete/trepComplete[_N]
		lab var trepComplete "TREP progress"
		notes trepComplete: Ordered by trepOrder, weighted by valid votes cast (sumVálidos)
		
		table progress if NumM~=61373, c(min trepTime max trepTime)

		*	Add 2016 data
		merge n:1 Pa-Rec using `d16', nogen
		lab var codREC_2016 "2016: Precinct identifier for possible match"
		lab var SÍ "2016: Yes votes in the possible precinct match"
		lab var NO "2016: No votes in the possible precinct match"
		
		*	Geographic codings
		gen foreign = Pa~="Bolivia"
		lab def foreign 0 "Bolivia" 1 "Outside"
		lab val foreign foreign
		lab var foreign "Indicates acta came from outside Bolivia"
		egen fcode = group(foreign), missing
		lab var fcode "Code for inside or outside Bolivia"
		egen pcode = group(fcode Pa), missing
		egen dcode = group(pcode Dep), missing
		egen vcode = group(dcode Pro), missing
		egen mcode = group(vcode Mun), missing
		egen lcode = group(mcode Loc), missing
		egen rcode = group(lcode Rec), missing
		lab var pcode "Country code"
		lab var dcode "Department code"
		lab var vcode "Province code"
		lab var mcode "Municipality code"
		lab var lcode "Locality code"
		lab var rcode "Precinct code"
		
		*	Alternative timestampa: Nooruddin
		gen new_computoTimeOAS = ComputoDate
		lab var new_computoTimeOAS "Nooruddin: Computo timestamp *** CORRECTED ***"
		sort new_computoTimeOAS NumM
		gen new_computoOrderOAS = _n
		lab var new_computoOrderOAS "Nooruddin: Computo ordering *** CORRECTED ***"
		gen new_computoCompleteOAS = sum(Emit)
		replace new_computoCompleteOAS = 100*new_computoCompleteOAS/new_computoCompleteOAS[_N]
		lab var new_computoCompleteOAS "Nooruddin: Computo progress *** CORRECTED ***"
		notes new_computoCompleteOAS: Ordered by new_computoOrderOAS, weighted by EmitidosReales 
		
		gen computoTimeOAS = ComputoDate
		tostring computoTimeOAS, force replace format(%tcDD/NN/CCYY_Hh:MM_a.m.)
		lab var computoTimeOAS "Nooruddin: Computo timestamp *** AS STRING ***"
		notes computoTimeOAS: Exactly reproduces ComputoDate at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGOFSC&version=1.0
		save `gt', replace
		use "../Nooruddin/nooruddin.bolivia 2019 oas analysis replication data.dta", clear
		keep num_mesa_computo cum_emitidos_computo
		ren num_mesa NumMesa
		merge 1:1 NumMesa using `gt', nogen
		sort computoTimeOAS cum_emitidos_computo computoEmit NumM
		gen computoOrderOAS = _n
		lab var computoOrderOAS "Nooruddin: Computo ordering"
		notes computoOrderOAS: It's unclear how Nooruddin broke ties, but those with "missing" Emitidos came last
		gen computoCompleteOAS = sum(Emit)
		replace computoCompleteOAS = 100*computoCompleteOAS/computoCompleteOAS[_N]
		lab var computoCompleteOAS "Nooruddin: Computo progress"
		notes computoCompleteOAS: Ordered by computoOrderOAS, weighted by EmitidosReales 

		
		*	tack on late reporters in computo order
		gen trepTimeOAS = VerificadorDate
		tostring trepTimeOAS, force replace format(%tcCCYY-NN-DD!THH:MM:SS!Z)
		*	this moves unverified actas to the end: fix later
		replace trepTimeOAS = "3" if progress>1
		lab var trepTimeOAS "Nooruddin: TREP timestamp *** AS STRING ***"
		notes trepTimeOAS: Exactly reproduces verificador_date at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGOFSC&version=1.0
		*	Verified
		gsort trepTimeOAS NumMesa
		gen trepOrderOAS = _n
		lab var trepOrderOAS "Nooruddin: TREP ordering"
		notes trepOrderOAS: Nooruddin puts non-verified tally sheets after all verified. The non-verified are possibly sorted by computoTimeOAS. Any further ties are broken by NumMesa
		*	fix late reporters
		replace trepTimeOAS = "" if progress>1
		gen trepCompleteOAS = sum(TREP_Emit*(progress<=1))
		replace trepCompleteOAS = 100*trepCompleteOAS/trepCompleteOAS[_N]
		replace trepCompleteOAS = . if progress>1
		lab var trepCompleteOAS "Nooruddin: TREP progress"
		notes trepCompleteOAS: Ordered by trepOrderOAS, weighted by TREP_EmitidosReales 		
		

		*	Alternative geographic codings: CEPR
		local dlist Beni Chuquisaca Cochabamba Paz Oruro Pando Potosí Cruz Tarija
		local mlist Trinidad Sucre Cochabamba Señora Oruro Cobija Potosí Sierra Tarija
		gen ceprSix = 0
		lab var ceprSix "Codes for CEPR assignments of -1 order admin"
		forvalues ii=1/`:list sizeof dlist' {
			replace ceprSix = regexm(Dep,"`:word `ii' of `dlist''") & regexm(Muni,"`:word `ii' of `mlist''") if ceprSix==0
		}
		replace ceprSix = 2 if regexm(Dep,"Cruz") & regexm(Muni,"Sierra")
		replace ceprSix = 3 if regexm(Dep,"Cochabamba") & regexm(Muni,"Cochabamba")
		replace ceprSix = -1 if regexm(Dep,"Paz") & regexm(Muni,"El Alto")
		replace ceprSix = -2 if Pa~="Bolivia"
		lab def ceprSix -2 "Foreign" -1 "El Alto" 0 "Other Bolivia" 1 "Other Capital Cities" 2 "Santa Cruz" 3 "Cochabamba"
		lab val ceprSix ceprSix
		egen sfcode = group(ceprSix), missing
		lab var sfcode "CEPR codes for six geographies"
		egen spcode = group(sfcode Pa), missing
		egen sdcode = group(spcode Pa), missing
		egen svcode = group(sdcode Pa), missing
		lab var spcode "ceprSix/Country code"
		lab var sdcode "ceprSix/Department code"
		lab var svcode "ceprSix/Province code"
		
		*	Alternative geographic codings: Chumacero
		gen NúmeroMesa = NumMesa
		noi merge 1:1 NúmeroMesa using ../Chumacero/Data/urb_rural, nogen
		lab var RUE "Chumacero's assignments of domestic urban/rural localities"
		drop NúmeroMesa	
		egen cfcode = group(RUE), missing
		lab var cfcode "Code for inside (rural or urban) or outside Bolivia"
		egen cpcode = group(cfcode Pa), missing
		egen cdcode = group(cpcode Dep), missing
		egen cvcode = group(cdcode Pro), missing
		egen cmcode = group(cvcode Mun), missing
		lab var cpcode "RUE/Country code"
		lab var cdcode "RUE/Department code"
		lab var cvcode "RUE/Province code"
		lab var cmcode "RUE/Municipality code"
		gen NúmeroMesa = NumMesa
		noi merge 1:1 NúmeroMesa using ../Chumacero/Data/urb_rural, nogen
		drop NúmeroMesa
		*	Chumacero includes 2433 in TREP, but with zero votes
		gen is2433 = NumM==2433
		lab var is2433 "Acta 2433"
		lab def is2433 0 "Not 2433" 1 "Acta 2433"
		lab val is2433 is2433
		gen SIN_TREP = progress>1 // | sumV==0 | Pa=="Colombia"
		lab var SIN_TREP "Chumacero's non-TREP actas"
		notes SIN_TREP: EXCLUDES actas with no valid votes and actas from Colombia
		gen APAGON = progress==1
		lab var APAGON "Chumacero's post-interruption TREP actas"
		gen SOMBRA = progress>0 // | TREP_sumV==0 | Pa=="Colombia"
		lab var SOMBRA "Chumacero's post-interruption actas"
		tempvar dmax cnum
		gen `dmax' = cond(mi(AprobComputoDate),ComputoDate,AprobComputoDate)
		sort `dmax'
		gen `cnum' = _n
		egen Q = cut(`cnum'), group(5)
		replace Q = Q+1
		lab var Q "(Approximately) Chumacero's computo quintiles"
		gen Q1 = Q==1
		lab var Q1 "(Approximately) Chumacero's first computo quintile"
		gen Q5 = Q==5
		lab var Q5 "(Approximately) Chumacero's last computo quintile"
		
		*	Alternative geographic codings: Escobari and Hoover (original)
		egen emcode = group(Muni)
		lab var emcode "Escobari and Hoover's original municipal code"
		notes emcode: Miscoded: should be as in mcode
		egen ercode = group(Reci)
		lab var ercode "Escobari and Hoover's original precinct code"
		notes ercode: Miscoded: should be as in rcode
		gen emargin = 100*(MAS-CC)/Validosen
		lab var emargin "Escobari and Hoover's differences in shares"
		notes emargin: Same as margin, but using Validosen instead of sumVálidos as denominator
		
		*	Alternative geographic codings: Villegas
		gen villegasThree = cond(ceprSix==-2,0,cond(ceprSix==0,1,2))
		lab def villegasThree 0 "Extranjero" 1 "Provincia" 2 "Ciudad"
		lab val villegasThree villegasThree
		lab var villegasThree "Villegas: geographic categories"
	
		drop _*
		compress
		save fullData, replace
		
	}

	notes
	
end


capture: program drop checkNEWruddin
program define checkNEWruddin

		*keep verificador_date NEWComputoDate OLDComputoDate num_mesa_computo

	tempfile t
	buildFullDataSet //, replace
	save `t'
	use "../Nooruddin/nooruddin.bolivia 2019 oas analysis data file.dta", clear
	keep verificador_date NEWComputoDate OLDComputoDate num_mesa_computo ps_natl_share_computo NEWcum_ps_natl_share_computo cum_ps_natl_share cum_emitidos_computo emitidos_reales
	li num_mesa_comp OLDComputoDate NEWComputoDate if inlist(num_mesa_comp,11359,70668,75093,50166,51097,11118)
	d
	sum
	ren num_mesa NumMesa
	merge 1:1 NumMesa using `t', nogen

	gen trepTS = clock(verificador_date,"20YMD#hms#")
	di as text ""
	di "Noooruddin's verificador_date is same as TSE's VerificadorDate, but formatted as string"
	di as input `". gen trepTS = clock(verificador_date,"20YMD#hms#")"'
	di ". count if trepTS~=VerificadorDate & progress<=1"
	count if trepTS~=VerificadorDate & progress<=1
	di as text "Verifying GUDS variable"
	di as input ". count if trepTimeOAS~=verificador_date"
	count if trepTimeOAS~=verificador_date
	
	gen inTREP = mi(verificador_date)
	sort inTREP verificador_date NumM
	gen vd_num = _n
	sort cum_ps_natl_share
	di as text ""
	di "Nooruddin's TREP order is first by verificador_date (string) breaking ties by mesa number"
	di as input  ". gen inTREP = mi(verificador_date)"
	di ". sort inTREP verificador_date NumM"
	di ". gen vd_num = _n"
	di ". sort cum_ps_natl_share"
	di ". count if vd_num~=_n"
	count if vd_num~=_n
	di as text "Verifying GUDS variable"
	gen terr = trepCompleteOAS-100*cum_ps_natl_share
	sort trepOrderOAS
	di as input ". sort trepOrderOAS"
	di ". count if vd_num~=_n"
	count if vd_num~=_n
	di ". gen terr = trepCompleteOAS-100*cum_ps_natl_share"
	di ". sum terr"
	sum terr

	gen computoTS = clock(OLDComputoDate,"DM20Y hm")
	format %tcDD/NN/CCYY_Hh:MM_a.m. computoTS
	di as text ""
	di "Noooruddin's ComputoDate is same as TSE, but formatted as string"
	di as input `". gen computoTS = clock(OLDnComputoDate,"DM20Y hm")"'
	di ". count if computoTS~=OLDComputoDate"
	count if computoTS~=ComputoDate
	di as input ". count if computoTimeOAS~=nooruddinComputoDate"
	count if computoTimeOAS~=OLDComputoDate

	count if ComputoDate~=NEWComputoDate
	li NumM ComputoDate OLDComp NEWComp MAS CC sumV margin if ComputoDate~=NEWComputoDate
	mata: VVV
	
	sort nooruddinComputoDate cum_emitidos_computo computoEmit NumM
	gen pVcomp = sum(Emit)
	sort computoOrderOAS
	gen pVcomp2 = sum(Emit)
	di as text ""
	di "It's not clear how Nooruddin broke ties in the computo, but progression is by ComputoDate (as string)"
	di "	and it appears ties resulting from zero emitidos have the zeroes last"
	di as input ". gsort nooruddinComputoDate cum_emitidos_computo -Emit NumM"
	di ". gen pVcomp = sum(Emit)"
	di ". count if pVcomp~=cum_emitidos_computo"
	count if pVcomp~=cum_emitidos_computo
	di as text "Verifying GUDS variable"
	di as input ". sort computoCompleteOAS"
	di ". gen pVcomp2 = sum(Emit)"
	di ". count if pVcomp2~=cum_emitidos_computo"
	count if pVcomp2~=cum_emitidos_computo
		
	twoway (scatter trepOrder trepOrderOAS) ///
		(scatter trepOrder trepOrderOAS if ~mi(AprobadorDate), msize(tiny)) ///
		(scatter trepOrder trepOrderOAS if NumM==2433, m(i) mlab(NumM)) ///
		if progress<=1, aspect(1) yti("Public Order") xti(" " "Nooruddin Order") ///
		legend(order(1 2) label(1 "In TREP") label(2 "Approval Delayed"))
	
	gen isAp = ~mi(AprobadorDate)
	replace isAp = 2 if progress>1
	table isAp [fw=sumV], c(mean margin) row
	
	sort trepOrder
	gen csV = sum(cond(progress>1,.,sumV))
	gen cm = sum(cond(progress>1,.,MAS-CC))
	gen cw = 100*(cm-cm[_n-1651])/(csV-csV[_n-1651])
	replace csV = 100*csV/csV[_N]
	sort cum_ps_natl_share
	gen nsV = sum(cond(progress>1,.,sumV))
	gen nm = sum(cond(progress>1,.,MAS-CC))
	gen nw = 100*(nm-nm[_n-1651])/(nsV-nsV[_n-1651])
	replace nsV = 100*nsV/nsV[_N]
	
	sum csV if isAp==0
	local ccm `r(max)'
	twoway (scatter cw csV, msize(tiny)) (scatter nw nsV, msize(tiny)) if progress<=1, ///
		xline(95, lc(ceprgraymedium%50)) xti(" " "Percent of TREP Valid Votes Counted") ///
		yti("Margin on Most Recent 5% of Tally Sheets") legend(label(1 "Public Data (Approved)") label(2 "Nooruddin (Verified)")) ///
		xline(`ccm', lp(dash) lc(ceprgraymedium%50)) ///
		note(" " "Approval may be implicit (no Aprobador Date) or explicit" "After broken line, ALL public data entries have explicit approval")
	
		
end

capture: program drop checkNooruddin
program define checkNooruddin

	tempfile t
	buildFullDataSet //, replace
	save `t'
	use "../Nooruddin/nooruddin.bolivia 2019 oas analysis replication data.dta", clear
	keep verificador_date ComputoDate num_mesa_computo ps_natl_share_computo cum_ps_natl_share_computo cum_ps_natl_share cum_emitidos_computo emitidos_reales
	d
	sum
	ren num_mesa NumMesa
	ren ComputoDate nooruddinComputoDate
	merge 1:1 NumMesa using `t', nogen

	gen trepTS = clock(verificador_date,"20YMD#hms#")
	di as text ""
	di "Noooruddin's verificador_date is same as TSE's VerificadorDate, but formatted as string"
	di as input `". gen trepTS = clock(verificador_date,"20YMD#hms#")"'
	di ". count if trepTS~=VerificadorDate & progress<=1"
	count if trepTS~=VerificadorDate & progress<=1
	di as text "Verifying GUDS variable"
	di as input ". count if trepTimeOAS~=verificador_date"
	count if trepTimeOAS~=verificador_date

	gen inTREP = mi(verificador_date)
	sort inTREP verificador_date NumM
	gen vd_num = _n
	sort cum_ps_natl_share
	di as text ""
	di "Nooruddin's TREP order is first by verificador_date (string) breaking ties by mesa number"
	di as input  ". gen inTREP = mi(verificador_date)"
	di ". sort inTREP verificador_date NumM"
	di ". gen vd_num = _n"
	di ". sort cum_ps_natl_share"
	di ". count if vd_num~=_n"
	count if vd_num~=_n
	di as text "Verifying GUDS variable"
	gen terr = trepCompleteOAS-100*cum_ps_natl_share
	sort trepOrderOAS
	di as input ". sort trepOrderOAS"
	di ". count if vd_num~=_n"
	count if vd_num~=_n
	di ". gen terr = trepCompleteOAS-100*cum_ps_natl_share"
	di ". sum terr"
	sum terr


	gen computoTS = clock(nooruddinComputoDate,"DM20Y hm")
	format %tcDD/NN/CCYY_Hh:MM_a.m. computoTS
	di as text ""
	di "Noooruddin's ComputoDate is same as TSE, but formatted as string"
	di as input `". gen computoTS = clock(nooruddinComputoDate,"DM20Y hm")"'
	di ". count if computoTS~=ComputoDate"
	count if computoTS~=ComputoDate
	di as text "Verifying GUDS variable"
	di as input ". count if computoTimeOAS~=nooruddinComputoDate"
	count if computoTimeOAS~=nooruddinComputoDate
	
	sort nooruddinComputoDate cum_emitidos_computo computoEmit NumM
	gen pVcomp = sum(Emit)
	sort computoOrderOAS
	gen pVcomp2 = sum(Emit)
	di as text ""
	di "It's not clear how Nooruddin broke ties in the computo, but progression is by ComputoDate (as string)"
	di "	and it appears ties resulting from zero emitidos have the zeroes last"
	di as input ". gsort nooruddinComputoDate cum_emitidos_computo -Emit NumM"
	di ". gen pVcomp = sum(Emit)"
	di ". count if pVcomp~=cum_emitidos_computo"
	count if pVcomp~=cum_emitidos_computo
	di as text "Verifying GUDS variable"
	di as input ". sort computoCompleteOAS"
	di ". gen pVcomp2 = sum(Emit)"
	di ". count if pVcomp2~=cum_emitidos_computo"
	count if pVcomp2~=cum_emitidos_computo
		
	twoway (scatter trepOrder trepOrderOAS) ///
		(scatter trepOrder trepOrderOAS if ~mi(AprobadorDate), msize(tiny)) ///
		(scatter trepOrder trepOrderOAS if NumM==2433, m(i) mlab(NumM)) ///
		if progress<=1, aspect(1) yti("Public Order") xti(" " "Nooruddin Order") ///
		legend(order(1 2) label(1 "In TREP") label(2 "Approval Delayed"))
	
	gen isAp = ~mi(AprobadorDate)
	replace isAp = 2 if progress>1
	table isAp [fw=sumV], c(mean margin) row
	
	sort trepOrder
	gen csV = sum(cond(progress>1,.,sumV))
	gen cm = sum(cond(progress>1,.,MAS-CC))
	gen cw = 100*(cm-cm[_n-1651])/(csV-csV[_n-1651])
	replace csV = 100*csV/csV[_N]
	sort cum_ps_natl_share
	gen nsV = sum(cond(progress>1,.,sumV))
	gen nm = sum(cond(progress>1,.,MAS-CC))
	gen nw = 100*(nm-nm[_n-1651])/(nsV-nsV[_n-1651])
	replace nsV = 100*nsV/nsV[_N]
	
	sum csV if isAp==0
	local ccm `r(max)'
	twoway (scatter cw csV, msize(tiny)) (scatter nw nsV, msize(tiny)) if progress<=1, ///
		xline(95, lc(ceprgraymedium%50)) xti(" " "Percent of TREP Valid Votes Counted") ///
		yti("Margin on Most Recent 5% of Tally Sheets") legend(label(1 "Public Data (Approved)") label(2 "Nooruddin (Verified)")) ///
		xline(`ccm', lp(dash) lc(ceprgraymedium%50)) ///
		note(" " "Approval may be implicit (no Aprobador Date) or explicit" "After broken line, ALL public data entries have explicit approval")
	
	
end

capture: program drop addNooruddinExtended
program define addNooruddinExtended

	gen shareMASoas = 100*computoMAS/computoEmit
	lab var shareMASoas "Nooruddin: MAS percent of emitidos (excluding zero)"
	gen shareCCoas = 100*computoCC/computoEmit
	lab var shareCCoas "Nooruddin: CC percent of emitidos (excluding zero)"
	gen oasDep = Dep if Pa=="Bolivia" & inlist(Dep,"Beni","Chuquisaca","Cochabamba","La Paz","Potosí","Santa Cruz","Tarija")
	replace oasDep = "others" if mi(oasDep)
	lab var oasDep "Nooruddin: Department categories (for tables)"
	gen oasDep2 = cond(Dep=="Potosí" & progress>1, "non-TREP Potosí", cond(inlist(Dep,"Tarija"),"others",oasDep))
	lab var oasDep2 "Nooruddin: Department categories (for tables) (with early/late Potosí)"
	gen trep95oas = trepCompleteOAS>95
	lab var trep95oas "Nooruddin: 'first 95' vs 'last 5+computo only'"
	gen trep95oas2 = cond(progress>1, 2, trep95oas)
	lab var trep95oas2 "Nooruddin: 'first 95' vs 'last 5' vs 'computo only'"
	gen computo95oas = computoCompleteOAS>95
	lab var computo95oas "Nooruddin: 'first 95' vs 'last 5'"
	notes computo95oas: Based on Nooruddin's erroneous sorting of timestamps in alphabetical order
	lab def oas95 0 "First 95" 1 "Last 5" 2 "Computo only"
	lab val trep95oas* computo95oas oas95

end

capture: program drop Nooruddin
program define Nooruddin

	buildFullDataSet, replace
	addNooruddinExtended
	
	di as input ""
	di "*	OAS top of p.90"
	table oasDep if trep95oas==0, c(sum computoValidosen sum computoMAS sum computoCC) format(%20.0fc) row
	table oasDep if trep95oas==1, c(sum computoValidosen sum computoMAS sum computoCC) format(%20.0fc) row
	di as input ""
	di "*	OAS bottom of p.90"
	table oasDep trep95oas2, c(mean shareMASoas) format(%4.1fc) row
	table oasDep trep95oas2, c(mean shareCCoas) format(%4.1fc) row
	di as input ""
	
	/*
	di "*	OAS p.92"
	twoway (scatter shareMASoas computoCompleteOAS, sort mcolor(gray%60) msymbol(point)) ///
			(lowess shareMASoas computoCompleteOAS if computoCompleteOAS<95, mean bwidth(0.3) lcolor(green) lwidth(vthick)) ///
			(lowess shareMASoas computoCompleteOAS if computoCompleteOAS>95, mean bwidth(0.6) lcolor(red) lwidth(vthick)) ///
		, yline(50, lcolor(black)) xscale(extend nofextend) xline(95, lcolor(black)) xlabel(0 95 100) leg(off) ///
			graphregion(style(none) color(none)) title("Bolivia Presidential Election 2019") ///
			xtitle("Cumulative National Vote Share Counted in Computo") ytitle("PS-Level MAS Vote Share")
	twoway (scatter shareCCoas computoCompleteOAS, sort mcolor(gray%60) msymbol(point)) ///
			(lowess shareCCoas computoCompleteOAS if computoCompleteOAS<95, mean bwidth(0.3) lcolor(green) lwidth(vthick)) ///
			(lowess shareCCoas computoCompleteOAS if computoCompleteOAS>95, mean bwidth(0.6) lcolor(red) lwidth(vthick)) ///
		, yline(50, lcolor(black)) xscale(extend nofextend) xline(95, lcolor(black)) xlabel(0 95 100) leg(off) ///
			graphregion(style(none) color(none)) title("Bolivia Presidential Election 2019") ///
			xtitle("Cumulative National Vote Share Counted in Computo") ytitle("PS-Level CC Vote Share")
	di as input ""
	
	di "	Reanalysis: OAS p.92"
	twoway (scatter shareCCoas computoComplete, sort mcolor(gray%60) msymbol(point)) ///
			(lowess shareCCoas computoComplete if computoComplete<95, mean bwidth(0.3) lcolor(green) lwidth(vthick)) ///
			(lowess shareCCoas computoComplete if computoComplete>95, mean bwidth(0.6) lcolor(red) lwidth(vthick)) ///
		, yline(50, lcolor(black)) xscale(extend nofextend) xline(95, lcolor(black)) xlabel(0 95 100) leg(off) ///
			graphregion(style(none) color(none)) title("Bolivia Presidential Election 2019") ///
			xtitle("Cumulative National Vote Share Counted in Computo") ytitle("PS-Level CC Vote Share")
	*/
	di "*	OAS top of p.93"
	table oasDep2 computo95oas, c(mean shareMASoas) format(%4.1fc) row
	table oasDep2 computo95oas, c(mean shareCCoas) format(%4.1fc) row

	table ceprSix computo95 [fw=sumV], c(mean margin) format(%4.1fc) row
	
	bys rcode progress: egen rpmean = mean(margin)
	bys rcode progress: egen rpmed = median(margin)
	bys rcode progress: egen rpmad = mad(margin)
	gen ad = margin-rpmed
	gen aad = abs(ad)
	gen iso = ad*invnormal(0.75)>rpmad*invnormal(0.999)
	table progress [fw=sumV], c(mean ad) row
	
end

capture: program drop NooruddinError
program define NooruddinError

	use "../Nooruddin/nooruddin.bolivia 2019 oas analysis replication data.dta", clear
	gen computoTS = clock(ComputoDate,"DM20Y hm")
	format %tcDD/NN/CCYY_Hh:MM_a.m. computoTS
	
	
	gen masnum = cond(mi(mas_computo),0,mas_computo)
	gen ccnum = cond(mi(cc_computo),0,cc_computo)
	egen sumV = rowtotal(cc_computo-PAN_BOL_computo)
	gen margin = 100*(masnum-ccnum)/sumV

	sort computoTS
	gen progress = sum(emitidos_computo)
	replace progress = progress/progress[_N]
	
	gen is95N = (cum_ps_natl_share_computo>0.9)+(cum_ps_natl_share_computo>0.95)
	gen is95corrected = (progress>0.9)+(progress>0.95)
	lab var is95N "Nooruddin"
	lab var is95c "Corrected"
	lab def is95 0 "0-90" 1 "90-95" 2 "95-100"
	lab val is95* is95
	
	table is95N [fw=sumV], c(mean margin) row format(%4.2fc)
	table is95corrected [fw=sumV], c(mean margin) row format(%4.2fc)

	sort cum_ps_natl_share_computo
	li ComputoDate cum_ps_natl_share_computo in 3625/3632,  sepby(ComputoDate)
	li ComputoDate cum_ps_natl_share_computo in 3661/3668,  sepby(ComputoDate)
	li ComputoDate cum_ps_natl_share_computo in 23649/23656,  sepby(ComputoDate)

	sort cum_ps_natl_share_computo
	local xcom
	local ycom
	levelsof cum_ps_natl_share_computo if dofc(computoTS)~=dofc(computoTS[_n-1]) & _n>1, local(clevs) clean
	foreach x of local clevs {
		local xcom `xcom' xline(`x')
		local ycom `ycom' yline(`x')
	}

	sort computoTS
	format %tcMon_DD!@HH:MM computoTS
	/*
	scatter computoTS cum_ps_natl_share_computo if dofc(computoTS)<=td(22oct2019) & dofc(computoTS)==dofc(computoTS[_n-1]), msiz(tiny) `xcom' ///
		xti(" " "Nooruddin: Cumulative National Vote Share Counted in Computo") xline(0.32) ///
		yti("") ylab(`=tc(21oct2019 00:00)' `=tc(21oct2019 01:00)' `=tc(21oct2019 13:00)' `=tc(22oct2019 00:00)' `=tc(22oct2019 01:00)' `=tc(22oct2019 13:00)', alt labs(tiny))
	*/
	sort computoTS
	line cum_ps_natl_share_computo computoTS if dofc(computoTS)<=td(22oct2019) & dofc(computoTS)==dofc(computoTS[_n-1]), msiz(tiny) ///
		yti("Nooruddin:" "Cumulative National Vote Share Counted in Computo") ///
		xti("") yline(0.95) xlab(`=tc(21oct2019 00:00)' `=tc(21oct2019 01:00)' `=tc(21oct2019 13:00)' `=tc(22oct2019 00:00)' `=tc(22oct2019 01:00)' `=tc(22oct2019 13:00)', alt labs(tiny))
		
		
end

capture: program drop Newman
program define Newman
	
	
	di as input ""
	di "*"
	di "*	Newman (Original Paper)"
	di "*"
	di "*	Newman (1) Table 3"
	table newmanPrecinctType, c(sum newmanPrecinctTag freq sum MAS sum CC sum sumV) format(%20.0fc) row col
	table newmanPrecinctType [fw=sumV], c(mean margin) format(%6.4fc) row col
	di as input ""
	di "*	Progress of Newman classes"
	table progress newmanPrecinctType, row col
	di as input ""
	di "*	Newman (1) Table 4"
	di "*	 - Mesa-level K-S Tests"
	table newmanPrecinctType, m
	ksmirnov margin if inlist(newmanPrecinctType,2,3), by(newmanPrecinctCode)
	ksmirnov margin if inlist(newmanPrecinctType,0,2), by(newmanPrecinctCode)
	ksmirnov margin if inlist(newmanPrecinctType,1,3), by(newmanPrecinctCode)
	ksmirnov margin if inlist(newmanPrecinctType,0,3), by(newmanPrecinctCode)
	ksmirnov margin if inlist(newmanPrecinctType,1,2), by(newmanPrecinctCode)
	ksmirnov margin if inlist(newmanPrecinctType,0,1), by(newmanPrecinctCode)
	di as input ""
	di "*	Newman (1) Table 6"
	di "*	 - Precinct-level K-S Tests"
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctType,2,3), by(newmanPrecinctCode)
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctType,2,3) & ~newmanPrecinctMAS, by(newmanPrecinctCode)
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctType,2,3) & newmanPrecinctMAS, by(newmanPrecinctCode)
	di as input ""
	di "*	Newman (1) Table 12"
	di "*	 - Municipality-level Counterfactual Results"
	tab newmanMunicipalityType [fw=sumV] 
	table newmanMunicipalityType [fw=sumV], c(mean margin mean newmanMunicipalityCF) format(%6.4fc) row col
	di as input ""
	di "*	Reanalysis: Newman (1) Table 12"
	di "*	 - Precinct-level Counterfactual Results"
	tab newmanPrecinctType [fw=sumV] 
	table newmanPrecinctType [fw=sumV], c(mean margin mean newmanPrecinctCF) format(%6.4fc) row col
	
end

capture: program drop Newman1496
program define Newman1496

	*	This program performs 1000 iterations of Newman's simulated test

	qui capture: use newman_1496, clear
	qui if (_rc~=0) {
		set seed 20200616
		clear
		set obs 1
		/*
		   nmin = 32
		   nmax = 52
		*/
		gen n = _n+31
		gen N = round(2^(n/4))
		replace N = 1496
		expand 1000
		sort N
		gen D = .
		gen rp = .
		gen N_left = .
		mat m = (0.0682, 0.0682)
		mat C = (0.0625, 0.0614 \ 0.0614, 0.0625)
		qui forvalues ii=1/`=_N' {
			local N = N[`ii']
			preserve
			clear
			set obs `N'
			gen n = _n
			drawnorm margin1 margin0, means(m) cov(C)
			gen isLeft = margin1<0
			count if isLeft
			local rnl = r(N)
			keep if isLeft
			reshape long margin, i(n) j(isEarly)
			ksmirnov margin, by(isEarly)
			restore
			replace N_left = `rnl' in `ii'
			replace D = r(D) in `ii'
			replace rp = r(p) in `ii'
			noi li in `ii'
		}
		save newman_1496, replace
	}
	qui {
		gen Dc = sqrt(-ln(0.05/2)/N_left)
		gen ismanualPos = D>Dc
		gen posRate = (rp<0.05)/10
		table isman posRate, m
		lab def ismanualPos 0 "Negative" 1 "Positive"
		lab val ismanualPos ismanualPos
		lab var ismanualPos "K-S Test Result"
	}
	di "* K-S Test Results on 1000 simulations"
	table ismanualPos, row col

end

capture: program drop Newman21000
program define Newman21000

	syntax [, NOGraphs]

	*
	*	Analysis of Positive Rate by Sample Size
	*
	qui capture: use newman_sims, clear
	qui if (_rc~=0) {
		set seed 20200616
		clear
		set obs 21
		/*
		   nmin = 32
		   nmax = 52
		*/
		gen n = _n+31
		gen N = round(2^(n/4))
		expand 1000
		sort N
		gen D = .
		gen Dp = .
		gen t = .
		gen tp = .
		gen N_left = .
		mat m = (0.0682, 0.0682)
		mat C = (0.0625, 0.0575 \ 0.0575, 0.0625)
		mat C = (0.0625, 0.0614 \ 0.0614, 0.0625)
		qui forvalues ii=1/`=_N' {
			local N = N[`ii']
			preserve
			clear
			set obs `N'
			gen n = _n
			drawnorm margin1 margin0, means(m) cov(C)
			ttest margin1==margin0
			scalar tt_cur = r(t)
			scalar tp_cur = r(p)
			gen isLeft = margin1+margin0<0
			count if isLeft
			local rnl = r(N)
			keep if isLeft
			reshape long margin, i(n) j(isEarly)
			ksmirnov margin, by(isEarly)
			restore
			replace N_left = `rnl' in `ii'
			replace D = r(D) in `ii'
			replace Dp = r(p) in `ii'
			replace t = tt_cur in `ii'
			replace tp = tp_cur in `ii'
			noi li in `ii'
		}
		save newman_sims, replace
	}

	qui {
		*	For graphical purposes, only care about absolute value
		replace t = abs(t)

		*	Critical values
		gen tc = invttail(N-1,0.025)
		gen Dc = sqrt(-ln(0.05/2)/N_left)

		gen tposRate = (t>tc)/10
		gen DposRate = (D>Dc)/10

		*	Check that the critical values match Stata's
		gen ismanualPos = D>Dc
		table isman DposRate, m
	}
	
	di as input ""
	di "*	Expected Bleed Rates in Newman Simulations"
	di ". scalar mu = 0.0682"
	di ". scalar s2 = 0.0625"
	di ". scalar rho = 0.0614/s2"
	di ". scalar cut = 0"
	di ""
	di ". scalar b1 = normal((cut-mu)/sqrt(s2))"
	scalar mu = 0.0682
	scalar s2 = 0.0625
	scalar rho = 0.0614/s2
	scalar cut = 0
	scalar b1 = normal((cut-mu)/sqrt(s2))
	scalar de1 = 1-binormal((cut-mu)/sqrt(s2),(cut-mu)/sqrt(s2),rho)/b1
	di as text "Expected percent of Mesa precincts = b1 = " as result b1
	di as text "Expected number of Mesa precincts = b1*1496 = " as result b1*1496
	di ""
	di ". scalar b2 = normal((mu-cut)/sqrt(s2))"
	scalar b2 = normal((mu-cut)/sqrt(s2))
	scalar de2 = 1-binormal((mu-cut)/sqrt(s2),(mu-cut)/sqrt(s2),rho)/b2
	di as text "Expected percent of Morales precincts = b2 = " as result b2
	di ""
	di ". scalar de1 = 1-binormal((cut-mu)/sqrt(s2),(cut-mu)/sqrt(s2),rho)/b1"
	di as text "Expected percent of Mesa precincts that bleed = de1 = " as result de1
	di as text "Critical value of D for 685 observations = sqrt(-ln(0.05/2)/685) = " as result sqrt(-ln(0.05/2)/685)
	di as text "Critical value of D for 545 observations = sqrt(-ln(0.05/2)/545) = " as result sqrt(-ln(0.05/2)/545)
	di ""
	di ". scalar de2 = 1-binormal((mu-cut)/sqrt(s2),(mu-cut)/sqrt(s2),rho)/b2"
	di as text "Expected percent of Morales precincts that bleed = de2 = " as result de2

	
	if ("`nographs'"=="") {
		di "*	Figure 3: K-S Test Results for Simulated Data"
		twoway (line Dc N_left, lp(solid dash)) (scatter D N_left, msize(vtiny) jitter(10)) ///
			(scatteri 0.0661 545 (5) "Newman", pstyle(p4) msize(tiny)) ///
			, xsc(log) ysc(r(0)) xlab(100 200 500 1000 2000) ///
			yline(`=1-binormal((0-mu)/sqrt(s2),(0-mu)/sqrt(s2),rho)/normal((0-mu)/sqrt(s2))', lc(ceprgraymedium)) ///
			yti("D (Equal distributions)") xti(" " "Number of Mesa Precincts") legend(off)
		graph export ${OUTPUT}/Figures/Figure3.png, replace	
		di as input "*	Sources: Newman and author's calculations"
		di
		di "*	Figure 4: t-Test Results for Simulated Data"
		twoway (line tc N) (scatter t N, msize(vtiny) jitter(10)) ///
			, xsc(log) xlab(500 1000 2000 5000) ///
			yti("|t| (Equal means)") xti(" " "Number of Precincts") legend(off)
		graph export ${OUTPUT}/Figures/Figure4.png, replace
		di as input "*	Sources: author's calculations"

		qui {
			collapse (p95) D (sum) DposRate tposRate (mean) N_left Dc, by(N)
			lab var DposRate "K-S Test (Mesa Precincts)"
			lab var tposRate "t-Test (Early vs Late Margin Means)"
		}
		di
		di "*	Figure 5: Comparing K-S and t Test Positive Rates"
		line tposRate DposRate N, yline(5, lc(ceprgraymedium) lp(dash)) ///
			xti(" " "Number of Precincts in Draw") yti("Positive Results (% of tests)") xsc(log) ///
			xlab(500 1000 2000 5000) legend(col(1))
		graph export ${OUTPUT}/Figures/Figure5.png, replace
		di as input "*	Sources: Newman and author's calculations"
		di
	}
	
end

capture: program drop FurtherRebuttal
program define FurtherRebuttal

	syntax [, NOGraphs]

	qui addNewmanExtended
	di "*"
	di "*	Further Rebuttal to Newman"
	di "*"
	Newman
	di as input ""
	di "*"
	di "*	Initial Graphs"
	di "*"
	qui {
		capture: mkdir ${OUTPUT}
		capture: mkdir ${OUTPUT}/Figures
		preserve
		keep if newmanPrecinctTag & newmanPrecinctType==3
			count if newmanPrecinctEarlyMargin<0 & newmanPrecinctMargin<0
		local ll = r(N)
		count if newmanPrecinctEarlyMargin<0 & ~mi(newmanPrecinctMargin)
		local ul = r(N)-`ll'
	}
	if ("`nographs'"=="") {
		di "*	Figure 1: Early and Late Margins in All Precincts"
		scatter newmanPrecinctMargin newmanPrecinctEarlyMargin, ///
			m(Oh) mlw(vthin) msize(tiny) aspect(1) yti("Late Margin") xti(" " "Early Margin")
		graph export ${OUTPUT}/Figures/Figure1.png, replace
		di as input "*	Sources: TSE, Newman, and author's calculations"
		di ""
		di "*	Figure 2: Early and Late Margins in “Mesa” Precincts"
		twoway (scatter newmanPrecinctMargin newmanPrecinctEarlyMargin, m(Oh) mlw(vthin) msize(tiny)) ///
				(scatter newmanPrecinctMargin newmanPrecinctEarlyMargin if newmanPrecinctMargin>=0, m(Oh) mlw(vthin) msize(tiny)) ///
				(scatteri -10 -90 "`ll' precincts", m(i) pstyle(p1)) ///
				(scatteri 90 -90 "`ul' precincts", m(i) pstyle(p2)) ///
			if newmanPrecinctEarlyMargin<0, aspect(1) ///
			yti("Late Margin") xti(" " "Early Margin") yline(0, lc(ceprgraymedium)) xline(0, lc(ceprgraymedium)) ///
			xlab(-100(50)100) ylab(-100(50)100) legend(off)
		graph export ${OUTPUT}/Figures/Figure2.png, replace
	}
	di as input "*	Sources: TSE, Newman, and author's calculations"
	di as input ""
	Newman1496
	di as input ""
	Newman21000, `nographs'
	di as input ""
	qui restore
	
	di "*"
	di "*	Newman's cutoff"
	di "*"
	di "*	Newman (2) Table 5"
	di "*	 - Cutoff at -3"
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctCode,0,1) & ~newmanPrecinctMAS3, by(newmanPrecinctCode)
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctCode,0,1) & newmanPrecinctMAS3, by(newmanPrecinctCode)
	di as input ""
	di "*	 - Cutoff at 0"
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctCode,0,1) & ~newmanPrecinctMAS, by(newmanPrecinctCode)
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctCode,0,1) & newmanPrecinctMAS, by(newmanPrecinctCode)
	di as input ""
	di "*	 - Cutoff at +6"
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctCode,0,1) & ~newmanPrecinctMAS6, by(newmanPrecinctCode)
	ksmirnov newmanPrecinctMargin if newmanPrecinctTag & inlist(newmanPrecinctCode,0,1) & newmanPrecinctMAS6, by(newmanPrecinctCode)
	di as input ""
	di "*	Comparison to Newman's alternative cutoff location"
	table newmanPrecinctMAS newmanPrecinctMAS6 if newmanPrecinctTag & inlist(newmanPrecinctCode,0) , row col
	di as input
	di "*	Comparison to soft cuoff"
	table newmanPrecinctMAS newmanPrecinctMAS if newmanPrecinctTag & inlist(newmanPrecinctCode,0) , row col
	di as input ""
	di "*	Example precincts"
	table after_cutoff if inlist(Rec,"Esc. Sub. Central la Porfia"), c(sum MAS sum CC) format(%20.0fc) row col
	table after_cutoff if inlist(Rec,"Colegio Walter Alpire 2do Patio"), c(sum MAS sum CC) format(%20.0fc) row col
	di as input
	di "*	Actual and Counterfactual results"
	di "*		- Actual, Adjusting late vote in precincts Mesa led early, and Adjusting all late vote in split precincts"
	table newmanPrecinctType [fw=sumV], c(mean margin mean newmanPrecinctCF mean newmanPrecinctCFall) format(%6.4fc) row col
	
end

capture: program drop addChumaceroExtended
program define addChumaceroExtended
	
	addChumaceroMargins if SIN_TREP | sumV==0 | Pa=="Colombia", prefix(chumacero5)
	addCounterfactual cf5n, p(chumacero5)
	addCounterfactual cf5l, p(chumacero5) l(cf cp cd cv cm l)
	gen d5l = margin-cf5l
	gen d5n = margin-cf5n
	lab var d5l "Dif 1"
	lab var d5n "Dif 2"

	addChumaceroMargins if SOMBRA, prefix(chumacero7) trep
	addCounterfactual cf7n, p(chumacero7) trep
	addCounterfactual cf7r, p(chumacero7) l(cf cp cd cv cm l r) trep
	addCounterfactual cf7l, p(chumacero7) l(cf cp cd cv cm l) trep
	gen d7l = TREP_margin-cf7l
	gen d7r = TREP_margin-cf7r
	gen d7n = TREP_margin-cf7n
	lab var d7l "Dif 1"
	lab var d7r "Dif 1.5"
	lab var d7n "Dif 2"
	
	addChumaceroMargins if Q5, prefix(chumacero11)
	addCounterfactual cf11n, p(chumacero11)
	addCounterfactual cf11l, p(chumacero11) l(cf cp cd cv cm l)
	gen d11l = margin-cf11l
	gen d11n = margin-cf11n
	lab var d11l "Dif 1"
	lab var d11n "Dif 2"


	
end

capture: program drop Chumacero
program define Chumacero

	di as input ""
	di "*"
	di "*	Chumacero"
	di "*"
	di "*	Cuadro 2"
	di "*	TREP0"
	di "*	 - note that acta 2433 has zero votes in TREP0, so we break that out"
	table is2433 if progress==0, c(freq sum Ins sum TREP_Emit sum TREP_sumV) format(%20.0fc) row col
	table is2433 if progress==0, c(sum TREP_CC sum TREP_MAS sum TREP_Bla sum TREP_Nul) format(%20.0fc) row col
	table is2433 if progress==0 [fw=sumV], c(mean TREP_margin) format(%6.4fc) row col
	di as input ""
	di "*	TREP1"
	table APAGON if ~SIN_TREP, c(freq sum Ins sum TREP_Emit sum TREP_sumV) format(%20.0fc) row col
	table APAGON if ~SIN_TREP, c(sum TREP_CC sum TREP_MAS sum TREP_Bla sum TREP_Nul) format(%20.0fc) row col
	table APAGON if ~SIN_TREP [fw=sumV], c(mean TREP_margin) format(%6.4fc) row col
	di as input ""
	di "*	COMP1"
	di "*	(same as Cuadro 4)"
	table SIN_TREP, c(freq sum Ins sum Emit sum sumV) format(%20.0fc) row col
	table SIN_TREP, c(sum CC sum MAS sum Bla sum Nul) format(%20.0fc) row col
	table SIN_TREP [fw=sumV], c(mean margin) format(%6.4fc) row col
	
	di as input ""
	di "*	Footnote 25"
	di "*	 - Note that we get 5 province and 5 department, rather than 10 department"
	di "*	 - This is NOT an error the match level is correct"
	di "*		1654-67 match at the FIRST-order administrative division (Salta)"
	di "*		Chumacero lists these as province (the Argentine name for the FIRST-order administrative division)"
	di "*		so consistent with TSE we call this a department match"
	table chumacero5Matchlevel if SIN_TREP & sumV>0 & Pa~="Colombia", row col

	di as input ""
	di "*	Cuadro 5"
	table chumacero5NearestType if SIN_TREP & sumV>0 & Pa~="Colombia", c(mean margin mean d5l mean d5n) row col format(%6.4fc)

	di as input ""
	di "*	Cuadro 6"
	table APAGON, c(freq sum Ins sum TREP_Emit sum TREP_sumV) format(%20.0fc) row col
	table APAGON, c(sum TREP_CC sum TREP_MAS) format(%20.0fc) row col
	table APAGON [fw=TREP_sumV], c(mean TREP_margin) format(%6.4fc) row col
	
	di as input ""
	di "*	Cuadro 7"
	table chumacero7NearestType if APAGON, c(mean TREP_margin mean d7l mean d7r mean d7n) row col format(%6.4fc)
	table chumacero7NearestType if ~SIN_TREP [fw=TREP_sumV], c(freq mean TREP_margin mean d7l mean d7r mean d7n) row col format(%6.4fc)
	mata: CCC

	di as input ""
	di "*	Cuadro 10"
	table Q [fw=sumV], c(mean margin) row col format(%6.4fc)

	di as input ""
	di "*	Cuadro 11"
	table chumacero11NearestType if Q5, c(mean margin mean d11l mean d11n) row col format(%6.4fc)
	
local tvar ComputoDate
	bys cdcode (`tvar'): gen wsum = sum(sumV)
	bys cdcode (`tvar'): gen p_margin = 100*sum(MAS-CC)/wsum[_N]
	bys cdcode (`tvar'): gen p_sumV = 100*wsum/wsum[_N]
	bys cmcode (`tvar'): gen mwsum = sum(sumV)
	bys cmcode (`tvar'): gen mp_margin = 100*sum(MAS-CC)/mwsum[_N]
	bys cmcode (`tvar'): gen mp_sumV = 100*mwsum/mwsum[_N]
	bys rcode (`tvar'): gen rwsum = sum(sumV)
	bys rcode (`tvar'): gen rp_margin = 100*sum(MAS-CC)/rwsum[_N]
	bys rcode (`tvar'): gen rp_sumV = 100*rwsum/rwsum[_N]

	*bys cdcode (`tvar'): replace p_margin = p_margin-(p_sumV/100)*p_margin[_N]
	*bys cmcode (`tvar'): replace mp_margin = mp_margin-(mp_sumV/100)*mp_margin[_N]
	*bys rcode (`tvar'): replace rp_margin = rp_margin-(rp_sumV/100)*rp_margin[_N]

	xtline p_margin if RUE==1, i(Dep) t(p_sumV) ov addplot(scatter p_margin p_sumV if RUE==1 & regexm(Dep,"Potos"))
	xtline mp_margin if RUE==1 & regexm(Dep,"Potos"), i(Muni) t(mp_sumV) ov addplot(scatter mp_margin mp_sumV if RUE==1 & regexm(Dep,"Potos") & regexm(Mun,"Atocha"))
	xtline margin if RUE==1 & regexm(Dep,"Potos") & regexm(Mun,"Atocha"), i(Rec) t(mp_sumV) ov recast(scatter)
		
	
end

capture: program drop NewmanFramework
program define NewmanFramework

	syntax [, NOGraphs]

	qui {
		addChumaceroMargins if ~preInt, prefix(base)
		addCounterfactual baser, p(base) l(r)
		addCounterfactual basel, p(base) l(l)
		addCounterfactual baselr, p(base) l(l r)
		addCounterfactual baseFULL, p(base) l(cf cp cd cv cm l r)
	}
	
	di as input ""
	di "*"
	di "*	Newman and a Framework for Analysis"
	di "*"
	di "*	Valid Votes by Type of Precinct"
	tab basePrecinctType [fw=sumV]
	di as input ""
	di "*	Election Results and Margin Calculation for the Split Precinct Colegio Sebastian Pagador"
	table basePrecinctType if Rec=="Colegio Sebastian Pagador", c(sum MAS sum CC sum sumV) format(%20.0fc)
	table basePrecinctType if Rec=="Colegio Sebastian Pagador" [fw=sumV], c(mean margin) format(%6.4fc)
	di as input ""
	di "*	Early and Late Margins in Two Select Precincts"
	table Rec basePrecinctType if inlist(Rec,"Colegio Sebastian Pagador","U.A.J.M.S Campus") [fw=sumV], ///
			c(freq mean margin mean baser) row
	di as input ""
	di "*	Overall Election Results: Official and with Precinct-Adjustment"
	table basePrecinctType [fw=sumV], c(mean margin mean baser) row format(%6.4fc)
	
	di as input ""
	di "*	Required counterfactual in split localities"
	qui sum margin if basePrecinctType==1 [fw=sumV]
	local lmar1 = r(mean)
	qui sum baser [fw=sumV]
	local rmar = r(mean)
	di as text `"`lmar1'-(`rmar'-10.00)*6137671/153890 = "' as result `lmar1'-(`rmar'-10.00)*6137671/153890
	di `lmar1'-(`rmar'-10.00)*6137671/153890
	di as input ""
	di "*	Locality adjustment for Precincts that were counted All After the interruption"
	tab baseLocalityType if basePrecinctType==1 [fw=sumV]
	table baseLocalityType if basePrecinctType==1 [fw=sumV], c(mean margin mean baser mean baselr) row format(%6.4fc)

	di as input ""
	di "*"
	di "*	Potosí (location)"
	di "*"
	preserve
	qui gen sim = 100*(1/(1+SÍ/NO)-0)	
	di "*	Results in Potosí: Precincts Counted All After the Interruption"
	table Rec if regexm(Dep,"Potos") & regexm(Mun,"Potos") & regexm(Loc,"Potos") & basePrecinctType~=3 ///
		& inlist(Rec,"Esc. Gregorio Barriga","Esc. 6 de Junio","Esc. Jose A.Zampa") [fw=sumV], ///
		c(mean margin mean sim min SÍ min NO) row col
	qui sum margin if regexm(Dep,"Potos") & regexm(Mun,"Potos") & regexm(Loc,"Potos") & basePrecinctType~=3 & preInt [fw=sumV]
	local em `r(mean)'
	qui sum margin if regexm(Dep,"Potos") & regexm(Mun,"Potos") & regexm(Loc,"Potos") & basePrecinctType~=3 & ~preInt [fw=sumV]
	local lm `r(mean)'
	di as input ""
	di "*	Votes in 2016 and 2019: Potosí Locality"
	table Rec preInt if regexm(Dep,"Potos") & regexm(Mun,"Potos") & regexm(Loc,"Potos") & basePrecinctType~=3 [fw=sumV], c(mean margin mean baselr mean sim) replace row
	qui {
		gen ismean = mi(Rec)
		replace Rec = "AVERAGE (before)" if mi(Rec) & preInt
		replace Rec = "AVERAGE (all after)" if mi(Rec)
		gsort -table1
		gen t2r = _n
	}
	if ("`nographs'"=="") {
		twoway (scatter t2r table1 if preInt & ~ismean, m(Oh) mlw(vthin) msize(tiny)) ///
			(scatter t2r table1 if ~preInt * ~ismean, m(Oh) mlw(vthin) msize(tiny) mlab(Rec) mlabs(vsmall) mlabpos(2)) ///
			(scatter t2r table1 if ismean & preInt, pstyle(p1) mlw(vthin) msize(tiny) mlab(Rec) mlabs(vsmall) mlabpos(2)) ///
			(scatter t2r table1 if ismean & ~preInt, pstyle(p2) mlw(thin) msize(tiny) mlab(Rec) mlabs(vsmall) mlabpos(2)) ///
				, ylab("") yti("") xti(" " "Margin (2019)") legend(order(1 2) label(1 "(any) Before") ///
					label(2 "All After"))
		graph export ${OUTPUT}/Figures/fig1.png, replace
		gsort -table3
		replace t2r = _n
		gen mlp = cond(Rec=="Esc. 6 de Junio",10,2)
		twoway (scatter table3 table1 if preInt & ~ismean, m(Oh) mlw(vthin) msize(tiny)) ///
			(scatter table3 table1 if ~preInt * ~ismean, m(Oh) mlw(vthin) msize(tiny) mlab(Rec) mlabs(vsmall) mlabvpos(mlp)) ///
			(scatter table3 table1 if ismean & preInt, pstyle(p1) mlw(vthin) msize(tiny) mlab(Rec) mlabs(vsmall) mlabvpos(mlp)) ///
			(scatter table3 table1 if ismean & ~preInt, pstyle(p2) mlw(thin) msize(tiny) mlab(Rec) mlabs(vsmall) mlabvpos(mlp)) ///
				if ~mi(table3) ///
				, yti("Percent NO (2016)") xti(" " "Margin (2019)") legend(order(1 2) label(1 "(any) Before") ///
					label(2 "All After"))
		graph export ${OUTPUT}/Figures/fig2.png, replace
	}
	
	restore
	di as input ""
	di "*	Overall Election Results: Official and with Closest Available Adjustment"
	table baseLocalityType if basePrecinctType==1 [fw=sumV], c(mean margin mean baser mean baselr mean basel) row format(%6.4fc)
	table basePrecinctType [fw=sumV], c(mean margin mean baser mean baselr mean basel) row format(%6.4fc)
	
	di as input ""
	di "*	Election Results and Margin Calculation for the Split Precinct Colegio Sebastian Pagador"
	*	(add to above)
	table preInt if Loc=="Cochabamba", c(sum MAS sum CC sum sumV) row col format(%14.0fc)
	table preInt if Loc=="Cochabamba" [fw=sumV], c(mean margin) row col format(%6.4fc)

	di as input ""
	di "*	Within Localities, Late Results Followed Early Results In Each Precinct"
	qui {
		egen rtag = tag(rcode preInt)
		bys lcode: egen nre = total(cond(preInt,rtag,.))
		gen dr = basePrecinctMargin-baseLocalityMargin
		gen dm = margin-baseLocalityMargin
		
		local ifs if basePrecinctType==3 & nre>1 // & regexm(Loc,"Cochamba")
		noi sum dr [fw=sumV] `ifs'
		local drm `r(mean)'
		noi sum dm [fw=sumV] `ifs'
		local dmm `r(mean)'
	}
	if ("`nographs'"=="") {
		twoway (function x, range(-100 100) lc(ceprgraymedium%50)) ///
			(scatter dm dr, m(Oh) mlw(vthin) msize(tiny) pstyle(p3)) ///
			(scatteri `dmm' `drm' "official" 0 `drm' "Newman estimate", pstyle(p1) m(Oh) mlw(vthin)) ///
			`ifs', aspect(1) legend(off) xline(0, lc(ceprgraymedium%50)) yline(0, lc(ceprgraymedium%50)) ///
			xti(" " "Early Margin (Precinct) - Early Margin (Locality)") yti("Late Margin (Sheet) - Early Margin (Locality)")
		graph export ${OUTPUT}/Figures/fig3.png, replace
	}
	
	di as input ""
	di "*	Contributions to Underestimate by Locality"
	preserve
	qui {
		sum sumV, meanonly
		local rsumV = r(sum)
		macro li _rsumV
		collapse (rawsum) sumV (mean) margin baselr basel baseLocalityMargin if ~preInt [fw=sumV], by(Pa-Loc)
		gen dm = baselr-basel
		gen vc = dm*sumV/`rsumV'
		sum vc if Loc=="Trinidad", meanonly
		scalar tv = r(sum)
		gsort -sumV
		gen sizerank = _n
		replace sizerank = . if abs(vc)<abs(tv)
		li Loc sumV vc sizerank in 1/22
		collapse (rawsum) sumV (mean) margin baselr basel baseLocalityMargin (firstnm) Pa Dep Mun Loc [fw=sumV], by(sizerank)	
		gen dm = baselr-basel
		macro li _rsumV
		gen vc = dm*sumV/`rsumV'
		gsort -vc
		gen conrank = cond(sumV>sumV[1],.,_n)
		sort conrank
		gen vcc = sum(vc)
		gen sc = sum(sumV)
		replace sc = 100*sc/sc[_N]
		format %20.0fc sumV
	}
	li Dep Mun Loc sumV sc baseLoc dm vc vcc
	restore
	
	di as input ""
	di "*	Footnote on Cochabamba"
	table preInt if Loc=="Cochabamba" [fw=sumV], c(mean margin mean baser mean basel) row format(%6.4fc)
	
	local g1 Department
	local g2 Country
	gen dc = .
	lab var dc "Geography Type"
	lab val dc baseType
	qui foreach g in Province Municipality Locality Precinct {
		replace dc = base`g1'Type-(base`g2'Type==1) if base`g'Type==1
		noi di as input ""
		noi di "* Overall Election Results: `g' Counted All After"
		noi tab dc [fw=sumV]
		noi table dc [fw=sumV], c(mean margin mean baseFULL) format(%6.4fc) row col
		replace dc = .
		local g2 `g1'
		local g1 `g'
	}
	replace dc = base`g1'Type-0.75*(base`g1'Type==1)+0.5*(base`g1'Type==1 & base`g2'Type==3)
	tab dc [fw=sumV]
	table dc [fw=sumV], c(mean margin mean baseFULL) format(%6.4fc) row col
	
	di as input ""
	di "*	Overall Election Results: Comparison to Newman"
	table dc [fw=sumV], c(mean baseFULL mean basel) row format(%6.4fc)
	
end

capture program drop InadequateControls
program define InadequateControls

	syntax [, NOGraphs]

	di as input ""
	di "*"
	di "*	Inadequate Geographical Controls in Other Analyses"
	di "*"
	di "*	Nooruddin/OAS"
	qui {
		addNooruddinExtended
		addChumaceroMargins if trep95oas, prefix(noor)
		addCounterfactual noorcf, p(noor) l(cf cp cd cv cm l r)
		capture: gen dc = noorPrecinctType
		lab val dc noorType
		replace dc = noorPrecinctType-0.75*(noorPrecinctType==1)+0.5*(noorPrecinctType==1 & noorLocalityType==3)
		gen m100 = margin/100
		gen n100 = noorcf/100
	}

	di as input ""
	di "*	Beni: Results across Nooruddin’s 95/5 split"
	tab dc if Pa=="Bolivia" & Dep=="Beni"  [fw=sumV]
	table dc if Pa=="Bolivia" & Dep=="Beni" [fw=sumV], c(rawsum MAS rawsum CC sum m100 sum n100) row format(%14.0fc)
	tab trep95oas if Pa=="Bolivia" & Dep=="Beni"  [fw=sumV]
	table trep95oas if Pa=="Bolivia" & Dep=="Beni" [fw=sumV], c(rawsum MAS rawsum CC sum m100 sum n100) row format(%14.0fc)
	table RUE trep95oas if Pa=="Bolivia" & Dep=="Beni" [fw=sumV], c(freq mean noorMunicipalityMargin) row col

	di as input ""
	di "*	Overall Election: Results across Nooruddin’s 95/5 split"
	tab dc  [fw=sumV]
	table dc [fw=sumV], c(rawsum MAS rawsum CC sum m100 sum n100) row format(%14.0fc)
	tab trep95oas [fw=sumV]
	table trep95oas [fw=sumV], c(rawsum MAS rawsum CC sum m100 sum n100) row format(%14.0fc)

	di as input ""
	di "*"
	di "*	Escobari and Hoover"
	di "*"
	qui {
		addChumaceroMargins if ~preInt, prefix(eh)  l(f p d v m l r)
		addCounterfactual ehm, p(eh) l(m)
		addCounterfactual ehr, p(eh) l(r)
		egen ehsumV = total(cond(preInt,sumV,.))
		egen ehX = total(cond(preInt,MAS-CC,.))
		replace ehX = 100*ehX/ehsumV
		replace ehX = margin if preInt
		
		sum sumV if preInt
		local ehsumV = r(sum)
		sum sumV
		foreach var of varlist ehX ehm ehr {
			gen err_`var' = (margin-`var')/(`r(sum)'-`ehsumV')
		}
		
	}
	di as input "*	Overall Election: Escobari and Hoover"
	tab ehPrecinctType [fw=sumV]
	table ehPrecinctType [fw=sumV], c(mean ehX mean ehm mean ehr mean margin) row format(%6.4fc)
	table ehPrecinctType [fw=sumV], c(sum err_ehX sum err_ehm sum err_ehr) row format(%6.4fc)
	di as input "*	Appendix"
	*	Replications
	*	No weights
		*	VV
		reg emargin ib(1).preInt, vce(robust)
		areg emargin ib(1).preInt, a(mcode) vce(robust)
		areg emargin ib(1).preInt, a(rcode) vce(robust)
		*	sumV
		reg margin ib(1).preInt, vce(robust)
		areg margin ib(1).preInt, a(mcode) vce(robust)
		areg margin ib(1).preInt, a(rcode) vce(robust)
	*	Weight by VV
		*	VV
		reg emargin ib(1).preInt [aw=Validosen], vce(robust)
		areg emargin ib(1).preInt [aw=Validosen], a(mcode) vce(robust)
		areg emargin ib(1).preInt [aw=Validosen], a(rcode) vce(robust)
		*	sumV
		reg margin ib(1).preInt [aw=Validosen], vce(robust)
		areg margin ib(1).preInt [aw=Validosen], a(mcode) vce(robust)
		areg margin ib(1).preInt [aw=Validosen], a(rcode) vce(robust)
	*	Weight by sumV
		*	VV
		reg emargin ib(1).preInt [aw=sumV], vce(robust)
		areg emargin ib(1).preInt [aw=sumV], a(mcode) vce(robust)
		areg emargin ib(1).preInt [aw=sumV], a(rcode) vce(robust)
		*	sumV
		reg margin ib(1).preInt [aw=sumV], vce(robust)
		areg margin ib(1).preInt [aw=sumV], a(mcode) vce(robust)
		areg margin ib(1).preInt [aw=sumV], a(rcode) vce(robust)

	di as input ""
	di "*"
	di "*	Villegas"
	di "*"
	qui {
		bys villegasThree: egen vsum = total(cond(preInt,sumV,.))
		bys villegasThree: egen vmar = total(cond(preInt,MAS-CC,.))
		bys rcode preInt: egen vpsum = total(sumV), missing
		bys rcode preInt: egen vpmar = total(MAS-CC), missing
		replace vmar = 100*vmar/vsum
		replace vmar = margin if preInt
		replace vpmar = 100*vpmar/vpsum
	}
	di "*	Proportions in Villegas' Three Geographies"
	tab villegasThree preInt, col
	tab villegasThree preInt [fw=sumV], col
	di as input ""
	di "*	Overall Election Results: Including Full Adjustment and Villegas's Estimate"
	table baseLocalityType if basePrecinctType==1 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	table basePrecinctType [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	di as input ""
	di "*	“Ciudad” Results: Including Full Adjustment and Villegas’s Estimate"
	table baseLocalityType if basePrecinctType==1 & villegasThree==2 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	table basePrecinctType if villegasThree==2 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	di as input ""
	di "*	“Provincia” Results: Including Full Adjustment and Villegas’s Estimate"
	table baseLocalityType if basePrecinctType==1 & villegasThree==1 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	table basePrecinctType if villegasThree==1 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	di as input ""
	di "*	“Extranjero” Results: Including Full Adjustment and Villegas’s Estimate"
	table baseLocalityType if basePrecinctType==1 & villegasThree==0 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	table basePrecinctType if villegasThree==0 [fw=sumV], c(mean margin mean baseFULL mean vmar) format(%6.4fc) row
	
	di "*	Outside Bolivia, Late Results Followed Early Results In Each Precinct"
	qui {
		capture: drop rtag nre dr dm
		egen rtag = tag(rcode preInt)
		bys cfcode: egen nre = total(cond(preInt,rtag,.))
		gen dr = basePrecinctMargin // -baseRUEMargin
		gen dm = vpmar // -baseRUEMargin
		
		local ifs if basePrecinctType==3 & Pa~="Bolivia"
		noi sum dr [fw=sumV] `ifs'
		local drm `r(mean)'
		noi sum dm [fw=sumV] `ifs'
		local dmm `r(mean)'
		noi sum vmar [fw=sumV] `ifs'
		local dfm `r(mean)'
	}
	if ("`nographs'"=="") {
		twoway (function x, range(-100 100) lc(ceprgraymedium%50)) ///
			(scatter dm dr [fw=sqrt(vpsum)], m(Oh) mlw(vthin) pstyle(p3)) ///
			(scatteri `dmm' `drm' "official" `dfm' `drm' "Villegas estimate", pstyle(p1) m(Oh) mlw(vthin)) ///
			`ifs', aspect(1) legend(off) /*xline(0, lc(ceprgraymedium%50)) yline(0, lc(ceprgraymedium%50))*/ ///
			xti(" " "Early Margin (Precinct)") yti("Late Margin (Precinct)")
		graph export ${OUTPUT}/Figures/fig4.png, replace
	}
	
end

capture program drop ComputoAnalysis
program define ComputoAnalysis

	syntax [, NOGraphs]

	di as input ""
	di "*"
	di "*	Chumacero (computo)"
	di "*"
	qui {
		*addChumaceroMargins if ~preInt, p(simple) l(f p d v m l r)
		*addChumaceroMargins if Q5, p(base) l(f p d v m l r)
		addChumaceroMargins if Q5, p(computo) l(cf cp cd cv cm l r)
		addChumaceroMargins if Q5 & ~preInt, p(both) l(cf cp cd cv cm l r)
		addChumaceroMargins if Q1, p(both1) l(cf cp cd cv cm l r)
		
		addCounterfactual cfull, p(computo) l(cf cp cd cv cm l r)
		addCounterfactual cnop, p(computo) l(cf cp cd cv cm l)
		addCounterfactual cnewman, p(computo) l(r)

		
		addCounterfactual bfull, p(both) l(cf cp cd cv cm l r)
		addCounterfactual bnop, p(both) l(cf cp cd cv cm l)
		addCounterfactual bnewman, p(both) l(r)

		addCounterfactual b1full, p(both1) l(cf cp cd cv cm l r)
		addCounterfactual b1nop, p(both1) l(cf cp cd cv cm l)
		addCounterfactual b1newman, p(both1) l(r)
	
	}
	/*
	tab simpleMatchlevel [fw=sumV]
	tab baseMatchlevel [fw=sumV]
	tab simpleMatchlevel if ~preInt [fw=sumV]
	tab baseMatchlevel if Q5 [fw=sumV]		
	*/
	
	di as input ""
	di "*	Overall Election Data Categories: Comparison of Counts"
	tab computoLocalityType if computoPrecinctType==1 [fw=sumV]
	tab computoPrecinctType [fw=sumV]
	di as input ""
	di "*	Overall Election Results: Based on the First 80 Percent of Tally Sheets"	
	table computoLocalityType if computoPrecinctType==1 [fw=sumV], c(mean margin mean cnewman mean cfull mean cnop) format(%6.4fc) row
	table computoPrecinctType [fw=sumV], c(mean margin mean cnewman mean cfull mean cnop) format(%6.4fc) row
	di as input ""
	di "*	Overall Election Results: Based on the First 80 Percent of Tally Sheets and Pre-Interruption Tally Sheets"			
	tab bothLocalityType if bothPrecinctType==1 [fw=sumV]
	tab bothPrecinctType [fw=sumV]
	table bothLocalityType if bothPrecinctType==1 [fw=sumV], c(mean margin mean bnewman mean bfull mean bnop) format(%6.4fc) row
	table bothPrecinctType [fw=sumV], c(mean margin mean bnewman mean bfull mean bnop) format(%6.4fc) row
	di as input ""
	di "*	Overall Election Results: Based on the Last 80 Percent of Tally Sheets"					
	tab both1LocalityType if both1PrecinctType==1 [fw=sumV]
	tab both1PrecinctType [fw=sumV]
	table both1LocalityType if both1PrecinctType==1 [fw=sumV], c(mean margin mean b1newman mean b1full mean b1nop) format(%6.4fc) row
	table both1PrecinctType [fw=sumV], c(mean margin mean b1newman mean b1full mean b1nop) format(%6.4fc) row
		
	di as input ""
	di "*"
	di "*	Nooruddin (computo)"
	di "*"
	qui {
		addNooruddinExtended
		sort ComputoDate NumM
		gen cc = sum(sumV)
		replace cc = 100*cc/cc[_N]
		gen ccfixed = cc>95
		lab var ccfixed "Nooruddin (fixed): 'first 95' vs 'last 5'"
		
		gen p90oas = computoCompleteOAS>90
		gen f90 = cc>90
		gen ft = cond(ccfixed,cond(preInt,1,2),0)
		lab var ft "Tally Sheet Type"
		lab def ft 0 "Verified in Computo" 1 "Unverified, but Early TREP" 2 "Unverified, Late TREP"
		lab val ft ft
		gen isf = ft==2
		lab var isf "Tally Sheet Adjusted"
		lab def isf 0 "Not adjusted" 1 "Adjusted"
		lab val isf isf
		
		addChumaceroMargins if isf, p(nfix) l(cf cp cd cv cm l r)
		addCounterfactual fcf, p(nfix) l(cf cp cd cv cm l r)
	}
	
	qui {
		preserve
		qui table oasDep2 computo95oas if p90oas, c(mean shareMASoas) format(%6.4fc) row replace
		ren table1 mas
		tempfile tg
		save `tg'
		restore, preserve
		qui table oasDep2 computo95oas if p90oas, c(mean shareCCoas) format(%6.4fc) row replace
		ren table1 cc
		merge 1:1 oasDep2 computo95oas using `tg'
		gen adv = mas-cc
		drop _merge
		reshape wide mas cc adv, i(oasDep) j(computo95)
		format %4.1fc mas* cc* adv*
		noi di as input ""
		noi di "*	MAS and CC Vote Shares in Last 10 Percent of Official Count (Alphabetical Sort)"
		noi li oasDep mas0 mas1 cc0 cc1 adv0 adv1 in 1/7
		restore, preserve
		qui table oasDep2 ccfixed if f90, c(mean shareMASoas) format(%6.4fc) row replace
		ren table1 mas
		tempfile tg
		save `tg', replace
		restore, preserve
		qui table oasDep2 ccfixed if f90, c(mean shareCCoas) format(%6.4fc) row replace
		ren table1 cc
		merge 1:1 oasDep2 ccfixed using `tg'
		gen adv = mas-cc
		drop _merge
		reshape wide mas cc adv, i(oasDep) j(ccfixed)
		format %4.1fc mas* cc* adv*
		noi di as input ""
		noi di "*	MAS and CC Vote Shares in Last 10 Percent of Official Count (Chronological Sort)"
		noi li oasDep mas0 mas1 cc0 cc1 adv0 adv1 in 1/7
		restore
	}
	
	noi di as input ""
	noi di "*	Overall Election: Adjusting the Last Uncounted, Unverified Tally Sheets in Official Count"	
	tab nfixPrecinctType [fw=sumV]
	table nfixPrecinctType [fw=sumV], c(mean margin mean fcf) format(%6.4fc) row
	tab isf [fw=sumV]
	table nfixPrecinctType if ft==2 [fw=sumV], c(mean margin mean fcf) format(%6.4fc) row
	table ft [fw=sumV], c(mean margin mean fcf) format(%6.4fc) row
	
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

	syntax anything(name=mcf) [, TREP COMPUTO REPLACE Sort(varlist) RUE IF(string asis)]
	
	if ("`trep'`computo'"=="trepcomputo") {
		di as error "Please select at most one of TREP or COMPUTO"
		exit(1)
	}
	
	if ("`rue'"=="rue") {
		local pf c
	}
	
	capture: use ${DATA}/montecarlo/prepped/`mcf', clear
	if (_rc~=0 | "`replace'"=="replace") {

		capture: mkdir ${DATA}/montecarlo
		capture: mkdir ${DATA}/montecarlo/prepped

		buildFullDataSet
		egen scode = group(rcode NumMesa), missing
		gen others = sumV-(MAS+CC)
		foreach var of varlist MAS CC others {
			if ("`trep'"=="trep'") {
				replace `var' = TREP_`var'
			}
			replace `var' = . if `if'
		}
		keep NumM RUE Dep-Rec *code MAS CC others Ins
		
		*	sample tags one counted acta for each geography
		gen ssample = ~mi(CC)
		gen scodex = scode if ssample
		local lg s
		foreach g in r l `pf'm `pf'v `pf'd `pf'p `pf'f {
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
		local lg `pf'f
		qui foreach gc in `pf'p `pf'd `pf'v `pf'm l r s {
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

	syntax [, Beta Gamma Sheet RUE]

	if ("`rue'"=="rue") {
		local pf c
	}

	*
	*	Single Iteration
	*
	*	Assign replacement geographies
	*		If method is sheet, need to assign all the way down to acta
	local gprefs `pf'p `pf'd `pf'v `pf'm l r
	if ("`sheet'"=="sheet") {
		local gprefs `gprefs' s
	}
	local lg `pf'f
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

	syntax [anything(name=mcf)], [Beta Gamma Sheet] [RUE] [OAS] [MAS CC] ///
		[Cut(real 95)] [TREP COMPUTO] [REPLACE] [NEWdata] [noGRaph] [USETrep] ///
		[Inner(integer 50)] [Outer(integer 10)] [SEED(integer 20200924)]

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

	if ("`newdata'"=="newdata") {
		local newdata replace
	}
	else {
		local newdata
	}
	
	if ("`mas'`cc'"=="mascc") {
		di as error "Please select at most one of mas or cc"
		exit(1)
	}
	if ("`trep'`computo'"=="trepcomputo") {
		di as error "Please select at most one of trep or computo"
		exit(1)
	}
	if ("`trep'`computo'"=="") {
		local trep trep
	}
	local tvar `trep'`computo'Complete`=upper("`oas'")'
	local ifcom `tvar'>`cut' 
	if ("`usetrep'"=="usetrep") {
		local ifcom `ifcom' & progress>0
		*local ifcom 0
	}
	set seed `seed'
	
	if ("`beta'`gamma'`sheet'"=="") {
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
						
						doBoliviaMCprep `mcf', if(`ifcom') `rue' `newdata'
						local newdata
						doMCiter, `beta' `gamma' `sheet' `rue'
						
						local byvars
						if ("`beta'"=="beta") {
							local byvars by(first next last)
						}
						keep if mi(CC)
						drop MAS CC others
						keep NumM MAS CC others
						gen inum = (`ii'-1)*`inner'+`jj'
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
			}
			capture: drop margin*
			*	Save the full batch of simulations
			save ${OUTPUT}/`mcf'/`mcf', replace
		}
				
		if ("`graph'"~="nograph") {
				
			capture: mkdir ${OUTPUT}/Figures
			merge n:1 NumM using fullData
			addNooruddinExtended
			local party = upper("`mas'`cc'")
			if ("`party'"=="") {
				local psl Margin
				local yc 10
			}
			else {
				local psl `party' Vote Share
				local yc 50
			}
			if ("`oas'"=="oas") {
				gen ez = Emit>0
				*	Note Emititos are missing instead of zero in Nooruddin
				table ez, c(freq sum Emit sum computoEmit mean Emit mean computoEmit) format(%12.0fc) row
				keep if `tvar'<=100
				gen ex = (MASs+CCs+otherss+Bla+Nul)
				foreach var of varlist MASs CCs otherss ex {
					replace `var' = . if `var'==0
				}
				if ("`party'"=="") {
					gen ms = shareMASoas-shareCCoas
					gen msx = ms
					replace msx = 100*(MASs-CCs)/ex if MASs>0 & CCs>0 & ~mi(`party's)
				}
				else {
					gen ms = share`party'oas
					gen msx = ms
					replace msx = 100*`party's/ex if `party's>0 & ~mi(`party's)
				}
				local loco95 mean bwidth(0.3) 
				local loco5 mean bwidth(0.6) 
			}
			else {
				if ("`party'"=="") {
					gen ms = margin
					gen msx = ms
					replace msx = 100*(MASs-CCs)/(MASs+CCs+otherss) if ~mi(MASs)
				}
				else {
					gen ms = 100*`party'/sumV
					gen msx = ms
					replace msx = 100*`party's/(MASs+CCs+otherss) if ~mi(MASs)
				}
				local loco95 
				local loco5
			}
					
			lowess ms `tvar' if `tvar'>`cut' & `tvar'<=100 & (mi(inum) | inum==1), `loco5' gen(msl) nog
			lowess ms `tvar' if `tvar'<`cut' & mi(inum), `loco95' gen(mss) nog
			
			*	blank, "p" "t" or "d"
			local cb
			local c1 `cb'ceprgreen
			local c2 `cb'ceprblue%10
			local c3 `cb'ceprorange

			local lcom (line mss `tvar' if mi(inum), lc(`c1') lwidth(vthick))
			levelsof inum, local(inuml) clean
			foreach i of local inuml {
				if (`i'<21000) {
				di `i'
				capture: lowess msx `tvar' if `tvar'>`cut' & `tvar'<=100 & (mi(inum) | inum==`i'), `loco5' gen(ms`i') nog
				local lcom `lcom' (line ms`i' `tvar' if `tvar'<=100 & (mi(inum) | inum==`i'), lc(`c2') lw(*1))
				}
			}

			sort `tvar'
			twoway `lcom' (line msl `tvar', lc(`c3') lw(*1)) if `tvar'>90 ///
				, legend(off) yline(`yc', lcolor(black)) xscale(extend nofextend) xline(`cut', lcolor(black)) ///
				xlabel(90 `cut' 100) leg(off) graphregion(style(none) color(none)) ///
				xtitle("Cumulative National Vote Share Counted") ytitle("Polling Station Level `psl'")
			graph export ${OUTPUT}/Figures/fig_`mcf'_`trep'`computo'_`oas'`party'_detail.png, replace
			
		}

	}
	
end

capture: program drop makeNooruddinGraph
program define makeNooruddinGraph

	syntax anything(name=party), [TREP COMPUTO] [Cut(real 95)] [OAS] [EQualize] [Label(string asis)]

	tempvar mss msl
	
	if ("`trep'`computo'"=="trepcomputo") {
		di as error "Please select at most one of trep or computo"
	}
	if ("`trep'`computo'"=="") {
		local trep trep
	}
	local tvar `trep'`computo'Complete`=upper("`oas'")'
	local svar share`party'oas

	local loco95 mean bwidth(0.3)
	if ("`equalize'"=="equalize") {
	
		count if trepCompleteOAS>95 & trepCompleteOAS<=100 & ~mi(`svar')
		local c `r(N)'
		count if trepCompleteOAS<95 & ~mi(`svar')
		local b `r(N)'
		local xf = (floor((0.6*`c'-0.5)/2)*2+0.5)/(`b'+0*`c')

		local loco95 mean bwidth(`=0.6*(100-`cut')/`cut'')
		local loco95 mean bwidth(`xf')
		macro li _loco95
		
	}
	local loco5 mean bwidth(0.6)
	local yc 50
	local psl `party' Vote Share
	local vs
	if ("`computo'"=="computo") {
		local vs in Computo
	}
	local ct base
	if ("`cut'"~="95") {
		local ct `cut'
	}
	if ("`equalize'"=="equalize") {
		local ct eq
	}
	if ("`label'"=="") {
		local label fig_`ct'_`trep'`computo'_`oas'`party'
		local rw vthick
	}
	else {
		local rw vthick
	}

	if (1) {
		lowess `svar' `tvar' if `tvar'>`cut' & `tvar'<=100, `loco5' gen(`msl') nog
		lowess `svar' `tvar' if `tvar'<`cut', `loco95' gen(`mss') nog
	}
	else {
		lpoly `svar' `tvar' if `tvar'>`cut' & `tvar'<=100, gen(`msl') nogra at(`tvar') degree(1)
		lpoly `svar' `tvar' if `tvar'<`cut', gen(`mss') nogra at(`tvar') degree(1)
	}
	local lcom (line `mss' `tvar' if `tvar'<`cut', lc(ceprgreen) lwidth(vthick))
	sort `tvar'
	twoway (scatter `svar' `tvar', mcolor(gray%60) msymbol(point)) `lcom' ///
		(line `msl' `tvar' if `tvar'>`cut' & `tvar'<=100, lc(ceprorange) lw(`rw')) ///
		, legend(off) yline(`yc', lcolor(black)) xscale(extend nofextend) xline(`cut', lcolor(black)) ///
		xlabel(0 `cut' 100) leg(off) graphregion(style(none) color(none)) ///
		xtitle(" " "Cumulative National Vote Share Counted `vs'") ytitle("Polling Station Level `psl'")
	graph export ${OUTPUT}/Figures/`label'.png, replace height(1024)

end

capture: program drop LowessGraph
program define LowessGraph

	syntax [, EQualize]

	buildFullDataSet
	addNooruddinExtended
	keep if ~mi(shareCCoas) & trepCompleteOAS<=100
	sort trepCompleteOAS
	count if trepCompleteOAS>95
	local c `r(N)'
	count if trepCompleteOAS<95
	local b `r(N)'
	local xp = `b'+1+floor((0.6*`c'-0.5)/2)
	local xf = 0.3
	local xls 80(5)95
	if ("`equalize'"=="equalize") {
		local xf = (floor((0.6*`c'-0.5)/2)*2+0.5)/(`b'+0*`c')
		local xls 94/96
	}
	local xm = `b'-floor((`xf'*`b'-0.5)/2)
	macro li _b _c _xf _xm _xp
	gen d = cond(trepCompleteOAS<95, trepCompleteOAS[`b']-trepCompleteOAS[`xm'], trepCompleteOAS[`xp']-trepCompleteOAS[`b'+1])
	gen v = cond(trepCompleteOAS<95, trepCompleteOAS[`b'], trepCompleteOAS[`b'+1])
	gen w = exp(3*ln(1-exp(3*ln(abs(trepCompleteOAS-v)/1.0001/d))))
	replace w = 1 in `b'/`=`b'+1'
	
	gen is95 = _n>`b' in `xm'/`xp'
	table is95 [iw=w], c(min trepCompleteOAS mean trepCompleteOAS max trepCompleteOAS)
	
	mean trepCompleteOAS [iw=w], over(is95)
	mat rt = r(table)
	local ml = rt[1,1]
	local mr = rt[1,2]
	mean shareCCoas [iw=w], over(is95)
	mat rt = r(table)
	gen ll = cond(is95,rt[5,2],rt[5,1],.)
	gen ul = cond(is95,rt[6,2],rt[6,1],.)
	gen ml = cond(is95,rt[1,2],rt[1,1],.)
		
	bys is95: egen sw = total(w)
	gen wc = ceil(10000*w/sw)
	local scom
	forvalues ii=1/100 {
		local scom `scom' (scatter shareCCoas trepCompleteOAS if wc==`ii' & is95==0, msize(small) mlw(none) mc(ceprgreen%`ii')) (scatter shareCCoas trepCompleteOAS if wc==`ii' & is95==1, mlw(none) msize(small) mc(ceprorange%`ii'))
	}
	sort trepCompleteOAS
	twoway `scom' ///
		(line ul ll trepCompleteOAS if is95==0 & trepCompleteOAS>0, pstyle(p4 p4) lw(vthin vthin)) ///
		(line ul ll trepCompleteOAS if is95==1 & trepCompleteOAS<100, pstyle(p2 p2) lw(vthin vthin)) ///
		(pci `=ll[`b']' `ml' `=ul[`b']' `ml', pstyle(p4)) ///
		(pci `=ll[`b'+1]' `mr' `=ul[`b'+1]' `mr', pstyle(p2)) ///
		, legend(off) xlab(`xls') ///
		xtitle(" " "Cumulative National Vote Share Counted") ytitle("Polling Station Level CC Vote Share")

end

capture: program drop NooruddinRecap
program define NooruddinRecap

	buildFullDataSet
	addNooruddinExtended
	gen isnCap = ceprSix<=0
	lab def isnCap 0 "Capital city" 1 "Not capital city"
	lab val isnCap isnCap

	di as input ""
	di "*"
	di "*	Introduction"
	di "*"
	di "*	TREP"
	gen is5 = trepComplete>95
	table is5 if trepComplete<=100 [fw=sumV], c(mean isnC) row
	table isnCap is5  if trepComplete<=100 [fw=sumV], c(mean margin) row col
	di as input ""
	di "*	Computo"
	replace is5 = computoComplete>95
	table is5 if computoComplete<=100 [fw=sumV], c(mean isnC) row
	table isnCap is5  if computoComplete<=100 [fw=sumV], c(mean margin) row col

	bys isnCap (trepComplete): gen tsum = sum(sumV/1e6)
	bys isnCap (computoComplete): gen csum = sum(sumV/1e6)
	xtline tsum, i(isnCap) t(trepComplete) ov xti(" " "Percent of Valid Votes Reported (preliminary)") yti("Valid Votes Reported (millions)") name(tc) ylab(0/3)
	xtline csum, i(isnCap) t(computoComplete) ov xti(" " "Percent of Valid Votes Reported (official)") yti("Valid Votes Reported (millions)") name(cc) ylab(0/3)
	grc1leg2 tc cc, ytol1
	graph export ${OUTPUT}/Figures/figProgress.png, replace

	makeNooruddinGraph CC, oas
	makeNooruddinGraph CC, oas c(90)
	makeNooruddinGraph CC, oas c(80)
	makeNooruddinGraph CC, oas eq
	
	LowessGraph,
	graph export ${OUTPUT}/Figures/lowessNoor.png, replace height(1024)
	LowessGraph, eq
	graph export ${OUTPUT}/Figures/lowessEq.png, replace height(1024)

	doBoliviaMC 1, sheet i(50) o(3) rue oas cc nogr
	keep if inum==10
	merge 1:1 NumM using fullData
	addNooruddinExtended
	table trep95oas, c(sum CC sum CCs)
	replace computoEmit = MASs+CCs+otherss+Bla+Nul
	foreach var of varlist MAS CC {
		replace `var' = `var's if ~mi(inum)
		replace share`var'oas = 100*cond(`var's==0,.,`var's)/computoEmit if ~mi(inum)
	}
	makeNooruddinGraph CC, oas l(fig_CC_i10)

	doBoliviaMC 1, sheet i(50) o(3) rue oas cc
	doBoliviaMC 1, sheet i(50) o(3) rue oas mas
	doBoliviaMC 2, sheet i(50) o(3) rue oas mas computo
	doBoliviaMC 3, sheet i(50) o(3) rue oas mas computo usetrep
	doBoliviaMC 2, sheet i(50) o(3) rue oas cc computo
	doBoliviaMC 3, sheet i(50) o(3) rue oas cc computo usetrep

end

capture: program drop runAll
program define runAll

	syntax [, NOGraphs REPLACE]
	
	qui buildFullDataSet, `replace'
	di as input ""
	di "*"
	di "*	Introduction"
	di "*"
	di "*	Final result"
	table progress [fw=sumV], c(mean shareMAS mean margin) format(%6.4fc) row col miss
	di as input ""
	
	
	NewmanFramework, `nographs'
	InadequateControls, `nographs'
	ComputoAnalysis, `nographs'
	FurtherRebuttal, `nographs'
	
	di as result ""
	di "*"
	di "*	OAS's Deceptive Math"
	di "*"
	qui {
		sum sumV
		local sv `r(sum)'
	}
	di as text "Valid Votes = " as result %10.0fc `sv'
	qui {
		sum MAS
		local sm `r(sum)'
		sum CC
		local sc `r(sum)'
	}
	di as text "Net votes for Morales = " as result %10.0fc `sm'-`sc'
	di as text "Morales margin = (" as result %10.0fc `sm' as text " - " ///
		as result %10.0fc `sc' as text ")/" as result %10.0fc `sv' ///
		as text " = " as result %4.2fc 100*(`sm'-`sc')/`sv' as text "%"
	di as text "Morales margin without the 226 if no votes for Mesa = (" as result %10.0fc `sm'-34718 as text " - " ///
		as result %10.0fc `sc' as text ")/" as result %10.0fc `sv'-38001 ///
		as text " = " as result %4.2fc 100*(`sm'-`sc'-34718)/(`sv'-38001) as text "%"
	di as text "Morales margin if ALL Morales votes on 226 go to minor candidates = (" as result %10.0fc `sm'-34718 as text " - " ///
		as result %10.0fc `sc' as text ")/" as result %10.0fc `sv' ///
		as text " = " as result %7.5fc 100*(`sm'-`sc'-34718)/(`sv') as text "%"	
	
end

runAll
