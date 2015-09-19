pro extract_years_laire
  ; Launch the application
  e = ENVI()

  rootdir = "/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/"

  ; Open a file
  file = rootdir + "LAIv3g_8211_INT_BSQ_resized"
  Raster = e.OpenRaster(file)

  ; Create a spatial subset of 100 samples x 100 lines
  ; and a spectral subset of Band 1.
  first_year = 1982
  last_year = 2011
  for year=first_year,last_year DO BEGIN
    bNHEM_start = (year-first_year)*24
    bNHEM_end = (year-first_year+1)*24-1
    curbands = [bNHEM_start:bNHEM_end]
    Subset_NHEM = ENVISubsetRaster(Raster, BANDS=curbands)

    outfile_NHEM = rootdir + "yearly/NHEM/" + "LAIre_NHEM_" + STRTRIM(year, 2) + "_05"
    ; newRaster = ENVIRaster(Subset_NHEM, URI=outfile_NHEM, NBANDS=24)
    ; newRaster.Save
    Subset_NHEM.Export, outfile_NHEM, 'ENVI'
    ;PRINT, curbands

    bSHEM_start = (year-first_year)*24+12
    if (year eq last_year) then begin
      bSHEM_end = (year-first_year+1)*24-1
      spec1 = bSHEM_start-12
      spec2 = bSHEM_start-1
      b1 = [bSHEM_start:bSHEM_end]
      b2 = [spec1:spec2]
      curbands = [b1,b2]
    endif else begin
      bSHEM_end = (year-first_year+1)*24+11
      curbands = [bSHEM_start:bSHEM_end]
    endelse
    ;PRINT, curbands
    Subset_SHEM = ENVISubsetRaster(Raster, BANDS=curbands)

    outfile_SHEM = rootdir + "yearly/SHEM/" + "LAIre_SHEM_" + STRTRIM(year, 2) + "_05"
    ;newRaster = ENVIRaster(Subset_SHEM, URI=outfile_SHEM, NBANDS=24)
    ;newRaster.Save
    Subset_SHEM.Export, outfile_SHEM, 'ENVI'

  endfor
end
