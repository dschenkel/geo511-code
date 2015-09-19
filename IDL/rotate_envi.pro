pro rotate_envi
  e = ENVI()
; rotate extracted ncdf files (since R produces mis-oriented files)
; IDL was found to be quicker in rotating them than R, can come in handy with big datasets in particular
  rootdir = "/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIre_envi/"

  for year=1982,2011 DO BEGIN

    filename = rootdir + "Global-0.5x0.5.analysis." + STRTRIM(year,2) + ".nc.envi"

    ENVI_Open_File, filename, R_FID=fid
    
    ENVI_File_Query, fid, DIMS=dims, NB=nb
    
    outfile = rootdir + "Global-0.5x0.5.analysis." + STRTRIM(year,2) + "_rot.envi"

    ENVI_Doit, 'Rotate_Doit', $
      FID = fid, $
      DIMS = dims, $
      POS = Lindgen(nb), $
      ROT_TYPE = 3, $
      OUT_NAME = outfile, $
      R_FID=rfid
  endfor
 

  
  
end