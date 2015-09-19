pro resize_LAI3g
; resize LAI3g from 1/12 degree spatial resolution to 1/2 degree
; ALL NA (value = 250) are now set to 0 and blindly resampled
; masking out pixels that contain water is done in a subsequent step with masks
  e = ENVI()

  ; Open a file
  rootdir = "/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/yearly/"
  ;file1 = FILEPATH("Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/LAIv3g_8211_INT_BSQ_resized", ROOT_DIR="/" )
  ;file = DIALOG_PICKFILE(/READ)
  ;print,file


  for year=1982,2011 DO BEGIN

    raster = e.OpenRaster(rootdir + "LAIv3g_" + STRTRIM(year,2) + "_nodata")
    NewRaster = ENVIResampleRaster(Raster, $
  DIMENSIONS=[720,360], $
  METHOD='Bilinear')
    outfile = rootdir + "LAIv3g_" + STRTRIM(year, 2) + "_0.5"
    print,outfile
    NewRaster.Export, outfile, 'ENVI'
  endfor
 
 end