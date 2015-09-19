pro hants_lai3g
; run hants for every year 
; define directories
  indir_nhem   = '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/yearly/NHEM/'
  indir_shem  =  '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/yearly/SHEM/'
  outdir_nhem  = '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/hantsout/NHEM/'
  outdir_shem = '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIv3g/hantsout/SHEM/'
  mask_nhem    = '/Users/davidschenkel/Documents/Uni/Masterarbeit/watermask/watermask_nhem.envi'
  mask_shem = '/Users/davidschenkel/Documents/Uni/Masterarbeit/watermask/watermask_shem.envi'
 
 ; go over years, usually it crashes inbetween, then restart at year..
  for year=1982,2011 DO BEGIN
    
   filename   = indir_nhem + "LAIv3g_NHEM_" + STRTRIM(year,2) + '_05'
   out_inter  = outdir_nhem + 'inter/inter' + STRTRIM(year,2)
   out_four   = outdir_nhem + 'fourier/fourier' + STRTRIM(year,2)
   out_smooth = outdir_nhem + 'smoothed/smoothed' + STRTRIM(year,2)
   out_stat   = outdir_nhem + 'status/status' + STRTRIM(year,2)

; set parameters
; remember: LAIv3g has LAI from 0 to <=80-ish
   cgi_hants, $
    fet = 7, $ 
    freqs = [0,1,2,3], $
    range = [-2,80], $
    tat = 10, $
    iMAX = 6, $
    data_if   = filename, $
    mask_if   = mask_nhem, $
    hants_of  = out_four, $
    status_of = out_stat, $
    smooth_of = out_smooth, $
    interp_of = out_inter
    
; same for southern hemisphere
    
  filename   = indir_shem + "LAIv3g_SHEM_" + STRTRIM(year,2) + '_05'
  out_inter  = outdir_shem + 'inter/inter' + STRTRIM(year,2)
  out_four   = outdir_shem + 'fourier/fourier' + STRTRIM(year,2)
  out_smooth = outdir_shem + 'smoothed/smoothed' + STRTRIM(year,2)
  out_stat   = outdir_shem + 'status/status' + STRTRIM(year,2)


  cgi_hants, $
    fet = 7, $
    freqs = [0,1,2,3], $
    range = [-2,80], $
    tat = 10, $
    iMAX = 6, $
    data_if   = filename, $
    mask_if   = mask_shem, $
    hants_of  = out_four, $
    status_of = out_stat, $
    smooth_of = out_smooth, $
    interp_of = out_inter
    
    
  endfor
end
