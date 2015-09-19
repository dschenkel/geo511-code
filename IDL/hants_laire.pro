pro hants_laire
; northern hemisphere hants
  indir_nhem   = '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/NHEM/'
  indir_shem  =  '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIre/bimonthly_means/SHEM/'
  outdir_nhem  = '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIre/bimonthly_hantsout/NHEM/'
  outdir_shem = '/Users/davidschenkel/Documents/Uni/Masterarbeit/LAIre/bimonthly_hantsout/SHEM/'
  mask_nhem    = '/Users/davidschenkel/Documents/Uni/Masterarbeit/watermask/watermask_nhem.envi'
  mask_shem = '/Users/davidschenkel/Documents/Uni/Masterarbeit/watermask/watermask_shem.envi'
 
  for year=1982,2011 DO BEGIN
    
   filename   = indir_nhem + "LAIre_nhem_bimonthly_" + STRTRIM(year,2)
   out_inter  = outdir_nhem + 'inter/inter' + STRTRIM(year,2)
   out_four   = outdir_nhem + 'fourier/fourier' + STRTRIM(year,2)
   out_smooth = outdir_nhem + 'smoothed/smoothed' + STRTRIM(year,2)
   out_stat   = outdir_nhem + 'status/status' + STRTRIM(year,2)

; laire ranges from 0 to <=8
   cgi_hants, $
    fet = 1, $
    freqs = [0,1,2,3], $
    range = [-2,8], $
    tat = 10, $
    iMAX = 6, $
    data_if   = filename, $
    mask_if   = mask_nhem, $
    hants_of  = out_four, $
    status_of = out_stat, $
    smooth_of = out_smooth, $
    interp_of = out_inter
    
    
;same for southern hemisphere
	
  filename   = indir_shem + "LAIre_shem_bimonthly_" + STRTRIM(year,2)
  out_inter  = outdir_shem + 'inter/inter' + STRTRIM(year,2)
  out_four   = outdir_shem + 'fourier/fourier' + STRTRIM(year,2)
  out_smooth = outdir_shem + 'smoothed/smoothed' + STRTRIM(year,2)
  out_stat   = outdir_shem + 'status/status' + STRTRIM(year,2)


  cgi_hants, $
    fet = 1, $
    freqs = [0,1,2,3], $
    range = [-2,8], $
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
