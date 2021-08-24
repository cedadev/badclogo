;
; draws logo to postscript file
;

restore,'plot.dat'
nlev=n_elements(red)

;cols=red+256L*green+256L*256L*blue

field[*,0] = total(field[0:95,1])/96.

set_plot,'ps'
;device,xsize=40.0,ysize=59.4,xoffset=1,yoffset=1,/portrait
device,xsize=20.0,ysize=20,/encapsulated

device,/color,bits=8,file='logo.eps'
tvlct,red,green,blue

map_set, -50.,0,180,/ortho, color=0, /noborder, /iso

contour,field,lons,lats,/over, $
        lev=levs,c_col=indgen(nlev),/cell

map_grid, londel=30, latdel=10, glinestyle=0, glinethick=1,col=nlev-1

device,/close

end
