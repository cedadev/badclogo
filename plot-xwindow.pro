;
; draws logo to an X window on true-colour display
;

restore,'plot.dat'
nlev=n_elements(red)

cols=red+256L*green+256L*256L*blue
white = 255 * (1 + 256L + 256L*256L )

map_set, -50.,0,180,/ortho, color=0, /noborder, /iso

erase,white

field[*,0] = total(field[0:95,1])/96.

contour,field,lons,lats,/over, $
        lev=levs,c_col=cols,/cell

map_grid, londel=30, latdel=10, glinestyle=0, glinethick=1,col=cols[nlev-1]

end
