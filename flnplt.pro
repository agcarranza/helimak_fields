pro flnplt
fline={line:{x:1.0,y:1.0,z:1.0},comp:{x:1.0,y:1.0,z:1.0},mag:1.0,arclen:1.0}
result = file_search('line_*.sav')
restore,result(0)
test = fline.line(0)
print,test.x
lblx = 'arc length (m)'
lbly = 'Bmag'
lbl = 'Magnitude of B. Starting point: ' $
       + 'x = ' + strtrim(string(test.x),2)$
       + ' y = ' + strtrim(string(test.y),2)$
       + ' z = ' + strtrim(string(test.z),2)
plot,fline.arclen,fline.mag,xtitle=lblx,ytitle=lbly,title=lbl



for i=1,n_elements(result)-1 do begin

restore,result(i)

oplot,fline.arclen,fline.mag
endfor

return
end
