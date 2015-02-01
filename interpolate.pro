PRO INTERPOLATE

r = [5.0,10.0,15.0,20.0,30.0,40.0]
bz = [24.0,12.0,7.0,5.0,1.9,0.7]
bt = [784.0,797.0,801.0,804.0,806.0,808.0]

x = findgen(401)/10
result1 = interpol(bz,r,x)
result2 = interpol(bt,r,x)

print, interpol(bz,r,12.5)
print, interpol(r,bz,18.0)


cgdisplay, 960, 480, /free
!P.MULTI = [0,2,1]
plot, x, result1
plot, x, result2



END
