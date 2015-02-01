pro bam
	x = fltarr(30)
	plot,x,yrange=[0,190]
	for i=0,190 do begin
		y = x+i
		oplot,y,color=i
	endfor
	
end


pro auto_homer2_widget,filename
;  variable list
;    rn, scalar, number of rectangular coils
;    rh, scalar, height of rectangular coils
;    rw; scalar, width of rectangular coils
;    rc; scalar, center of rectangular coils
;    rj; scalar, total current through coils
;    cn; scalar, number of circular coils
;    ch; vector of length cn, height of each coil
;    cr; vector of length cn, radius of each coil
;    cj; vector of length cn, current through each coil
;    nlines; scalar, number of field lines to be traced
;    flines; structure, each element is a field line description
;    limits; structure, terminate the tracing of a field line if it steps outside these limits
;    addcoils; scalar, if = 0, do not plot rectangular coils
;                      if = 1, plot rectangular coils
;    bg; scalar, background color
;    range; structure, range of the axes in the plots
;    rset; scalar, if = 0, do no use the range structure to set the plot axes
;                if = 1, use the range structure to set the plot axes
bg = 0L

	; hardcode default values
	rn = 16
	rh = 2.375
	rw = 1.17
	rc = 1.3
	rj = 4.4E4
	cn = 3
	ch = [-1,0,1]
	cr = [2,2,2]
	cj = [400,0,400]
	nlines = 0
	flines = 0
	limits = {xm:-1.5,xp:1.5,ym:-1.5,yp:1.5,zm:-1.5,zp:1.5}
	addcoils = 0
	bg = 255
	clr = 0
	range = [0,0,0,0,0,0]
	rset = 0
	PLT_AL = -1
	;len = 2
	;lim = {xm:-1.5,xp:1.5,ym:-1.5,yp:1.5,zm:-1.5,zp:1.5}
	;line1 = {x0:{vector,1.3,0,-1},step:0.1,length:len,limits:lim,fname:'B'}
	;line2 = {x0:{vector,1.2,0,-1},step:0.1,length:len,limits:lim,fname:'B'}
	;line3 = {x0:{vector,1.1,0,-1},step:0.1,length:len,limits:lim,fname:'B'}
	;lines = [line1,line2,line3]

	close,1
	openr,1,filename
	location = 0
	line = ''
	while (not eof(1)) do begin
		readf,1,line
		location = location+1
		strings = strsplit(line,',',/extract,/preserve_null)
		name = strtrim(strings[0])
		if (n_elements(strings) lt 2) then begin
			print,'incorrect syntax on line',location,' of file.  line ignored'
		endif else begin
		if (name ne '') then val = float(strings[1])
		case (name) of
			;-----------------------------------------------------
			'rect_n' : rn = fix(val)
			'rect_h' : rh = val
			'rect_w' : rw = val
			'rect_c' : rc = val
			'rect_j' : rj = val
			'viewcoils' : addcoils = fix(val)
			'xminus' : limits.xm = val
			'yminus' : limits.ym = val
			'zminus' : limits.zm = val
			'xplus' : limits.xp = val
			'yplus' : limits.yp = val
			'zplus' : limits.zp = val
			'background' : bg = long(strings[1])
			;-----------------------------------------------------
			'circ_h' : begin
				if (n_elements(strings) eq 4) then begin
					ch = float(strings(1:3))					
				endif else begin
					print,'incorrect syntax on line',location,' of file.  line ignored'
				endelse
			end
			;-----------------------------------------------------
			'circ_r' : begin
				if (n_elements(strings) eq 4) then begin
					cr = float(strings(1:3))					
				endif else begin
					print,'incorrect syntax on line',location,' of file.  line ignored'
				endelse
			end
			;-----------------------------------------------------
			'circ_j' : begin
				if (n_elements(strings) eq 4) then begin
					cj = float(strings(1:3))					
				endif else begin
					print,'incorrect syntax on line',location,' of file.  line ignored'
				endelse
			end
			;-----------------------------------------------------
			'range' : begin
				if (n_elements(strings) eq 7) then begin
					range = float(strings(1:6))
					rset = 1				
				endif else begin
					print,'incorrect syntax on line',location,' of file.  line ignored'
				endelse
			end
			;-----------------------------------------------------
			'line' : begin
				if (n_elements(strings) eq 7) then begin
					s = float(strings(1:6))
					;following line added by wlr to change line color
					;also required a change in fline definition
					lncolor=long(strings(6))
					x0 = {vector,s[0],s[1],s[2]}
					fline = {x0:x0,step:s[3],length:s[4],limits:limits,fname:'B',color:lncolor}
					nlines = nlines + 1				
					if (nlines eq 1) then flines = fline $
					else flines = [flines,fline]
				endif else begin
					print,'incorrect syntax on line',location,' of file.  line ignored'
				endelse	
			end
			;-----------------------------------------------------
			'plotB' : begin
			  IF (N_ELEMENTS(STRINGS) EQ 10) THEN BEGIN
			    S = float(strings(1:9))
			    X_AXIS = S(0)
			    Y_AXIS = S(1)
			    PLT_X = MAKE_ARRAY(S(8),/INDEX)*(S(5)-S(2))/(S(8)-1) + S(2)
			    PLT_Y = MAKE_ARRAY(S(8),/INDEX)*(S(6)-S(3))/(S(8)-1) + S(3)
			    PLT_Z = MAKE_ARRAY(S(8),/INDEX)*(S(7)-S(4))/(S(8)-1) + S(4)
			  ENDIF ELSE BEGIN
			    PRINT, 'INCORRECT SYNTAX ON LINE',LOCATION,' OF FILE.  LINE IGNORED'
			  ENDELSE
			END
			;-----------------------------------------------------
			'' : ; do nothing for blank lines or comments
			;-----------------------------------------------------
			else : print,name,' variable not found on line',location,' of file.  line ignored'
		endcase
		endelse
	endwhile
	close,1	

	trace_param = {nlines:nlines,line:flines,addcoils:addcoils,bgcolor:bg,range:range,rset:rset}
print,bg,trace_param.bgcolor

	init_param,rn,rh,rw,rc,rj,cn,ch,cr,cj,rectparam,circparam
	init_geometry,geom,rectparam,circparam
	IF (NLINES GT 0) THEN auto_trace_widget,geom,trace_param
	
	

	

end


pro auto_trace_widget,geom,trace_param

;WLR ADDED
;SET_PLOT,'PS'
;LOADCT,5
;DEVICE,/ENCAPSULATED,/COLOR,BITS_PER_PIXEL=8,FILENAME='TEST.EPS'


	
	cp = trace_param.line[0]
	r = trace_param.range
	bg = trace_param.bgcolor
	fieldline_rk4,cp.x0,cp.step,cp.length,cp.limits,function_name=cp.fname,geom,fline
	
	if (trace_param.rset eq 1) then begin
		;WLR:  MODIFIED THE FOLLOING LINE AUGUST 20, 2003
	    ;ADDED THE CHARSIZE KEYWORD
	    plot_3dbox,fline.line.x,fline.line.y,fline.line.z,background=bg,color=0,xrange=[r[0],r[1]],yrange=[r[2],r[3]],zrange=[r[4],r[5]],$
		           xtitle='X',ytitle='Y',ztitle='Z',CHARSIZE=2.0,CHARTHICK=1.5
	endif else begin
	    ;WLR:  MODIFIED THE FOLLOING LINE AUGUST 20, 2003
	    ;ADDED THE CHARSIZE KEYWORD
		plot_3dbox,fline.line.x,fline.line.y,fline.line.z,background=bg,color=0,$
		           xtitle='X',ytitle='Y',ztitle='Z',CHARSIZE=2.0,CHARTHICK=1.5
	endelse

	plots,fline.line.x,fline.line.y,fline.line.z,/t3d,color=cp.color
	
	projz = make_array(n_elements(fline.line.z),value=!z.crange(0))
	plots,fline.line.x,fline.line.y,projz,/t3d,noclip=0,color=cp.color

flname = 'line_' + strtrim(string(0),2) + '.sav'
save,fline,filename=flname

	for i=1,trace_param.nlines-1 do begin
		cp = trace_param.line[i]
		fieldline_rk4,cp.x0,cp.step,cp.length,cp.limits,function_name=cp.fname,geom,fline
		plots,fline.line.x,fline.line.y,fline.line.z,/t3d,noclip=0,color=cp.color
		projz = make_array(n_elements(fline.line.z),value=!z.crange(0))
		plots,fline.line.x,fline.line.y,projz,/t3d,noclip=0,color=cp.color
	;added by wlr


flname = 'line_' + strtrim(string(i),2) + '.sav'
save,fline,filename=flname





endfor
	if (trace_param.addcoils) then add_coils
	
	;DEVICE,/CLOSE

RETURN
end


pro add_coils
	fast_init_geometry,geom
	for i=0,geom.nrects-1 do begin
		rect = geom.rect[i]
		
		; get the center and size of the loop
		c = rect.center
		h = rect.height
		w = rect.width
	
		; find the distances the segments are shifted from the center
		d = cross(rect.nhat,{vector,0,0,1})
		dx = w/2*d.x
		dy = w/2*d.y
		dz = h/2	
		
		x = {vector,c.x - dx,c.y - dy,-dz}
		xplus = x
		xplus.z = xplus.z + 2*dz
		plots,[x.x,xplus.x],[x.y,xplus.y],[x.z,xplus.z],/t3d,color=0

		x = xplus
		xplus.x  = x.x + 2*dx
		xplus.y = x.y + 2*dy
		plots,[x.x,xplus.x],[x.y,xplus.y],[x.z,xplus.z],/t3d,/continue,color=0

		x = xplus
		xplus.z = x.z - 2*dz
		plots,[x.x,xplus.x],[x.y,xplus.y],[x.z,xplus.z],/t3d,/continue,color=0

		x = xplus
		xplus.x = x.x -2*dx
		xplus.y = x.y - 2*dy
		plots,[x.x,xplus.x],[x.y,xplus.y],[x.z,xplus.z],/t3d,/continue,color=0
		plots,[x.x,xplus.x],[x.y,xplus.y],[x.z,xplus.z],/t3d,color=0
	
	endfor
end



pro manytrace,step,ln1,ln2,ln3,rxl,rxh,ryl,ryh,rzl,rzh,fline1,fline2,fline3
	trace,step,ln1,1.3,0,-1,-5,5,-5,5,-5,5,fline1
	plot_3dbox,fline1.line.x,fline1.line.y,fline1.line.z; ,$
	;	xrange=[rxl,rxh],yrange=[ryl,ryh],zrange=[rzl,rzh]
	projz1 = make_array(n_elements(fline1.line.z),value=!z.crange(0))
	plots,fline1.line.x,fline1.line.y,projz1,/t3d,noclip=0

	trace,step,ln2,1.2,0,-1,-5,5,-5,5,-5,5,fline2
	plots,fline2.line.x,fline2.line.y,fline2.line.z,/t3d,noclip=0
	projz2 = make_array(n_elements(fline2.line.z),value=!z.crange(0))
	plots,fline2.line.x,fline2.line.y,projz2,/t3d,noclip=0

	
	trace,step,ln3,1.1,0,-1,-5,5,-5,5,-5,5,fline3
	plots,fline3.line.x,fline3.line.y,fline3.line.z,/t3d,noclip=0
	projz3 = make_array(n_elements(fline3.line.z),value=!z.crange(0))
	plots,fline3.line.x,fline3.line.y,projz3,/t3d,noclip=0
	
end

pro trace,step,length,x,y,z,x1,x2,y1,y2,z1,z2,fline
	fast_init_geometry,geom
	x0 = {vector,x,y,z}
	limits = {xm:x1,xp:x2,ym:y1,yp:y2,zm:z1,zp:z2}
	fieldline_rk4,x0,step,length,limits,function_name='B',geom,fline
end


function rk4advance,obs,step,function_name=vfield,vfieldarg
	; this function advances the variable forward
	; according to a runge-kuta technique
	; it is called by the runge-kutta fieldline routine

	; find the local field direction
	vf = call_function(vfield,obs,vfieldarg)
	b1= unit(vf)
	; create step in that direction, but only half the length
	dr = scale(step/2.0,b1)
	; move forward that half step
	x23 = plus(obs,dr)
	; then evaluate the field there	
	b23 = call_function(vfield,x23,vfieldarg)
	b23 = unit(b23)
	; move forward in the direction of the intermediate field
	x4 = plus(x23,dr)
	b4 = call_function(vfield,x4,vfieldarg)
	b4 = unit(b4)

	; use the proper weightings for each of the steps, b1, b23, b4
	y1 = scale(step/6.0,b1)
	y23 = scale(step*2.0/3.0,b23)
	y4 = scale(step/6.0,b4)

	; add the weighted steps together for the total
	result = plus(obs,y1)
	result = plus(result,y23)
	result = plus(result,y4)
	return,result
end

function inside,lim,pos
	x = pos.x
	y = pos.y
	z = pos.z
	
	resultx = (x gt lim.xm) and (x lt lim.xp)	
	resulty = (y gt lim.ym) and (y lt lim.yp)	
	resultz = (z gt lim.zm) and (z lt lim.zp)	

	result = (resultx) and (resulty) and (resultz)

	return,result
end


pro fieldline_rk4,x0,step,length,limits,function_name=vfield,vfieldarg,fline
	
	dummy = 1
	t0 = systime(dummy)
	
	; array for fieldline positions
	linef = x0
	; current position
	obs = x0
	; current field strength
	vf = call_function(vfield,x0,vfieldarg)
	compf = vf	
	magf = mag(vf)
	arclenf = 0

	; trace through line
	s = 0.0
	while ((inside(limits,obs)) and (s lt length)) do begin
	;for s=0.0,length,step do begin
		; advance the position
		obs = rk4advance(obs,step,function_name=vfield,vfieldarg)
		; find the field strength there
		vf = call_function(vfield,obs,vfieldarg)
		compf = [compf,vf]
		magf = [magf,mag(vf)]
		arclenf = [arclenf,s]
		; store position
		linef = [linef,obs]
		;print,s
		s = s + step
	;endfor
	endwhile
	if (s lt length) then print,"passed outside limits" $
	else print,"total length traced = ",s
	
	fline = {line:linef,comp:compf,mag:magf,arclen:arclenf}
	print,'elapsed time ',systime(dummy)-t0
	print,'starting point:  (x,y,z) = ',x0
	print,'ending point:    (x,y,z) = ',obs
	print,' '
end






function adaptive_advance,obs,step,geom
	; I was trying to write a routine to use
	; an adaptive step size.  it turned out to be slower
	; than just runge-kutta or even euler's method
	; this is the guts of the routine that actually decides
	; the step sizes
	
	; take a full step
	full = rk4advance(obs,step,geom)
	; take two half steps in a row
	half1 = rk4advance(obs,step/2,geom)
	half2 = rk4advance(half1,step/2,geom)
	; calculate the difference between doing it in one or two steps
	diff = mag(minus(full,half2))/mag(half2)
	; some tolerance for the difference I made up
	eps = 0.001
	
	; if the single step gives an answer close enough to the two
	; double steps, then it's probably ok to increase the step size
	if diff lt eps then begin
		; this increases the step size by some amount
		; I tried a few things, like doubling and smaller
		; factors
		step = step*1.1
		return,full
	endif else begin
		; otherwise need to be more careful and not take huge
		; steps, so the step size is reduced by some factor
		step = step/1.1
		; go back and do it with the smaller step size
		return,adaptive_advance(obs,step,geom)
	endelse
end

function fieldline_adapt,geom,x0,initstep,length,xfinal,magb
	; I was trying to write a routine to use
	; an adaptive step size.  it turned out to be slower
	; than just runge-kutta or even euler's method
	
	; this function uses adaptive step size method to trace out a fieldline
	; geom is just the structure holding the geometry (i.e. loops)
	; x0 is starting position
	; initstep is initial step size to be used
	; length is total length of field line to find
	; xfinal stores the 3D position at the end of the line to compare
	; results with different step sizes
	; magB will store the magnitude of the B field at every point on the line


	; array for positions
	line = x0
	; current position
	obs = x0
	; field strength
	magB = mag(B(x0,geom))
	step = initstep
	
	; distance along fieldline
	s = 0
	; trace through fieldline
	while s lt length do begin
		; calculate where to move
		obs = adaptive_advance(obs,step,geom)
		; store field
		magB = [magB,mag(B(obs,geom))]
		; store position
		line = [line,obs]
		; increment arc length travelled so far
		s = s+step
	endwhile
	; final position
	xfinal = obs
	return,line
end



function fieldline_euler,geom,x0,step,length,xfinal,magB
	; this function uses euler's method to trace out a fieldline
	; geom is just the structure holding the geometry (i.e. loops)
	; x0 is starting position
	; step is step size to be used
	; length is total length of field line to find
	; xfinal stores the 3D position at the end of the line to compare
	; results with different step sizes
	; magB will store the magnitude of the B field at every point on the line

	; start at the starting!
	; line will be the array of results
	line = x0
	; obs is just the current position
	obs = x0
	; get the initial field strength
	magB = mag(B(x0,geom))
	
	; trace through a field line
	for s=0.0,length,step do begin
		; find the local field
		b = B(obs,geom)
		; store its magnitude
		magB = [magB,mag(b)]
		; make small step in the direction that the field points in
		dr = scale(step,unit(b))
		; take that step
		obs = plus(obs,dr)
		; store the new position
		line = [line,obs]
	endfor
	; the last point of the fieldline
	xfinal = obs
	return,line
end



pro speedtest,f,f2
	; routine to see how fast the various ways of integrating
	; the fieldline up work.
	; turns out my adaptive stuff was very slow and not
	; really any more accurate
	
	fast_init_geometry,geom
	dummy=1
	obs = {vector,1,0,0}
	start = systime(dummy)
	f = fieldline(geom,obs,.3,80,mb)
	time0 = systime(dummy)-start
	start = systime(dummy)
	f2 = fieldline_rk4(geom,obs,0.3,40,mb2)
	time1 = systime(dummy)-start
	print,'adaptive',time0
	print,'normal,',time1
end



pro step_size_test,start,length
	; this routine compares how long it takes to do runge-kutta
	; verses euler's method for various step sizes

	step = 0.01
	fast_init_geometry,geom
	dummy = 1

	t0 = systime(dummy)
	f = fieldline_rk4(geom,start,step,length,rkf,mb)
	rktime = systime(dummy)-t0
	t0 = systime(dummy)
	f = fieldline_euler(geom,start,step,length,ef,mb)
	etime = systime(dummy)-t0
	
	close,1
	openw,1,'stepsizetest.txt'
	print,step,rktime,etime
	rk0 = rkf
	e0 = ef
	printf,1,step,mag(minus(rkf,rk0)),rktime,mag(minus(ef,e0)),etime,mag(minus(rkf,ef))
	while step lt 8 do begin
		step = step*1.5
		
		t0 = systime(dummy)
		f = fieldline_rk4(geom,start,step,length,rkf,mb)
		rktime = systime(dummy)-t0
		t0 = systime(dummy)
		f = fieldline_euler(geom,start,step,length,ef,mb)
		etime = systime(dummy)-t0
		printf,1,step,mag(minus(rkf,rk0)),rktime,mag(minus(ef,e0)),etime,mag(minus(rkf,ef))
		
		print,step,rktime,etime
	endwhile
	close,1
end

PRO PLOT_FIELD,GEOM,X_AXIS,Y_AXIS,X,Y,Z
  window,/free
  X_FLD = FLTARR(N_ELEMENTS(X))
  Y_FLD = FLTARR(N_ELEMENTS(Y))
  Z_FLD = FLTARR(N_ELEMENTS(Z))
  FOR I = 0,N_ELEMENTS(X)-1 DO BEGIN
    obs = {vector,x(I),y(I),z(I)}
    B = B(obs,GEOM)
    X_FLD(I) = B.X
    Y_FLD(I) = B.Y
    Z_FLD(I) = B.Z
  ENDFOR
  ;IF (X_AXIS EQ 0) THEN X_PLT = X
  ;IF (X_AXIS EQ 1) THEN X_PLT = Y
  ;IF (X_AXIS EQ 2) THEN X_PLT = Z
  ;IF (Y_AXIS EQ 0) THEN Y_PLT = X_FLD
  ;IF (Y_AXIS EQ 1) THEN Y_PLT = Y_FLD
  ;IF (Y_AXIS EQ 2) THEN Y_PLT = Z_FLD
  if x_axis eq 0 then begin
    X_PLT = X
    xttl = 'x'
  endif
  if x_axis eq 1 then begin
    X_PLT = y
    xttl = 'y'
  endif
  if x_axis eq 2 then begin
    X_PLT = z
    xttl = 'z'
  endif
  if y_axis eq 0 then begin
    y_PLT = x_fld
    yttl = 'Bx'
  endif
  if y_axis eq 1 then begin
    y_PLT = y_fld
    yttl = 'By'
  endif
  if y_axis eq 2 then begin
    y_PLT = z_fld
    yttl = 'Bz'
  endif
  if x_axis eq 1 then xttle='y'
  if x_axis eq 2 then xttle='z'
  PLOT,X_PLT,Y_PLT,background=-1,color=0,xtitle=xttl,ytitle=yttl
RETURN
END



