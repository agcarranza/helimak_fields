; elliptical.pro ; by abinadab dieter / adieter@mail.utexas.edu; elliptical integrals from numerical recipes; needed by homer2 to calculate the magnetic; field from circular coilsfunction rf,x,y,z	xt = x	yt = y	zt = z	errtol = 0.08	condition = 1	c1 = 1.0/24.0	c2 = 0.1	c3 = 3.0 / 44.0	c4 = 1.0/14.0	while (condition) do begin		sx = sqrt(xt)		sy = sqrt(yt)		sz = sqrt(zt)		alamb = sx*(sy+sz)+sy*sz		xt = 0.25*(xt+alamb)		yt = 0.25*(yt+alamb)		zt = 0.25*(zt+alamb)		ave = 1.0/3.0 * (xt+yt+zt)		delx = (ave-xt)/ave		dely = (ave-yt)/ave		delz = (ave-zt)/ave		del = [abs(delx),abs(dely),abs(delz)]		condition = (max(del) gt errtol)	endwhile	e2 = delx*dely-delz*delz	e3 = delx*dely*delz	result = (1.0+(c1*e2-c2-c3*e3)*e2+c4*e3)/sqrt(ave)	return,resultendfunction rd,x,y,z	xt = x	yt = y	zt = z	errtol = 0.05	condition = 1	sum = 0.0	fac = 1.0	while (condition) do begin		sx = sqrt(xt)		sy = sqrt(yt)		sz = sqrt(zt)		alamb = sx*(sy+sz)+sy*sz		sum = sum + fac/(sz*(zt+alamb))		fac = 0.25*fac		xt = 0.25*(xt+alamb)		yt = 0.25*(yt+alamb)		zt = 0.25*(zt+alamb)		ave = 0.2*(xt+yt+3*zt)		delx = (ave-xt)/ave		dely = (ave-yt)/ave		delz = (ave-zt)/ave		del = [delx,dely,delz]		condition = (max(del) gt errtol)	endwhile	ea = delx*dely	eb = delz*delz	ec = ea-eb	ed = ea-6*eb	ee = ed+ec+ec	c1 = 3./14	c2 = 1./6	c3 = 9./22	c4 = 3./26	c5 = 0.25*c3	c6 = 1.5*c4	result = 3*sum+fac*(1+ed*(-c1+c5*ed-c6*delz*ee)+delz*(c2*ee+delz*(-c3*ec+delz*c4*ea)))/(ave*sqrt(ave))	return,resultendfunction E,arg	; 2nd kind	q = 1.0-arg*arg	return,rf(0,q,1)-arg*arg/3*rd(0,q,1)endfunction K,arg	; 1st kind	return,rf(0,1.0-arg*arg,1.0)	end