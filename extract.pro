@write_csv.pro

;ask about shot 131002028

FUNCTION DATA, PITCH, RUN, SHOT_START, SHOT_END, REAL_START

current_vertical = ('\MACHINE::TOP.N6023F_01:INPUT_01')
current_total = ('\MACHINE::TOP.N6023F_01:INPUT_00')
time = 'dim_of('+current_total+')'

sum_vertical = 0
sum_toroidal = 0

for i=shot_start, shot_end do begin
	;REMOVE OUTLIERS
	if run eq 131003 then begin
		if (i eq 28)||(i eq 7)||(i eq 9)||(i eq 17)||(i eq 18)||(i eq 19)||(i eq 26) then continue
	endif
	if run eq 131004 then begin
		if (i eq 4)||(i eq 5)||(i eq 12)||(i eq 13)||(i eq 20)||(i eq 21)||(i eq 22) then continue
	endif
	if run eq 130918 then begin
		if (i eq 6)||(i eq 7)||(i eq 8)||(i eq 9)||(i eq 10)||(i eq 18)||(i eq 19)||(i eq 20)||(i eq 28)||(i eq 29)||(i eq 34)||(i eq 35)||(i eq 36)||(i eq 37)||(i eq 38)||(i eq 45)||(i eq 46)||(i eq 53)||(i eq 54) then continue
	endif
	if run eq 130913 then begin
		if (i eq 5)||(i eq 6)||(i eq 7)||(i eq 8)||(i eq 9)||(i eq 16)||(i eq 17)||(i eq 24)||(i eq 25) then continue
	endif
	if run eq 130911 then begin
		if (i eq 5)||(i eq 6)||(i eq 13)||(i eq 14)||(i eq 15)||(i eq 23)||(i eq 24) then continue
	endif
	if run eq 131018 then begin
		if (i eq 5)||(i eq 6)||(i eq 9)||(i eq 14)||(i eq 15)||(i eq 22)||(i eq 23) then continue
	endif
	if run eq 131025 then begin
		if (i eq 27)||(i eq 28)||(i eq 36)||(i eq 37)||(i eq 44)||(i eq 45)||(i eq 5)||(i eq 6)||(i eq 13)||(i eq 14)||(i eq 21)||(i eq 22) then continue
	endif
	if run eq 131016 then begin
		if (i eq 6)||(i eq 7)||(i eq 15)||(i eq 16)||(i eq 24)||(i eq 25)||(i eq 26) then continue
	endif
	if run eq 131009 then begin
		if (i eq 6)||(i eq 7)||(i eq 8)||(i eq 16)||(i eq 17)||(i eq 24)||(i eq 25) then continue
	endif

	;SET SHOT AS STRING
	r = string(run)
	case 1 of
		(i lt 10): begin
			s = r + '00' + string(i)
		end
		(i ge 10): begin
			s = r + '0' + string(i)
		end
	endcase
		shot = strcompress(s, /remove_all)
		mdsopen, 'machine', shot , status=stat
		
		;TAKE ABS VAL OF 40 OHM RUNS
		if (run eq 131016 || 131009) then begin
			t = mdsvalue(time)
			total = mdsvalue(current_total)*1000
			vertical = abs(smooth(mdsvalue(current_vertical)*10*12.1,3))
			toroidal = total - vertical

		endif else begin
			t = mdsvalue(time)
			total = abs(mdsvalue(current_total)*1000)
			vertical = abs(mdsvalue(current_vertical)*10*12.1)
			toroidal = total - vertical
		endelse
	
		;TIME WINDOW OF AVERAGING
		time1 = 5
		time2 = 20

		start = time1*2500.0
		finish = time2*2500.0
		interval = finish - start

		total_int = total[start:finish]
		vertical_int = vertical[start:finish]
		toroidal_int = toroidal[start:finish]
		
		;MAKE ARRAY OF COMPLETE RUN
		if i eq real_start then begin
			ver = [make_array(1), vertical_int]
			tor = [make_array(1), toroidal_int]
		endif else begin
			ver = [ver, vertical_int]
			tor = [tor, toroidal_int]
		endelse
		ver = ver[1:*]
		tor = tor[1:*]
		sum_vertical = double(total(ver))
		sum_toroidal = double(total(tor))
endfor

;;;

;TAKE AVERAGE AND STANDARD DEVIATION
avg_v = mean(ver)
avg_t = mean(tor)
s_v = stddev(ver)
s_t = stddev(tor)

show = [[strcompress('Pitch:' + string(pitch)) + '   ' +  strcompress('Run:' + string(run))], [strcompress('Average Vertical:' + string(avg_v)) + '   ' +  strcompress('Average Toroidal:' + string(avg_t))], [strcompress('Sigma Vertical:' + string(s_v)) + '   ' +  strcompress('Sigma Toroidal:' + string(s_t))], [' ']]

return, show
;;;



;WRITE DATA ONTO CSV FILE
;		if i eq shot_start then begin
;			ver = make_array(interval+1,1)
;			ver = [[ver],[vertical_int]]
;			ver = transpose(ver)
;		endif else begin
;			ver = [ver, [transpose(vertical_int)]]
;		endelse
;
;		if i eq shot_start then begin
;			tor = make_array(interval+1,1)
;			tor = [[tor],[toroidal_int]]
;			tor = transpose(tor)
;		endif else begin
;			tor = [tor, [transpose(toroidal_int)]]
;		endelse
;
;		if i eq shot_start then begin
;			ver = make_array(interval+1,1)
;			ver = [[ver],[vertical_int]]
;			ver = transpose(ver)
;		endif else begin
;			ver = [ver, [transpose(vertical_int)]]
;		endelse
;
;		if i eq shot_start then begin
;			tor = make_array(interval+1,1)
;			tor = [[tor],[toroidal_int]]
;			tor = transpose(tor)
;		endif else begin
;			tor = [tor, [transpose(toroidal_int)]]
;		endelse
;
;vertical_data = ver[1:*,*]
;toroidal_data = tor[1:*,*]
;write_csv_data, vertical_data, filename='vertical.csv', append=1
;write_csv_data, toroidal_data, filename='toroidal.csv', append=1

END


PRO EXTRACT

;PRINT AVG AND STDDEV OF RUNS ONTO .TXT FILE
close,1
openw,1,'mean.txt'

printf,1, data(3.6, 131003, 7, 32, 8)
printf,1, data(3.6, 131004, 4, 26, 6)
printf,1, data(5.0, 130918, 6, 32, 11)
printf,1, data(5.0, 130913, 5, 29, 10)
printf,1, data(5.0, 130918, 34, 59, 39)
printf,1, data(5.0, 130911, 5, 28, 7)
printf,1, data(10.0, 131018, 5, 27, 7)
printf,1, data(10.0, 131025, 27, 49, 29)
printf,1, data(40.0, 131025, 5, 26, 7)
printf,1, data(40.0, 131016, 6, 31, 8)
printf,1, data(40.0, 131009, 6, 29, 9)

close,1

END

