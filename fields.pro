
;by Aldo Carranza

;06/09/14
;This program creates a widget that creates a 3D plot of the magnetic
;field lines in the helimak, given an input for the parameters.
;There are two ways to input values. One is a 'shot input' of the shot number
;which automatically finds the values of the currents and pitch and
;sends them as inputs into the plot.
;The other is a 'manual input' of the parameters, either the pitch
;(then interpolates the currents) or the currents (then interpolates the pitch).

PRO FIELDS

; BASE WIDGET
base = widget_base(title='Vacuum Magnetic Fields in Helimak', row=2)
b1  = widget_base(base, column=2)
base1 = widget_base(b1, /column)
base2 = widget_base(b1, /column)
b2  = widget_base(base, /column, /frame)


number = cw_field(b2, title='Number of Field Lines:', uvalue='number', uname='number', value='4', /return_events, /integer)
column = ['X','Y','Z','Step','Length', 'Color']
r = indgen(5)
row = string(r[1:*])
default = string(double([[0.0,-0.760,-1.0,0.1,99,255],[0.0,-0.963,-1.0,0.1,77,65280],[0.0,-1.166,-1.0,0.1,62,16771180],[0.0,-1.370,-1.0,0.1,51.5,65535]]))

table_label = widget_label(b2, value='Field Lines:')
table = widget_table(b2, xsize=6, ysize=4, value=default, row_labels=row, column_labels=column, uvalue='table', uname='table', column_widths=173, /editable)


draw = widget_draw(base1, xsize=800, ysize = 725)

win1 = widget_base(base2, /column, /frame)
win2 = widget_base(base2, /column, /frame)
win2_0 = widget_base(win2, /row)
win2_1 = widget_base(win2, /row)
win2_2 = widget_base(win2, /column)
win3 = widget_base(base2, /row, /frame)


; SHOT
win1_1 = widget_base(win1, row=2)
shot_label = widget_label(win1_1, value='Shot Input')
shot = cw_field(win1_1, /return_events, title="Shot Number", uname ='shot', uvalue='shot',VALUE='140606001', /long)

win1_2 = widget_base(win1, /row)
time1 = cw_field(win1_2, title='Start Time (s):', uname='time1', value='5.0', xsize=4, /floating)
time2 = cw_field(win1_2, title='End Time (s):', uname='time2', value='20.0', xsize=4, /floating)

win1_3 = widget_base(win1, /row)
print_shot = widget_button(win1_3, uname='print_shot', value='Run Shot')



win1_2 = widget_base(win1, /column, /base_align_right)
total_current = cw_field(win1_2, title='Total Current (A):', /noedit)
toroidal_current = cw_field(win1_2, title='Toroidal Coil Current (A):', /noedit)
vertical_current = cw_field(win1_2, title='Vertical Coil Current (A):', /noedit)
ratio = cw_field(win1_2, title='Ratio V/T:', /noedit)
pitch_estimate = cw_field(win1_2, title='Estimate of Pitch (Ohm):', /noedit)




;CURRENT IN COILS
manual_label = widget_label(win2_0, value='Manual Input')
pitch = cw_field(win2_1, title="Pitch (Ohm):", uname='pitch', value='20', /floating)
pitch_run = widget_button(win2_1, uname='pitch_run', value='Run Pitch')


current_label = widget_label(win2_2, uname='current_label', value='Current (A):', /align_left) 

current_t = cw_field(win2_2, title="Rectangular Coils", uname ='current_t', uvalue='current_t',VALUE='21952.0', /floating)


circ_label = widget_label(win2_2, value='Circular Coils:', /align_left)

win2_2_1 = widget_base(win2_2, /column, /base_align_right)
current_c1 = cw_field(win2_2_1, title="Top", uname ='current_c1', uvalue='current_c1',VALUE='3024.0', /floating)

current_c3 = cw_field(win2_2_1, title="Middle", uname ='current_c3', uvalue='current_c3',VALUE='1800.0', /floating)

current_c2 = cw_field(win2_2_1, title="Bottom", uname ='current_c2', uvalue='current_c1',VALUE='3024.0', /floating)

current_run = widget_button(win2_2, uname='current_run', value='Run Currents')


quit = widget_button(win3, uname='quit', value='Quit')



;realize widgets and setup event manager
widget_control, base, /realize
widget_control, draw, get_value=index
wset, index

xmanager, 'fields', base, /no_block


;create structure of widgets that will pass events
widget_control, base, set_uvalue={shot:shot, draw:draw, total_current:total_current, toroidal_current:toroidal_current, vertical_current:vertical_current, ratio:ratio, pitch_estimate:pitch_estimate, pitch:pitch, time1:time1, time2:time2, print_shot:print_shot, current_t:current_t, current_c1:current_c1, current_c2:current_c2, current_c3:current_c3, number:number, table:table}


return
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO FIELDS_EVENT, EVENT
;create event manager that redirects event occurrences
;to their respective parts of the code
;pass widget data that holds info

common data, r, bz, bt
r = [5.0,10.0,15.0,20.0,30.0,40.0]
bz = [24.0,12.0,7.0,5.0,1.9,0.7]
bt = [784.0,797.0,801.0,804.0,806.0,808.0]
default = string(double([[0.0,-0.760,-1.0,0.1,99,255],[0.0,-0.963,-1.0,0.1,77,65280],[0.0,-1.166,-1.0,0.1,62,16771180],[0.0,-1.370,-1.0,0.1,51.5,65535]]))

widget_control, event.top, get_uvalue=widgetdata

eventval = widget_info(event.id, /uname)
case eventval of
	'shot': shot_plot, widgetdata
	'print_shot': shot_plot, widgetdata
	'pitch_run': pitch_plot, widgetdata
	'current_run': current_plot, widgetdata
	'quit': widget_control, event.top, /destroy
	'number': begin
		widget_control, widgetdata.number, get_value=y
		widget_control, widgetdata.table, table_ysize=y
		r = indgen(y+1)
		row = string(r[1:*])
		widget_control, widgetdata.table, row_labels=row
		a = string(dindgen(6,y-4)*0.0)
		array = [[default], [a]]
		widget_control, widgetdata.table, set_value=array
		end
	'table':
	end

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO FIELD_PLOT, rc, cc1, cc2, cc3, widgetdata
;part of code that creates plots
;creates a temporary input file with static information on the plot
;but modifiable input values of the current
;created a new, modified version of auto_homer2, the procedure that
;plots the data, so that it would work with the widget by setting the
;window to plot inside the widget
;extracts data from table and writes it into input.in
;if color is not a number, takes input of color as word (string)
;and converts to number by using col.pro function



close,1
openw,1,'input.in'
input = [['background,16777215'],['rect_n,16'],['rect_h,2.312'],['rect_w,1.365'],['rect_c,1.109'],['circ_r,1.972,1.972,1.988'],['circ_h,1.050,-1.050,0.0'],['plotB,2,1,1.1,0.0,-1.0,1.1,0.0,+1.0,100'],['range,-1.5,1.5,-1.5,1.5,-1.1,1.1']]

printf,1,input



widget_control, widgetdata.table, get_value=data
c = size(data)
rows = c[2]
cols = c[1]
array = make_array(1)

FOR i=0, rows-1 do begin
	d = data[cols-1,i]
	b = byte(d)
	if b[0] gt 60 then begin
		array = string(double([array, col(d)]))
	endif else begin
		array = string(double([array, d]))
	endelse
ENDFOR

color = array[1:*]
FOR i=0, rows-1 do begin
	printf,1, 'line,', format='(A,I,$)'
		FOR j=0, cols-2 do begin
			printf,1, string(data[j,i])+',', format='(A,I,$)'
		ENDFOR
	printf,1, color[i]
ENDFOR



rect_current = 'rect_j,' + string(rc)
circ_current = 'circ_j,' + string(cc1) + ',' + string(cc2) + ',' + string(cc3)

printf,1, rect_current
printf,1, circ_current

close,1

auto_homer2_widget,'input.in'

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO SHOT_PLOT, widgetdata
;creates plot when 'Print Shot' button is pressed
;extract currents data of inputted shot number
;if shot does not exist, give error
;average values of currents within specified time window
;multiply currents by number of turns of coils
;send values to field_plot
;75000 data entries in 30 seconds, therefore 2500 entries per second
;added error handling

common data

current_vertical = ('\MACHINE::TOP.N6023F_01:INPUT_01')
current_total = ('\MACHINE::TOP.N6023F_01:INPUT_00')
time = 'dim_of('+current_total+')'

widget_control, widgetdata.shot, get_value=shot
shot = abs(shot)

;error handling
if strlen(strcompress(string(abs(shot)),/remove_all)) ne 9 then begin
	ok = dialog_message(['Invalid Shot Number. Must be a long of form: YYMMDD###'], /center,/error)
	shot = 140606001
	error = 1
endif else begin
	error = 0
endelse

mdsopen, 'machine', shot, status=stat

if error eq 1 then begin
	stat = 265392042
endif


t = mdsvalue(time)
total = mdsvalue(current_total)*1000
vertical = mdsvalue(current_vertical)*10*12.1
toroidal = total - vertical

widget_control, widgetdata.time1, get_value=time1
widget_control, widgetdata.time2, get_value=time2

start = time1*2500.0
finish = time2*2500.0
interval = finish - start

total_int = total[start:finish]
vertical_int = vertical[start:finish]
toroidal_int = toroidal[start:finish]

total_avg = total(total_int)/interval
vertical_avg = total(vertical_int)/interval
toroidal_avg = total(toroidal_int)/interval
ratio = vertical_avg/toroidal_avg
int_r = interpol(r,bz,vertical_avg)

rc = string(toroidal_avg*28)
cc1 = string(vertical_avg*126)
cc2 = string(vertical_avg*126)
cc3 = string(vertical_avg*75)

widget_control, widgetdata.total_current, set_value=total_avg
widget_control, widgetdata.toroidal_current, set_value=toroidal_avg
widget_control, widgetdata.vertical_current, set_value=vertical_avg
widget_control, widgetdata.ratio, set_value=ratio
widget_control, widgetdata.pitch_estimate, set_value=int_r

if (stat eq 265392042) then begin
	if error ne 1 then begin
		ok=DIALOG_MESSAGE(['Shot does not exist.'],/center,/error)
	endif
endif else begin
	field_plot, rc, cc1, cc2, cc3, widgetdata
endelse

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO CURRENT_PLOT, widgetdata
;creates plot when 'Run Currents' button is pressed
;takes entries for current, interpolates values of currents
;and sends values to field_plots

common data

widget_control, widgetdata.current_t, get_value=rc
widget_control, widgetdata.current_c1, get_value=cc1
widget_control, widgetdata.current_c2, get_value=cc2
widget_control, widgetdata.current_c3, get_value=cc3

print, datatype(rc)

vertical = cc1/126
int_r = interpol(r,bz, vertical)
widget_control, widgetdata.pitch, set_value=int_r

field_plot, rc, cc1, cc2, cc3, widgetdata

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO PITCH_PLOT, widgetdata
;creates plot when 'Run Pitch' button is pressed
;takes entry for pitch, interpolates values of currents
;and sends values to field_plots

common data

widget_control, widgetdata.pitch, get_value=pitch

int_bz = interpol(bz,r,pitch)
int_bt = interpol(bt,r,pitch)
cc1 = string(int_bz*126)
cc2 = string(int_bz*126)
cc3 = string(int_bz*75)
rc = string(int_bt*28)

widget_control, widgetdata.current_t, set_value=rc
widget_control, widgetdata.current_c1, set_value=cc1
widget_control, widgetdata.current_c2, set_value=cc2
widget_control, widgetdata.current_c3, set_value=cc3

field_plot, rc, cc1, cc2, cc3, widgetdata

END
