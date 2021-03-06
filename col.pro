function col, color_input
;Maps nanmes to True colors.
;use:
;plot, x,y, color=col('color_name')
;
  col=fix([$
           [0,0,0],$
   	  	   [2,0,0],$.c
	  	   [2,1,0],$
		   [2,2,0],$
		   [1,2,0],$
		   [0,2,0],$
		   [0,2,1],$
		   [0,2,2],$
		   [0,1,2],$
		   [0,0,2],$
		   [1,0,2],$
		   [2,0,2],$
		   [2,0,1]$
		          ]*127.5)
  color_name=strtrim(string(color_input),2)
  ;print,color_name
  if !d.n_colors le 256 then tvlct,transpose(col),1
  case 1 of
      color_name eq 'black' or color_name eq '0': begin
        c=0
        color=0L
      end
      color_name eq 'red' or color_name eq '1': begin
        c=1
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'orange' or color_name eq '2': begin
        c=2
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'yellow' or color_name eq '3': begin
        c=3
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end    
      color_name eq 'yellow green' or color_name eq 'dew' or color_name eq '4': begin
        c=4
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'green' or color_name eq '5' : begin
        c=5
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'seafoam' or color_name eq 'green blue' or color_name eq '6': begin
        c=6
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'cyan' or color_name eq 'sky blue' or color_name eq '7': begin
        c=7
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'light blue' or color_name eq '8': begin
        c=8
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end    
      color_name eq 'blue' or color_name eq '9': begin
        c=9
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'violet' or color_name eq '10': begin
        c=10
        color=col[0,c] + 256L * (col[1,c] + 256L * col[2,c])
      end
      color_name eq 'magenta' or color_name eq '11': begin
        c=11
        color=col[0, c] + 256L * (col[1, c] + 256L * col[2, c])
      end
      color_name eq 'purple' or color_name eq '12': begin
        c=12
        color=col[0, c] + 256L * (col[1, c] + 256L * col[2, c])
      end
    else: begin
      print,'Your Selection is Not on the Color List:'
      print,'Valid Color List:'
      print,'[black,0], [red,1] [orange,2] [Yellow,3] [yellow green, dew,4]'
      print,'[green,5], [seafoam, green blue,6], [cyan, sky blue,7], [light blue,8]'
      print,'[blue,9], [violet,10],[magenta,11], [purple,12], else returns white'
      color=16777215L
      c=0
    endelse
  endcase
  ;print,c
  if !d.n_colors le 256 then color=c+1
  if (!d.name eq 'PS' and c eq 0) then c=255
  if (!d.name eq 'PS' and c eq 255) then c=0
  ;print,color
return,color
end
