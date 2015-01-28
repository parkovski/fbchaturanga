#include once "fbgfx.bi"
#include once "fbpng.bi"
#inclib "fbpng"

#define getpng(fn) png_load( fn, PNG_TARGET_FBNEW )

const red as integer = 1
const black as integer = 2
const yellow as integer = 4
const green as integer = 8

const raja as integer = 1
const elephant as integer = 2
const horse as integer = 4
const ship as integer = 8
const pawn as integer = 16

'' if this number is 1, they are in.
'' if 2, they control the other team.
dim shared as integer redplaying = 1, blackplaying = 1, yellowplaying = 1, greenplaying = 1

'' the images
dim shared as any ptr raja_red, raja_black, raja_yellow, raja_green, _
	elephant_red, elephant_black, elephant_yellow, elephant_green, _
	horse_red, horse_black, horse_yellow, horse_green, _
	ship_red, ship_black, ship_yellow, ship_green, _
	pawn_red, pawn_black, pawn_yellow, pawn_green, _
	throne, flower

dim shared as integer board( 0 to 7, 0 to 7 )
dim shared as integer xmove( 0 to 13 )
dim shared as integer ymove( 0 to 13 )
dim shared as integer nummoves
dim shared as integer piecex = -1, piecey = -1
dim shared as integer usedroll1, usedroll2

dim shared as integer dice1, dice2
dim shared as integer turn '' whose turn is it?

'' themes
dim shared as integer clr_background
dim shared as integer clr_nomoves
dim shared as integer clr_okmoves
dim shared as integer clr_openspot
dim shared as integer clr_border
dim shared as integer clr_clickborder
dim shared as integer clr_okborder
dim shared as string fn_raja, fn_elephant, fn_horse, fn_ship, fn_pawn, fn_throne, fn_flower
dim shared as string nm_raja, nm_rajapawn, nm_elephant, nm_horse, nm_ship, nm_pawn
dim shared as string nm_team1, nm_team2, nm_team3, nm_team4
dim shared as integer clr_team1, clr_team2, clr_team3, clr_team4
dim shared as integer draw_team_color = 0
dim shared as integer back_color, text_color
dim shared as integer dice_good_color, dice_bad_color

dim shared as integer can_use_cheat_code = 0
dim shared as integer cheat_code_activated = 0
dim shared as integer cheat_step = 0

declare function pieceexists( piece as integer ) as integer
#define hasraja(team) pieceexists( raja or ((team)shl 8) )
#define haselephant(team) pieceexists( elephant or ((team)shl 8) )
#define hashorse(team) pieceexists( horse or ((team)shl 8) )
#define hasship(team) pieceexists( ship or ((team)shl 8) )
declare function countpawns( team as integer ) as integer

declare sub next_turn
declare sub roll_dice
declare sub winner( team as integer )
declare function get_playable_spots( x as integer, y as integer, xarr() as integer, yarr() as integer ) as integer
declare function spot_in_moves( x as integer, y as integer ) as integer
declare sub get_piece_back( x as integer, y as integer )

declare sub loaddefaulttheme
declare sub loadtheme( fn as string )

declare sub resetboard
declare sub loadpieces
declare sub freepieces
declare sub drawboard
declare sub drawpieces
declare sub drawdice

declare sub new_game

declare sub wannabeclown(go as integer)

randomize timer

screenres 800, 600, 32

loaddefaulttheme

dim themename as string

/'
print
print
print
print
print ,"Do you want to see a wannabe clown? ";
input themename
wannabeclown(themename = "yeah")
'/

cls
print
print
print
print
print ,"Press <ENTER> to start game"
print ,"--OR--"
print ,"Give me a theme name (filename without the '.theme.txt'): ";
input themename
if themename <> "" then loadtheme( themename + ".theme.txt" )

loadpieces
resetboard
roll_dice

do
	screenlock
	''cls
	line ( 0, 0 ) - ( 800, 600 ), back_color, BF
	drawboard
	drawpieces
	drawdice

	dim e as FB.EVENT
	dim as integer mx, my
	if screenevent( @e ) then
		getmouse mx, my
		if e.type = FB.EVENT_MOUSE_BUTTON_RELEASE then
			'' inside the grid?
			if e.button = 1 and mx >= 5 and mx < 517 and my >= 5 and my < 517 then
				mx = ( mx - 5 ) \ 64
				my = ( my - 5 ) \ 64
				if mx = piecex and my = piecey then
					nummoves = 0
					piecex = -1
					piecey = -1
				elseif nummoves > 0 then
					'' if we can move here, then do
					if spot_in_moves( mx, my ) then
						'' which one do we cancel?
						'' note the special case: dice1=raja, dice2=pawn, and piece=pawn then we cancel dice2.
						if usedroll1 then
							usedroll2 = 1
						elseif usedroll2 then
							usedroll1 = 1
						else
							if dice1 = 0 and dice2 = 4 and ( board( piecex, piecey ) and &hFF ) = pawn then
								usedroll2 = 1
							elseif dice1 = 0 and ( board( piecex, piecey ) and &hFF ) = pawn then
								usedroll1 = 1
							elseif ( board( piecex, piecey ) and &hFF ) = ( 1 shl dice1 ) then
								usedroll1 = 1
							else
								usedroll2 = 1
							end if
						end if
						
						board( mx, my ) = board( piecex, piecey )
						board( piecex, piecey ) = 0
						piecex = -1
						piecey = -1
						nummoves = 0
						if ( board( mx, my ) and &hFF ) = pawn then
							select case board( mx, my ) shr 8
							case red
								if my = 0 then get_piece_back( mx, my )
								
							case black
								if mx = 7 then get_piece_back( mx, my )
							
							case yellow
								if my = 7 then get_piece_back( mx, my )
							
							case green
								if mx = 0 then get_piece_back( mx, my )
								
							end select
						elseif ( board( mx, my ) and &hFF ) = raja then
							select case board( mx, my ) shr 8
							case red
								if mx = 4 and my = 0 and hasraja( yellow ) = 0 then redplaying = 2
								
							case black
								if mx = 7 and my = 4 and hasraja( green ) = 0 then blackplaying = 2
								
							case yellow
								if mx = 3 and my = 7 and hasraja( red ) = 0 then yellowplaying = 2
								
							case green
								if mx = 0 and my = 3 and hasraja( black ) = 0 then greenplaying = 2
							
							end select
						end if
					end if
				else
					nummoves = get_playable_spots( mx, my, xmove(), ymove() )
					piecex = mx
					piecey = my
				end if
			elseif e.button = 1 and mx >= 670 and mx <= 680 and my >= 545 and my <= 555 then
				exit do
			elseif e.button = 1 and mx >= 550 and mx <= 640 and my >= 100 and my <= 110 then
				next_turn
			elseif e.button = 1 and mx >= 550 and mx <= 640 and my >= 112 and my <= 122 then
				new_game
			end if
		elseif e.type = FB.EVENT_KEY_RELEASE then
			'' code is "/xzt[r/e/h/s/p/n]"
			select case e.ascii
			case asc("/")
				cheat_step = 1
			case asc("x")
				if cheat_step = 1 then cheat_step = 2 else cheat_step = 0
			case asc("z")
				if cheat_step = 2 then cheat_step = 3 else cheat_step = 0
			case asc("t")
				if cheat_step = 3 then cheat_step = 4 else cheat_step = 0
			case asc("r")
				if cheat_step = 4 then cheat_code_activated = 1
				cheat_step = 0
			case asc("e")
				if cheat_step = 4 then cheat_code_activated = 2
				cheat_step = 0
			case asc("h")
				if cheat_step = 4 then cheat_code_activated = 4
				cheat_step = 0
			case asc("s")
				if cheat_step = 4 then cheat_code_activated = 8
				cheat_step = 0
			case asc("p")
				if cheat_step = 4 then cheat_code_activated = 16
				cheat_step = 0
			case asc("n")
				if cheat_step = 4 then cheat_code_activated = 3
				cheat_step = 0
			case asc(".")
				if cheat_step = 4 then
					usedroll1 = 0
					usedroll2 = 0
				end if
				cheat_step = 0
			case asc(",")
				if cheat_step = 4 then roll_dice
				cheat_step = 0
			case else
				cheat_step = 0
			end select
		end if
	end if
	screenunlock
loop

freepieces
end

''-----
sub loaddefaulttheme
	clr_background = &h9999FF
	clr_nomoves = &hFF6666
	clr_okmoves = &h6666FF
	clr_openspot = &h66CC66
	clr_border = 0
	clr_clickborder = &hFFFFFF
	clr_okborder = &h0000FF
	fn_raja = "raja"
	fn_elephant = "elephant"
	fn_horse = "horse"
	fn_ship = "ship"
	fn_pawn = "pawn"
	fn_throne = "throne"
	fn_flower = "flower"
	nm_raja = "Raja"
	nm_rajapawn = "Raja/Pawn"
	nm_elephant = "Elephant"
	nm_horse = "Horse"
	nm_ship = "Ship"
	nm_pawn = "Pawn"
	nm_team1 = "Red Team"
	nm_team2 = "Black Team"
	nm_team3 = "Yellow Team"
	nm_team4 = "Green Team"
	clr_team1 = cint( rgb( 255, 0, 0 ) )
	clr_team2 = cint( rgb( 255, 255, 255 ) )
	clr_team3 = cint( rgb( 255, 255, 0 ) )
	clr_team4 = cint( rgb( 0, 255, 0 ) )
	can_use_cheat_code = 0
	draw_team_color = 0
	back_color = 0
	text_color = &hFFFFFF
	dice_good_color = &h55FF55
	dice_bad_color = &hFF5555
end sub

sub loadtheme( fn as string )
	dim fnr as integer = freefile
	dim ln as string
	
	dim eqpos as integer
	dim leftside as string, rightside as string
	
	can_use_cheat_code = 0
	
	if open( fn for input as #fnr ) = 0 then
		do
			line input #fnr, ln
			eqpos = instr( ln, "=" )
			
			if eqpos then
				leftside = lcase( trim( left( ln, eqpos - 1 ) ) )
				rightside = trim( mid( ln, eqpos + 1 ) )
				
				if leftside = "" or rightside = "" then continue do
				select case leftside
				case "background"
					clr_background = val( "&h" & rightside )
				case "nomoves"
					clr_nomoves = val( "&h" & rightside )
				case "okmoves"
					clr_okmoves = val( "&h" & rightside )
				case "openspot"
					clr_openspot = val( "&h" & rightside )
				case "border"
					clr_border = val( "&h" & rightside )
				case "clickborder"
					clr_clickborder = val( "&h" & rightside )
				case "okborder"
					clr_okborder = val( "&h" & rightside )
				case "raja"
					fn_raja = rightside
				case "elephant"
					fn_elephant = rightside
				case "horse"
					fn_horse = rightside
				case "ship"
					fn_ship = rightside
				case "pawn"
					fn_pawn = rightside
				case "raja"
					fn_raja = rightside
				case "throne"
					fn_throne = rightside
				case "flower"
					fn_flower = rightside
				case "rajaname"
					nm_raja = rightside
				case "rajapawn"
					nm_rajapawn = rightside
				case "elephantname"
					nm_elephant = rightside
				case "horsename"
					nm_horse = rightside
				case "shipname"
					nm_ship = rightside
				case "pawnname"
					nm_pawn = rightside
				case "team1name"
					nm_team1 = rightside
				case "team2name"
					nm_team2 = rightside
				case "team3name"
					nm_team3 = rightside
				case "team4name"
					nm_team4 = rightside
				case "team1color"
					clr_team1 = val( "&h" & rightside )
				case "team2color"
					clr_team2 = val( "&h" & rightside )
				case "team3color"
					clr_team3 = val( "&h" & rightside )
				case "team4color"
					clr_team4 = val( "&h" & rightside )
				case "showteamcolors"
					if rightside = "low" then
						draw_team_color = 1
					elseif rightside = "high" then
						draw_team_color = 2
					else
						draw_team_color = 0
					end if
				case "cheatcode"
					if rightside = "active" then
						can_use_cheat_code = 1
					else
						can_use_cheat_code = 0
					end if
				case "backcolor"
					back_color = val( "&h" & rightside )
				case "textcolor"
					text_color = val( "&h" & rightside )
				case "dicegoodcolor"
					dice_good_color = val( "&h" & rightside )
				case "dicebadcolor"
					dice_bad_color = val( "&h" & rightside )
				end select
			end if
		loop while eof( fnr ) = 0
		
		close fnr
	end if
end sub

function pieceexists( piece as integer ) as integer
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			if board( i, j ) = piece then return 1
		next
	next
	return 0
end function

function countpawns( team as integer ) as integer
	dim count as integer = 0
	
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			if board( i, j ) = pawn or ( team shl 8 ) then count += 1
		next
	next
	
	return count
end function

sub next_turn
	dim teamscount as integer = 5
	inc_turn:
	turn += 1
	teamscount -= 1
	if turn = 4 then turn = 0
	if hasraja( 1 shl turn ) = 0 then goto inc_turn
	if teamscount = 1 then winner( 1 shl turn ) else roll_dice
	nummoves = 0
	piecex = -1
	piecey = -1
end sub

sub roll_dice
	usedroll1 = 0
	usedroll2 = 0
	
	dice1 = int( rnd * 4 )
	'' make sure we have this piece.
	if pieceexists( ( 1 shl dice1 ) or ( ( 1 shl turn ) shl 8 ) ) then '' nothing
	elseif turn = 0 and pieceexists( ( 1 shl dice1 ) or ( yellow shl 8 ) ) and redplaying = 2 then
	elseif turn = 1 and pieceexists( ( 1 shl dice1 ) or ( green shl 8 ) ) and blackplaying = 2 then
	elseif turn = 2 and pieceexists( ( 1 shl dice1 ) or ( red shl 8 ) ) and yellowplaying = 2 then
	elseif turn = 3 and pieceexists( ( 1 shl dice1 ) or ( black shl 8 ) ) and greenplaying = 2 then
	else
		dice1 = 4 '' pawn only
	end if
	
	dice2 = int( rnd * 4 )
	'' same thing
	if pieceexists( ( 1 shl dice2 ) or ( ( 1 shl turn ) shl 8 ) ) then '' nothing
	elseif turn = 0 and pieceexists( ( 1 shl dice2 ) or ( yellow shl 8 ) ) and redplaying = 2 then
	elseif turn = 1 and pieceexists( ( 1 shl dice2 ) or ( green shl 8 ) ) and blackplaying = 2 then
	elseif turn = 2 and pieceexists( ( 1 shl dice2 ) or ( red shl 8 ) ) and yellowplaying = 2 then
	elseif turn = 3 and pieceexists( ( 1 shl dice2 ) or ( black shl 8 ) ) and greenplaying = 2 then
	else
		dice2 = 4 '' pawn only
	end if
end sub

sub winner( team as integer )
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			board( i, j ) = raja or ( team shl 8 )
		next
	next
end sub

function second_ok_move( clr as integer, x as integer, y as integer ) as integer
	'' is the space defined?
	if x < lbound( board, 1 ) then return 0
	if x > ubound( board, 1 ) then return 0
	if y < lbound( board, 2 ) then return 0
	if y > ubound( board, 2 ) then return 0

	'' if it's my team
	if ( ( board( x, y ) shr 8 ) = clr ) or ( ( board( x, y ) shr 8 ) = ( 1 shl turn ) ) then return 0
	'' or if it's a color I took over
	if turn = 0 and ( board( x, y ) shr 8 ) = yellow and redplaying = 2 then return 0
	if turn = 1 and ( board( x, y ) shr 8 ) = green and blackplaying = 2 then return 0
	if turn = 2 and ( board( x, y ) shr 8 ) = red and yellowplaying = 2 then return 0
	if turn = 3 and ( board( x, y ) shr 8 ) = black and greenplaying = 2 then return 0
	return 1
end function


function ok_move( clr as integer, x as integer, y as integer ) as integer
	'' is it my turn?
	if clr <> ( 1 shl turn ) then
		'' if I got the other team's pieces, it's still okay.
		if turn = 0 and clr = yellow and redplaying = 2 then return second_ok_move( clr, x, y )
		if turn = 1 and clr = green and blackplaying = 2 then return second_ok_move( clr, x, y )
		if turn = 2 and clr = red and yellowplaying = 2 then return second_ok_move( clr, x, y )
		if turn = 3 and clr = black and greenplaying = 2 then return second_ok_move( clr, x, y )
		return 0
	end if
	
	return second_ok_move( clr, x, y )
end function

function did_cheat_code( x as integer, y as integer ) as integer
	if can_use_cheat_code = 0 then return 0
	if cheat_code_activated then
		if cheat_code_activated = 3 then _
			board( x, y ) = 0 _
		else _
			board( x, y ) = ( ( 1 shl turn ) shl 8 ) or cheat_code_activated
			
		cheat_code_activated = 0
		return 1
	end if
	return 0
end function

function get_playable_spots( x as integer, y as integer, xarr() as integer, yarr() as integer ) as integer
	dim as integer piece = board( x, y ) and &hFF
	dim as integer clr = board( x, y ) shr 8
	dim as integer n = 0
	dim as integer roll1, roll2
	
	if usedroll1 then roll1 = 100 else roll1 = 1 shl dice1
	if usedroll2 then roll2 = 100 else roll2 = 1 shl dice2
	
	if did_cheat_code( x, y ) <> 0 then return 0
	
	'' return 0 if it's not a piece that we rolled
	if piece <> roll1 and piece <> roll2 then
		'' make sure it's not a pawn with a raja dice
		if ( piece = pawn and roll1 <> 1 and roll2 <> 1 ) or ( piece <> pawn ) then return 0
	end if
		
	select case as const piece
	case raja
		for xn as integer = -1 to 1
			for yn as integer = -1 to 1
				if ok_move( clr, x+xn, y+yn ) then
					xarr( n ) = x+xn
					yarr( n ) = y+yn
					n += 1
				end if
			next
		next
		
	case elephant
		'' x +- 7, or y +- 7 (until bad move encountered)
		
		for xn as integer = 1 to 7 step 1
			if ok_move( clr, x+xn, y ) then
				xarr( n ) = x+xn
				yarr( n ) = y
				n += 1
				'' taking a piece? (last move)
				if board( x+xn, y ) <> 0 then exit for
			else
				exit for
			end if
		next
		
		for xn as integer = -1 to -7 step -1
			if ok_move( clr, x+xn, y ) then
				xarr( n ) = x+xn
				yarr( n ) = y
				n += 1
				if board( x+xn, y ) <> 0 then exit for
			else
				exit for
			end if
		next
		
		for yn as integer = 1 to 7 step 1
			if ok_move( clr, x, y+yn ) then
				xarr( n ) = x
				yarr( n ) = y+yn
				n += 1
				if board( x, y+yn ) <> 0 then exit for
			else
				exit for
			end if
		next
		
		for yn as integer = -1 to -7 step -1
			if ok_move( clr, x, y+yn ) then
				xarr( n ) = x
				yarr( n ) = y+yn
				n += 1
				if board( x, y+yn ) <> 0 then exit for
			else
				exit for
			end if
		next
		
	case horse
		'' x+-2, y+-1, or x+-1, y+-2
#macro h(b,c)
	if ok_move( clr, b, c ) then
		xarr( n ) = b
		yarr( n ) = c
		n += 1
	end if
#endmacro

		h(x+2,y+1)
		h(x+2,y-1)
		h(x-2,y+1)
		h(x-2,y-1)
		
		h(x+1,y+2)
		h(x+1,y-2)
		h(x-1,y+2)
		h(x-1,y-2)

#undef h
	
	case ship
		'' x+-2, y+-2
#macro s(b,c)
	if ok_move( clr, b, c ) then
		if ( board( b, c ) and &hFF ) <> raja then
			xarr( n ) = b
			yarr( n ) = c
			n += 1
		end if
	end if
#endmacro

	s(x+2,y+2)
	s(x+2,y-2)
	s(x-2,y+2)
	s(x-2,y-2)
	
#undef s
	
	case pawn
		'' red: y-1, x+-1 for capture
		'' black: x+1, y+-1 for capture
		'' yellow: y+1, x+-1 for capture
		'' green: x-1, y+-1 for capture
		dim new_x as integer = x, new_y as integer = y

#macro p(m,op,c)
	new_##m = m op 1
	if ok_move( clr, new_x, new_y ) then
		if board( new_x, new_y ) = 0 then
			xarr( n ) = new_x
			yarr( n ) = new_y
			n += 1
		end if
	end if
	
	new_##c = c + 1
	if ok_move( clr, new_x, new_y ) then
		if ( board( new_x, new_y ) and raja ) = 0 and board( new_x, new_y ) <> 0 then
			xarr( n ) = new_x
			yarr( n ) = new_y
			n += 1
		end if
	end if
	
	new_##c = c - 1
	if ok_move( clr, new_x, new_y ) then
		if ( board( new_x, new_y ) and raja ) = 0 and board( new_x, new_y ) <> 0 then
			xarr( n ) = new_x
			yarr( n ) = new_y
			n += 1
		end if
	end if
	
	new_x = x
	new_y = y
#endmacro

		select case as const clr
		case red
			p(y,-,x)
			
		case black
			p(x,+,y)
			
		case yellow
			p(y,+,x)
			
		case green
			p(x,-,y)
			
		end select

#undef p
	
	end select
	
	return n
end function

function spot_in_moves( x as integer, y as integer ) as integer
	for i as integer = 0 to nummoves - 1
		if xmove( i ) = x and ymove( i ) = y then return 1
	next
	return 0
end function

sub get_piece_back( x as integer, y as integer )
	dim clr as integer = board( x, y ) shr 8

	do
	screenlock
	''cls
	line ( 0, 0 ) - ( 800, 600 ), back_color, BF
	drawboard
	drawpieces
	drawdice
	
	draw string ( 560, 150 ), "Retrieve a lost piece?", text_color
	if haselephant( clr ) = 0 then
		draw string ( 560, 170 ), nm_elephant, text_color
	end if
	if hashorse( clr ) = 0 then
		draw string ( 560, 190 ), nm_horse, text_color
	end if
	if hasship( clr ) = 0 then
		draw string ( 560, 210 ), nm_ship, text_color
	end if
	draw string ( 560, 230 ), "None", text_color
	
	dim e as FB.EVENT
	dim as integer mx, my
	if screenevent( @e ) then
		getmouse mx, my
		if e.type = FB.EVENT_MOUSE_BUTTON_RELEASE then
			if mx >= 560 and mx <= 660 then
				if my >= 170 and my < 190 then
					if haselephant( clr ) = 0 then
						board( x, y ) = elephant or ( clr shl 8 )
						exit sub
					end if
				elseif my >= 190 and my < 210 then
					if hashorse( clr ) = 0 then
						board( x, y ) = horse or ( clr shl 8 )
						exit sub
					end if
				elseif my >= 210 and my < 230 then
					if hasship( clr ) = 0 then
						board( x, y ) = ship or ( clr shl 8 )
						exit sub
					end if
				elseif my >= 230 and my < 250 then
					exit sub
				end if
			end if
		end if
	end if
	
	screenunlock
	loop
end sub

sub resetboard
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			board( i, j ) = 0
		next
	next
	
	turn = 0

	'' black team
	board( 0, 0 ) = ship or ( black shl 8 )
	board( 0, 1 ) = horse or ( black shl 8 )
	board( 0, 2 ) = elephant or ( black shl 8 )
	board( 0, 3 ) = raja or ( black shl 8 )
	board( 1, 0 ) = pawn or ( black shl 8 )
	board( 1, 1 ) = pawn or ( black shl 8 )
	board( 1, 2 ) = pawn or ( black shl 8 )
	board( 1, 3 ) = pawn or ( black shl 8 )
	
	'' yellow team
	board( 4, 0 ) = raja or ( yellow shl 8 )
	board( 5, 0 ) = elephant or ( yellow shl 8 )
	board( 6, 0 ) = horse or ( yellow shl 8 )
	board( 7, 0 ) = ship or ( yellow shl 8 )
	board( 4, 1 ) = pawn or ( yellow shl 8 )
	board( 5, 1 ) = pawn or ( yellow shl 8 )
	board( 6, 1 ) = pawn or ( yellow shl 8 )
	board( 7, 1 ) = pawn or ( yellow shl 8 )
	
	'' green team
	board( 7, 4 ) = raja or ( green shl 8 )
	board( 7, 5 ) = elephant or ( green shl 8 )
	board( 7, 6 ) = horse or ( green shl 8 )
	board( 7, 7 ) = ship or ( green shl 8 )
	board( 6, 4 ) = pawn or ( green shl 8 )
	board( 6, 5 ) = pawn or ( green shl 8 )
	board( 6, 6 ) = pawn or ( green shl 8 )
	board( 6, 7 ) = pawn or ( green shl 8 )
	
	'' red team
	board( 0, 7 ) = ship or ( red shl 8 )
	board( 1, 7 ) = horse or ( red shl 8 )
	board( 2, 7 ) = elephant or ( red shl 8 )
	board( 3, 7 ) = raja or ( red shl 8 )
	board( 0, 6 ) = pawn or ( red shl 8 )
	board( 1, 6 ) = pawn or ( red shl 8 )
	board( 2, 6 ) = pawn or ( red shl 8 )
	board( 3, 6 ) = pawn or ( red shl 8 )
end sub

sub loadpieces
	raja_red = getpng( fn_raja & "1.png" )
	raja_black = getpng( fn_raja & "2.png" )
	raja_yellow = getpng( fn_raja & "3.png" )
	raja_green = getpng( fn_raja & "4.png" )
	elephant_red = getpng( fn_elephant & "1.png" )
	elephant_black = getpng( fn_elephant & "2.png" )
	elephant_yellow = getpng( fn_elephant & "3.png" )
	elephant_green = getpng( fn_elephant & "4.png" )
	horse_red = getpng( fn_horse & "1.png" )
	horse_black = getpng( fn_horse & "2.png" )
	horse_yellow = getpng( fn_horse & "3.png" )
	horse_green = getpng( fn_horse & "4.png" )
	ship_red = getpng( fn_ship & "1.png" )
	ship_black = getpng( fn_ship & "2.png" )
	ship_yellow = getpng( fn_ship & "3.png" )
	ship_green = getpng( fn_ship & "4.png" )
	pawn_red = getpng( fn_pawn & "1.png" )
	pawn_black = getpng( fn_pawn & "2.png" )
	pawn_yellow = getpng( fn_pawn & "3.png" )
	pawn_green = getpng( fn_pawn & "4.png" )
	throne = getpng( fn_throne & ".png" )
	flower = getpng( fn_flower & ".png" )
end sub

sub freepieces
	deallocate raja_red
	deallocate raja_black
	deallocate raja_yellow
	deallocate raja_green
	deallocate elephant_red
	deallocate elephant_black
	deallocate elephant_yellow
	deallocate elephant_green
	deallocate horse_red
	deallocate horse_black
	deallocate horse_yellow
	deallocate horse_green
	deallocate ship_red
	deallocate ship_black
	deallocate ship_yellow
	deallocate ship_green
	deallocate pawn_red
	deallocate pawn_black
	deallocate pawn_yellow
	deallocate pawn_green
	deallocate throne
	deallocate flower
end sub

sub drawboard
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_background, BF
			line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_border, B
		next
	next
	
	if draw_team_color <> 0 then
		line ( 5 + 1, 197 + 1 ) - ( 5 + 64 - 1, 197 + 64 - 1 ), clr_team2, BF
		line ( 261 + 1, 5 + 1 ) - ( 261 + 64 - 1, 5 + 64 - 1 ), clr_team3, BF
		line ( 197 + 1, 453 + 1 ) - ( 197 + 64 - 1, 453 + 64 - 1 ), clr_team1, BF
		line ( 453 + 1, 261 + 1 ) - ( 453 + 64 - 1, 261 + 64 - 1 ), clr_team4, BF
		
		if draw_team_color = 2 then
			line ( 5 + 1, 5 + 1 ) - ( 5 + 64 - 1, 5 + 64 - 1 ), clr_team2, BF
			line ( 5 + 1, 69 + 1 ) - ( 5 + 64 - 1, 69 + 64 - 1 ), clr_team2, BF
			line ( 5 + 1, 133 + 1 ) - ( 5 + 64 - 1, 133 + 64 - 1 ), clr_team2, BF
			
			line ( 453 + 1, 5 + 1 ) - ( 453 + 64 - 1, 5 + 64 - 1 ), clr_team3, BF
			line ( 325 + 1, 5 + 1 ) - ( 325 + 64 - 1, 5 + 64 - 1 ), clr_team3, BF
			line ( 389 + 1, 5 + 1 ) - ( 389 + 64 - 1, 5 + 64 - 1 ), clr_team3, BF
			
			line ( 5 + 1, 453 + 1 ) - ( 5 + 64 - 1, 453 + 64 - 1 ), clr_team1, BF
			line ( 69 + 1, 453 + 1 ) - ( 69 + 64 - 1, 453 + 64 - 1 ), clr_team1, BF
			line ( 133 + 1, 453 + 1 ) - ( 133 + 64 - 1, 453 + 64 - 1 ), clr_team1, BF
			
			line ( 453 + 1, 453 + 1 ) - ( 453 + 64 - 1, 453 + 64 - 1 ), clr_team4, BF
			line ( 453 + 1, 325 + 1 ) - ( 453 + 64 - 1, 325 + 64 - 1 ), clr_team4, BF
			line ( 453 + 1, 389 + 1 ) - ( 453 + 64 - 1, 389 + 64 - 1 ), clr_team4, BF
		end if
	end if
	
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			for n as integer = 0 to nummoves-1
				if xmove( n ) = i and ymove( n ) = j then
					line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_openspot, BF
					line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_okborder, B
				end if
			next
			if piecex = i and piecey = j then
				if nummoves > 0 then
					line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_okmoves, BF
				else
					line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_nomoves, BF
				end if
				line ( 5 + i * 64, 5 + j * 64 ) - ( 69 + i * 64, 69 + j * 64 ), clr_clickborder, B
			end if
		next
	next
	
	'' draw flowers and thrones
	put ( 5, 5 ), flower, alpha, &hFF00FF
	put ( 5, 69 ), flower, alpha, &hFF00FF
	put ( 5, 133 ), flower, alpha, &hFF00FF
	put ( 5, 197 ), throne, alpha, &hFF00FF
	
	put ( 261, 5 ), throne, alpha, &hFF00FF
	put ( 325, 5 ), flower, alpha, &hFF00FF
	put ( 389, 5 ), flower, alpha, &hFF00FF
	put ( 453, 5 ), flower, alpha, &hFF00FF
	
	put ( 5, 453 ), flower, alpha, &hFF00FF
	put ( 69, 453 ), flower, alpha, &hFF00FF
	put ( 133, 453 ), flower, alpha, &hFF00FF
	put ( 197, 453 ), throne, alpha, &hFF00FF
	
	put ( 453, 261 ), throne, alpha, &hFF00FF
	put ( 453, 325 ), flower, alpha, &hFF00FF
	put ( 453, 389 ), flower, alpha, &hFF00FF
	put ( 453, 453 ), flower, alpha, &hFF00FF
	
	draw string ( 550, 550 ), "Click to close:", text_color
	line ( 670, 545 ) - ( 680, 555 ), &hFFFFFF, BF
	line ( 670, 545 ) - ( 680, 555 ), 0, B
	
	draw string ( 560, 102 ), "Next Turn", text_color
	line ( 550, 100 ) - ( 640, 110 ), text_color, B
	
	draw string ( 565, 114 ), "New Game", text_color
	line ( 550, 112 ) - ( 640, 122 ), text_color, B
end sub

sub drawpieces
	for i as integer = 0 to 7
		for j as integer = 0 to 7
			
			select case board( i, j ) and &hFF
			case raja
				select case board( i, j ) shr 8
				case red
					put ( 5 + i * 64, 5 + j * 64 ), raja_red, alpha, &hFF00FF
				case black
					put ( 5 + i * 64, 5 + j * 64 ), raja_black, alpha, &hFF00FF
				case yellow
					put ( 5 + i * 64, 5 + j * 64 ), raja_yellow, alpha, &hFF00FF
				case green
					put ( 5 + i * 64, 5 + j * 64 ), raja_green, alpha, &hFF00FF
				end select
			
			case elephant
				select case board( i, j ) shr 8
				case red
					put ( 5 + i * 64, 5 + j * 64 ), elephant_red, alpha, &hFF00FF
				case black
					put ( 5 + i * 64, 5 + j * 64 ), elephant_black, alpha, &hFF00FF
				case yellow
					put ( 5 + i * 64, 5 + j * 64 ), elephant_yellow, alpha, &hFF00FF
				case green
					put ( 5 + i * 64, 5 + j * 64 ), elephant_green, alpha, &hFF00FF
				end select
			
			case horse
				select case board( i, j ) shr 8
				case red
					put ( 5 + i * 64, 5 + j * 64 ), horse_red, alpha, &hFF00FF
				case black
					put ( 5 + i * 64, 5 + j * 64 ), horse_black, alpha, &hFF00FF
				case yellow
					put ( 5 + i * 64, 5 + j * 64 ), horse_yellow, alpha, &hFF00FF
				case green
					put ( 5 + i * 64, 5 + j * 64 ), horse_green, alpha, &hFF00FF
				end select
			
			case ship
				select case board( i, j ) shr 8
				case red
					put ( 5 + i * 64, 5 + j * 64 ), ship_red, alpha, &hFF00FF
				case black
					put ( 5 + i * 64, 5 + j * 64 ), ship_black, alpha, &hFF00FF
				case yellow
					put ( 5 + i * 64, 5 + j * 64 ), ship_yellow, alpha, &hFF00FF
				case green
					put ( 5 + i * 64, 5 + j * 64 ), ship_green, alpha, &hFF00FF
				end select
			
			case pawn
				select case board( i, j ) shr 8
				case red
					put ( 5 + i * 64, 5 + j * 64 ), pawn_red, alpha, &hFF00FF
				case black
					put ( 5 + i * 64, 5 + j * 64 ), pawn_black, alpha, &hFF00FF
				case yellow
					put ( 5 + i * 64, 5 + j * 64 ), pawn_yellow, alpha, &hFF00FF
				case green
					put ( 5 + i * 64, 5 + j * 64 ), pawn_green, alpha, &hFF00FF
				end select
			
			end select
		next
	next
end sub

sub drawdice
	dim teamstr as string
	dim clr as uinteger
	if turn = 0 then
		teamstr = nm_team1
		clr = clr_team1''rgb( 255, 0, 0 )
	elseif turn = 1 then
		teamstr = nm_team2
		clr = clr_team2''rgb( 255, 255, 255 )
	elseif turn = 2 then
		teamstr = nm_team3
		clr = clr_team3''rgb( 255, 255, 0 )
	elseif turn = 3 then
		teamstr = nm_team4
		clr = clr_team4''rgb( 0, 255, 0 )
	end if
	
	dim dice1str as string, dice2str as string
	if dice1 = 0 then
		dice1str = nm_rajapawn''"Nam Vet/Army"
	elseif dice1 = 1 then
		dice1str = nm_elephant''"Nam Elephant"
	elseif dice1 = 2 then
		dice1str = nm_horse''"Nam Charger"
	elseif dice1 = 3 then
		dice1str = nm_ship''"Nam Cruiser"
	elseif dice1 = 4 then
		dice1str = nm_pawn''"Nam Army"
	end if
	
	if dice2 = 0 then
		dice2str = nm_rajapawn''"Nam Vet/Army"
	elseif dice2 = 1 then
		dice2str = nm_elephant''"Nam Elephant"
	elseif dice2 = 2 then
		dice2str = nm_horse''"Nam Charger"
	elseif dice2 = 3 then
		dice2str = nm_ship''"Nam Cruiser"
	elseif dice2 = 4 then
		dice2str = nm_pawn''"Nam Army"
	end if
	
	draw string ( 520, 5 ), "Turn: " & teamstr, clr
	draw string ( 520, 25 ), "Dice 1: " & dice1str, iif( usedroll1, dice_bad_color, dice_good_color )
	draw string ( 520, 37 ), "Dice 2: " & dice2str, iif( usedroll2, dice_bad_color, dice_good_color )
end sub

sub new_game
	dim answer as string
	
	screenunlock
	
	cls
	
	print
	print ,"Are you sure you want to start a new game? (y/n)"
	input answer
	
	if answer = "y" then
		resetboard
		roll_dice
	end if
	
	screenlock
end sub

sub wannabeclown(go as integer)
	dim as fb.image ptr clown = getpng( "required_files.txt" )
	
	if clown = 0 then end

	do
		screenlock
		cls
		put ( 50, 50 ), clown, pset
		screenunlock
	loop while inkey = ""
	
	deallocate clown
	if go = 0 then end
end sub
