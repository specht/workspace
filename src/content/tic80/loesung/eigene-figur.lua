-- title:   Star Catcher
-- author:  your name
-- desc:    Catch the falling stars
-- script:  lua

x = 112
sx = 120
sy = 0
score = 0

function TIC()
	if btn(2) then x = x - 2 end
	if btn(3) then x = x + 2 end

	sy = sy + 1
	if sy > 136 then
		sy = 0
		sx = math.random(0, 232)
	end

	if sy > 104 and sy < 128 and sx + 8 > x and sx < x + 16 then
		score = score + 1
		sy = 0
		sx = math.random(0, 232)
	end

	cls(0)
	circ(sx + 4, sy + 4, 3, 4)
	spr(1, x, 112, 14, 1, 0, 0, 2, 2)
	print("Score: " .. score, 4, 4, 12)
end

-- <TILES>
-- 001:000cc00000cccc0000c44c0000cccc000cccccc0cc0cc0ccc00cc00c00033000
-- 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
-- 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
-- 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
-- 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- </TILES>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
-- </SFX>

-- <TRACKS>
-- 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </TRACKS>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

