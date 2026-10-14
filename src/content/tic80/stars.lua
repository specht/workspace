-- title:   Star Catcher
-- author:  your name
-- desc:    Catch the falling stars
-- script:  lua

function newGame()
	x = 112
	score = 0
	lives = 3
	speed = 1
	stars = {}
	for i = 1, 3 do
		newStar(i)
	end
end

function newStar(i)
	stars[i] = { x = math.random(0, 232), y = -math.random(0, 120) }
end

newGame()

function TIC()
	if lives > 0 then
		if btn(2) and x > 0 then x = x - 2 end
		if btn(3) and x < 224 then x = x + 2 end

		for i, s in ipairs(stars) do
			s.y = s.y + speed
			if s.y > 104 and s.y < 128 and s.x + 8 > x and s.x < x + 16 then
				score = score + 1
				speed = speed + 0.05
				sfx(0, "C-6", 8)
				newStar(i)
			elseif s.y > 136 then
				lives = lives - 1
				newStar(i)
			end
		end
	elseif btnp(4) then
		newGame()
	end

	cls(0)
	for i, s in ipairs(stars) do
		circ(s.x + 4, s.y + 4, 3, 4)
	end
	spr(1, x, 112, 14, 1, 0, 0, 2, 2)
	print("Score: " .. score, 4, 4, 12)
	print("Lives: " .. lives, 196, 4, 12)

	if lives <= 0 then
		print("GAME OVER", 94, 56, 2)
		print("Z: play again", 82, 68, 12)
	end
end

-- <TILES>
-- 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
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

