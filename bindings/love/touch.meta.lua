if not love then love = {} end
if not love.touch then love.touch = {} end
return {
	["get-pressure"] = { tag = "var", contents = "love.touch.getPressure", value = love.touch.getPressure},
	["get-touches"] = { tag = "var", contents = "love.touch.getTouches", value = love.touch.getTouches},
	["get-position"] = { tag = "var", contents = "love.touch.getPosition", value = love.touch.getPosition},
}