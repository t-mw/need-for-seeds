if not love then love = {} end
if not love.timer then love.timer = {} end
return {
	["step"] = { tag = "var", contents = "love.timer.step", value = love.timer.step},
	["get-delta"] = { tag = "var", contents = "love.timer.getDelta", value = love.timer.getDelta},
	["get-time"] = { tag = "var", contents = "love.timer.getTime", value = love.timer.getTime},
	["get-average-delta"] = { tag = "var", contents = "love.timer.getAverageDelta", value = love.timer.getAverageDelta},
	["sleep"] = { tag = "var", contents = "love.timer.sleep", value = love.timer.sleep},
	["get-fps"] = { tag = "var", contents = "love.timer.getFPS", value = love.timer.getFPS},
}