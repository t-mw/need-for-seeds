if not love then love = {} end
if not love.event then love.event = {} end
return {
	["pump"] = { tag = "var", contents = "love.event.pump", value = love.event.pump},
	["poll"] = { tag = "var", contents = "love.event.poll", value = love.event.poll},
	["push"] = { tag = "var", contents = "love.event.push", value = love.event.push},
	["wait"] = { tag = "var", contents = "love.event.wait", value = love.event.wait},
	["clear"] = { tag = "var", contents = "love.event.clear", value = love.event.clear},
	["quit"] = { tag = "var", contents = "love.event.quit", value = love.event.quit},
}