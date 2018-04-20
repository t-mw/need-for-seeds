if not love then love = {} end
if not love.thread then love.thread = {} end
return {
	["get-channel"] = { tag = "var", contents = "love.thread.getChannel", value = love.thread.getChannel},
	["new-thread"] = { tag = "var", contents = "love.thread.newThread", value = love.thread.newThread},
	["new-channel"] = { tag = "var", contents = "love.thread.newChannel", value = love.thread.newChannel},
}