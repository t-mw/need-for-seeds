if not love then love = {} end
if not love.mouse then love.mouse = {} end
return {
	["set-x"] = { tag = "var", contents = "love.mouse.setX", value = love.mouse.setX},
	["get-system-cursor"] = { tag = "var", contents = "love.mouse.getSystemCursor", value = love.mouse.getSystemCursor},
	["is-grabbed"] = { tag = "var", contents = "love.mouse.isGrabbed", value = love.mouse.isGrabbed},
	["is-visible"] = { tag = "var", contents = "love.mouse.isVisible", value = love.mouse.isVisible},
	["get-cursor"] = { tag = "var", contents = "love.mouse.getCursor", value = love.mouse.getCursor},
	["set-visible"] = { tag = "var", contents = "love.mouse.setVisible", value = love.mouse.setVisible},
	["get-relative-mode"] = { tag = "var", contents = "love.mouse.getRelativeMode", value = love.mouse.getRelativeMode},
	["set-grabbed"] = { tag = "var", contents = "love.mouse.setGrabbed", value = love.mouse.setGrabbed},
	["set-cursor"] = { tag = "var", contents = "love.mouse.setCursor", value = love.mouse.setCursor},
	["get-position"] = { tag = "var", contents = "love.mouse.getPosition", value = love.mouse.getPosition},
	["has-cursor"] = { tag = "var", contents = "love.mouse.hasCursor", value = love.mouse.hasCursor},
	["set-y"] = { tag = "var", contents = "love.mouse.setY", value = love.mouse.setY},
	["is-down"] = { tag = "var", contents = "love.mouse.isDown", value = love.mouse.isDown},
	["set-position"] = { tag = "var", contents = "love.mouse.setPosition", value = love.mouse.setPosition},
	["get-x"] = { tag = "var", contents = "love.mouse.getX", value = love.mouse.getX},
	["get-y"] = { tag = "var", contents = "love.mouse.getY", value = love.mouse.getY},
	["new-cursor"] = { tag = "var", contents = "love.mouse.newCursor", value = love.mouse.newCursor},
	["set-relative-mode"] = { tag = "var", contents = "love.mouse.setRelativeMode", value = love.mouse.setRelativeMode},
}