if not love then love = {} end
if not love.keyboard then love.keyboard = {} end
return {
	["set-text-input"] = { tag = "var", contents = "love.keyboard.setTextInput", value = love.keyboard.setTextInput},
	["has-text-input"] = { tag = "var", contents = "love.keyboard.hasTextInput", value = love.keyboard.hasTextInput},
	["get-scancode-from-key"] = { tag = "var", contents = "love.keyboard.getScancodeFromKey", value = love.keyboard.getScancodeFromKey},
	["has-screen-keyboard"] = { tag = "var", contents = "love.keyboard.hasScreenKeyboard", value = love.keyboard.hasScreenKeyboard},
	["set-key-repeat"] = { tag = "var", contents = "love.keyboard.setKeyRepeat", value = love.keyboard.setKeyRepeat},
	["get-key-from-scancode"] = { tag = "var", contents = "love.keyboard.getKeyFromScancode", value = love.keyboard.getKeyFromScancode},
	["is-down"] = { tag = "var", contents = "love.keyboard.isDown", value = love.keyboard.isDown},
	["has-key-repeat"] = { tag = "var", contents = "love.keyboard.hasKeyRepeat", value = love.keyboard.hasKeyRepeat},
	["is-scancode-down"] = { tag = "var", contents = "love.keyboard.isScancodeDown", value = love.keyboard.isScancodeDown},
}