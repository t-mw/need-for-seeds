if not love then love = {} end
if not love.system then love.system = {} end
return {
	["set-clipboard-text"] = { tag = "var", contents = "love.system.setClipboardText", value = love.system.setClipboardText},
	["vibrate"] = { tag = "var", contents = "love.system.vibrate", value = love.system.vibrate},
	["open-url"] = { tag = "var", contents = "love.system.openURL", value = love.system.openURL},
	["get-power-info"] = { tag = "var", contents = "love.system.getPowerInfo", value = love.system.getPowerInfo},
	["get-clipboard-text"] = { tag = "var", contents = "love.system.getClipboardText", value = love.system.getClipboardText},
	["get-os"] = { tag = "var", contents = "love.system.getOS", value = love.system.getOS},
	["get-processor-count"] = { tag = "var", contents = "love.system.getProcessorCount", value = love.system.getProcessorCount},
}