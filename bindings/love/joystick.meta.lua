if not love then love = {} end
if not love.joystick then love.joystick = {} end
return {
	["save-gamepad-mappings"] = { tag = "var", contents = "love.joystick.saveGamepadMappings", value = love.joystick.saveGamepadMappings},
	["set-gamepad-mapping"] = { tag = "var", contents = "love.joystick.setGamepadMapping", value = love.joystick.setGamepadMapping},
	["load-gamepad-mappings"] = { tag = "var", contents = "love.joystick.loadGamepadMappings", value = love.joystick.loadGamepadMappings},
	["get-gamepad-mapping"] = { tag = "var", contents = "love.joystick.getGamepadMapping", value = love.joystick.getGamepadMapping},
	["get-joysticks"] = { tag = "var", contents = "love.joystick.getJoysticks", value = love.joystick.getJoysticks},
	["get-joystick-count"] = { tag = "var", contents = "love.joystick.getJoystickCount", value = love.joystick.getJoystickCount},
}