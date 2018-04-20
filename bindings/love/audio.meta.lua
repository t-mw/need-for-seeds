if not love then love = {} end
if not love.audio then love.audio = {} end
return {
	["stop"] = { tag = "var", contents = "love.audio.stop", value = love.audio.stop},
	["set-velocity"] = { tag = "var", contents = "love.audio.setVelocity", value = love.audio.setVelocity},
	["set-distance-model"] = { tag = "var", contents = "love.audio.setDistanceModel", value = love.audio.setDistanceModel},
	["resume"] = { tag = "var", contents = "love.audio.resume", value = love.audio.resume},
	["get-doppler-scale"] = { tag = "var", contents = "love.audio.getDopplerScale", value = love.audio.getDopplerScale},
	["play"] = { tag = "var", contents = "love.audio.play", value = love.audio.play},
	["set-orientation"] = { tag = "var", contents = "love.audio.setOrientation", value = love.audio.setOrientation},
	["new-source"] = { tag = "var", contents = "love.audio.newSource", value = love.audio.newSource},
	["get-source-count"] = { tag = "var", contents = "love.audio.getSourceCount", value = love.audio.getSourceCount},
	["get-distance-model"] = { tag = "var", contents = "love.audio.getDistanceModel", value = love.audio.getDistanceModel},
	["get-velocity"] = { tag = "var", contents = "love.audio.getVelocity", value = love.audio.getVelocity},
	["get-position"] = { tag = "var", contents = "love.audio.getPosition", value = love.audio.getPosition},
	["rewind"] = { tag = "var", contents = "love.audio.rewind", value = love.audio.rewind},
	["get-orientation"] = { tag = "var", contents = "love.audio.getOrientation", value = love.audio.getOrientation},
	["set-position"] = { tag = "var", contents = "love.audio.setPosition", value = love.audio.setPosition},
	["pause"] = { tag = "var", contents = "love.audio.pause", value = love.audio.pause},
	["set-volume"] = { tag = "var", contents = "love.audio.setVolume", value = love.audio.setVolume},
	["get-volume"] = { tag = "var", contents = "love.audio.getVolume", value = love.audio.getVolume},
	["set-doppler-scale"] = { tag = "var", contents = "love.audio.setDopplerScale", value = love.audio.setDopplerScale},
}