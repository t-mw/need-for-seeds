if not love then love = {} end
if not love.sound then love.sound = {} end
return {
	["new-sound-data"] = { tag = "var", contents = "love.sound.newSoundData", value = love.sound.newSoundData},
	["new-decoder"] = { tag = "var", contents = "love.sound.newDecoder", value = love.sound.newDecoder},
}