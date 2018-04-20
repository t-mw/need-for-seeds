if not love then love = {} end
if not love.image then love.image = {} end
return {
	["is-compressed"] = { tag = "var", contents = "love.image.isCompressed", value = love.image.isCompressed},
	["new-image-data"] = { tag = "var", contents = "love.image.newImageData", value = love.image.newImageData},
	["new-compressed-data"] = { tag = "var", contents = "love.image.newCompressedData", value = love.image.newCompressedData},
}