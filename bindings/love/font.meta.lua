if not love then love = {} end
if not love.font then love.font = {} end
return {
	["new-image-rasterizer"] = { tag = "var", contents = "love.font.newImageRasterizer", value = love.font.newImageRasterizer},
	["new-glyph-data"] = { tag = "var", contents = "love.font.newGlyphData", value = love.font.newGlyphData},
	["new-true-type-rasterizer"] = { tag = "var", contents = "love.font.newTrueTypeRasterizer", value = love.font.newTrueTypeRasterizer},
	["new-bmfont-rasterizer"] = { tag = "var", contents = "love.font.newBMFontRasterizer", value = love.font.newBMFontRasterizer},
	["new-rasterizer"] = { tag = "var", contents = "love.font.newRasterizer", value = love.font.newRasterizer},
}