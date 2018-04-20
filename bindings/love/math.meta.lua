if not love then love = {} end
if not love.math then love.math = {} end
return {
	["random-normal"] = { tag = "var", contents = "love.math.randomNormal", value = love.math.randomNormal},
	["triangulate"] = { tag = "var", contents = "love.math.triangulate", value = love.math.triangulate},
	["noise"] = { tag = "var", contents = "love.math.noise", value = love.math.noise},
	["get-random-seed"] = { tag = "var", contents = "love.math.getRandomSeed", value = love.math.getRandomSeed},
	["set-random-state"] = { tag = "var", contents = "love.math.setRandomState", value = love.math.setRandomState},
	["new-random-generator"] = { tag = "var", contents = "love.math.newRandomGenerator", value = love.math.newRandomGenerator},
	["is-convex"] = { tag = "var", contents = "love.math.isConvex", value = love.math.isConvex},
	["gamma-to-linear"] = { tag = "var", contents = "love.math.gammaToLinear", value = love.math.gammaToLinear},
	["linear-to-gamma"] = { tag = "var", contents = "love.math.linearToGamma", value = love.math.linearToGamma},
	["set-random-seed"] = { tag = "var", contents = "love.math.setRandomSeed", value = love.math.setRandomSeed},
	["compress"] = { tag = "var", contents = "love.math.compress", value = love.math.compress},
	["get-random-state"] = { tag = "var", contents = "love.math.getRandomState", value = love.math.getRandomState},
	["random"] = { tag = "var", contents = "love.math.random", value = love.math.random},
	["decompress"] = { tag = "var", contents = "love.math.decompress", value = love.math.decompress},
	["new-bezier-curve"] = { tag = "var", contents = "love.math.newBezierCurve", value = love.math.newBezierCurve},
}