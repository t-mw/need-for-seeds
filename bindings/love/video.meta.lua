if not love then love = {} end
if not love.video then love.video = {} end
return {
	["new-video-stream"] = { tag = "var", contents = "love.video.newVideoStream", value = love.video.newVideoStream},
}