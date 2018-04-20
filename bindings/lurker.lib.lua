local lurker = {}
if love and love.filesystem then
  lurker = require "lib.lurker"
end
return {
  ["*lurker*"] = lurker,
  update = lurker.update
}
