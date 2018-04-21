local camera = {}
camera.smooth = {}
if love then
   camera = require "lib.hump.camera"
end
return {
  ["new"] = camera.new,
  ["smooth-none"] = camera.smooth.none,
  ["smooth-linear"] = camera.smooth.linear,
  ["smooth-damped"] = camera.smooth.damped,
}
