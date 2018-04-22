local model_viewer = {}
if love then
   model_viewer = require "lib.sprit3r.model_viewer"
end
return {
  ["new"] = model_viewer.new,
}
