local model_viewer = {}
if love then
   model_viewer = require "lib.sprit3r.model_viewer"
end
return {
  ["*model-viewer*"] = function()
  	model_viewer.hideUI = true
  	model_viewer.voxelMode = true
  	return model_viewer 
  end,
}
