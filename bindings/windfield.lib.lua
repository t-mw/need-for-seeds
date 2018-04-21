local windfield = {}
if love and love.math then
  windfield = require "lib.windfield"
end
return {
  ["new-world"] = windfield.newWorld,
}