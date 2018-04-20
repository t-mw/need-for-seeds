local gamestate = {}
if love then
   gamestate = require "lib.hump.gamestate"
end
return {
  ["current"] = gamestate.current,
  ["pop"] = gamestate.pop,
  ["push"] = gamestate.push,
  ["register-events"] = gamestate.registerEvents,
  ["switch"] = gamestate.switch,
}
