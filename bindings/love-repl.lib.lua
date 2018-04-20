local repl = require "lib.love-repl"
return {
  ["*love-repl*"] = repl,
  ["expose"] = function(name, var) _G[name] = var end,
  ["initialize"] = repl.initialize,
  ["toggle"] = repl.toggle,
  ["toggled"] = repl.toggled,
  ["eval"] = repl.eval,
  ["print"] = repl.print,
  ["keypressed"] = repl.keypressed,
  ["wheelmoved"] = repl.wheelmoved,
  ["textinput"] = repl.textinput,
  ["draw"] = repl.draw,
}