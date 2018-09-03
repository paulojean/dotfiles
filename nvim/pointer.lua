local pointer = require("pointer")

-- This configures github for all projects
pointer.config{
  url = pointer.formatters.url.github,
  nu = {
    url = pointer.formatters.url.github
  },
  proj = {
    url = pointer.formatters.url.github
  }
}

-- I'll explain mapping later. leave this to bind to default keybindings
pointer.map{}
