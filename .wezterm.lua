-- WezTerm config (cross-platform: Windows + Linux)
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

local is_win = wezterm.target_triple:find('windows') ~= nil

-- Shell for new tabs.
-- Windows needs an explicit path: bare 'bash' resolves to WSL's bash.exe in System32.
local shell = is_win
  and { 'C:\\Program Files\\Git\\bin\\bash.exe', '-l' }
  or { 'zsh', '-l' }

-- Shell used to run a command inside a new tab.
local run_shell = is_win
  and { 'C:\\Program Files\\Git\\bin\\bash.exe', '-lc' }
  or { 'bash', '-lc' }

config.default_prog = shell

-- Windows: the system HOME can be misconfigured, force it to USERPROFILE
if is_win then
  config.set_environment_variables = { HOME = os.getenv('USERPROFILE') }
end

-- Font (Nerd Font + CJK, required by p10k)
config.font = wezterm.font('Maple Mono NF CN')
config.font_size = 18.0

-- Theme
config.color_scheme = 'Catppuccin Mocha'
config.window_decorations = 'TITLE | RESIZE'
config.hide_tab_bar_if_only_one_tab = true

-- Keys
config.keys = {
  -- move tab order (Ctrl+Alt+arrows is taken by GNOME workspace switching)
  { key = 'PageUp', mods = 'CTRL|ALT', action = wezterm.action.MoveTabRelative(-1) },
  { key = 'PageDown', mods = 'CTRL|ALT', action = wezterm.action.MoveTabRelative(1) },

  -- pass Alt+Enter to terminal (codex/claude newline)
  { key = 'Enter', mods = 'ALT', action = wezterm.action.SendKey { key = 'Enter', mods = 'ALT' } },

  -- fullscreen
  { key = 'F11', action = wezterm.action.ToggleFullScreen },
}

-- Run a command in a new tab
local function run(cmd)
  return wezterm.action.SpawnCommandInNewTab {
    args = { run_shell[1], run_shell[2], cmd },
  }
end

-- Command palette entries
wezterm.on('augment-command-palette', function(window, pane)
  local entries = {
    {
      brief = 'SSH to host',
      icon = 'md_login',
      action = wezterm.action.PromptInputLine {
        description = 'SSH to (user@host [-p port]):',
        action = wezterm.action_callback(function(window, pane, line)
          if line and line ~= '' then
            window:perform_action(run('ssh ' .. line), pane)
          end
        end),
      },
    },
  }

  -- the work machine is only reachable from the office network
  if is_win then
    table.insert(entries, 1, {
      brief = 'Connect to 10.22.1.62',
      icon = 'md_server',
      action = run('ssh 10.22.1.62'),
    })
  end

  return entries
end)

return config
