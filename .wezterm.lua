-- WezTerm config (cross-platform: Windows + Linux)
local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

local is_win = wezterm.target_triple:find('windows') ~= nil
local git_bash = 'C:\\Program Files\\Git\\bin\\bash.exe'

-- Shell for new tabs.
-- Windows needs an explicit path: bare 'bash' resolves to WSL's bash.exe in System32.
config.default_prog = is_win
  and { git_bash, '-l' }
  or { 'zsh', '-l' }

-- Windows: the system HOME can be misconfigured, force it to USERPROFILE
if is_win then
  config.set_environment_variables = { HOME = os.getenv('USERPROFILE') }
end

-- Font (Nerd Font + CJK, required by p10k)
config.font = wezterm.font('Maple Mono NF CN')
config.font_size = 18.0
config.line_height = 1.05

-- Theme
config.color_scheme = 'Catppuccin Mocha'
config.hide_tab_bar_if_only_one_tab = true

config.scrollback_lines = 20000
config.exit_behavior = 'CloseOnCleanExit'
config.exit_behavior_messaging = 'Terse'

-- 85% opacity by default; Ctrl+Shift+B toggles transparency for the current window.
config.window_background_opacity = 0.85

-- Use plain transparency: Acrylic was unstable in testing on this Windows machine.
if is_win then
  config.win32_system_backdrop = 'Disable'
end

config.default_cursor_style = 'BlinkingBlock'
config.cursor_blink_rate = 700
config.cursor_blink_ease_in = 'EaseInOut'
config.cursor_blink_ease_out = 'EaseInOut'

config.quick_select_patterns = {
  '[0-9a-fA-F]{2}(?::[0-9a-fA-F]{2}){5}',
  'oid:0x[0-9a-fA-F]+',
  -- Long options, excluding any '=value' suffix.
  [[(?<![\w-])--\w+(?:-\w+)*(?![\w-])]],
}

local toggle_transparency = wezterm.action_callback(function(window)
  local overrides = window:get_config_overrides() or {}
  local opacity = window:effective_config().window_background_opacity
  overrides.window_background_opacity = opacity < 1 and 1.0 or 0.85
  window:set_config_overrides(overrides)
end)

-- Keys
config.keys = {
  -- move tab order (Ctrl+Alt+arrows is taken by GNOME workspace switching)
  { key = 'PageUp', mods = 'CTRL|ALT', action = act.MoveTabRelative(-1) },
  { key = 'PageDown', mods = 'CTRL|ALT', action = act.MoveTabRelative(1) },

  -- pass Alt+Enter to terminal (codex/claude newline)
  { key = 'Enter', mods = 'ALT', action = act.DisableDefaultAssignment },

  -- fullscreen
  { key = 'F11', action = act.ToggleFullScreen },

  -- tabs and SSH hosts; Ctrl+Shift+P remains the command palette
  {
    key = 'o',
    mods = 'CTRL|SHIFT',
    action = act.ShowLauncherArgs {
      flags = 'FUZZY|TABS|LAUNCH_MENU_ITEMS',
      title = 'Launcher',
    },
  },

  -- toggle between opaque and 85% opacity without changing other overrides
  {
    key = 'b',
    mods = 'CTRL|SHIFT',
    action = toggle_transparency,
  },
}

local ssh_start = [[
printf 'Connecting: ssh'
printf ' %s' "$@"
printf '\nWaiting for SSH. Ctrl+C cancels the connection attempt.\n\n'
exec ssh "$@"
]]

local function ssh_command(ssh_args)
  -- Keep Git Bash's login environment; never interpolate input into shell code.
  local args = is_win
    and { git_bash, '-lc', ssh_start, 'wezterm-ssh' }
    or { '/bin/sh', '-c', ssh_start, 'wezterm-ssh' }
  for _, arg in ipairs(ssh_args) do
    table.insert(args, arg)
  end
  return {
    args = args,
    domain = { DomainName = 'local' },
  }
end

-- SSH hosts from the standard user configuration.
local ssh_home = is_win and os.getenv('USERPROFILE') or wezterm.home_dir
local hosts = {}
for host in pairs(wezterm.enumerate_ssh_hosts(ssh_home .. '/.ssh/config')) do
  table.insert(hosts, host)
end
table.sort(hosts)

config.launch_menu = {}
for _, host in ipairs(hosts) do
  local command = ssh_command { host }
  command.label = 'SSH: ' .. host
  table.insert(config.launch_menu, command)
end

-- Command palette entries
local last_ssh_input = ''
wezterm.on('augment-command-palette', function()
  return {
    {
      brief = 'Toggle transparency',
      icon = 'md_opacity',
      action = toggle_transparency,
    },
    {
      brief = 'SSH to host',
      icon = 'md_login',
      action = act.PromptInputLine {
        description = 'SSH to (user@host [-p port]):',
        initial_value = last_ssh_input,
        action = wezterm.action_callback(function(window, pane, line)
          if not line or line:match('^%s*$') then
            return
          end
          local ok, args = pcall(wezterm.shell_split, line)
          if not ok then
            window:toast_notification('SSH', 'Invalid quoting in SSH arguments.', nil, 4000)
            return
          end
          if #args > 0 then
            last_ssh_input = line
            window:perform_action(act.SpawnCommandInNewTab(ssh_command(args)), pane)
          end
        end),
      },
    },
  }
end)

return config
