# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os
import subprocess

from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Screen
from libqtile.config import EzKey as Key
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = guess_terminal()

keys = [
    # System stuff
    Key("M-l",                    lazy.spawn("dm-tool lock"),                         desc="Lock workstation"),
    Key("M-w",                    lazy.window.kill(),                                 desc="Kill focused window"),
    Key("M-C-r",                  lazy.restart(),                                     desc="Restart qtile"),
    Key("M-C-q",                  lazy.shutdown(),                                    desc="Shutdown qtile"),
    Key("M-C-b",                  lazy.spawn("sudo shutdown -r now"),                 desc="Reboot"),
    Key("M-C-h",                  lazy.spawn("sudo shutdown now"),                    desc="Halt"),

    # Layout selection 
    Key("M-<space>",              lazy.next_layout(),                                 desc="Toggle between layouts"),
    # TODO: Rofi menu, JumpToLayout x

    # Layout manipulation
    Key("M-S-<Up>",               lazy.layout.shuffle_up(),                           desc="Move window up in current stack "),
    Key("M-S-<Down>",             lazy.layout.shuffle_down(),                         desc="Move window down in current stack "),
    Key("M-S-<Left>",             lazy.layout.shuffle_left(),                         desc="Move window up in current stack "),
    Key("M-S-<Right>",            lazy.layout.shuffle_right(),                        desc="Move window down in current stack "),
    Key("M-<Tab>",                lazy.layout.flip(),                                 desc="Switch window focus to other pane(s) of stack"),
    Key("M-<KP_Add>",             lazy.layout.grow(),                                 desc="Expand window"),
    Key("M-<KP_Subtract>",        lazy.layout.shrink(),                               desc="Shrink window"),
    Key("M-<End>",                lazy.window.toggle_floating(),                      desc="Toggles floating"),
    # TODO: promote (swap with master)

    # Layout navigation
    Key("M-A-<Left>",             lazy.to_screen(0),                                  desc="Move focus to screen 0"),
    Key("M-A-<Right>",            lazy.to_screen(1),                                  desc="Move focus to screen 1"),
    Key("M-<Up>",                 lazy.layout.up(),                                   desc="Move focus up in stack pane"),
    Key("M-<Down>",               lazy.layout.down(),                                 desc="Move focus down in stack pane"),
    Key("M-<Left>",               lazy.layout.left(),                                 desc="Move focus down in stack pane"),
    Key("M-<Right>",              lazy.layout.right(),                                desc="Move focus up in stack pane"),
    # TODO: Move between screens with M-<Left> and M-<Right>, when appropriate

    # Applications
    Key("M-p",                    lazy.spawn("rofi -show drun -show-icons"),          desc="Desktop Apps"),
    Key("M-S-p",                  lazy.spawn("rofi -show run"),                       desc="Prompt"),
    Key("M-A-p",                  lazy.spawn("rofi -show ssh"),                       desc="SSH Menu"),
    Key("M-C-p",                  lazy.spawn("Scripts/pass-qr.sh"),                   desc="pass and QR-code"),
    Key("M-n",                    lazy.spawn("Scripts/notifications-panel.sh"),       desc="Notification Panel"),
    Key("M-<Return>",             lazy.spawn(terminal),                               desc="Launch terminal"),
    Key("M-1",                    lazy.spawn(terminal),                               desc="Launch terminal"),
    Key("M-2",                    lazy.spawn("nautilus"),                             desc="Launch Nautilus"),
    Key("M-3",                    lazy.spawn("firefox"),                              desc="Launch Firefox"),
    Key("M-4",                    lazy.spawn("steam"),                                desc="Launch Steam"),

    # Misc
    # TODO: This line makes it so all the keys are ignored everywhere except if I hold MOD... The fuck?
    # Key("<XF86ModeLock>", lazy.spawn("dm-tool lock"), desc="Lock workstation"),
    Key("<XF86HomePage>",         lazy.spawn("nautilus"),                             desc="Launch Nautilus"),
    Key("<XF86Calculator>",       lazy.spawn("gnome-calculator"),                     desc="Launch Gnome Calculator"),
    Key("<Print>",                lazy.spawn("gnome-screenshot --interactive"),       desc="Take screenshot"),

    # Media
    Key("<XF86AudioMute>",        lazy.spawn("amixer -D pulse set Master 1+ toggle"), desc="Mute/Unmute"),
    Key("<XF86AudioLowerVolume>", lazy.spawn("amixer set Master 5%- unmute"),         desc="Volume Down"),
    Key("<XF86AudioRaiseVolume>", lazy.spawn("amixer set Master 5%+ unmute"),         desc="Volume Up"),
    Key("<XF86AudioPlay>",        lazy.spawn("playerctl play-pause"),                 desc="Play/Pause"),
    Key("<XF86AudioPrev>",        lazy.spawn("playerctl previous"),                   desc="Prev"),
    Key("<XF86AudioNext>",        lazy.spawn("playerctl next"),                       desc="Next"),

    # Switch focus to workspace
    Key("M-<F1>",                 lazy.group["1"].toscreen(),                         desc="Switch to workspace #1"),
    Key("M-<F2>",                 lazy.group["2"].toscreen(),                         desc="Switch to workspace #2"),
    Key("M-<F3>",                 lazy.group["3"].toscreen(),                         desc="Switch to workspace #3"),
    Key("M-<F4>",                 lazy.group["4"].toscreen(),                         desc="Switch to workspace #4"),
    Key("M-<F5>",                 lazy.group["5"].toscreen(),                         desc="Switch to workspace #5"),
    Key("M-<F6>",                 lazy.group["6"].toscreen(),                         desc="Switch to workspace #6"),
    Key("M-<F7>",                 lazy.group["7"].toscreen(),                         desc="Switch to workspace #7"),
    Key("M-<F8>",                 lazy.group["8"].toscreen(),                         desc="Switch to workspace #8"),
    Key("M-<F9>",                 lazy.group["9"].toscreen(),                         desc="Switch to workspace #9"),
    Key("M-<F10>",                lazy.group["10"].toscreen(),                        desc="Switch to workspace #10"),
    Key("M-<F11>",                lazy.group["11"].toscreen(),                        desc="Switch to workspace #11"),
    Key("M-<F12>",                lazy.group["12"].toscreen(),                        desc="Switch to workspace #12"),

    # Move window to workspace
    Key("M-S-<F1>",               lazy.window.togroup("1"),                           desc="Move focused window to workspace #1"),
    Key("M-S-<F2>",               lazy.window.togroup("2"),                           desc="Move focused window to workspace #2"),
    Key("M-S-<F3>",               lazy.window.togroup("3"),                           desc="Move focused window to workspace #3"),
    Key("M-S-<F4>",               lazy.window.togroup("4"),                           desc="Move focused window to workspace #4"),
    Key("M-S-<F5>",               lazy.window.togroup("5"),                           desc="Move focused window to workspace #5"),
    Key("M-S-<F6>",               lazy.window.togroup("6"),                           desc="Move focused window to workspace #6"),
    Key("M-S-<F7>",               lazy.window.togroup("7"),                           desc="Move focused window to workspace #7"),
    Key("M-S-<F8>",               lazy.window.togroup("8"),                           desc="Move focused window to workspace #8"),
    Key("M-S-<F9>",               lazy.window.togroup("9"),                           desc="Move focused window to workspace #9"),
    Key("M-S-<F10>",              lazy.window.togroup("10"),                          desc="Move focused window to workspace #10"),
    Key("M-S-<F11>",              lazy.window.togroup("11"),                          desc="Move focused window to workspace #11"),
    Key("M-S-<F12>",              lazy.window.togroup("12"),                          desc="Move focused window to workspace #12"),
]

groups = [
    Group(name="1",  label=" ", layout="monadtall"),
    Group(name="2",  label="2",  layout="monadtall"),
    Group(name="3",  label="3",  layout="monadtall"),
    Group(name="4",  label=" ", layout="monadtall"),
    Group(name="5",  label=" ", layout="max"),
    Group(name="6",  label="6",  layout="monadtall"),
    Group(name="7",  label="7",  layout="monadtall"),
    Group(name="8",  label="8",  layout="monadtall"),
    Group(name="9",  label="9",  layout="monadtall"),
    Group(name="10", label="10", layout="monadtall"),
    Group(name="11", label="11", layout="monadtall"),
    Group(name="12", label=" ", layout="max")
]

layouts = [
    layout.MonadTall(),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Columns(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='SauceCodePro Nerd Font',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

widgets_screen_1 = [
    widget.GroupBox(),
    widget.CurrentLayoutIcon(),
    widget.CurrentLayout(),
    widget.Spacer(),
    widget.Clock(format='%Y-%m-%d %a %H:%M %p'),
    widget.Pomodoro(),
    widget.Spacer(),
    widget.CPU(format=" {freq_current}GHz {load_percent}%"),
    widget.Memory(format=" {MemFree}M"),
    widget.ThermalSensor(tag_sensor="Package id 0"),
    widget.DF(partition="/", format=" {p} {uf}{m}", visible_on_warn=False, warn_space=10),
    widget.DF(partition="/mnt/Data", format=" {p} {uf}{m}", visible_on_warn=False, warn_space=10),
    widget.Net(format=" {down}  {up}"),
    widget.Systray(),
]

screen1 = Screen(top=bar.Bar(widgets_screen_1, 24))

screens = [
    screen1,
    # TODO: bar on screen #2
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/scripts/startup_once.sh'])

@hook.subscribe.startup
def start_always():
    # Set the cursor to something sane in X
    subprocess.Popen(['xsetroot', '-cursor_name', 'left_ptr'])

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
    {'wmclass': 'Gnome-calculator'},
    {'wmclass': 'Gnome-screenshot'},
    {'wmclass': 'Dragon-drag-and-drop'},
    {'wname': 'Steam Login'},
    {'wname': 'Steam Guard - Computer Authorization Required'}
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
