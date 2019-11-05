from ..utils import BasicSegment, warn
import os
import sys
import re
import subprocess

name_re = re.compile(r";([a-zA-Z ]+);")
status_re = re.compile(r"'([a-zA-Z ]+)'\n")
cap_re = re.compile(r"(\d\d\d*)%")

class Segment(BasicSegment):
    def add_to_powerline(self):
        # See discussion in https://github.com/banga/powerline-shell/pull/204
        # regarding the directory where battery info is saved
        if sys.platform == "linux2":
            if os.path.exists("/sys/class/power_supply/BAT0"):
                dir_ = "/sys/class/power_supply/BAT0"
            elif os.path.exists("/sys/class/power_supply/BAT1"):
                dir_ = "/sys/class/power_supply/BAT1"
            else:
                warn("battery directory could not be found")
                return

            with open(os.path.join(dir_, "capacity")) as f:
                cap = int(f.read().strip())
            with open(os.path.join(dir_, "status")) as f:
                status = f.read().strip()
        if sys.platform == "darwin":
            result = subprocess.run(["pmset", "-g", "batt"], capture_output=True).stdout.decode('utf-8')
            name = status_re.search(result).group().strip("' \n")
            status = name_re.search(result).group().strip(' ;').title()
            cap = int(cap_re.search(result).group().strip(' %'))

            if status == "Charged":
                status = "Full"

            print("." + name + ".", "." + status + ".", "." + str(cap) + ".")
        else:
            warn("platform not supported")
            return

        if status == "Full":
            if self.powerline.segment_conf("battery", "always_show_percentage", False):
                pwr_fmt = u" {cap:d}% \U0001F50C "
            else:
                pwr_fmt = u" \U0001F50C "
        elif status == "Charging":
            pwr_fmt = u" {cap:d}% \u26A1 "
        else:
            pwr_fmt = " {cap:d}% "

        if cap < self.powerline.segment_conf("battery", "low_threshold", 20):
            bg = self.powerline.theme.BATTERY_LOW_BG
            fg = self.powerline.theme.BATTERY_LOW_FG
        else:
            bg = self.powerline.theme.BATTERY_NORMAL_BG
            fg = self.powerline.theme.BATTERY_NORMAL_FG
        self.powerline.append(pwr_fmt.format(cap=cap), fg, bg)