#!/usr/bin/python3
""" sample programto use few Escort 3136A multimeters simultaneously  """

import sys
import time
import tty
import termios
from datetime import datetime
import serial
import serial.tools.list_ports
import escort

__version__ = '30.09.2022'
__author__ = 'Serhiy Kobyakov'


# max number of multimeters that can be simultaneously procesed:
MAXNMETERS = 4
NUMDIGITS = 6

mmeter = []
n_mmeters = 0
data_file_header = ""
the_data = ""

# Modes (see Table 6-6 of the Escort 3136A manual)
#
# 0 - Vdc
# 1 - Vac
# 2 - Resistance
# 4 - Adc
# 5 - Aac
# 6 - diode
# 7 - frequency
# 8 - V (ac+dc)
# 9 - A (ac+dc)
# A - resistance continuity
# B - dBm

#      (primary), (secondary)
#             leave empty string for the second parameter
#             if you don't need secondary function
modes = (("0", ""),  #0
         ("1", ""),  #1
         ("2", ""),  #2
         ("2", ""),  #3
         ("1", ""),  #4
         ("1", ""),  #5
         ("1", ""),  #6
         ("1", ""),  #7
         ("1", ""))  #8

def getch():
    "read single character from keyboard"
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        thech = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    return thech


def clearstr():
    "clear string and put carret at the first character"
    print("", end="\r")
    print (' ' * 100, end="\r")


def savedata():
    "save the obtained data to file"
    clearstr()
    if len(the_data) > 0:
        now = datetime.now()
        fname = now.strftime("%Y-%m-%d_%H-%M-%S") + ".dat"
        f = open(fname, "w", encoding="utf_8")
        f.write(data_file_header)
        f.write(the_data)
        f.close()
        print("the data has been saved into file:", fname)
    print()


def printheader():
    "print header of the table"
    global data_file_header
    data_file_header = ""
    for n in range(n_mmeters):
        print("#".rjust(NUMDIGITS + 2) + str(n).ljust(NUMDIGITS + 3), "", end="")
    print()
    for n in range(n_mmeters):
        the_mode, the_units, the_mode2, the_units2 = mmeter[n].mode
        thestr = " (" + the_units + ")"
        print(the_mode.rjust(NUMDIGITS + 2) + thestr.ljust(NUMDIGITS + 3), "", end="")
        data_file_header = data_file_header + the_mode + thestr +\
                    "\tΔ" + the_mode + thestr +"\t"
    print()
    data_file_header = data_file_header[:-1] + "\n"

if __name__ == "__main__":
    print("\nLooking for devices at serial ports...", end="\r")
    ports = serial.tools.list_ports.comports()

    for port in ports:
        if escort.Model3136A.atport(port.device):
            print(f"Found Escort 3136A at {port.device}, conected as device #{n_mmeters}")
            mmeter.append(escort.Model3136A(port.device, modes[n_mmeters]))
            if n_mmeters == MAXNMETERS:
                break
            n_mmeters += 1

    if n_mmeters == 0:
        print("\n Please attach at least one Escort 3136A multimeter\n")
        sys.exit(1)

    print()
    printheader()

    while True:
        time.sleep(0.1)
        the_char = 0
        print("   ---> [Esc], [q] - quit, [r] - start again, [Space] -\
 take measurement(s) <---   ", end="", flush=True)
        the_char = getch()

        if ord(the_char) == 0:
            pass
        elif ord(the_char) == 27:   # Esc
            savedata()
            sys.exit(0)
        elif ord(the_char) == 113:  # q
            savedata()
            sys.exit(0)
        elif ord(the_char) == 114:  # r - restart
            clearstr()
            print("\n")
            printheader()
        elif ord(the_char) == 32: # Space
            clearstr()
            for i in range(n_mmeters):
                value_1, uncertainty_1 = mmeter[i].value_1
                print(str(value_1).rjust(NUMDIGITS + 2) + " ± " +\
                    str(uncertainty_1).ljust(NUMDIGITS), "", end="")
                the_data = the_data + str(value_1) + "\t" +\
                    str(uncertainty_1) + "\t"
            the_data = the_data[:-1] + "\n"
            print()
        else:
            pass
