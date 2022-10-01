""" Escort 3136A multimeter module

This module implements a communication interface to Escort 3136A multimeter,
allowing to get the measured values along with their uncertainties.
"""



__version__ = '01.09.2022'
__author__ = 'Serhiy Kobyakov'

import sys
import time
#from unittest import result
import tkinter as tk
import math
import serial


class Model3136A:
    "Escort 3136A multimeter class"

    COMPORTSPEED = 9600
    COMPORTPARITY = serial.PARITY_NONE
    COMPORTSTOPBITS = serial.STOPBITS_ONE
    COMPORTBITS = serial.EIGHTBITS
    COMPORTTIMEOUT = 0.4
    COMPORTWRITETIMEOUT = 0.2
    SHORTESTTIMEBETWEENREADS = 0.4

    __ser = None
    __comport = None
    __company = 'Escort'
    __model = '3136A'
    __device = 'multimeter'

    __err_prompts = (b'*>', b'!>', b'?>', b'#>', b'S>', b'@>', b'W>', b'E>')
    __err_prompts_msg = ("The meter is reset to power-up initialization status",
                         "Command error detected",
                         "Parameter error detected",
                         "Local key is pressed",
                         "Setup function is under executing",
                         "No numeric reading is available",
                         "Execution error")

    __ModeTable = ('Vdc', 'Vac', 'R', '', 'Adc', 'Aac', 'Diode', 'Frequency', 'V(ac+dc)',\
#                    0      1     2   3     4      5       6          7           8
        'A(ac+dc)', 'R_cont', 'dBm')
#           9          10       11

    __UnitsTable = ('V', 'V', 'Ohm', '', 'A', 'A', '', 'Hz', 'V', 'A', 'Ohm', 'dBm')

    __RangeTable = (('', '500mV',  '5V',   '50V',   '500V',   '1000V', ''),     # Vdc
                    ('', '500mV',  '5V',   '50V',   '500V',   '750V',  ''),     # Vac
                    ('', '500Ω',   '5kΩ',  '50kΩ',  '500kΩ',  '5MΩ',   '50MΩ'), # R
                    ('',     '',       '',     '',      '',       '',      ''),
                    ('', '500mkA', '5mA',  '50mA',  '500mA',  '5A',    '10A'),  # Adc
                    ('', '500mkA', '5mA',  '50mA',  '500mA',  '5A',    '10A'),  # Aac
                    ('', '2,3V',   '',     '',      '',       '',      ''),     # Diode
                    ('', '500Hz',  '5kHz', '50kHz', '500kHz', '',      ''),     # Frequency
                    ('', '500mV',  '5V',   '50V',   '500V',   '750V',  ''),     # V(ac+dc)
                    ('', '500mkA', '5mA',  '50mA',  '500mA',  '5A',    '10A'),  # A(ac+dc)
                    ('', '500Ω',   '5kΩ',  '50kΩ',  '500kΩ',  '5MΩ',   '50MΩ'), # R cont
                    ('', '-105,56..59,72', '', '',  '',       '',      ''))     # dBm

    # [self.__S1][self.__r1]
    __RandErrTable = ((0,    0.02,  0.02,   0.02,  0.02,  0.02, 0),     # Vdc
                      (0,    0,     0,      0,     0,     0,    0),     # Vac
                      (0,    0.1,   0.1,    0.1,   0.1,   0.1,  0.3),   # R
                      (0,    0,     0,      0,     0,     0,    0),
                      (0,    0.05,  0.05,   0.05,  0.05,  0.25, 0.25),  # Adc
                      (0,    0,     0,      0,     0,     0,    0),     # Aac
                      (0,    0.05,  0,      0,     0,     0,    0),     # Diode
                      (0,    0.01,  0.01,   0.01,  0.01,  0,    0),     # Frequency
                      (0,    0,     0,      0,     0,     0,    0),     # V(ac+dc)
                      (0,    0,     0,      0,     0,     0,    0),     # A(ac+dc)
                      (0,    0.1,   0.1,    0.1,   0.1,   0.1,  0.3),   # R cont
                      (0,    0.7,   0,      0,     0,     0,    0))     # dBm

    # [self.__S1][self.__r1]
    __SystErrTable = ((0,    4e-5,  4e-4,   4e-3,  4e-2,  0.4,  0),     # Vdc
                      (0,    0,     0,      0,     0,     0,    0),     # Vac
                      (0,    5e-2,  0.3,    3,     30,    300,  3000),  # R
                      (0,    0,     0,      0,     0,     0,    0),
                      (0,    5e-8,  4e-7,   4e-6,  4e-5,  5e-4, 5e-3),  # Adc
                      (0,    0,     0,      0,     0,     0,    0),     # Aac
                      (0,    5e-4,  0,      0,     0,     0,    0),     # Diode
                      (0,    5e-2,  0.3,    3,     30,    0,    0),     # Frequency
                      (0,    0,     0,      0,     0,     0,    0),     # V(ac+dc)
                      (0,    0,     0,      0,     0,     0,    0),     # A(ac+dc)
                      (0,    5e-2,  0.3,    3,     30,    300,  3000),  # R cont
                      (0,    0,     0,      0,     0,     0,    0))     # dBm

    # [sub_code[self.__S1]][self.__r1][self.__freq_code]
    __ACRandErrTable = (((0, 1, 0.5,  2, 3, 0),   # Range 1
                         (0, 1, 0.35, 1, 3, 0),   # Range 2
                         (0, 1, 0.35, 1, 3, 0),   # Range 3     AC Voltage
                         (0, 0, 0.5,  1, 3, 0),   # Range 4
                         (0, 0, 0.5,  1, 0, 0),   # Range 5
                         (0, 0, 0,    0, 0, 0)),  # Range 6

                        ((0, 1.5, 0.5, 1.5, 3, 0),   # Range 1
                         (0, 1.5, 0.5, 1.5, 3, 0),   # Range 2
                         (0, 1.5, 0.5, 1.5, 3, 0),   # Range 3     AC Current
                         (0, 1.5, 0.5, 1.5, 3, 0),   # Range 4
                         (0, 2,   0.5, 0,   0, 0),   # Range 5
                         (0, 2,   0,   0,   0, 0)),  # Range 6

                        ((0, 0, 0.5, 2, 3, 0),   # Range 1
                         (0, 0, 0.5, 1, 3, 0),   # Range 2
                         (0, 0, 0.5, 1, 3, 0),   # Range 3     AC Voltage (AC+DC)
                         (0, 0, 0.5, 1, 3, 0),   # Range 4
                         (0, 0, 0.5, 1, 0, 0),   # Range 5
                         (0, 0, 0,   0, 0, 0)),  # Range 6

                        ((0, 0, 0.5, 1.5, 3, 0),   # Range 1
                         (0, 0, 0.5, 1.5, 3, 0),   # Range 2
                         (0, 0, 0.5, 1.5, 3, 0),   # Range 3     AC Current (AC+DC)
                         (0, 0, 0.5, 1.5, 3, 0),   # Range 4
                         (0, 0, 0.5, 0,   0, 0),   # Range 5
                         (0, 0, 0,   0,   0, 0)))  # Range 6

    # [sub_code[self.__S1]][self.__r1][self.__freq_code]
    __ACSystErrTable = (((0, 4e-4,  4e-4, 6e-4, 12e-4, 0),   # Range 1
                         (0, 2e-3, 15e-4, 2e-3,  5e-3, 0),   # Range 2
                         (0, 2e-2, 15e-3, 2e-2,  5e-2, 0),   # Range 3     AC Voltage
                         (0,    0, 15e-2,  0.2,   0.5, 0),   # Range 4
                         (0,    0,   1.5,    2,     0, 0),   # Range 5
                         (0,    0,     0,    0,     0, 0)),  # Range 6

                        ((0, 5e-7, 2e-7, 5e-7, 7.5e-7, 0),   # Range 1
                         (0, 4e-6, 2e-6, 4e-6, 6e-6,   0),   # Range 2
                         (0, 4e-5, 2e-5, 4e-5, 6e-5,   0),   # Range 3     AC Current
                         (0, 4e-4, 2e-4, 4e-4, 6e-4,   0),   # Range 4
                         (0, 4e-3, 3e-3, 0,    0,      0),   # Range 5
                         (0, 4e-2, 0,    0,    0,      0)),  # Range 6

                        ((0, 0, 5e-4,   7e-4, 13e-4, 0),   # Range 1
                         (0, 0, 2.5e-3, 3e-3, 6e-3,  0),   # Range 2
                         (0, 0, 2.5e-2, 3e-2, 6e-2,  0),   # Range 3     AC Voltage (AC+DC)
                         (0, 0, 0.25,   0.3,  0.6,   0),   # Range 4
                         (0, 0, 2.5,    3,    0,     0),   # Range 5
                         (0, 0, 0,      0,    0,     0)),  # Range 6

                        ((0, 0, 3e-7, 6e-7, 8.5e-7, 0),   # Range 1
                         (0, 0, 3e-6, 5e-6, 7e-6,   0),   # Range 2
                         (0, 0, 3e-5, 5e-5, 7e-5,   0),   # Range 3     AC Current (AC+DC)
                         (0, 0, 3e-4, 5e-4, 7e-4,   0),   # Range 4
                         (0, 0, 4e-3, 0,    0,      0),   # Range 5
                         (0, 0, 0,    0,    0,      0)))  # Range 6

    __lastcomtimestamp = 0.0

    __modestring = ''
    __mode_1 = ''
    __mode_2 = ''
    __units_1 = ''
    __units_2 = ''
    __S1 = 0
    __S2 = 0
    __r1 = 0
    __r2 = 0
    __freq_code = 0
    __value_1 = 0.0
    __value_2 = 0.0
    __rand_err_1 = 0.0
    __syst_err_1 = 0.0
    __rand_err_2 = 0.0
    __syst_err_2 = 0.0




    @classmethod
    def atport(cls, comport):
        "Returns True if an Escort 3136A multimeter is connected at COM port \"comport\""
        res = False
        if not isinstance(comport, str):
            raise TypeError("comport: string value expected, got", type(str), "instead")

        ser = serial.Serial(port = comport,
                            baudrate = cls.COMPORTSPEED,
                            writeTimeout = cls.COMPORTWRITETIMEOUT,
                            timeout = cls.COMPORTTIMEOUT,
                            parity = cls.COMPORTPARITY,
                            stopbits = cls.COMPORTSTOPBITS,
                            bytesize = cls.COMPORTBITS)
        ser.write(b"BON\r\n")
        try:
            thestr = ''
            thestr = ser.readline().strip()
            if thestr == b'=>':
                res = True
                ser.write(b"GTL\r\n")
                ser.readline()
        finally:
            ser.close()
        return res


    def __repr__(self) -> str:
        return f'{self.__company} {self.__model} {self.__device} at {self.__comport}'


    def __str__(self) -> str:
        return f'{self.__company} {self.__model} {self.__device} at {self.__comport}'


    def __init__(self, comport, themode):
        self.__ser = serial.Serial(port = comport,
                            baudrate = self.COMPORTSPEED,
                            write_timeout = self.COMPORTWRITETIMEOUT,
                            timeout = self.COMPORTTIMEOUT,
                            parity = self.COMPORTPARITY,
                            stopbits = self.COMPORTSTOPBITS,
                            bytesize = self.COMPORTBITS)
        # self.__ser.write(b"RST\r\n")
        # thestr = b''
        # while thestr != b'*>':
        #     time.sleep(0.2)
        #     thestr = self.__ser.readline().strip()
        # time.sleep(0.4)
        self.__ser.write(b"LLO\r\n") # init device locked, no manual mode setting allowed
        self.__ser.readline()

        # set the device mode:
        if themode[0] in ["0", "1", "2", "4", "5", "6", "7", "8", "9", "A", "B"]:
            cmd = "S1" + themode[0] + "\r\n"
            self.__ser.write(bytes(cmd, 'ascii'))
            prompt = self.__ser.readline().strip()
            if prompt in self.__err_prompts:
                print(f" Error: In __init__, command \"{cmd.strip()}\" command problem: {self.__prompt_err_msg(prompt)}")
        if themode[0] in ["1", "5", "8", "9"]:
            # if the primary display is in AC mode - the secondary must be in frequency mode
            # in order to obtain the frequency range (to calculate the uncertainty)
            cmd = "S27\r\n"
            self.__ser.write(bytes(cmd, 'ascii'))
            prompt = self.__ser.readline().strip()
        else:
            if themode[1] in ["0", "1", "4", "5", "7", "B"]:
                cmd = "S2" + themode[1] + "\r\n"
                self.__ser.write(bytes(cmd, 'ascii'))
                prompt = self.__ser.readline().strip()
                if prompt in self.__err_prompts:
                    print(f" Error: In __init__, command \"{cmd.strip()}\" command problem: {self.__prompt_err_msg(prompt)}")

        time.sleep(0.1)
        self.__check_mode()
        self.__comport = comport
        self.__lastcomtimestamp = time.time()


    def __del__(self):
        self.__ser.write(b"GTL\r\n")
        self.__ser.readline()
        self.__ser.close()


    def __showmsg(self, msg: str):
        """ function shows separate small window at the center of the screen\
        with the message 'msg' """
        root = tk.Tk()
        root.title('Escort 3136A')
        window_width = 400
        window_height = 120
        # get the screen dimension
        screen_width = root.winfo_screenwidth()
        screen_height = root.winfo_screenheight()
        # find the center point
        center_x = int(screen_width/2 - window_width / 2)
        center_y = int(screen_height/2 - window_height / 2)
        # set the position of the window to the center of the screen
        root.geometry(f'{window_width}x{window_height}+{center_x}+{center_y}')
        root.resizable(False, False)
        root.attributes('-topmost', 1)
        tk.Label(root, text = "\n" + msg).pack()
        def exit_this_function():
            root.destroy()
        button = tk.Button(root, text = 'OK!', command = exit_this_function)
        button.pack(side=tk.BOTTOM)
        button.focus_set()
        root.mainloop()

    def __prompt_err_msg(self, prompt) -> str:
        "converts the prompt to appropriate error message"
        return self.__err_prompts_msg[self.__err_prompts.index(prompt)]

    @staticmethod
    def __is_float(s_str: str) -> bool:
        "checks if the string s_str contains a valid float number"
        try:
            float(s_str)
            return True
        except ValueError:
            return False

    @staticmethod
    def __roundunc(unc) -> float:
        "Round uncertainty value to two significant digits"
        if unc == 0.0:
            the_answer = 0.0
        else:
            dec_place = 1 - round(math.log10(unc))
            the_answer = round(unc, dec_place)
        return the_answer


    def __check_mode(self):
        "Get the actual modes of operation from the device"
        thestr = ''
        self.__mode_1 = ''
        self.__units_1 = ''
        self.__mode_2 = ''
        self.__units_2 = ''
        got_answer = False
        while got_answer is not True:
            thestr = self.__send_and_read("R0")
            if len(thestr) == 9 or len(thestr) == 11:
                self.__modestring = thestr
                got_answer = True
                self.__S1 = int(thestr[7:8].decode(), base = 16)
                self.__mode_1 = self.__ModeTable[self.__S1]
                self.__units_1 = self.__UnitsTable[self.__S1]
                self.__r1 = int(thestr[8:9].decode(), base = 16)
                if len(thestr) == 11:
                    self.__S2 = int(thestr[9:10].decode(), base = 16)
                    self.__r2 = int(thestr[10:11].decode(), base = 16)
                    self.__mode_2 = self.__ModeTable[self.__S2]
                    self.__units_2 = self.__UnitsTable[self.__S2]
                else:
                    self.__mode_2 = ''
                    self.__units_2 = ''


    def __check_freq_range(self):
        "Update the frequency code value"
        the_code = 0
        if self.__S1 in [1, 8]:
            # Voltage modes:
            if self.__value_2 <= 30.0:
                the_code = 0
            elif 30.0 < self.__value_2 and self.__value_2 <= 50.0:
                the_code = 1
            elif 50.0 < self.__value_2 and self.__value_2 <= 10000.0:
                the_code = 2
            elif 10000.0 < self.__value_2 and self.__value_2 <= 30000.0:
                the_code = 3
            elif 30000.0 < self.__value_2 and self.__value_2 <= 100000.0:
                the_code = 4
            elif 100000.0 < self.__value_2:
                the_code = 5

        if self.__S1 in [5, 9]:
            # Current modes:
            if self.__value_2 <= 30.0:
                the_code = 0
            elif 30.0 < self.__value_2 and self.__value_2 <= 50.0:
                the_code = 1
            elif 50.0 < self.__value_2 and self.__value_2 <= 2000.0:
                the_code = 2
            elif 2000.0 < self.__value_2 and self.__value_2 <= 5000.0:
                the_code = 3
            elif 5000.0 < self.__value_2 and self.__value_2 <= 20000.0:
                the_code = 4
            elif 20000.0 < self.__value_2:
                the_code = 5

        #print(f"f1: {self.__S1}, Freq: {self.__value_2} Hz,  Fcode:{the_code},  r1:{self.__r1}")
        self.__freq_code = the_code


    def __get_values(self):
        "Obtain measured values from the device"
        self.__check_mode()
        if time.time() - self.__lastcomtimestamp > self.SHORTESTTIMEBETWEENREADS:
            val1 = val2 = ''
            self.__value_2 = 0.0
            got_answer = False

            while got_answer is not True:
                val1 = self.__send_and_read("R1")
                if self.__is_float(val1):
                    self.__value_1 = float(val1)
                    self.__rand_err_1 = 0.01 *\
                        self.__RandErrTable[self.__S1][self.__r1] * abs(self.__value_1)
                    self.__syst_err_1 = self.__SystErrTable[self.__S1][self.__r1]
                    got_answer = True

            if len(self.__modestring) == 11:
                got_answer = False
                while got_answer is not True:
                    val2 = self.__send_and_read("R2")
                    if self.__is_float(val2):
                        self.__value_2 = float(val2)
                        if self.__S1 in [1, 5, 8, 9]:
                            self.__check_freq_range()
                            sub_code = (0, 0, 0, 0, 0, 1, 0, 0, 2, 3, 0, 0)
                            self.__rand_err_1 = 0.01 *\
                                self.__ACRandErrTable[sub_code[self.__S1]][self.__r1][self.__freq_code] * abs(self.__value_1)
                            self.__syst_err_1 = self.__ACSystErrTable[sub_code[self.__S1]][self.__r1][self.__freq_code]
                        self.__rand_err_2 = 0.01 *\
                            self.__RandErrTable[self.__S2][self.__r2] * abs(self.__value_2)
                        self.__syst_err_2 = self.__SystErrTable[self.__S2][self.__r2]
                        got_answer = True

            self.__lastcomtimestamp = time.time()
        return self.__value_1, self.__value_2


    def __empty_serial_buffer(self):
        # Empty serial buffer if there is some data available
        while self.__ser.in_waiting > 0:
            thestr = self.__ser.readline().strip()
            # for testing purpouses only:
            print(" Err: got in serial buffer:", thestr)


    def __send_and_read(self, the_command: str) -> str:
        "Send the_command to device and read the answer"
        thestr = b''
        prompt = b''
        answer = b''
        read_problem_counter = 0
        got_answer = False
        self.__empty_serial_buffer()

        while got_answer is not True:
            self.__ser.write(the_command.encode('ascii') + b"\r\n")

            # Send the command
            thestr = self.__ser.readline().rstrip()
            if len(thestr) == 0:
                read_problem_counter += 1
                time.sleep(self.SHORTESTTIMEBETWEENREADS)
            else:
                answer = thestr

            # get the prompt
            prompt = self.__ser.readline().strip()
            if len(prompt) == 0:
                read_problem_counter += 1
                time.sleep(self.SHORTESTTIMEBETWEENREADS)
            else:
                if prompt in self.__err_prompts:
                    read_problem_counter += 1
                    print(f" Error: \"{the_command}\" command problem: {self.__prompt_err_msg(prompt)}")
                    time.sleep(self.SHORTESTTIMEBETWEENREADS)

            # if problems
            if read_problem_counter > 4:
                self.__showmsg("""The multimeter is off
                \nSwitch it on and start the application again!""")
                sys.exit(1)

            if prompt == b"=>":
                got_answer = True

        return answer


    @property
    def mode(self) -> str:
        "returns the primary mode as a string"
        self.__check_mode()
        return self.__mode_1, self.__units_1, self.__mode_2, self.__units_2


    @property
    def value_1(self) -> float:
        "returns actual value and it's uncertainty from the primary display"
        self.__get_values()
        return self.__value_1, self.__roundunc(self.__rand_err_1 + self.__syst_err_1)


    @property
    def value_2(self) -> float:
        "returns actual value and it's uncertainty from the secondary display"
        if len(self.__modestring) == 11:
            self.__get_values()
            the_answer = self.__value_2, self.__roundunc(self.__rand_err_2 + self.__syst_err_2)
        else:
            the_answer = 0.0, 0.0
        return the_answer


    @property
    def range_str_1(self) -> str:
        "Returns the measurement range of the primary display"
        return self.__RangeTable[self.__S1][self.__r1]

    @property
    def range_str_2(self) -> str:
        "Returns the measurement range of the primary display"
        if len(self.__modestring) == 11:
            the_answer = self.__RangeTable[self.__S2][self.__r2]
        else:
            the_answer = "None"
        return the_answer


    @property
    def firmware_str(self) -> float:
        "returns firmware string"
        return self.__send_and_read("RV").decode()
