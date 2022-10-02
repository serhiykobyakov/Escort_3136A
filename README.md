## Free Pascal and Python units for Escort 3136A multimeter

### What they do

You can connect your Escort 3136A multimeter(s) to PC using nullmodem RS232 cable and acquire the data from multimeter(s) using this software.

The units can acquire the actual multimeter readings and calculate the uncertainties of the reading (the full and the random part, see the description below).

The software has been tested for errors, stability and speed (bugs free is not guaranteed, see the licence).

### Uncertainties
The uncertainty of the measurement is a sum of it's random part and the systematic part. It is defined by the manufacturer and described in the manual as follows:

uncertainty = n% * reading + N * resolution

If we measure a one separate value - it's uncertainty will be the sum of both parts, but if we acquire few values at once and we are interested in the relative difference between the measurements - we do not necessary have to consider the systematic part. For example, if we measure temperature dependence of some parameter in order to calculate it's temperature coefficient - the systematic part of the uncertainties can be discarded.

Taking this into account there is an option to get the full uncertainty using function GetUncertaintySI() or just it's random part using GetRandUncertaintySI().


### Supported platforms
Linux

### Run

The multimeter will be locked when communicating with the program so set it as you like before you start the program.


### Contact
For reporting [bugs, suggestions, patches](https://github.com/serhiykobyakov/Escort_3136A_FPC/issues)

### License
The project is licensed under the [MIT license](https://github.com/serhiykobyakov/Escort_3136A_FPC/blob/main/LICENSE)
