## Free Pascal and Python units for Escort 3136A multimeter

### What it does

You can connect and use use your Escort 3136A multimeter in your Free Pascal (Lazarus) projects using nullmodem RS232 cable and this software.

The unit can acquire the actual multimeter readings and calculate the uncertainties of the reading (the full and the random part, see the description below).

The software has been tested for errors, stability and speed (bugs free is not guaranteed, see the licence).

### Uncertainties
The uncertainty of the measurement is a sum of it's random part and the systematic part. It is defined by the manufacturer and described in the manual as follows:

uncertainty = n% * reading + N * resolution

If we measure a one separate value - it's uncertainty will be the sum of both parts, but if we acquire few values at once and we are interested in the relative difference between the measurements - we do not necessary have to consider the systematic part. For example, if we measure temperature dependence of some parameter in order to calculate it's temperature coefficient - the systematic part of the uncertainties can be discarded.

Taking this into account there is an option to get the full uncertainty using function GetUncertaintySI() or just it's random part using GetRandUncertaintySI().


### Supported platforms
Windows (has not been tested yet but must do the job)
Linux

### Install

1. Put the Escort_3136A.pas in your project's directory.
2. Put the files from Synapse project (http://synapse.ararat.cz/doku.php/download) in your project's directory:
 - synautil.pas
 - jedi.inc
 - synafpc.pas
 - synaser.pas

### How To Use
1. Put "Escort_3136A" in the Uses section of your file, put theDevice: Escort_3136A_device; into the var section of the application.
2. Put the initialization code into the FormCreate section of main application Form:

```
procedure TForm1.FormCreate(Sender: TObject);
begin  
...
// First you have to find the port your device is connected
// using the function APPA_109N_on(<COM port address>):
//
// iterating through ports we check if the multimeter tere and
// remember the port if we are lucky:
  if Escort_3136A_on(<COM port address>) then <remember the Com port addres>

// And finally the device initialization:
  theDevice.Init(<COM port address>);
end;
```

3. Deactivate the device on exit:

```
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin 
...
  theDevice.Done;
end;
```

4. In your measurement loop you have to trigger the measurement first and after that use the other functions to put the data into the appropriate places, the functions are pretty self-explanatory:
```
...
// -- begin of the measurement loop

// trigger the measurement (get the data into memory, it takes few milliseconds)
theDevice.GetData();

// use the data from the memory in your code using these functions:
//  function GetMode(): string;
//  function GetRange(): string;
//  function GetValueSI(): Real;
//  function GetUnitsSI(): string;
//  function GetUncertaintySI(): Real;
//  function GetRandUncertaintySI(): Real;
//  function GetMode2(): string;
//  function GetRange2(): string;
//  function GetValue2SI(): Real;
//  function GetUnits2SI(): string;
//  function GetUncertainty2SI(): Real;
//  function GetRandUncertainty2SI(): Real;

// for example:
theValue := GetValueSI();

...

// -- end of the measurement loop
...     
```

### Run

The multimeter will be locked when communicating with the program so set it as you like before you start the program.

### Contact
For reporting [bugs, suggestions, patches](https://github.com/serhiykobyakov/Escort_3136A_FPC/issues)

### License
The project is licensed under the [MIT license](https://github.com/serhiykobyakov/Escort_3136A_FPC/blob/main/LICENSE)
