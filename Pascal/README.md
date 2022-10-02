# Free Pascal unit for Escort 3136A multimeter

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
