unit Escort_3136A;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialogs, StdCtrls, Controls, Forms,
  FileUtil,
  Unix,
  DateUtils,
  Math,
  synaser;


{ Escort_3136A_device }

type

  Escort_3136A_device = object
  private
    ser: TBlockSerial;
    theComPort: string;
    theModestr, theRange, theModestr2, theRange2, theUnitsSI, theUnits2SI: string;
    ModeCode, ModeCode2, RangeCode, RangeCode2: Integer;   // leave it here!!!
    theValueSI, theRandErr, theSystErr, theValue2SI, theRandErr2, theSystErr2: Real;
    theTestStr: string; // remove after testing!
    theDualDisplayMode: boolean;


  const
    ModeTable: array[0..11] of String =
     ('Vdc', 'Vac', 'R', '', 'Adc', 'Aac', 'Diode', 'Frequency', 'V(ac+dc)', 'A(ac+dc)', 'R_cont', 'dBm');
//      0      1     2   3     4      5       6          7           8           9          10       11
//  f1, f2 or ModeCode

    ACModeCode: array[0..11] of byte =
     (0, 1, 0, 0, 0, 2, 0, 0, 3, 4, 0, 0);

    UnitsTable: array[0..11] of String =
     ('V', 'V', 'Ohm', '', 'A', 'A', '', 'Hz', 'V', 'A', 'Ohm', 'dBm');

    RangeTable: array[0..11, 0..6] of String =
     (('', '500mV',  '5V',   '50V',   '500V',   '1000V', ''),     // Vdc
      ('', '500mV',  '5V',   '50V',   '500V',   '750V',  ''),     // Vac
      ('', '500Ω',   '5kΩ',  '50kΩ',  '500kΩ',  '5MΩ',   '50MΩ'), // R
      ('',     '',       '',     '',      '',       '',      ''),
      ('', '500mkA', '5mA',  '50mA',  '500mA',  '5A',    '10A'),  // Adc
      ('', '500mkA', '5mA',  '50mA',  '500mA',  '5A',    '10A'),  // Aac
      ('', '2,3V',   '',     '',      '',       '',      ''),     // Diode
      ('', '500Hz',  '5kHz', '50kHz', '500kHz', '',      ''),     // Frequency
      ('', '500mV',  '5V',   '50V',   '500V',   '750V',  ''),     // V(ac+dc)
      ('', '500mkA', '5mA',  '50mA',  '500mA',  '5A',    '10A'),  // A(ac+dc)
      ('', '500Ω',   '5kΩ',  '50kΩ',  '500kΩ',  '5MΩ',   '50MΩ'), // R cont
      ('', '-105,56..59,72', '', '',  '',       '',      ''));    // dBm

    RandErrTable: array[0..11, 0..6] of Real =
     ((0,    0.02,  0.02,   0.02,  0.02,  0.02, 0),     // Vdc
      (0,    0,     0,      0,     0,     0,    0),     // Vac
      (0,    0.1,   0.1,    0.1,   0.1,   0.1,  0.3),   // R
      (0,    0,     0,      0,     0,     0,    0),
      (0,    0.05,  0.05,   0.05,  0.05,  0.25, 0.25),  // Adc
      (0,    0,     0,      0,     0,     0,    0),     // Aac
      (0,    0.05,  0,      0,     0,     0,    0),     // Diode
      (0,    0.01,  0.01,   0.01,  0.01,  0,    0),     // Frequency
      (0,    0,     0,      0,     0,     0,    0),     // V(ac+dc)
      (0,    0,     0,      0,     0,     0,    0),     // A(ac+dc)
      (0,    0.1,   0.1,    0.1,   0.1,   0.1,  0.3),   // R cont
      (0,    0.7,   0,      0,     0,     0,    0));    // dBm

    SystErrTable: array[0..11, 0..6] of Real =
     ((0,    4e-5,  4e-4,   4e-3,  4e-2,  0.4,  0),     // Vdc
      (0,    0,     0,      0,     0,     0,    0),     // Vac
      (0,    5e-2,  0.3,    3,     30,    300,  3000),  // R
      (0,    0,     0,      0,     0,     0,    0),
      (0,    5e-8,  4e-7,   4e-6,  4e-5,  5e-4, 5e-3),  // Adc
      (0,    0,     0,      0,     0,     0,    0),     // Aac
      (0,    5e-4,  0,      0,     0,     0,    0),     // Diode
      (0,    5e-2,  0.3,    3,     30,    0,    0),     // Frequency
      (0,    0,     0,      0,     0,     0,    0),     // V(ac+dc)
      (0,    0,     0,      0,     0,     0,    0),     // A(ac+dc)
      (0,    5e-2,  0.3,    3,     30,    300,  3000),  // R cont
      (0,    0,     0,      0,     0,     0,    0));    // dBm


                 // (ACModeCode, RangeCode, FreqCode)
    ACRandErrTable: array[1..4, 1..6, 0..5] of Real =
     (((0, 1, 0.5,  2, 3, 0),   // Range 1
       (0, 1, 0.35, 1, 3, 0),   // Range 2
       (0, 1, 0.35, 1, 3, 0),   // Range 3     AC Voltage
       (0, 0, 0.5,  1, 3, 0),   // Range 4
       (0, 0, 0.5,  1, 0, 0),   // Range 5
       (0, 0, 0,    0, 0, 0)),  // Range 6

      ((0, 1.5, 0.5, 1.5, 3, 0),   // Range 1
       (0, 1.5, 0.5, 1.5, 3, 0),   // Range 2
       (0, 1.5, 0.5, 1.5, 3, 0),   // Range 3     AC Current
       (0, 1.5, 0.5, 1.5, 3, 0),   // Range 4
       (0, 2,   0.5, 0,   0, 0),   // Range 5
       (0, 2,   0,   0,   0, 0)),  // Range 6

      ((0, 0, 0.5, 2, 3, 0),   // Range 1
       (0, 0, 0.5, 1, 3, 0),   // Range 2
       (0, 0, 0.5, 1, 3, 0),   // Range 3     AC Voltage (AC+DC)
       (0, 0, 0.5, 1, 3, 0),   // Range 4
       (0, 0, 0.5, 1, 0, 0),   // Range 5
       (0, 0, 0,   0, 0, 0)),  // Range 6

      ((0, 0, 0.5, 1.5, 3, 0),   // Range 1
       (0, 0, 0.5, 1.5, 3, 0),   // Range 2
       (0, 0, 0.5, 1.5, 3, 0),   // Range 3     AC Current (AC+DC)
       (0, 0, 0.5, 1.5, 3, 0),   // Range 4
       (0, 0, 0.5, 0,   0, 0),   // Range 5
       (0, 0, 0,   0,   0, 0))); // Range 6


    ACSystErrTable: array[1..4, 1..6, 0..5] of Real =
    (((0, 4e-4, 4e-4,  6e-4, 12e-4, 0),   // Range 1
      (0, 2e-3, 15e-4, 2e-3, 5e-3,  0),   // Range 2
      (0, 2e-2, 15e-3, 2e-2, 5e-2,  0),   // Range 3     AC Voltage
      (0, 0,    15e-2, 0.2,  0.5,   0),   // Range 4
      (0, 0,    1.5,   2,    0,     0),   // Range 5
      (0, 0,    0,     0,    0,     0)),  // Range 6

     ((0, 5e-7, 2e-7, 5e-7, 7.5e-7, 0),   // Range 1
      (0, 4e-6, 2e-6, 4e-6, 6e-6,   0),   // Range 2
      (0, 4e-5, 2e-5, 4e-5, 6e-5,   0),   // Range 3     AC Current
      (0, 4e-4, 2e-4, 4e-4, 6e-4,   0),   // Range 4
      (0, 4e-3, 3e-3, 0,    0,      0),   // Range 5
      (0, 4e-2, 0,    0,    0,      0)),  // Range 6

     ((0, 0, 5e-4,   7e-4, 13e-4, 0),   // Range 1
      (0, 0, 2.5e-3, 3e-3, 6e-3,  0),   // Range 2
      (0, 0, 2.5e-2, 3e-2, 6e-2,  0),   // Range 3     AC Voltage (AC+DC)
      (0, 0, 0.25,   0.3,  0.6,   0),   // Range 4
      (0, 0, 2.5,    3,    0,     0),   // Range 5
      (0, 0, 0,      0,    0,     0)),  // Range 6

     ((0, 0, 3e-7, 6e-7, 8.5e-7, 0),   // Range 1
      (0, 0, 3e-6, 5e-6, 7e-6,   0),   // Range 2
      (0, 0, 3e-5, 5e-5, 7e-5,   0),   // Range 3     AC Current (AC+DC)
      (0, 0, 3e-4, 5e-4, 7e-4,   0),   // Range 4
      (0, 0, 4e-3, 0,    0,      0),   // Range 5
      (0, 0, 0,    0,    0,      0))); // Range 6


    function RoundUnc(val: Real): Real;
    function validFloat(val: string): Boolean;
    function FreqRangeI(freq: Real): Byte;
    function FreqRangeU(freq: Real): Byte;


   public
     constructor Init(ComPort: string);
     destructor Done;
     procedure GetData();

     function GetValueSI(): Real;             // Get the measurement value

     function GetUnitsSI(): string;
     function GetRange(): string;
     function GetMode(): string;

     function GetUncertaintySI(): Real;       // get the measurement uncertainty in SI units
     function GetRandUncertaintySI(): Real;   // get the only random part of the measurement

     function GetValue2SI(): Real;             // Get the measurement value
     function GetUnits2SI(): string;
     function GetRange2(): string;
     function GetMode2(): string;
     function GetUncertainty2SI(): Real;       // get the measurement uncertainty in SI units
     function GetRandUncertainty2SI(): Real;   // get the only random part of the measurement

     function GetTestStr(): string;  // reove after testing!

     procedure ResetDevice;
   end;

procedure SleepFor(thetime: LongInt);
function Escort_3136A_on(ComPort: string): Boolean;


//var


const
  theDevice = 'Escort_3136A';    // the device name according to the manual
  waitForAnswer: Integer = 200;  // how long one have to wait for the answer (in milliseconds)
  waitForRead: Integer = 140;     // how long one have to wait for reading the answer from COM port (in milliseconds)
  AcquisitionDelay = 20;         // the minimal delay between consequent readings (in miliseconds)



implementation

procedure SleepFor(thetime: LongInt);
var
  tstartwait: TDateTime;
begin
  tstartwait := Now;
  repeat
    Application.ProcessMessages;         // do something useful while waiting
  until MillisecondsBetween(Now, tstartwait) > thetime;
end;

function Escort_3136A_on(ComPort: string): Boolean;
var
  ser: TBlockSerial;
  str: string;
  res: Boolean;
begin
// test for APPA 109N
  ser := TBlockSerial.Create;
    try
      ser.RaiseExcept:=true;
      ser.Connect(ComPort);
      ser.config(9600, 8, 'N', SB1, False, False);
      Application.ProcessMessages;
      ser.SendString('BON'+CRLF);
      if ser.lastError=0 then
         if ser.canread(waitForAnswer) then str := ser.Recvstring(waitForRead);
      if (str <> '=>') then res := False
      else res := True;

    finally
      ser.Purge;
    end;
  ser.Free;

  Result := res;
end;

function Escort_3136A_device.RoundUnc(val: Real): Real;
var
  factor: Real;
begin
  if (val = 0) then Result := 0
  else
    begin
      val := abs(val);
      factor := Power(10, 2 - ceil(log10(val)));
      Result := Round(val*factor)/factor;
    end;
end;

function Escort_3136A_device.validFloat(val: string): Boolean;
var
  fs: TFormatSettings;
  Value: Extended;
  theRes: Boolean;
begin
  theRes := False;
  Value := 0.0;
  fs := DefaultFormatSettings;
  fs.DecimalSeparator  := ',';
  if not(TryStrToFloat(val, Value, fs)) then // if failed, do the second attempt
    begin
      fs.DecimalSeparator  := '.';
      if TryStrToFloat(val, Value, fs) then theRes := True
      else theRes := False;
    end
  else theRes := True;
  Result := theRes;
end;

function Escort_3136A_device.FreqRangeI(freq: Real): Byte;
var
  thefRange: Byte;
begin
  thefRange := 0;

  if InRange(freq, 0, 30) then thefRange := 0
  else if InRange(freq, 30.001, 50) then thefRange := 1
  else if InRange(freq, 50.001, 2000) then thefRange := 2
  else if InRange(freq, 2000.001, 5000) then thefRange := 3
  else if InRange(freq, 5000.001, 20000) then thefRange := 4
  else if (freq > 20000.001) then thefRange := 5;

  Result := thefRange;
end;

function Escort_3136A_device.FreqRangeU(freq: Real): Byte;
var
  thefRange: Byte;
begin
  thefRange := 0;

  if InRange(freq, 0, 30) then thefRange := 0
  else if InRange(freq, 30.001, 50) then thefRange := 1
  else if InRange(freq, 50.001, 10000) then thefRange := 2
  else if InRange(freq, 10000.001, 30000) then thefRange := 3
  else if InRange(freq, 30000.001, 100000) then thefRange := 4
  else if (freq > 100000.001) then thefRange := 5;

  Result := thefRange;
end;

constructor Escort_3136A_device.Init(ComPort: string);
var
  MyForm: TForm;
  MyLabel: TLabel;
  str: string;
//  AppIni: TIniFile;
  FindFiles: TStringList;
//  i: Integer;
begin

// save the ComPort so we can use it later in error dialogs
  theComPort := ComPort;
  //i := 0;

// -- make the splash screen ---
  MyForm := TForm.Create(nil);
  with MyForm do
   begin
      SetBounds(0, 0, 450, 70);
      Position:=poDesktopCenter;
      BorderStyle := bsNone;
   end;
  MyForm.Color := $00EEEEEE;

  MyLabel := TLabel.Create(MyForm);
  with MyLabel do
   begin
     Align := alClient;
     Alignment := taCenter;
     Parent := MyForm;
     Visible := True;
   end;

  MyForm.Show;
  MyForm.BringToFront;

  MyLabel.Caption:= LineEnding + 'Initializing ' + theDevice + LineEnding + 'Connected to ' + ComPort + '...';
  SleepFor(100);

// Remove lock-file
  FindFiles := TStringList.Create;
  try
    FindAllFiles(FindFiles, '/var/lock', '*' + ExtractFileName(ComPort) + '*', true);
    if (FindFiles.Count = 1) then DeleteFile(FindFiles.Strings[0]);
  finally
    FindFiles.Free;
  end;

// Open serial communication
  ser := TBlockSerial.Create;
  try
    ser.RaiseExcept:=true;
    ser.Connect(ComPort);
    ser.config(9600, 8, 'N', SB1, False, False);
    Application.ProcessMessages;
    Application.ProcessMessages;
    if ser.lastError<>0 then
      showmessage('Error in communication while initializing ' +
                  theDevice + LineEnding + 'Connected to ' + ComPort + '...');
  finally
    ser.Purge;
  end;

// read numerical value for the first time so the next readings would be OK
// do not check the answers!
  try
    ser.SendString('R1'+CRLF);
    Application.ProcessMessages;
    if ser.lastError<>0 then showmessage('Error in communication after ser.SendString("R1"+CRLF)');
    if ser.canread(waitForAnswer) then
      str := ser.Recvstring(waitForRead);
      if (str = '=>') then
         if ser.canread(waitForAnswer) then str := ser.Recvstring(waitForRead);
  finally
    ser.Purge;
  end;

  MyForm.Close;
  FreeAndNil(MyForm);
end;

destructor Escort_3136A_device.Done;
var
  MyForm: TForm;
  MyLabel: TLabel;
begin

  Application.ProcessMessages;

  MyForm := TForm.Create(nil);
  with MyForm do
   begin
      SetBounds(0, 0, 450, 65);
      Position:=poDesktopCenter;
      BorderStyle := bsNone;
   end;
  MyForm.Color := $00EEEEEE;

  MyLabel := TLabel.Create(MyForm);
  with MyLabel do
   begin
     Align := alClient;
     Alignment := taCenter;
     Parent := MyForm;
     Caption:= LineEnding +'Shutdown ' + theDevice + LineEnding + 'Connected to ' + theComPort + '...';
     Visible := True;
   end;

  MyForm.Show;
  MyForm.BringToFront;
  SleepFor(50);

// send GTL command - close remote communication
  try
    ser.SendString('GTL'+CRLF);
  finally
    ser.Purge;
  end;

  ser.Free;
// give it some time so the window appears for a while
  SleepFor(200);

  MyForm.Close;
  FreeAndNil(MyForm);
end;

procedure Escort_3136A_device.GetData();
var
  str, str1, str2, str3, str4, str5: string;
  Code: Integer;
  tmpval: integer;
begin
  str := ''; str1 := ''; str2 := ''; str3 := ''; str4 := ''; str5 := '';
  ser.Purge;
  Repeat
    str := '';
    try
      ser.SendString('R0'+#13+#10);
      Application.ProcessMessages;
      if ser.canread(waitForAnswer) then str := ser.Recvstring(waitForRead);
    finally
      ser.Purge;
    end;
//  if (Length(str) <> 9) then showmessage('got ' + str + '!!!');
  Until (Length(str) > 0);

theTestStr := 'after R0 get: ' + str + LineEnding;
theTestStr := theTestStr + 'length: ' + inttostr(length(str)) + LineEnding;
theTestStr := theTestStr + 'first 2 chars: ' + copy(str, 1, 2) + LineEnding;
theTestStr := theTestStr + 'hex: ' + 'x' + copy(str, 1, 2) + LineEnding;
Val('x' + copy(str, 1, 2), tmpval, Code);
theTestStr := theTestStr + 'value:' + IntToStr(tmpval) + LineEnding;


// decoding f1 value
  Val('x' + copy(str, 8, 1), ModeCode, Code);    //  f1 := copy(str, 8, 1);
  if (Code = 0) then begin theModestr := ModeTable[ModeCode]; theUnitsSI := UnitsTable[ModeCode]; end
  else begin theModestr := ModeTable[3]; theUnitsSI := UnitsTable[3]; end;

// decoding r1 value
  Val('x' + copy(str, 9, 1), RangeCode, Code);    //  r1 := copy(str, 9, 1);
  if (Code = 0) then theRange := RangeTable[ModeCode, RangeCode]
  else theRange := RangeTable[0, 0];

// Read the 1st value
  Repeat
  str2 := '';
    try
      ser.SendString('R1'+#13+#10);
      Application.ProcessMessages;
      if ser.canread(waitForAnswer) then str2 := ser.Recvstring(waitForRead);
    finally
      ser.Purge;
    end;
  Until validFloat(str2);

  theValueSI := StrToFloat(str2);
  theRandErr := 0.01*RandErrTable[ModeCode, RangeCode]*theValueSI;
  theSystErr := SystErrTable[ModeCode, RangeCode];

  if (length(str) = 11) then // if we have the second value
    begin
// decoding f2 value
      Val('x' + copy(str, 10, 1), ModeCode2, Code);    //  f1 := copy(str, 8, 1);
      if (Code = 0) then begin theModestr2 := ModeTable[ModeCode2]; theUnits2SI := UnitsTable[ModeCode2]; end
      else begin theModestr2 := ModeTable[3]; theUnits2SI := UnitsTable[3]; end;

// decoding r2 value
      Val('x' + copy(str, 11, 1), RangeCode2, Code);    //  r1 := copy(str, 9, 1);
      if (Code = 0) then theRange2 := RangeTable[ModeCode2, RangeCode2]
      else theRange2 := RangeTable[0, 0];

// Read the 2nd value
      Repeat
        str2 := '';
        try
          ser.SendString('R2'+#13+#10);
          Application.ProcessMessages;
          if ser.lastError<>0 then showmessage('Error in communication after ser.SendString("R0"+CRLF)');
          if ser.canread(waitForAnswer) then str2 := ser.Recvstring(waitForRead);
        finally
          ser.Purge;
        end;
      Until validFloat(str2);

      theValue2SI := StrToFloat(str2);
      theRandErr2 := 0.01*RandErrTable[ModeCode2, RangeCode2]*theValueSI;
      theSystErr2 := SystErrTable[ModeCode2, RangeCode2];

// voltage AC uncertainties
      if (ModeCode = 1) or (ModeCode = 8) then
        begin
          theRandErr := 0.01*ACRandErrTable[ACModeCode[ModeCode], RangeCode, FreqRangeU(theValue2SI)]*theValueSI;
          theSystErr := ACSystErrTable[ACModeCode[ModeCode], RangeCode, FreqRangeU(theValue2SI)];
        end;

// current AC uncertainties
      if (ModeCode = 5) or (ModeCode = 9) then
        begin
          theRandErr := 0.01*ACRandErrTable[ACModeCode[ModeCode], RangeCode, FreqRangeI(theValue2SI)]*theValueSI;
          theSystErr := ACSystErrTable[ACModeCode[ModeCode], RangeCode, FreqRangeI(theValue2SI)];
        end;
    end;
end;


function Escort_3136A_device.GetValueSI(): Real;
begin
  Result := theValueSI;
end;

function Escort_3136A_device.GetRange(): string;
begin
  Result := theRange;
end;

function Escort_3136A_device.GetMode(): string;
begin
  Result := theModestr;
end;

function Escort_3136A_device.GetUnitsSI(): string;
begin
  Result := theUnitsSI;
end;

function Escort_3136A_device.GetUncertaintySI(): Real;
begin
  Result := RoundUnc(theRandErr + theSystErr);
end;

function Escort_3136A_device.GetRandUncertaintySI(): Real;
begin
  Result := RoundUnc(theRandErr);
end;

function Escort_3136A_device.GetValue2SI(): Real;
begin
  Result := theValue2SI;
end;

function Escort_3136A_device.GetUnits2SI(): string;
begin
  Result := theUnits2SI;
end;

function Escort_3136A_device.GetRange2(): string;
begin
  Result := theRange2;
end;

function Escort_3136A_device.GetMode2(): string;
begin
  Result := theModestr2;
end;

function Escort_3136A_device.GetUncertainty2SI(): Real;
begin
  Result := RoundUnc(theRandErr2 + theSystErr2);
end;

function Escort_3136A_device.GetRandUncertainty2SI(): Real;
begin
  Result := RoundUnc(theRandErr2);
end;

function Escort_3136A_device.GetTestStr(): string;
begin
  Result := theTestStr;
end;


procedure Escort_3136A_device.ResetDevice;
var
  str: string;

begin
  try
    ser.SendString('RST'+CRLF);
    if ser.lastError=0 then
      if ser.canread(waitForAnswer) then str := ser.Recvstring(waitForRead)
      else showmessage('Err in cummunication!');
  finally
    ser.Purge;
  end;

  SleepFor(600); // wait a little bit until the numeric value is available
end;


{ Escort_3136A_device }


end.

