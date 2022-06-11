//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit gps_sensor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Sensors,
  System.Sensors.Components, Vcl.ExtCtrls;

type
  TForm5 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    lsLocation: TLocationSensor;
    msAccelerometr: TMotionSensor;
    osCompass: TOrientationSensor;
    osInclinometer: TOrientationSensor;
    tOrientation: TTimer;
    lCompass: TLabel;
    lInclinometr: TLabel;
    Button1: TButton;
    GroupBox4: TGroupBox;
    Panel1: TPanel;
    bSwitch: TButton;
    lCoordinates: TLabel;
    lAddress: TLabel;
    lGeoCoordinates: TLabel;
    tMotion: TTimer;
    lAccel: TLabel;
    procedure osCompassSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    procedure osInclinometerSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    procedure msAccelerometrSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);

    procedure tOrientationTimer(Sender: TObject);
    procedure bSwitchClick(Sender: TObject);
    procedure lsLocationLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
    procedure tMotionTimer(Sender: TObject);
  private
    { Private declarations }
    //FGeocoder : TGeocoder;
    //procedure OnGeocodeReverseEvent(const Address: TCivicAddress);
    //procedure OnGeocodeEvent(const Coords: TArray<TLocationCoord2D>);
  public
    { Public declarations }
  end;


procedure StartGPS;

implementation

{$R *.dfm}


uses
   LocationApiLib_TLB,
   Petmar,Petmar_types;

var
  LocationDllHandle : THandle;


function IsLocationAPIAvailable: Boolean;
begin
  Result := False;
  LocationDllHandle := LoadLibrary('LocationAPI.dll');
  if LocationDllHandle<>0 then begin
    Result := True;
    FreeLibrary(LocationDllHandle);
  end;
end;


(*
  TLocation = class(TOleServer)
  private
    FIntf: ILocation;
    function GetDefaultInterface: ILocation;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILocation);
    procedure Disconnect; override;
    function RegisterForReport(const pEvents: ILocationEvents; var reportType: TGUID; dwRequestedReportInterval: LongWord): HResult;
    function UnregisterForReport(var reportType: TGUID): HResult;
    function GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult;
    function GetReportStatus(var reportType: TGUID; out pStatus: LOCATION_REPORT_STATUS): HResult;
    function GetReportInterval(var reportType: TGUID; out pMilliseconds: LongWord): HResult;
    function SetReportInterval(var reportType: TGUID; millisecondsRequested: LongWord): HResult;
    function GetDesiredAccuracy(var reportType: TGUID; out pDesiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult;
    function SetDesiredAccuracy(var reportType: TGUID; desiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult;
    function RequestPermissions(var hParent: _RemotableHandle; var pReportTypes: TGUID; count: LongWord; fModal: Integer): HResult;
    property DefaultInterface: ILocation read GetDefaultInterface;
  published
  end;
*)


(*
  TGpsStatus = class abstract(TObject)
  private class var
    FCurrent: TGpsStatusClass;

    class function GetCurrent: TGpsStatusClass; static; inline;
  protected
    // raises a GPS exception
    class procedure GpsStatusError(const Msg: String);

    // descendants return the class the implements the Gps status functionality
    class function GetGpsStatusImplementer: TGpsStatusClass; virtual; abstract;
    // issued when the class is first used through the Current property; if
    // some finalization must be done, descendants can use the class destructor
    class procedure Initialize; virtual;
  public
    // determines whether the device supports querying the Gps status
    class function Supported: Boolean; virtual; abstract;
    // determines whether the application is authorized to use the service
    class function Authorized: TAuthorizationType; virtual; abstract;
*)


procedure StartGPS;
var
   Form5 : TForm5;
   Lat,Long : float64;
   //Loc : tLocation;
   //LocReport : ILocationReport;
   LatLongReport : tLatLongReport;
   TStr : shortstring;
   Code : LongInt;
   //GPSStatus : tGPSStatus;
begin
   Form5 := TForm5.Create(Application);
   Form5.FormStyle := fsStayOnTop;
   Form5.Show;
   Form5.bSwitchClick(Nil);

   //Form5.lsLocation.UsageAuthorization := atAuthorized;

   WriteLineToDebugFile('LocationAPIAvailable: ' + TrueOrFalse(IsLocationAPIAvailable));

   //GPSStatus := tGPSStatus.Create;
   //WriteLineToDebugFile('GPSStatus.Supported: ' + TrueOrFalse(GPSStatus.Supported));
   //WriteLineToDebugFile('GPSStatus.Authorized: ' + IntToStr(ord(GPSStatus.Authorized)));

(*
   Loc.Create(application);
   Loc.Connect;
   Loc.GetReport(IID_ILocationReport,LocReport);

   LocReport.GetLatitude(Lat);
   LocReport.GetLongitude(Long);
*)
   try
      Code := 0;
      WritelineToDebugFile('point 1');
      LatLongReport := TLatLongReport.Create(Application);
      TStr := 'created';
      WritelineToDebugFile(TStr);

      LatLongReport.Connect;
      TStr := 'connected, code=' + IntToStr(Code);
      WritelineToDebugFile(TStr);

      Code := LatLongReport.GetLatitude(Lat);
      TStr := 'Lat OK, code=' + IntToStr(Code);
      WritelineToDebugFile(TStr);

      Code := LatLongReport.GetLongitude(Long);
      WritelineToDebugFile(TStr);

      WritelineToDebugFile(LatLongDegreeToString(Lat,Long,DecDegrees));
   except
      on Exception do WritelineToDebugFile('Failed to get lat/long ' + TStr);
   end;


(*
   Loc.GetReport(IID_ILatLongReport,LatLongReport);
   LatLongReport.GetLatitude(Lat);
   LatLongReport.GetLongitude(Long);
*)
   //LocationApiLib_TLB.DispLatLongReport.
    //LocationApiLib_TLB.ILatLongReport.GetLatitude(Lat);
   // LocationApiLib_TLB.ILatLongReport.GetLongitude(Long);

end;

procedure TForm5.bSwitchClick(Sender: TObject);
begin
  if bSwitch.Caption = 'Turn On' then begin
    bSwitch.Caption := 'Turn off';
    lsLocation.Active := True;
    msAccelerometr.Active := True;
    osCompass.Active := True;
    osInclinometer.Active := True;
    //lsLocation.DoStart;
  end
  else begin
    bSwitch.Caption := 'Turn On';
    lsLocation.Active := False;
    msAccelerometr.Active := False;
    osCompass.Active := False;
    osInclinometer.Active := False;
  end;
end;

procedure TForm5.lsLocationLocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
begin
  lCoordinates.Caption := Format('Latitude : %2.7f; Longitude : %2.7f', [NewLocation.Latitude, NewLocation.Longitude]);
end;

procedure TForm5.msAccelerometrSensorChoosing(Sender: TObject;  const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  I : integer;
begin
  for I := 0 to Length(Sensors) - 1 do
    if (Sensors[I] as TCustomMotionSensor).SensorType = TMotionSensorType.Accelerometer3D then begin
      ChoseSensorIndex := I;
      Break;
    end;
end;


procedure TForm5.osCompassSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  I : integer;
begin
  for I := 0 to Length(Sensors) - 1 do
    if (Sensors[I] as TCustomOrientationSensor).SensorType = TOrientationSensorType.Compass3D then begin
      ChoseSensorIndex := I;
      Break;
    end;
end;

procedure TForm5.osInclinometerSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  I : integer;
begin
  for I := 0 to Length(Sensors) - 1 do
    if (Sensors[I] as TCustomOrientationSensor).SensorType = TOrientationSensorType.Inclinometer3D then begin
      ChoseSensorIndex := I;
      Break;
    end;
end;

procedure TForm5.tMotionTimer(Sender: TObject);
begin
  if msAccelerometr.Sensor <> nil then
    lAccel.Caption := Format('AccelX : %-1.5f'#13#10'AccelY : %-1.5f'#13#10'AccelZ : %-1.5f',[msAccelerometr.Sensor.AccelerationX, msAccelerometr.Sensor.AccelerationY, msAccelerometr.Sensor.AccelerationZ] )
  else
    lAccel.Caption := '';
end;

procedure TForm5.tOrientationTimer(Sender: TObject);
begin
  if osCompass.Sensor <> nil then lCompass.Caption := Format('Heading : %3.1f',[osCompass.Sensor.CompMagHeading])
  else lCompass.Caption := '';
  if osInclinometer.Sensor <> nil then lInclinometr.Caption := Format('TiltX : %-3.5f '#13#10'TiltY : %-3.5f '#13#10'TiltZ : %-3.5f ',[osInclinometer.Sensor.TiltX, osInclinometer.Sensor.TiltY, osInclinometer.Sensor.TiltZ])
  else lInclinometr.Caption := '';
end;



end.





