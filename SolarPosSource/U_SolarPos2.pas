unit U_SolarPos2;


 {Copyright  © 2003-2005, Gary Darby,  www.DelphiForFun.org
  This program may be used or modified for any non-commercial purpose
  so long as this original notice remains in place.
  All other rights are reserved
  }

 {SunPos displays solar position and sunrise/sunset infor for any given date
 and time at any given location on earth.
 Also displays a plot of the shadow analemma for a location and time of day
 }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, shellapi;

type

  T3DVector=record
    x,y,z:extended;
  end;

  TDrawMode=(sunlines, analemma, none );

  TSolorPosForm1 = class(TForm)
    ShowBtn: TButton;
    Memo1: TMemo;
    AnalemmaBtn: TButton;
    Panel1: TPanel;
    Label2: TLabel;
    LongEdt: TEdit;
    EWRGrp: TRadioGroup;
    NSRGrp: TRadioGroup;
    LatEdt: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    DatePicker: TDateTimePicker;
    Label8: TLabel;
    Label9: TLabel;
    TimePicker: TDateTimePicker;
    DLSRGrp: TRadioGroup;
    PBox: TPaintBox;
    AnTypegrp: TRadioGroup;
    StaticText1: TStaticText;
    TzBox: TComboBox;
    ImageLbl: TLabel;
    GetSystimeBtn: TButton;
    Label4: TLabel;
    Edit1: TEdit;
    Label5: TLabel;
    procedure ShowBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AnalemmaBtnClick(Sender: TObject);
    procedure DatePickerUserInput(Sender: TObject; const UserString: String; var DateAndTime: TDateTime;
      var AllowChange: Boolean);
    procedure PBoxPaint(Sender: TObject);
    procedure LongEdtExit(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure GetSystimeBtnClick(Sender: TObject);
    procedure PlotLastClick(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }

    Adate:TDateTime;
    JDate:integer;
    tzHours:extended; {time zone add to Greewich time. west = neg}
    dlshours:integer;  {Daylight savings - hours added to Local time}
    LocalTime:TDateTime;
    drawmode:TDrawMode;
    long,lat:extended;
    LastPlot:char;  {'A' for Analemma, 'S' for SunRise/Sunset'}

    {Variables used or set by SunPos function}

    {Sunpos outputs}
    sunrise :single; { Sunrise time                 }
    sunset:single; { Sunset time                  }
    Civilsunrise :single; { Civil Sunrise time  6 degrees below horizon }
    Civilsunset:single; { Civil Sunset time - 6 degrees below horizon   }
    sunriseangle, sunsetangle:extended;
    az,alt:extended;
    midday,middayaz,middayalt:extended;
    errmsg, civilerrmsg:string;
    Hourangle:extended;
    declination:extended;     { Angle of declination         }
    EqOfTimeMinutes:extended;

    {Drawing variables}
    cx,cy,clen:integer;
    anpoints:array of tPoint;  {analemma points for drawing}
    anscale:single; {analemma drawing scaling factor}
    anplotx, anploty:integer; {coordinates for plot of "date" on analemma}

    function sunpos( longitude, latitude, hrtime, tz:extended; day:integer; var pos:T3DVector):boolean;
    procedure drawsunlines;
    function GetBaseData:boolean;
 end;


var
  SolorPosForm1: TSolorPosForm1;

implementation

{$R *.DFM}
Uses
   Petmar,PetDBUtils,Nevadia_Main,Petmar_types,DEMDefs,DEMDatabase,
   math;

{************************** Local Routines ******************}
  function GetTimeZone(aStr: String): extended;
  {extract Time zone offset from a string in TZbox.item format}
    begin
      Result := StrToInt(copy(aStr, 4, 3));
      if copy(aStr, 8, 2) = '30' then
        if Result < 0 then
          Result := Result - 1/2
        else
          Result := Result + 1/2;
    end;

  {degree trig functions}
  function cosd(t:extended):extended;
  begin  result:= cos(0.01745329251994 * t); end;

  function sind(t:extended):extended;
  begin  result:= sin(0.01745329251994 * t); end;

  function tand(t:extended):extended;
  var  x:extended;
  begin
    x:=degtorad(t);
    result:= tan(x);
  end;



  {Angle/time  to/from string}

       {**************** AngleToStr ***************}
       function AngleToStr(angle:extended):string;
       var
         D:integer;
         M,S:extended;
       begin
         d:=Trunc(angle);
         m:=abs(frac(angle)*60);
         s:=frac(M)*60;
         m:=int(M);
         if s>=60-1e-10 then
         begin
           s:=s-60;
           m:=m+1;
         end;
         if M=60 then
         begin
           m:=0;
           if d>=0 then inc(d) else dec(d);
         end;
         result:=format('%3d %2d %5.2f',[d,trunc(m),s]);
       end;

       {****************** StrToAngle ******************}
       function StrToAngle(s:string; var angle:extended):boolean;
       var
         n, sign:integer;
         w,ds,ms,ss:string;
       begin
         result:=true;
         ds:='0';
         ms:='0';
         ss:='0.0';
         w:=ptTrim(s);
         if w[1]='-' then
         begin
           sign:=-1;
           delete(w,1,1);
         end
         else sign:=1;
         n:=pos(' ',w);
         if n>0 then
         begin
           ds:=copy(w,1,n-1);
           delete(w,1,n);
           w:=ptTrim(w);
           n:=pos(' ',w);
           if n>0 then
           begin
             ms:=copy(w,1,n-1);
             delete(w,1,n);
             ss:=ptTrim(w);
           end
           else ms:=w;
         end
         else ds:=w;
         try
           angle:=sign*(strtoint(ds)+strtoint(ms)/60+strtofloat(ss)/3600);
         except
           result:=false;
           angle:=0;
         end;
       end;

      {Day of year function}
      function JulianDay(d:TDateTime):integer;
      var
        y,m,day:word;
      begin
        decodedate(d,y,m,day);
        result:=trunc(d-encodedate(y-1,12,31));
      end;

(*
/* ---- sunpos - Compute the position of the sun. --------------------- */
/*                                                                      */
/*                                                                      */
/*      Description:                                                    */
/*          Sunpos computes the position of the sun given the location  */
/*          (longitude and latitude) on the earth, the day of the year, */
/*          and the time.  The origin (i.e. 0,0,0 in world space) is    */
/*          the specified location on the earth.  With an orientation   */
/*          angle of 0, east = -z, west = +z, south = +x, north = -x.   */
/*                                                                      */
/*      On entry:                                                       */
/*          lon, lat= The longitude and latitude in degrees of the      */
/*                    location of the origin on the earth.              */
/*                    (i.e., West Lafayette, Indiana = 89-40)           */
/*          lontz   = The longitude for the time zone in degrees.       */
/*                    Each time zone is 15 degrees, advancing to the    */
/*                    west.  For daylight savings, the longitude is     */
/*                    advanced 1 timezone to the east.                  */
/*                    (i.e., 75 for EST, 90 for CST, 75 for CDT)        */
/*          time    = The time of the day in hours.                     */
/*                    (i.e., 13.5 = 1:30 pm)                            */
/*          day     = The day of the year.                              */
/*                    (i.e., 172 for June 21st)                         */
/*          gamma   = Orientation angle defining south, rotating the    */
/*                    sweep of the sun.                                 */
/*                    (i.e., 14 = 14 degrees east of south)             */
/*                                                                      */
/*      On return:                                                      */
/*          pos     = Direction cosines of a vector pointing toward     */
/*                    the sun from the origin.                          */
/*                                                                      */
/*      Returns:  True if the sun is visible.                           */
/*                                                                      */
/*      Author:   Joe Cychosz                                           */
/*                Purdue University CADLAB                              */
/*                Potter Engineering Center                             */
/*                W. Lafayette, IN  47907                               */
/*                                                                      */
/* -------------------------------------------------------------------- */

boolean sunpos          ( lon, lat, lontz, time, day, gamma, pos)

        double          lon, lat;       /* Longitude, latitude          */
        double          lontz;          /* Longitude for timezone       */
        double          time;           /* Time of day                  */
        int             day;            /* Day of the year              */
        double          gamma;          /* Orientation angle            */
        Vector3         *pos;           /* Position vector of sun       */

{                       /* Angles are in degrees, times are in hours.   */
        double          del;            /* Angle of declination         */
        double          b;              /* Time angle                   */
        double          e;              /* Time                         */
        double          w;              /* Hour angle                   */
        double          we, gs;
        double          phi;
        double          ws;             /* Sunrise, sunset angle        */
        double          sunrise;        /* Sunrise time                 */
        double          sunset;         /* Sunset time                  */

/*      Compute solar declination (23.45 deg on June 21)                */
        del = 23.45 * sind (360.0 * (284+day) / 365);

/*      Compute the hour angle in degrees.                              */
/*                                                                      */
/*      w = 0 at solar noon, => 180 at midnight, + for am, - for pm.  */
/*      1 hour = 15 degrees, w = 22.5 at 10:30 am (solar)               */
        b   = 360.0 * (day-81) / 364;
        e   = (9.87*sind(2.0*b) - 7.53*cosd(b) - 1.5*sind(b)) / 60.;
        w   = 180.0 - lontz + lon - 15.0*(time+e);

        phi = acos (sind(lat)*sind(del) + cosd(lat)*cosd(del)*cosd(w));
        we  = acos (tand(del) / tand(lat)) * rad2deg;
        gs  = asin (cosd(del) * sind(w) / sin(phi));
        if  ( w >  we ) gs =  pi - gs;
        if  ( w < -we ) gs = -pi - gs;

/*      Compute the position of the sun.                                */
/*                                                                      */
/*      For gamma = 0, the sun will rise at -z and set at +z.           */
        pos->x =   cos (gs + gamma*deg2rad);
        pos->y =   cos (phi);
        pos->z = - sin (gs - gamma*deg2rad);

/*      Compute sunrise, sunset angle.                                  */
        ws = rad2deg * acos (-tand(lat) * tand(del));

/*      Compute sunrise time and sunset time.                           */
        sunrise = (180.0 - ws - lontz + lon) / 15.0 - e;
        sunset  = (180.0 + ws - lontz + lon) / 15.0 - e;

        printf (" sunpos: sun position: 0.000000 0.000000 0.000000\n", pos->x, pos->y, pos->z);
        printf (" sunpos: sunrise time: 0.000000\n", sunrise);
        printf (" sunpos: sunset  time: 0.000000\n", sunset);

        return (time >= sunrise && time <= sunset);
}
*)

(*


/*      Compute solar declination (23.45 deg on June 21)                */
        del = 23.45 * sind (360.0 * (284+day) / 365);

/*      Compute the hour angle in degrees.                              */
/*                                                                      */
/*      w = 0 at solar noon, => 180 at midnight, + for am, - for pm.  */
/*      1 hour = 15 degrees, w = 22.5 at 10:30 am (solar)               */
        b   = 360.0 * (day-81) / 364;
        e   = (9.87*sind(2.0*b) - 7.53*cosd(b) - 1.5*sind(b)) / 60.;
        w   = 180.0 - lontz + lon - 15.0*(time+e);

        phi = acos (sind(lat)*sind(del) + cosd(lat)*cosd(del)*cosd(w));
        we  = acos (tand(del) / tand(lat)) * rad2deg;
        gs  = asin (cosd(del) * sind(w) / sin(phi));
        if  ( w >  we ) gs =  pi - gs;
        if  ( w < -we ) gs = -pi - gs;

/*      Compute the position of the sun.                                */
/*                                                                      */
/*      For gamma = 0, the sun will rise at -z and set at +z.           */
        pos->x =   cos (gs + gamma*deg2rad);
        pos->y =   cos (phi);
        pos->z = - sin (gs - gamma*deg2rad);

/*      Compute sunrise, sunset angle.                                  */
        ws = rad2deg * acos (-tand(lat) * tand(del));

/*      Compute sunrise time and sunset time.                           */
        sunrise = (180.0 - ws - lontz + lon) / 15.0 - e;
        sunset  = (180.0 + ws - lontz + lon) / 15.0 - e;

        printf (" sunpos: sun position: 0.000000 0.000000 0.000000\n", pos->x, pos->y, pos->z);
        printf (" sunpos: sunrise time: 0.000000\n", sunrise);
        printf (" sunpos: sunset  time: 0.000000\n", sunset);

        return (time >= sunrise && time <= sunset);
 *)

 var deg2rad:extended=pi/180;
     rad2deg:extended=180/pi;


{****************************   SUNPOS *****************************}
function TSolorPosForm1.sunpos( longitude, latitude, hrtime, tz:extended;
                 day:integer; var pos:T3DVector) : boolean;
  {
  /*  Adapted from C code written by :
  /*                                                                      */
  /*                Joe Cychosz                                           */
  /*                Purdue University CADLAB                              */
  /*                Potter Engineering Center                             */
  /*                W. Lafayette, IN  47907                               */
  /*  http://jedi.ks.uiuc.edu/~johns/raytracer/rtn/rtnv7n2.html#art23     */
  /* -------------------------------------------------------------------- */
  }

     function eqoftime(jday:integer):extended;
     {Equations of time - hours that solar differs from mean time an any day}
     var b:extended;
     begin
        b:= 360.0 * (day-81) / 364;
        result:=(9.87*sind(2.0*b) - 7.53*cosd(b) - 1.5*sind(b)) / 60.;  {eq of time}
     end;



 var
    lontz:extended;   { time zone longitude}
    e:extended;       { Time                         }
    gs:extended;
    phi:extended;     {altitude }
    cws, ws,{civilws,} civilcws:extended;      { Sunrise, sunset angle        }
    x:extended;

   procedure getRiseSet(wsx:extended; var sunrisex,sunsetx:single);
   var x:extended;
   begin
     x:=(180.0 - wsx + lontz - longitude);

     while x<0 do x:=x+360;
     sunrisex:=  x/ 15.0 - e + DLSHours;
     {if sunrise<0 then sunrise:=sunrise+24;}
       x:=(180.0 + wsx + lontz - longitude);
       while x<0 do x:=x+360;
       sunsetx:=  x/ 15.0 - e + DLSHours;
      { result:=(hrtime >= sunrise) and (hrtime <= sunset);}
    end;


begin
   result:=false;
   errmsg:='';
   civilerrmsg:='';
   lontz:=tz*15; {time zone in degrees}
   e:=eqoftime(day);
   midday:=12-long/15+tzhours-e;
   while midday<0 do midday:=midday+24.0;
   {sunpos(longitude, latitude, midday, tz,day,temppos);}
   declination := 23.45 * sind (360.0 * (284+day) / 365.25);
   Hourangle:=180.0 + lontz - longitude - 15.0*(midday+e);
   phi:=arccos(sind(latitude)*sind(declination) + cosd(latitude)*cosd(declination)*cosd(Hourangle));{alt}
   if latitude<0 then hourangle:=hourangle + 180;
   gs:=arcsin(cosd(declination) * sind(HourAngle) / sin(phi)); {az}
   middayaz:=180-radtodeg(gs);
   middayalt:=90-radtodeg(phi);
   midday:=midday+dlshours;

   {Current position calculations}

   {Compute solar declination (23.45 deg on June 21) }
   declination := 23.45 * sind (360.0 * (284+day) / 365.25);
   {Compute the hour angle in degrees. }
   Hourangle:=180.0 + lontz - longitude - 15.0*(hrtime-dlshours+e);
   //sin(ALT) = sin(DEC)*sin(LAT)+cos(DEC)*cos(LAT)*cos(HA)
   //ALT = asin(ALT)
   Alt:=radtodeg(arcsin(sind(declination)*sind(latitude)+cosd(declination)*cosd(latitude)*cosd(hourangle)));

   //               sin(DEC) - sin(ALT)*sin(LAT)
   //cos(A)   =   ---------------------------------
   //                   cos(ALT)*cos(LAT)
  //A = acos(A)

 gs:=arccos((sind(declination)-sind(alt)*sind(latitude))/(cosd(alt)*cosd(latitude)));
 az:=radtodeg(gs);
  //If sin(HA) is negative, then AZ = A, otherwise AZ = 360 - A
  If sind(Hourangle)<0{>0}
  then az:=360-az;
   { Compute the position of the sun. }
   If sind(Hourangle)<0 then gs:=2*pi-gs;
   pos.x :=  sin (gs);    {East}
   pos.y :=   -cos (gs);  {North}
   pos.z :=   cos (phi);                    {Up}
   EqOfTimeMinutes:=e*60;

   x:=-tand(Declination)*tand(Latitude) ; {replacement}
   cws:=x+cosd(90.833)/(cosd(latitude)*cosd(declination));
   Civilcws:=x+cosd(96)/(cosd(latitude)*cosd(declination));
   if cws<-1 then begin
      errmsg:='Sun never sets';
      sunrise:=0;
      sunset:=24;
   end
   else if cws>1 then begin
      errmsg:='Sun never rises';
      sunrise:=0;
      sunset:=0;
   end
   else
   begin
     result:=true;
     {Compute sunrise, sunset times and angles}
     ws:=radtodeg(arccos(cws));
     getRiseSet(ws, sunrise,sunset);
     phi:=arccos(sind(declination)/cosd(latitude));
     sunriseangle:=radtodeg(phi);
     sunsetangle:=360-sunriseangle;
   end;
   if civilcws<-1 then Civilerrmsg:='no Civil Sunset'
   else if civilcws>1 then Civilerrmsg:='no Civil Sunrise'
   else
   begin
     Ws:=radtodeg(arccos(civilcws));
     getriseset(ws, civilsunrise,  civilSunset);
   end;
 end;


{**************** FormActivate ****************}
procedure TSolorPosForm1.FormActivate(Sender: TObject);
begin
  tag:=1;   {stop auto sunposition compute as fields are set}
  {GetsystimebtnClick(sender);  }

  TZBox.itemindex:=7;
  datepicker.date:=date;

  drawmode:=none;
  doublebuffered:=true;
  cx := pbox.width div 2;
  cy := pbox.height div 2;
  clen:=3*pbox.width div 8; {lines 3/4 out from center}
  tag:=0;
end;

{******************** DatePickerUserInput  *********}
procedure TSolorPosForm1.DatePickerUserInput(Sender: TObject;
  const UserString: String; var DateAndTime: TDateTime;
  var AllowChange: Boolean);
{to overcome some default date entry edit problems}
begin
  try
    DateAndTime:=strtodate(userstring);
  except
  end;
end;

{********************** GetBaseData ***************}
function TSolorPosForm1.GetBaseData:boolean;
begin
  begin
    result:=true;
    Adate:=datepicker.date;
    JDate:=Julianday(Adate);
    with tzbox do
    if itemindex>=0 then tzHours := GetTimeZone(Items[ItemIndex])
    else TzHours:=0;

    if strtoangle(longedt.text,long) then begin
      Long:=abs(Long);
      if EWRGrp.itemindex>0 then Long:=-Long;
    end
    else begin
       showmessage('Invalid longitude, must be 1 to 3 numbers separated by spaces');
       result:=false;
    end;
    if strtoangle(latedt.text,Lat) then  begin
      Lat:=abs(Lat);
      if NSRGrp.itemindex>0 then Lat:=-Lat;
    end
    else begin
      showmessage('Invalid latitude, must be 1 to 3 numbers separated by spaces');
      result:=false;
    end;
    dlshours:=DLSRGrp.itemindex;
    LocalTime:=(timepicker.time);
  end;
end;

procedure TSolorPosForm1.ShowbtnClick(Sender: TObject);
var
   {Direction cosines of a vector pointing toward the sun from the origin.}
   pos:T3dVector;
begin
  if tag>0 then exit; {don't compute during initialization}
  If GetBaseData then begin
    Lastplot:='S';
    if drawmode=analemma then memo1.lines.clear;
    sunpos(long,lat,Timepicker.time*24,tzHours, Julianday(datepicker.date), pos);
    {if errmsg='' then} drawsunlines;
    with memo1,lines do begin
      add('**********************************');
      add('SUNPOS: Longitude:'+angletostr(long) +', Latitude:' + angletostr(Lat)
                   + '  '+datetostr(datepicker.date)
                   );
      if errmsg='' then begin
        add('Sunrise:     ' +formatdatetime('hh:nn AM/PM',sunrise/24)
                +format(', Azimuth %6.1f degrees from North',[sunriseangle]));
        add('Sunset:      ' +formatdatetime('hh:nn AM/PM',sunset/24)
                +format(', Azimuth %6.1f degrees from North',[sunsetangle]));
      end
      else  add('For '+datetostr(datepicker.date)+' the sun '+errmsg);
      If civilerrmsg='' then begin
        add('Civil Twilight - AM (For VFR pilots  - 6 degrees below horizon) :     '
                +formatdatetime('hh:nn AM/PM',Civilsunrise/24));
        add('Civil Twilight - PM (For VFR pilots - 6 degrees below horizon:      '
                +formatdatetime('hh:nn AM/PM',civilsunset/24));
      end
      else  add('Civil Twilight - For '+datetostr(datepicker.date)+ ' there is '+ civilerrmsg);
      add('Solar noon: ' + timetostr(midday/24)+', Azimuth: '
               +angletostr(middayaz)+', Altitude:'+angletostr(middayalt));
         add(format('Eq of time:  %4.2f minutes',[EqOfTimeMinutes])
                  +' Amount that sun position varies from average because '
                  +' earth''s equator and orbit do not lie in the same plane and the '
                  +'because the earth''s orbit is slightly elliptical.');
      add('Declination: ' + angletostr(declination)+' degrees(D M S).  The angle of the sun today '
           +'if observed from the equator (because the earth''s axis is tilted relative to '
           +'it''s orbit)');
         add('');
      add(timetostr(timepicker.time) + format(' local time,  Azimuth: %5.1f, Altitude: %5.1f,  ',[az,alt]));
      add(format('Position Vector (E,N,Up): %6.3f,%6.31f,%6.3f', [pos.x, pos.y, pos.z]));
    end;
    imagelbl.caption:='Sun path mapped onto earth''s surface and viewed from directly above current Latitude';
  end;
end;


{********************* AnalemmaBtnClick **************************}
procedure TSolorPosForm1.AnalemmaBtnClick(Sender: TObject);
{calculate the analemma as a shadow cast by a vertical pole
 plot top = south}
var
  i,n,nx,ny:integer;
  dayinc:integer;
  pos:T3DVector;
  {L:extended; }
  Hour:extended;
  xmax, ymax, xmin, ymin:integer;
  range:integer;
  nbrpoints:integer;
  anscaley,anscalex:extended;
  adjustx, adjusty:integer;
  pointto:extended;

        procedure makepoint(az,alt:extended; var x,y:integer);
          var
            L:extended;
          begin
          case antypegrp.itemindex of
            0: {shadow}
            begin
              L:=1000/tand(alt); {defines a circle (x^2+y^2=L^2) upon which lies the point}
              x:=-trunc(L*sind(az-180));
              y:=trunc(L*cosd(az-180));

            end;
            1: {camera south}
            begin
              x:=trunc(1000*sind(180-az));
              y:=trunc(1000*cosd(alt));
            end;
            2: {camera at sun on input date time}
            begin
              x:=trunc(1000*sind(pointto-az));
              y:=trunc(1000*cosd(alt));
            end;
          end; {case}
        end;

begin
  if getbasedata then
  Begin
    sunpos(long,lat,Timepicker.time*24,
            tzHours, Julianday(datepicker.date), pos);
    with memo1.lines do
    begin
      Lastplot:='A';
      clear;
      add('An analemma describes the shape of a figure formed by the sun''s');
      add('position in the sky if observed at the same time each day for a year.');
      add('');
      add('A common way to do this is to plot the tip of the shadow cast on the');
      add('ground by vertical stick.  This plot simulates that shadow as well as');
      add('a camera view with camera fixed pointing south or rotated to sun postition.');
      add('Camera fixed south produces "fisheye" lens effect for early or late observation times');
      add('');
      add('The analemma looks best when plotted for hours around noon.  It is');
      add('automatically scaled and panned to fit in the plot area but when the sun gets near');
      add('the horizon, shadow lengths become very large.  This can result in');
      add('truncated plot results');
      add('');
      add('The Red dot on analemma reflects the sun''s position on the date and time');
      add('entered in the input area');
    end;

    drawmode:=analemma;
    dayinc:=5; {sample interval in days}
    hour:=frac(localtime)*24; {which hour of the day to plot}
    nbrpoints:= 365 div dayinc+1;
    setlength(anpoints, nbrpoints);
    xmax:=0; ymax:=0;
    xmin:=maxint; ymin:=maxint;
    sunpos(long, Lat,hour, trunc(tzhours),  JDate, pos);
    pointto:=az;  {direction of camera}
    {plotindex:= trunc(0.0+round(JDate / dayinc));}
    n:=0;
    i:=0;
    while i<nbrpoints do
    begin
      inc(i);

         sunpos(long, Lat,hour, trunc(tzhours),  dayinc*i, pos);
        {only generate points if altiude > 0
         - before shadow tries to go to infinite length}
      if alt>0 then begin
        inc(n);

        with anpoints[n-1] do begin
          makepoint(az,alt,x,y);
          xmax:=max(x,xmax);
          ymax:=max(y,ymax);
          xmin:=min(x,xmin);
          ymin:=min(y,ymin);
        end;
      end;
    end;
    setlength(anpoints,n);
    {set scale}
    range:=ymax-ymin;
    anscaley:=0.9*pbox.height/range;
    range:=xmax-xmin;
    anscalex:=0.9*pbox.width/range;
    anscale:=min(anscalex,anscaley);
    if anscale<0.01 then anscale:=0.01;

    adjustx:=(xmax+xmin) div 2;
    adjusty:=(ymax+ymin) div 2;
    {and scale the values for plotting}
    for i:= 0 to high(anpoints) do
    with anpoints[i] do begin
      x:=cx+trunc(anscale*(x-adjustx));
      y:=cy+trunc(anscale*(y-adjusty));
    end;
    {calc plot coordinates for date entered by user}
    sunpos(long, Lat,hour, trunc(tzhours),  JDate, pos);
    makepoint(az,alt,nx,ny);
    anplotx:=cx+trunc(anscale*(nx-adjustx));
    anploty:=cy+trunc(anscale*(ny-adjusty));
  end;
  pbox.invalidate;

  with ImageLbl do
  case antypegrp.itemindex of
    0: {shadow}  caption:='Shadow analemma';
    1: {camera south}  caption:='Analemma - Camera facing South';
    2: caption:='Analemma - Camera facing sun on input date & time';
  end; {case}
end;

{******************** DrawSumLines *************}
procedure TSolorPosForm1.drawsunlines;
 begin
    drawmode:=sunlines;
    pbox.invalidate;
 end;

{****************** PBoxPaint ****************}
procedure TSolorPosForm1.PBoxPaint(Sender: TObject);
var
  i:integer;
  Rad:integer;
  CosA1,SinA1:extended;
  c,x,y:integer;
  t,a:extended;
  pos:T3Dvector;
  sign:integer;
  //PLG additions
  outf : tStringList;
  fName : PathStr;
  tinc : float64;


     procedure FigureTime;
     begin
         sunpos(long,lat,T,tzHours, Julianday(datepicker.date),pos);
         outf.Add(RealToString(t,-8,2) + ',' + RealToString(az,-8,1) + ',' +  RealToString(alt,-8,1));
         //memo1.lines.add(format('T:%4.1f, Az:%5.1f, Alt:%5.1f,HA:%4.1f',[t,az,alt,hourangle]));
     end;

begin
  if drawmode=none then exit;
  with pbox.canvas do begin
    If drawmode=sunlines then begin
      Rad:=clen;
      pen.color:=clblack;
      brush.color:=clblack;
      rectangle(rect(0,0,width-1,height-1));
      brush.color:=$E2C257; {cllightblue;}
      ellipse(cx-Rad, cy-Rad, cx+Rad, cy+Rad);
      brush.color:=clblack;
      font.color:=clwhite;
      font.size:=12;
      font.style:=[fsbold];
      textout(cx-10, cy-Rad-25,'N');
      textout(cx-10, cy+Rad,'S');
      textout(cx-Rad-25, cy-10,'W');
      textout(cx+Rad+10, cy-10,'E');
      ellipse(cx-2,cy-2,cx+2,cy+2);
      {draw equator}
      c:=trunc(rad*sin(degtorad(lat)));
      if lat >0 then sign:=+1 else sign:=-1;
      arc(cx-rad,cy-c,cx+rad,cy+c,cx-sign*rad,cy,cx+sign*rad,cy);
      outf := tStringList.Create;
      Outf.Add('TIME,AZIMUTH,ALTITUDE');
      CheckEditString(Edit1.text,tinc);
      t := Sunrise;
      FigureTime;
      {Draw projection of sun onto earth surface}
      t:=int(sunrise+1);  {next hour after sunrise}
      while t<= sunset do begin
         FigureTime;
         CosA1:=cos(degtorad(az-90));
         SinA1:=sin(degtorad(az-90));
         a:=alt;
         c:=trunc(rad*cos(degtorad(a)));
         x:=trunc(cx+c*cosA1);
         y:=trunc(cy+c*sinA1);
         if t=int(sunrise+1) then moveto(x,y)
         else lineto(x,y);
         brush.color:=clyellow;
         ellipse(x-3,y-3,x+3,y+3);

         t:=t+tinc;
       end;
      t := SunSet;
      FigureTime;

      fName := NextFileNumber(MDTempDir, 'solar_angles_','.csv');
      Outf.SaveToFile(fName);
      OutF.Destroy;
      OpenMultipleDataBases('',fName);

       {restore real values}
       sunpos(long,lat,Timepicker.time*24,tzHours, Julianday(datepicker.date), pos);

    end
    else if (drawmode=analemma) and (length(anpoints)>0) then  begin
      brush.color:=clblue;
      pen.color:=clblack;
      rectangle(rect(0,0,width,height));
      pen.color:=clyellow;
      with anpoints[high(anpoints)] do moveto(x,y);
      for i:=low (anpoints) to high(anpoints) do
      with anpoints[i] do lineto(x,y);
      {draw current sun position}
      pen.color:=clred;
      ellipse(anplotx-2,anploty-2,anplotx+2,anploty+2);
    end;
  end;
end;

procedure TSolorPosForm1.LongEdtExit(Sender: TObject);
{Autoset the time zone when longitude changes}
var W:extended;

begin
  if strtoangle(longedt.text,W) then
  begin
    if ewrgrp.itemindex=1 then W:=-w;
    If (W<>long) then tzbox.itemindex:=12+trunc(round(w) / 15); {guess 15 degrees per time zone}
  end;
  plotLastClick(sender);
end;


procedure TSolorPosForm1.StaticText1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.delphiforfun.org/',
  nil, nil, SW_SHOWNORMAL) ;
end;

{*********** GetSystimeBtn *******}
procedure TSolorPosForm1.GetSystimeBtnClick(Sender: TObject);
var
  TZ:TTimeZoneInformation;
  h:extended;
  dt:TDateTime;
  i:integer;
begin
  case getTimeZOneInformation(TZ) of
    Time_Zone_Id_daylight:
    begin
      if tz.daylightbias=-60 then dlsrgrp.itemindex:=1
      else if tz.daylightbias=-120 then dlsrgrp.itemindex:=2
      else
      begin
        memo1.lines.add('');
         memo1.lines.add('Unexpected Daylight Savings Time bias = '+ inttostr(tz.daylightbias)
                      +' minutes.'+#13+'Please, notify feedbackk@delphiforfun.org');
      end;
    end;
    Time_Zone_id_Unknown:
    begin
        memo1.lines.add('');
        memo1.lines.add('Time zone information not available in  this system');
        exit;
    end;
  end;

  {set time zone index}
  h:=-tz.bias/60;
  for i:= 0 to tzbox.items.count-1 do
  with tzbox do
  begin
    if gettimezone(items[i])=h then
    begin
      itemindex:=i;
      break;
    end;
  end;

  {set date and time displays}
   dt:=now;
   timepicker.time:=frac(dt);
   datepicker.date:=int(dt);
 end;


procedure TSolorPosForm1.PlotLastClick(Sender: TObject);
begin
  If lastplot='A' then AnalemmaBtnClick(sender)
  else ShowBtnClick(sender);
end;



end.



