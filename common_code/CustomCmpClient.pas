// Basis to create component units to communicate with map server


this is no longer used

unit CustomCmpClient;

interface
                    

uses
  //QGraphics,     //PLG changes 7/22/2010
  Graphics,       //PLG changes 7/22/2010
  SysUtils, Classes, StrUtils;

type
  // the component type to communicate with a map server (abstract type)
  TCustomCmpClient = class(TComponent)
  protected   
    // Protected-Deklarationen
    // url to the map server
    FUrl: ANSIstring;

    // version of the map server
    FVersion: ANSIstring;

    // the result of an @link(ExecMapRequest) or @link(GetMap)
    FImage: tBitmap;
    // image width for the map, info request
    FImageWidth: Integer;
    // image height for the map, info request
    FImageHeight: Integer;

    // comma separated list of layers for the map, info request
    FLayers: ANSIstring;

    // min. X coordinate for the map, info request
    FMinX: Double;
    // max. X coordinate for the map, info request
    FMaxX: Double;
    // min. Y coordinate for the map, info request
    FMinY: Double;
    // max. Y coordinate for the map, info request
    FMaxY: Double;

    // auto. generated map request
    FMapRequest: ANSIstring;

    // comma separated list of layers for the info request (query layers have to be also defined in @link(FLayers))
    FInfoLayers: ANSIstring;
    // auto. generated info request
    FInfoRequest: ANSIstring;
    FCapabilityRequest : ANSIstring;
    // result of the info request (after calling @link(ExecInfoRequest) or @link(GetInfo))
    FInfoResult: tStrings;

    // overview map
    FOverviewMap: tBitmap;
    // request for a overview map
    FOverviewMapRequest: ANSIstring;

    // legend image
    FLegend: tBitmap;
    // request for a legend image
    FLegendRequest: ANSIstring;
                   
    // adds a need '?' or '&' to a URL
    function GetUrlForRequest(url: ANSIstring): ANSIstring;
  private
    // Private-Deklarationen
  public
    // Public-Deklarationen
    // creator
    constructor Create(AOwner: TComponent); override;
    // destructor
    destructor Destroy; override;

    // executes the two procedures @link(GetMapRequest) and @link(ExecMapRequest): result as tMyBitmap in @link(Map)
    procedure GetMap;
    // executes the map request (@link(MapRequest)), result in @link(Map)
    procedure ExecMapRequest; virtual; abstract;
    // generates a map request with the properties
    procedure GetMapRequest; virtual; abstract;

    // executes the two procedures @link(GetInfoRequest) and @link(ExecInfoRequest): result as TStrings in @link(Info); posX, posY: mouse column, row (from left, top)
    procedure GetInfo(posX, posY: Integer);
    // executes the info request (@link(InfoRequest)), result in @link(Info)
    procedure ExecInfoRequest; virtual; abstract;
    // generates a info request with the same properties as defined in @link(GetMapRequest) and additional properties; posX, posY: mouse column, row (from left, top)
    procedure GetInfoRequest(posX, posY: Integer);  virtual; abstract;
    
    // executes the two procedures @link(GetOverviewMapRequest) and @link(ExecOverviewMapRequest): result as tMyBitmap in @link(OverviewMap)
    procedure GetOverviewMap;
    // executes the legend request (@link(OverviewMapRequest)), result in @link(OverviewMap)
    procedure ExecOverviewMapRequest; virtual; abstract;
    // generates a legend request
    procedure GetOverviewMapRequest; virtual; abstract; 

    // executes the two procedures @link(GetLegendRequest) and @link(ExecLegendRequest): result as tMyBitmap in @link(Legend)
    procedure GetLegend;
    // executes the legend request (@link(LegendRequest)), result in @link(Legend)
    procedure ExecLegendRequest; virtual; abstract;
    // generates a legend request with the property @link(CustomCmpClient.TCustomCmpClient.Layers)
    procedure GetLegendRequest; virtual; abstract;

    // the result of a @link(ExecMapRequest) or @link(GetMap) (readonly)
    property Map: tBitmap read FImage;
    // result of the info request (after calling @link(ExecInfoRequest) or @link(GetInfo)) (readonly)
    property Info: TStrings read FInfoResult;
    // result of the legend request (after calling @link(ExecOverviewMapRequest) or @link(GetOverviewMap)) (readonly)
    property OverviewMap: tBitmap read FOverviewMap;
    // result of the legend request (after calling @link(ExecLegendRequest) or @link(GetLegend)) (readonly)
    property Legend: tBitmap read FLegend;

  published
    // Published-Deklarationen 
    // Base URL to the map server
    property Url: ANSIstring read FUrl write FUrl;

    // version of the map server
    property Version: ANSIstring read FVersion write FVersion;

    // image width for the map, info request
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    // image height for the map, info request
    property ImageHeight: Integer read FImageHeight write FImageHeight;

    // comma separated list of layers for the map, info request (no spaces!)
    property Layers: ANSIstring read FLayers write FLayers;
    // comma separated list of layers for the info request (query layers have to be also defined in @link(Layers))
    property InfoLayers: ANSIstring read FInfoLayers write FInfoLayers;

    // min. longitude coordinate for the map, info request
    property MinX: Double read FMinX write FMinX;
    // max. longitude coordinate for the map, info request
    property MaxX: Double read FMaxX write FMaxX;
    // min. longatitude coordinate for the map, info request
    property MinY: Double read FMinY write FMinY;
    // max. latitude coordinate for the map, info request
    property MaxY: Double read FMaxY write FMaxY;

    // auto. generated map request (generated with @link(GetInfoRequest))
    property MapRequest: ANSIstring read FMapRequest write FMapRequest;
    // auto. generated info request (without url, generated with @link(GetInfoRequest))
    property InfoRequest: ANSIstring read FInfoRequest write FInfoRequest;
    // auto. generated overview map request (generated with @link(GetOverviewMapRequest))
    property OverviewMapRequest: ANSIstring read FOverviewMapRequest write FOverviewMapRequest;
    // auto. generated legend request (generated with @link(GetLegendRequest))
    property LegendRequest: ANSIstring read FLegendRequest write FLegendRequest;
    // auto. generated legend request (generated with @link(GetLegendRequest))
    property CapabilityRequest: ANSIstring read FCapabilityRequest write FCapabilityRequest;
  end;


implementation 

constructor TCustomCmpClient.Create(AOwner: TComponent);
begin  
  inherited Create(AOwner);
  (*
  FImage := tMyBitmap.Create;
  FOverviewMap := tMyBitmap.Create;
  FInfoResult := TStringList.Create;
  *)
end;

destructor TCustomCmpClient.Destroy;
begin
(*
  FImage.Free;
  FOverviewMap.Free;
  FInfoResult.Free;
*)
  inherited Destroy;
end;


procedure TCustomCmpClient.GetMap;
begin
  GetMapRequest;
  ExecMapRequest;
end;       

procedure TCustomCmpClient.GetInfo(posX, posY: Integer);
begin
  GetInfoRequest(posX, posY);
  ExecInfoRequest;
end; 

procedure TCustomCmpClient.GetOverviewMap;
begin
  GetOverviewMapRequest;
  ExecOverviewMapRequest;
end;

procedure TCustomCmpClient.GetLegend;
begin
  GetLegendRequest;
  ExecLegendRequest;
end;

function TCustomCmpClient.GetUrlForRequest(url: ANSIstring): ANSIstring;
var
  res: ANSIstring;
begin
  if AnsiPos('?',url) = 0 then  res := url + '?'
  else  begin
    if AnsiEndsStr('&',url) then res := url
    else res := url + '&';
  end;
  Result := res;
end;



end.

