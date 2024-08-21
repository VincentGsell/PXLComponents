unit PXL.Comps.Surfaces;

interface

Uses
  System.SysUtils, System.Variants, System.Classes,
  PXL.Classes,
  PXL.TypeDef,
  PXL.Types,
  PXL.Canvas,
  PXL.Canvas.SRT,
  PXL.Rasterizer.SRT,
  PXL.Surfaces,
  PXL.Comps.Core;


Const
  CST_STD_WIDTH = 32;
  CST_STD_HEIGHT = 32;

Type

//TPixelSurfaceCanvas is an "Hacked" TSRTCanvas to perfom same behaviour on TPixelSurface (Treated here as a DIB).
// Internal use only.
TPixelSurfaceCanvas = Class(TSRTCanvas)
Protected
  function BeginDraw: Boolean; override;
  procedure EndDraw; override;
Public
  Function Surface : TPixelSurface;
  Constructor Create(aSurface : TPixelSurface); Reintroduce;

  Procedure SetSize(aWidth, aHeight : Integer);

  Procedure DrawImage( const TargetSurface : TPixelSurface;
                                         const Source : TPixelSurface;
                                         x,y : Single;
                                         Const ScaleX : Single = 1.0;
                                         Const ScaleY : Single = 1.0;
                                         Const Alpha : Byte = 255
                                       );
End;

TCustomPXLSurface = Class(TPXLComponent)
private
Protected
  FSurface : TPixelSurface;
  function GetHeigth: Integer;
  function GetPixelFormat: TPixelFormat;
  function GetWidth: Integer;
  procedure SetHeight(const Value: Integer);
  procedure SetPixelFormat(const Value: TPixelFormat);
  procedure SetWidth(const Value: Integer);
  function GetBytesPerPixel: Cardinal;
  function GetBufferSize: Cardinal;
  function GetHasAlphaChannel: Boolean;
  function GetPitch: Cardinal;

  function GetSurface: TPixelSurface; Virtual;
Public
  Canvas : TPixelSurfaceCanvas;
  Constructor Create(AOwner: TComponent); Override;
  Destructor Destroy; Override;
  Property Surface : TPixelSurface read GetSurface;
Published
  Property Width : Integer read GetWidth write SetWidth;
  Property Height : Integer read GetHeigth write SetHeight;
  Property PixelFormat : TPixelFormat read GetPixelFormat Write SetPixelFormat;
  Property BytesPerPixel : Cardinal read GetBytesPerPixel;
  Property BufferSize : Cardinal read GetBufferSize;
  Property HasAplhaChannel : Boolean Read GetHasAlphaChannel;
  Property HorizontalPitch : Cardinal read GetPitch;
End;

implementation


{ TCustomPXLSurface }

constructor TCustomPXLSurface.Create(AOwner: TComponent);
begin
  inherited;
  FSurface := TPixelSurface.Create;
  FSurface.SetSize(CST_STD_WIDTH,CST_STD_HEIGHT,TPixelFormat.A8R8G8B8);
  Canvas := TPixelSurfaceCanvas.Create(FSurface);
end;

destructor TCustomPXLSurface.Destroy;
begin
  FreeAndNil(FSurface);
  inherited;
end;

function TCustomPXLSurface.GetBufferSize: Cardinal;
begin
  result := FSurface.BufferSize;
end;

function TCustomPXLSurface.GetBytesPerPixel: Cardinal;
begin
  Result := FSurface.BytesPerPixel;
end;

function TCustomPXLSurface.GetHasAlphaChannel: Boolean;
begin
  Result := FSurface.HasAlphaChannel;
end;

function TCustomPXLSurface.GetHeigth: Integer;
begin
  Result := FSurface.Height;
end;

function TCustomPXLSurface.GetPitch: Cardinal;
begin
  Result := FSurface.Pitch;
end;

function TCustomPXLSurface.GetPixelFormat: TPixelFormat;
begin
  Result := FSurface.PixelFormat;
end;

function TCustomPXLSurface.GetSurface: TPixelSurface;
begin
  Result := FSurface;
end;

function TCustomPXLSurface.GetWidth: Integer;
begin
  Result := FSurface.Width;
end;

procedure TCustomPXLSurface.SetHeight(const Value: Integer);
begin
  surface.SetSize(FSurface.Width,Value);
  Canvas.SetSize(FSurface.Width,FSurface.Height);
end;

procedure TCustomPXLSurface.SetPixelFormat(const Value: TPixelFormat);
begin
  Surface.SetSize(FSurface.Width,FSurface.Height,Value);
end;

procedure TCustomPXLSurface.SetWidth(const Value: Integer);
begin
  surface.SetSize(Value,FSurface.Height);
  Canvas.SetSize(FSurface.Width,FSurface.Height);
end;

{ TPixelSurfaceCanvas }

function TPixelSurfaceCanvas.BeginDraw: Boolean;
begin
  FClipRect := IntRect(0,0,Surface.Width,Surface.Height);
  Result := True;
end;

constructor TPixelSurfaceCanvas.Create(aSurface: TPixelSurface);
begin
  Assert(Assigned(aSurface));
  Inherited Create(nil);
  FSurface := aSurface;
  SetSize(aSurface.Width,aSurface.Height);
end;

procedure TPixelSurfaceCanvas.DrawImage( const TargetSurface : TPixelSurface;
                                         const Source : TPixelSurface;
                                         x,y : Single;
                                         Const ScaleX : Single;
                                         Const ScaleY : Single;
                                         Const Alpha : Byte
                                       );

var l : TIntColor;
//    q : TQuad;
begin
//    q :=  TQuad.Rotated(Point2f(0,0),Point2f(TargetCanvas.Surface.Width,TargetCanvas.Surface.Height),
//    Point2f(TargetCanvas.Surface.Width/2,TargetCanvas.Surface.Height/2),45);
    l := IntColorRGB(255,255,255,Alpha);
    DrawTriangle( TargetSurface,
                  Source,
                  point2f(x, y),
                  point2f(x, y + Source.Height * ScaleY),
                  point2f(x + Source.Width * ScaleX, y + Source.Height * ScaleY),
                  point2f(0,0),
                  point2f(0,1),
                  point2f(1,1),
                  l,
                  l,
                  l,
                  IntRect(0,0,TargetSurface.Width,TargetSurface.Height),
                  False
                );

    DrawTriangle( TargetSurface,
                  Source,
                  point2f(x + Source.Width * ScaleX,y + Source.Height * ScaleY),
                  point2f(x + Source.Width * ScaleX, y),
                  point2f(x , y),
                  point2f(1,1),
                  point2f(1,0),
                  point2f(0,0),
                  l,
                  l,
                  l,
                  IntRect(0,0,TargetSurface.Width,TargetSurface.Height),
                  False
                );

end;

procedure TPixelSurfaceCanvas.EndDraw;
begin
end;

procedure TPixelSurfaceCanvas.SetSize(aWidth, aHeight: Integer);
begin
  FClipRect := IntRect(0,0,aWidth,aHeight);
end;

function TPixelSurfaceCanvas.Surface: TPixelSurface;
begin
  Result := TPixelSurface(FSurface);
end;



end.
