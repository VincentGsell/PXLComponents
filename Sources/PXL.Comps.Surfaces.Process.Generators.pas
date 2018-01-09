unit PXL.Comps.Surfaces.Process.Generators;

interface

Uses Classes,
     Sysutils,
     PXL.Types,
     PXL.Canvas,
     PXL.Surfaces,
     PXL.Rasterizer.SRT,
     PXL.Comps.Core,
     PXL.Comps.Surfaces,
     PXL.Comps.Surfaces.Process;

Type

TCustomPXLSurfaceGenerator = class(TCustomPXLSurfaceProcessor)
Private
Public
end;

TCustomPXLSurfaceGeneratorGourault = class(TCustomPXLSurfaceGenerator)
Private
    FWidth: Integer;
    FX: Integer;
    FY: Integer;
    FHeight: Integer;
    FTLRed: Byte;
    FTLBlue: Byte;
    FTLGreen: Byte;
    FTRBlue: Byte;
    FTRGreen: Byte;
    FTRRed: Byte;
    FBLRed: Byte;
    FBLBlue: Byte;
    FBLGreen: Byte;
    FBRGreen: Byte;
    FBRRed: Byte;
    FBRBlue: Byte;
    procedure SetHeight(const Value: Integer);
    procedure SetTLRed(const Value: Byte);
    procedure SetWidth(const Value: Integer);
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
    procedure SetTLBlue(const Value: Byte);
    procedure SetTLGreen(const Value: Byte);
    procedure SetTRBlue(const Value: Byte);
    procedure SetTRGreen(const Value: Byte);
    procedure SetTRRed(const Value: Byte);
    procedure SetBLRed(const Value: Byte);
    procedure SettBLlue(const Value: Byte);
    procedure SettBLreen(const Value: Byte);
    procedure SetBRBlue(const Value: Byte);
    procedure SetBRRed(const Value: Byte);
    procedure SetBRreen(const Value: Byte);
Public
  Constructor Create(aowner : TComponent); Override;
  Procedure Execute; Override;
Published
  Property X : Integer read FX Write SetX;
  Property Y : Integer read FY Write SetY;
  Property Width : Integer read FWidth Write SetWidth;
  Property Height : Integer read FHeight Write SetHeight;

  Property ColorTopLeftRed : Byte read FTLRed Write SetTLRed;
  Property ColorTopLeftBlue : Byte read FTLBlue Write SetTLBlue;
  Property ColorTopLeftGreen : Byte read FTLGreen Write SetTLGreen;

  Property ColorTopRightRed : Byte read FTRRed Write SetTRRed;
  Property ColorTopRightBlue : Byte read FTRBlue Write SetTRBlue;
  Property ColorTopRightGreen : Byte read FTRGreen Write SetTRGreen;

  Property ColorBottomLeftRed : Byte read FBLRed Write SetBLRed;
  Property ColorBottomLeftBlue : Byte read FBLBlue Write SettBLlue;
  Property ColorBottomLeftGreen : Byte read FBLGreen Write SettBLreen;

  Property ColorBottomRightRed : Byte read FBRRed Write SetBRRed;
  Property ColorBottomRightBlue : Byte read FBRBlue Write SetBRBlue;
  Property ColorBottomRightGreen : Byte read FBRGreen Write SetBRreen;
End;

TCustomPXLSurfaceGeneratorPlasma = class(TCustomPXLSurfaceGenerator)
Private
  Sr : Array[0..7] of Integer;
  Sc : Array[0..7] of Integer;
  sinHalfTable, sinOneTable, sinTwoTable, sinFourTable : Array[0..511] of Integer;
  PlasmaGun : TPixelSurface;

  FX: Integer;
  FY: Integer;
  FGrayScaleMode: Boolean;
  FAlpha: Byte;
  FScaleX: Single;
  FScaleY: Single;
  procedure SetHeight(const Value: Integer);
  procedure SetWidth(const Value: Integer);
  procedure SetX(const Value: Integer);
  procedure SetY(const Value: Integer);

  Procedure UpdatePlasma;
  procedure SetGraySCaleMode(const Value: Boolean);
  procedure SetAlpha(const Value: Byte);
  function GetHeight: Integer;
  function GetWidth: Integer;
  procedure SetScaleX(const Value: Single);
  procedure SetScaleY(const Value: Single);

Public
  Constructor Create(aOwner : TComponent); OVerride;
  Destructor Destroy; Override;
  Procedure Execute; Override;
Published
  Property X : Integer read FX Write SetX;
  Property Y : Integer read FY Write SetY;
  Property ScaleX : Single read FScaleX Write SetScaleX;
  Property ScaleY : Single read FScaleY Write SetScaleY;
  Property Width : Integer read GetWidth Write SetWidth;
  Property Height : Integer read GetHeight Write SetHeight;
  Property Alpha : Byte read FAlpha Write SetAlpha;
  Property GrayScaleMode : Boolean read FGrayScaleMode Write SetGraySCaleMode;
End;




TPXLSurfaceGeneratorGourault = Class(TCustomPXLSurfaceGeneratorGourault)
End;

TPXLSurfaceGeneratorPlasma = class(TCustomPXLSurfaceGeneratorPlasma)
end;

Procedure Register;

implementation

Procedure Register;
begin
  RegisterComponents('PXL Processors',[TPXLSurfaceGeneratorGourault,TPXLSurfaceGeneratorPlasma]);
end;


{ TCustomPXLSurfaceGeneratorGourault }

constructor TCustomPXLSurfaceGeneratorGourault.Create(aowner: TComponent);
begin
  inherited;

  FTLRed := 255;
  FTRRed := 255;
  FBLRed := 255;
  FBRRed := 255;
  FTLGreen := 255;
  FTRGreen := 255;
  FBLGreen := 255;
  FBRGreen := 255;
  FTLBlue := 255;
  FTRBlue := 255;
  FBLBlue := 255;
  FBRBlue := 255;

  x := 20;
  y := 20;
  Width := 300;
  Height := 200;
end;

procedure TCustomPXLSurfaceGeneratorGourault.Execute;
var FColorTopLeft,
    FColorTopRight,
    FColorBottomRight,
    FColorBottomLeft : TIntColor;

begin
  if TargetCanvas.BeginScene then
  begin
    try
      FColorTopLeft := IntColorRGB(FTLRed,FTLBlue,FTLGreen);
      FColorTopRight := IntColorRGB(FTRRed,FTRBlue,FTRGreen);
      FColorBottomRight := IntColorRGB(FBRRed,FBRBlue,FBRGreen);
      FColorBottomLeft := IntColorRGB(FBLRed,FBLBlue,FBLGreen);
      TargetCanvas.FillQuad( Quad(FX,FY,FWidth,FHeight),
                             ColorRect(
                               FColorTopLeft,
                               FColorTopRight,
                               FColorBottomRight,
                               FColorBottomLeft)
                           );
    finally
      TargetCanvas.EndScene;
    end;
  end;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetBLRed(const Value: Byte);
begin
  FBLRed := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetBRBlue(const Value: Byte);
begin
  FBRBlue := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetBRRed(const Value: Byte);
begin
  FBRRed := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetBRreen(const Value: Byte);
begin
  FBRGreen := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SettBLlue(const Value: Byte);
begin
  FBLBlue := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SettBLreen(const Value: Byte);
begin
  FBLGreen := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetTLBlue(const Value: Byte);
begin
  FTLBlue := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetTLGreen(const Value: Byte);
begin
  FTLGreen := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetTLRed(const Value: Byte);
begin
  FTLRed := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetTRBlue(const Value: Byte);
begin
  FTRBlue := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetTRGreen(const Value: Byte);
begin
  FTRGreen := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetTRRed(const Value: Byte);
begin
  FTRRed := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetX(const Value: Integer);
begin
  FX := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorGourault.SetY(const Value: Integer);
begin
  FY := Value;
  DesignRefresh;
end;

{ TCustomPXLSurfaceGeneratorPlasma }

constructor TCustomPXLSurfaceGeneratorPlasma.Create(aOwner: TComponent);
var i : integer;
    Temp : Double;
begin
  inherited;
  PlasmaGun := TPixelSurface.Create;
  PlasmaGun.SetSize(50,50,TPixelFormat.A8R8G8B8);
  for I :=0 to 511 do
  begin
    Temp :=4*pi*i/512;
    sinHalfTable[i] := Round(sin(temp/2)*128 + 128);
    sinOneTable[i]  := Round(sin(temp  )*128 + 128);
    sinTwoTable[i]  := Round(sin(temp*2)*128 + 128);
    sinFourTable[i] := Round(sin(temp*4)*128 + 128);
  end;

  Randomize;
  GrayScaleMode := False;

  Fx := 50;
  FY := 50;
  FScaleX := 1.0;
  FScaleY := 1.0;
  FAlpha := 255;
end;

destructor TCustomPXLSurfaceGeneratorPlasma.Destroy;
begin
  FreeAndNil(PlasmaGun);
  inherited;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.Execute;
begin
  if TargetCanvas.BeginScene then
  begin
    try
      UpdatePlasma;
      TargetCanvas.DrawImage(TargetCanvas.Surface,PlasmaGun,FX,FY,FScaleX,FScaleY,FAlpha);
    finally
      TargetCanvas.EndScene;
    end;
  end;
end;

function TCustomPXLSurfaceGeneratorPlasma.GetHeight: Integer;
begin
  Result := PlasmaGun.Height;
end;

function TCustomPXLSurfaceGeneratorPlasma.GetWidth: Integer;
begin
  Result := PlasmaGun.Width;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetAlpha(const Value: Byte);
begin
  FAlpha := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetGraySCaleMode(const Value: Boolean);
var i : integer;
begin
  FGrayScaleMode := Value;
  // if sr and sc = 0 then its a greyscale image
  if Not(FGrayScaleMode) Then
  begin
    for I :=0 to 7 do
    begin
      sr[i] := Random(256);
      sc[i] := sr[i];
    end
  end
  else
  begin
    for I :=0 to 7 do
    begin
      sr[i] := 0;
      sc[i] :=sr[i];
    end;
  end;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetHeight(const Value: Integer);
begin
  PlasmaGun.Setsize(PlasmaGun.Width, Value);
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetScaleX(const Value: Single);
begin
  FScaleX := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetScaleY(const Value: Single);
begin
  FScaleY := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetWidth(const Value: Integer);
begin
  PlasmaGun.SetSize(Value, PlasmaGun.Height,TPixelFormat.A8R8G8B8);
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetX(const Value: Integer);
begin
  FX := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.SetY(const Value: Integer);
begin
  FY := Value;
  DesignRefresh;
end;

procedure TCustomPXLSurfaceGeneratorPlasma.UpdatePlasma;
var X, XCount, Y, YCount : Array[0..3] of Word;
    I, J, c : Integer;
    Pixel : PIntColorRec;
begin
  for C :=0 to 3 do
  begin
    Y[c] :=sr[c+4];
  end;

  for C :=0 to 3 do
  begin
    XCount[c] :=sc[c];
    YCount[c] :=sc[c+4];
  end;

  Pixel := PlasmaGun.Bits;
  for I :=0 to PlasmaGun.Height-1 do
  begin
    // reset the x values & counters
    for C :=0 to 3 do
      X[c] :=sr[c];

    for C :=0 to 3 do
    begin
      XCount[c] :=(sc[c] + I*2) MOD 512;
      YCount[c] :=(sc[c+4] + I*2) MOD 512;
    end;

    for J :=0 to PlasmaGun.Width-1 do
    begin
      inc(Cardinal(Pixel),4);

      Pixel.Red :=Round((sinHalfTable[X[0]] + sinOneTable[X[1]] + sinTwoTable[X[2]] + sinFourTable[X[3]]) +
                      (sinHalfTable[Y[0]] + sinOneTable[Y[1]] + sinTwoTable[Y[2]] + sinFourTable[Y[3]])) SHR 3;
      Pixel.Green :=Round((sinHalfTable[X[2]] + sinOneTable[X[3]] + sinTwoTable[X[0]] + sinFourTable[X[1]]) +
                      (sinHalfTable[Y[2]] + sinOneTable[Y[3]] + sinTwoTable[Y[0]] + sinFourTable[Y[1]])) SHR 3;
      Pixel.Blue :=Round((sinHalfTable[X[3]] + sinOneTable[X[0]] + sinTwoTable[X[1]] + sinFourTable[X[2]]) +
                      (sinHalfTable[Y[1]] + sinOneTable[Y[2]] + sinTwoTable[Y[3]] + sinFourTable[Y[0]])) SHR 3;

      Pixel.Alpha := FAlpha;
      // increment the x values
      for C :=0 to 3 do
      begin
        X[c] := Round((X[c] + (sinTwoTable[XCount[c]]) /32 -4 + 512)) MOD 512;
        XCount[c] := Round(XCount[c] + 2) MOD 512;
      end;
    end;
    Pixel := PlasmaGun.Bits;
    inc(Cardinal(Pixel),PlasmaGun.Width * I * PlasmaGun.BytesPerPixel);

    // increment the y values
    for C:=0 to 3 do
      Y[c] := Round(Y[c] + (sinOneTable[YCount[c]])/32 -4 + 512) MOD 512;
  end;

  // update statics
  for c :=0 to 7 do
  begin
    sr[c] := Round(sr[c] + (sinTwoTable[sc[c]])/32-4+512) MOD 512;
    sc[c] := Round(sc[c] + 2) MOD 512;
  end;

end;



end.
