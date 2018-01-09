unit PXL.Comps.Surfaces.Process;

interface

Uses Classes,
     SysUtils,
     PXL.Types,
     PXL.Surfaces,
     PXL.Rasterizer.SRT,
     PXL.Canvas,
     PXL.Comps.Core,
     PXL.Comps.Assets,
     PXL.Comps.Surfaces;

Const
  CST_UNNAMED_PROC = 'Unnamed';
Type

TCustomPXLSurfaceProcessor = class;

TPXLSurfaceProcessorUnit = Class(TCollectionItem)
private
Protected
  FPXLSurface : TCustomPXLSurface; //Main compoment access.
  FProcessor: TCustomPXLSurfaceProcessor;
  FEnabled: Boolean;
  function GetName: string;
  procedure SetEnabled(const Value: Boolean);
  procedure SetProcessor(const Value: TCustomPXLSurfaceProcessor);
  function GetDisplayName: string; Override;
Public
  constructor Create(Collection: TCollection); Override;
Published
  Property Name : string read GetName;
  Property ProcessorUnit : TCustomPXLSurfaceProcessor read FProcessor Write SetProcessor;
  Property Enabled : Boolean read FEnabled Write SetEnabled;
End;

TPXLSurfaceProcessorCollection = class (TCollection)
Protected
  FComp: TComponent;
  FCollString: string;
public
  constructor Create (CollOwner: TComponent);
  function GetOwner: TPersistent; override;
  procedure Update(Item: TCollectionItem); override;

  Procedure DesignRefreshCollection;

end;

TCustomPXLSurfaceProcessor = Class(TPXLComponent)
Protected
  FDisplayName: String;
Public
  TargetCanvas: TPixelSurfaceCanvas;
  Procedure Execute; Virtual; Abstract;
  Constructor Create(aowner : TComponent); Override;
  Procedure DesignRefresh; //To dig.
Published
  Property DisplayName : String read FDisplayName Write FDisplayName;
End;

TCustomPXLSurfaceProcessorImage = class(TCustomPXLSurfaceProcessor)
Private
protected
  FAlpha: Byte;
  FScaleX: Single;
  FScaleY: Single;
  FOffsetX: Integer;
  FOffsetY: Integer;
  FInternalSurface : TPixelSurface;
  FUsualName: string;
  FAssets: TPXLAssets;
  procedure SetAsset(const Value: TPXLAssets);
  procedure SetUsualName(const Value: string);
  function GetImageHeight: Integer;
  function GetImageWitdh: Integer;
  function GetImageFormat: TPixelFormat;
  Procedure CheckAssets;
Public
  Constructor Create(aowner : TComponent); Override;
  Destructor Destroy; Override;
  Procedure Execute; Override;
  Procedure Loaded; Override;
Published
  Property Assets : TPXLAssets read FAssets Write SetAsset;
  property AssetUsualName : string read FUsualName Write SetUsualName;
  Property Alpha : Byte read FAlpha Write FAlpha;
  Property ImageWith : Integer read GetImageWitdh;
  Property ImageHeight : Integer read GetImageHeight;
  Property ImageFormat : TPixelFormat read GetImageFormat;
  Property OffsetX : Integer read FOffsetX Write FOffsetX;
  Property OffsetY : Integer read FOffsetY Write FOffsetY;
  Property ScaleX : Single read FScaleX Write FScaleX;
  Property ScaleY : Single read FScaleY Write FScaleY;
  //Add here some other picture stuff (rotate, Quattext.).
end;

TPXLSurfaceProcessorDrawingLayerEvent = Procedure(Sender : TObject; Canvas : TPixelSurfaceCanvas) Of Object;
TCustomPXLSurfaceProcessorDrawingLayer = class(TCustomPXLSurfaceProcessor)
Protected
  FOnRender: TPXLSurfaceProcessorDrawingLayerEvent;
Published
  Procedure Execute; Override;
  Property OnRender : TPXLSurfaceProcessorDrawingLayerEvent read FOnRender Write FOnRender;
end;

TPXLSurfaceProcessorBackGroundColorMode = (TopLeftOnly, TopLeftToTopRight, TopLeftToBottomLeft, AllFourth);
TCustomPXLSurfaceProcessorBackGround = class(TCustomPXLSurfaceProcessor)
private
Protected
  FColorTopLeft: TIntColor;
  FColorTopRight: TIntColor;
  FColorMode: TPXLSurfaceProcessorBackGroundColorMode;
  FColorBottomLeft: TIntColor;
  FColorBottomRight: TIntColor;

  function GetColorBottomLeft: String;
  function GetColorBottomRight: String;
  function GetColorTopLeft: String;
  function GetColorTopRight: String;
  procedure SetColorBottomLeft(const Value: String);
  procedure SetColorBottomRight(const Value: String);
  procedure SetColorTopLeft(const Value: String);
  procedure SetColorTopRight(const Value: String);
Published
  Procedure Execute; Override;
  Property ColorTopLeft : String read GetColorTopLeft Write SetColorTopLeft;
  Property ColorTopRight : String read GetColorTopRight Write SetColorTopRight;
  Property ColorBottomLeft : String read GetColorBottomLeft Write SetColorBottomLeft;
  Property ColorBottomRight : String read GetColorBottomRight Write SetColorBottomRight;
  Property ColorMode : TPXLSurfaceProcessorBackGroundColorMode read FColorMode Write FColorMode;
end;


TCustomPXLSurfaceWithProcessor = Class(TCustomPXLSurface)
private
Protected
  FProcCol: TPXLSurfaceProcessorCollection;
Public
  function GetSurface: TPixelSurface; Override;
  Constructor Create(AOwner: TComponent); Override;
  Destructor Destroy; Override;

  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
Published
  Property Processors : TPXLSurfaceProcessorCollection read FProcCol Write FProcCol;
end;


implementation

Uses PXL.ImageFormats,
     PXL.ImageFormats.Auto;


constructor TPXLSurfaceProcessorCollection.Create(CollOwner: TComponent);
begin
  inherited Create(TPXLSurfaceProcessorUnit);
  FComp := CollOwner;
end;

procedure TPXLSurfaceProcessorCollection.DesignRefreshCollection;
var i : integer;
begin
  if Assigned(FComp) then
  begin
    if Not(csDesigning in FComp.ComponentState) then
      Exit;

    if Assigned(FComp.Owner) then
    begin
      for i := 0 to TComponent(FComp.Owner).ComponentCount-1 do
      begin
        if TComponent(FComp.Owner).Components[i] is TPXLComponent then
        begin
          //? To dig.
        end;
      end;
    end;
  end;
end;

function TPXLSurfaceProcessorCollection.GetOwner: TPersistent;
begin
  result := FComp;
end;

procedure TPXLSurfaceProcessorCollection.Update(Item: TCollectionItem);
var
  str: string;
  i : integer;
begin
  inherited;
  // update everything in any case...
  str := IntToStr(Count)+' Processor(s) ';
  for i := 0 to Count - 1 do
  begin
    str := str + (Items [i] as TPXLSurfaceProcessorUnit).Name;
    if i < Count - 1 then
      str := str + '-';
  end;
  FCollString := str;
  DesignRefreshCollection;
end;

{ TPXLSurfaceProcessorUnit }

constructor TPXLSurfaceProcessorUnit.Create(Collection: TCollection);
begin
  inherited;
  FEnabled := True;
  FPXLSurface := TCustomPXLSurface(TPXLSurfaceProcessorCollection(Collection).FComp);
end;

function TPXLSurfaceProcessorUnit.GetDisplayName: string;
var fe : String;
begin
  //Inherited;
  if Assigned(FProcessor) then
  begin
    fe :=' ';
    if FEnabled then
      fe := 'x';
    Result := 'Item['+IntToStr(Index)+']['+fe+'] - "'+Fprocessor.DisplayName + '" '+FProcessor.Name;
  end
  else
  begin
    Result := 'Item['+IntToStr(Index)+'] - Not Assigned';
  end;
end;

function TPXLSurfaceProcessorUnit.GetName: string;
begin
  result := 'Not Assigned';
  if Assigned(FProcessor) then
  begin
    Result := '['+FProcessor.Name+']';
  end;
end;

procedure TPXLSurfaceProcessorUnit.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Changed(false);
end;

procedure TPXLSurfaceProcessorUnit.SetProcessor(const Value: TCustomPXLSurfaceProcessor);
begin
  FProcessor := Value;
  TPXLSurfaceProcessorCollection(GetOwner).DesignRefreshCollection;
end;


{ TCustomPXLSurfaceProcessor }

constructor TCustomPXLSurfaceProcessor.Create(aowner: TComponent);
begin
  inherited;
  FDisplayName := CST_UNNAMED_PROC;
end;

procedure TCustomPXLSurfaceProcessor.DesignRefresh;
var i : integer;
begin
  if Not(csDesigning in ComponentState) then
    Exit;

  if Assigned(Owner) then
  begin
    for i := 0 to TComponent(Owner).ComponentCount-1 do
    begin
      if TComponent(Owner).Components[i] is TPXLComponent then
      begin
        //? Update...
      end;
    end;
  end;
end;


{ TCustomPXLSurfaceWithProcessor }

constructor TCustomPXLSurfaceWithProcessor.Create(AOwner: TComponent);
begin
  inherited;
  FProcCol := TPXLSurfaceProcessorCollection.Create(Self);
end;

destructor TCustomPXLSurfaceWithProcessor.Destroy;
begin
  FreeAndNil(FProcCol);
  inherited;
end;

function TCustomPXLSurfaceWithProcessor.GetSurface: TPixelSurface;
var i : integer;
    lp : TPXLSurfaceProcessorUnit;
begin
  Result := Inherited GetSurface;
  //Apply all processors
  for I := 0 to FProcCol.Count-1 do
  begin
    lp := TPXLSurfaceProcessorUnit(FProcCol.Items[i]);
    if lp.Enabled And Assigned(lp.ProcessorUnit) then
    begin
      lp.ProcessorUnit.TargetCanvas := Canvas;
      lp.ProcessorUnit.Execute;
    end;
  end;
end;

procedure TCustomPXLSurfaceWithProcessor.Notification(AComponent: TComponent;
  Operation: TOperation);
var i : IntegeR;
begin
  inherited;
  if AComponent is TCustomPXLSurfaceProcessor then
  begin
    for i := 0 to FProcCol.Count-1 do
    begin
      if TPXLSurfaceProcessorUnit(FProcCol.Items[i]).ProcessorUnit = AComponent then
      begin
        TPXLSurfaceProcessorUnit(FProcCol.Items[i]).ProcessorUnit := Nil;
      end;
    end;
  end;
end;

{ TCustomPXLSurfaceProcessorImage }

procedure TCustomPXLSurfaceProcessorImage.CheckAssets;
var lPXLAssetUnit : TPXLAssetUnit;
    lm : TImageFormatManager;
    lh : TCustomImageFormatHandler;
begin
  if Assigned(FAssets) then
  begin
    If FAssets.GetAssetFromName(FUsualName,lPXLAssetUnit) then
    begin
      if lPXLAssetUnit.StreamSize>0 then
      begin
        lPXLAssetUnit.AvailableStream.Position := 0;
        if lPXLAssetUnit.AssetType.IsImageType then
        begin
          lm := TImageFormatManager.Create;
          lh := CreateDefaultImageFormatHandler(lm);
          try
            lh.LoadFromStream( nil,
                               lPXLAssetUnit.AssetType.TypeToExt,
                               lPXLAssetUnit.AvailableStream,
                               TPixelSurface(FInternalSurface),
                               TAlphaFormatRequest.DontCare);
            FInternalSurface.ConvertPixelFormat(TPixelFormat.A8R8G8B8); //Needed, because raster operation.
          finally
            FreeAndNil(lh);
            FreeAndNil(lm);
          end;
          FUsualName := lPXLAssetUnit.UsageName; //Correct case.
        end;
      end;
    end;
  end;
end;

constructor TCustomPXLSurfaceProcessorImage.Create(aowner: TComponent);
begin
  inherited;
  FInternalSurface := TPixelSurface.Create;
  FAlpha := 255;
  FOffsetX := 0;
  FOffsetY := 0;
  FScaleX := 1.0;
  FScaleY := 1.0;
end;

destructor TCustomPXLSurfaceProcessorImage.Destroy;
begin
  FreeAndNil(FInternalSurface);
  inherited;
end;

procedure TCustomPXLSurfaceProcessorImage.Execute;
begin
  if Assigned(TargetCanvas) then
  begin
    TargetCanvas.DrawImage( TargetCanvas.Surface,
                            FInternalSurface,
                            FOffsetX,
                            FOffsetY,
                            FScaleX,
                            FScaleY,
                            FAlpha);
  end;
end;


function TCustomPXLSurfaceProcessorImage.GetImageFormat: TPixelFormat;
begin
  Result := FInternalSurface.PixelFormat;
end;

function TCustomPXLSurfaceProcessorImage.GetImageHeight: Integer;
begin
  Result := FInternalSurface.Height;
end;

function TCustomPXLSurfaceProcessorImage.GetImageWitdh: Integer;
begin
  Result := FInternalSurface.Width;
end;

procedure TCustomPXLSurfaceProcessorImage.Loaded;
begin
  inherited;
  CheckAssets;
end;

procedure TCustomPXLSurfaceProcessorImage.SetAsset(const Value: TPXLAssets);
begin
  FAssets := Value;
  if Not(assigned(Assets)) then
  begin
    AssetUsualName := EmptyStr;
  end;
end;

procedure TCustomPXLSurfaceProcessorImage.SetUsualName(const Value: string);
begin
  FUsualName := Value;
  CheckAssets; //load pictur in internal surface.
end;

{ TCustomPXLSurfaceProcessorDrawingLayer }

procedure TCustomPXLSurfaceProcessorDrawingLayer.Execute;
begin
  if Assigned(FOnRender) then
  begin
    if TargetCanvas.BeginScene then
    begin
      try
        FOnRender(Self,TargetCanvas);
      finally
        TargetCanvas.EndScene;
      end;
    end;
  end;
end;

{ TCustomPXLSurfaceProcessorBackGround }

procedure TCustomPXLSurfaceProcessorBackGround.Execute;
begin
  if TargetCanvas.BeginScene then
  begin
    try
      case FColorMode of
        TopLeftOnly:
        begin
          TargetCanvas.FillQuad( Quad(0,0,TargetCanvas.Surface.Width,TargetCanvas.Surface.Height),
                                 ColorRect(FColorTopLeft)
                                );
        end;
        TopLeftToTopRight:
        begin
          TargetCanvas.FillQuad( Quad(0,0,TargetCanvas.Surface.Width,TargetCanvas.Surface.Height),
                                 ColorRect(
                                   FColorTopLeft,
                                   FColorTopRight,
                                   FColorTopRight,
                                   FColorTopLeft)
                                );
        end;
        TopLeftToBottomLeft:
        begin
          TargetCanvas.FillQuad( Quad(0,0,TargetCanvas.Surface.Width,TargetCanvas.Surface.Height),
                                 ColorRect(
                                   FColorTopLeft,
                                   FColorTopLeft,
                                   FColorBottomLeft,
                                   FColorBottomLeft)
                                );
        end;
        AllFourth:
        begin
          TargetCanvas.FillQuad( Quad(0,0,TargetCanvas.Surface.Width,TargetCanvas.Surface.Height),
                                 ColorRect(
                                   FColorTopLeft,
                                   FColorTopRight,
                                   FColorBottomRight,
                                   FColorBottomLeft)
                                );
        end;
      end;
    finally
      TargetCanvas.EndScene;
    end;
  end;
end;

function TCustomPXLSurfaceProcessorBackGround.GetColorBottomLeft: String;
begin
  Result := IntToHex(FColorBottomLeft,8);
end;

function TCustomPXLSurfaceProcessorBackGround.GetColorBottomRight: String;
begin
  Result := IntToHex(FColorBottomRight,8);
end;

function TCustomPXLSurfaceProcessorBackGround.GetColorTopLeft: String;
begin
  Result := IntToHex(FColorTopLeft,8);
end;

function TCustomPXLSurfaceProcessorBackGround.GetColorTopRight: String;
begin
  Result := IntToHex(FColorTopRight,8);
end;

Function LocalHexStringToInt(aVal : String) : Cardinal;
var lSuffix : String;
begin
  lSuffix := '$';
  if pos('$',aVal) > 0 then
  begin
    lSuffix := EmptyStr;
  end;
  Result := StrToInt(lSuffix + aVal);
end;

procedure TCustomPXLSurfaceProcessorBackGround.SetColorBottomLeft(
  const Value: String);
begin
  FColorBottomLeft := LocalHexStringToInt(Value);
end;

procedure TCustomPXLSurfaceProcessorBackGround.SetColorBottomRight(
  const Value: String);
begin
  FColorBottomRight := LocalHexStringToInt(Value);
end;

procedure TCustomPXLSurfaceProcessorBackGround.SetColorTopLeft(
  const Value: String);
begin
  FColorTopLeft := LocalHexStringToInt(Value);
end;

procedure TCustomPXLSurfaceProcessorBackGround.SetColorTopRight(
  const Value: String);
begin
  FColorTopRight := LocalHexStringToInt(Value);
end;

end.
