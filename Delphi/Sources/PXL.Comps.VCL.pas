unit PXL.Comps.VCL;

interface

Uses
  PXL.Comps.Core,
  PXL.Comps.Surfaces,
  PXL.Comps.Surfaces.Process,
  PXL.Comps.Assets,
  PXL.Comps.AtlasImage,
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,  Vcl.Forms, Vcl.Controls,
  PXL.Classes,
  PXL.TypeDef,
  PXL.Types,
  PXL.Timing,
  PXL.ImageFormats,
  PXL.Devices,
  PXL.Textures,
  PXL.Canvas,
  PXL.SwapChains,
  PXL.Bitmaps,
  PXL.Surfaces,
  PXL.Images,
  PXL.Fonts,
  PXL.Providers;

Type
TPXLEngine = Class(TCustomPXLEngine)
Protected
  FInternalControl : TWinControl;

  procedure InternalApplicationIdle(Sender: TObject; var Done: Boolean);
  Procedure InternalBuild; Override;
  procedure InternalResize; Override;
Public
  Constructor Create(AOwner: TComponent); Override;

  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
Published
  Property OnInit;
  Property OnRender;
  Property OnProcess;
  Property MaxFPS;
  Property Device;
  Property ViewControl : TWinControl read FInternalControl write FInternalControl;
  Property Active;
End;

TPXLSurface = Class(TCustomPXLSurfaceWithProcessor)
Protected
Public
End;

TPXLAtlasImage = Class(TCustomPXLAtlasImage)
End;

TPXLProcessorImage = Class(TCustomPXLSurfaceProcessorImage)
End;


TPXLProcessorDrawingLayer = Class(TCustomPXLSurfaceProcessorDrawingLayer)
End;

TPXLSurfaceProcessorBackGround = Class(TCustomPXLSurfaceProcessorBackGround)
End;


Procedure Register;

implementation

Uses
  PXL.ImageFormats.Auto,
  PXL.Providers.Auto,
  PXL.Providers.DX11,
  PXL.Providers.DX9,
  PXL.Providers.GL,
  PXL.Providers.SRT;

Procedure Register;
begin
  RegisterComponents('PXL',[TPXLEngine, TPXLAtlasImage]);
  RegisterComponents('PXL Surfaces',[TPXLSurface,TPXLAssets]);
  RegisterComponents('PXL Processors',[TPXLSurfaceProcessorBackGround, TPXLProcessorImage,TPXLProcessorDrawingLayer]);
end;

{ TPXLEngine }


procedure TPXLEngine.InternalBuild;
begin
  if Not(Assigned(FInternalControl)) then
  begin
    if GetOwner is TForm then
    begin
      FInternalControl := TForm(GetOwner);
    end
    else
    begin
      raise Exception.Create('Unable to get render target control.');
    end;
  end;

  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := CreateDefaultImageFormatHandler(ImageFormatManager);

  case FDevice of
    Automatic:                DeviceProvider := CreateDefaultProvider(ImageFormatManager);
    DX11:                     DeviceProvider := TDX11Provider.Create(ImageFormatManager);
    DX9:                      DeviceProvider := TDX9Provider.Create(ImageFormatManager);
    OpenGL:                   DeviceProvider := TGLProvider.Create(ImageFormatManager);
    SoftwareRasterizer:       DeviceProvider := TSRTProvider.Create(ImageFormatManager);
  end;

  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  if Assigned(FInternalControl) then
  begin
    DisplaySize := Point2i(FInternalControl.ClientWidth, FInternalControl.ClientHeight);
  end;
  EngineDevice.SwapChains.Add(FInternalControl.Handle, DisplaySize);

  if not EngineDevice.Initialize then
  begin
    raise Exception.Create('Failed to initialize PXL Device.');
  end;

  EngineCanvas := DeviceProvider.CreateCanvas(EngineDevice);
  if not EngineCanvas.Initialize then
  begin
    raise Exception.Create('Failed to initialize PXL Canvas.');
  end;

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;
  FSystemFont := EngineFonts.AddSystemFont;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := InternalEnginerRender;
  EngineTimer.OnProcess := InternalEngineProcess;
  EngineTimer.MaxFPS := FMaxFPS;

  Application.OnIdle := InternalApplicationIdle;

  if Assigned(FOnInit) then
  begin
    FOnInit(Self);
  end;
end;


constructor TPXLEngine.Create(AOwner: TComponent);
begin
  inherited;
  FInternalControl := Nil;
end;

procedure TPXLEngine.InternalApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  Process;
  Done := False;
end;


procedure TPXLEngine.InternalResize;
begin
  if (EngineDevice <> nil) and (EngineTimer <> nil) and EngineDevice.Initialized then
  begin
    DisplaySize := Point2i(FInternalControl.ClientWidth, FInternalControl.ClientHeight);
    EngineDevice.Resize(0, DisplaySize);
    EngineTimer.Reset;
  end;
end;


procedure TPXLEngine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = TOperation.opRemove then
  begin
    if (AComponent = FInternalControl) then
    begin
      Active := False;
      FInternalControl := Nil;
    end;
  end;

end;


end.

