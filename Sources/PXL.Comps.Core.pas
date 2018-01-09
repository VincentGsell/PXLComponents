unit PXL.Comps.Core;

interface

Uses
  System.SysUtils, System.Variants, System.Classes,
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

Const
  CST_MAX_FPS = 4000;
Type

TPXLEngineDevice = (Automatic, DX11, DX9, OpenGL, SoftwareRasterizer);

TPXLComponent = Class(TComponent)
End;

TCustomPXLEngine = Class(TPXLComponent)
  private
    function GetEngineDevice: TCustomDevice;
Protected
    ImageFormatManager: TImageFormatManager;
    ImageFormatHandler: TCustomImageFormatHandler;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2i;

    FOnProcess: TNotifyEvent;
    FOnRender: TNotifyEvent;

    FActive: Boolean;
    FOnInit: TNotifyEvent;
    FSystemFont : Integer;
    FMaxFPS : Integer;
    FDevice: TPXLEngineDevice;

    procedure InternalEnginerRender(const Sender: TObject); VirtuaL;
    procedure InternalEngineProcess(const Sender: TObject); Virtual;

    procedure InternalResize; Virtual; Abstract;
    Procedure InternalBuild; Virtual; Abstract;

    Procedure InternalUnBuild; Virtual;

    procedure SetActive(const Value: Boolean);
    function GetFPS: Integer;

    function GetMaxFPS: Integer;
    procedure SetMaxFPS(const Value: Integer);

    Procedure Loaded; Override;

Public
  constructor Create(AOwner: TComponent); virtual;
  destructor Destroy; override;

  Procedure ControlResized;

  Procedure Render;
  Procedure Process;

  Procedure SystemDrawText(X,Y : Single; aText : String);

  Function DeviceTechDescription : String;

  Function GetBitmap(Const lX : Integer = 32; Const lY : Integer = 32) : TBitmap;

  Property Canvas : TCustomCanvas read EngineCanvas;
  Property DevicePXL : TCustomDevice read GetEngineDevice;
  Property FPS : Integer read GetFPS;
Published
  Property Device : TPXLEngineDevice read FDevice Write FDevice;
  Property OnInit : TNotifyEvent read FOnInit Write FOnInit;
  Property OnRender : TNotifyEvent read FOnRender Write FOnRender;
  Property OnProcess : TNotifyEvent read FOnProcess Write FOnProcess;
  Property MaxFPS : Integer read GetMaxFPS Write SetMaxFPS;
  Property Active : Boolean read FActive Write SetActive;
End;


implementation

{ TCustomPXLEngine }


procedure TCustomPXLEngine.InternalUnBuild;
begin
  if FActive then
  begin
    FActive := False;
    EngineTimer.Enabled := False;
    EngineTimer.Free;
    EngineFonts.Free;
    EngineCanvas.Free;
    EngineDevice.Free;
    DeviceProvider.Free;
    ImageFormatHandler.Free;
    ImageFormatManager.Free;
  end;
end;

procedure TCustomPXLEngine.Loaded;
begin
  if FActive then
  begin
    if not(csDesigning in ComponentState) then
    begin
      InternalBuild;
    end;
  end;
end;

procedure TCustomPXLEngine.Process;
begin
  EngineTimer.NotifyTick;
end;

procedure TCustomPXLEngine.Render;
begin
  InternalEnginerRender(Self);
end;

procedure TCustomPXLEngine.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if Not(Value) then
  begin
    if not(csDesigning in ComponentState) then
    begin
      if Assigned(EngineTimer) then
      begin
        EngineTimer.Enabled := False;
        InternalUnBuild;
      end;
    end;
  end;

end;

procedure TCustomPXLEngine.SetMaxFPS(const Value: Integer);
begin
  FMaxFPS := Value;
  if FMaxFPS = 0 then
  begin
    FMaxFPS := CST_MAX_FPS;
  end;

  if Assigned(EngineTimer) then
  begin
    EngineTimer.MaxFPS := Value;
  end;
end;

procedure TCustomPXLEngine.SystemDrawText(X, Y: Single; aText: String);
begin
  if FSystemFont>-1 then
  begin
    EngineFonts[FSystemFont].DrawText(
      Point2f(X, Y),
      aText,
      ColorPair($FFFFE887, $FFFF0000));
  end;
end;

constructor TCustomPXLEngine.Create(AOwner: TComponent);
begin
  Inherited;
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  FSystemFont := -1;
  FMaxFPS := CST_MAX_FPS;
  FDevice := TPXLEngineDevice.SoftwareRasterizer;
end;

destructor TCustomPXLEngine.Destroy;
begin
  if not(csDesigning in ComponentState) then
  begin
    InternalUnBuild;
  end;
  inherited;
end;

function TCustomPXLEngine.DeviceTechDescription: String;
begin
  Result :=  emptyStr;
  if Assigned(EngineDevice) then
  begin
    Result := GetFullDeviceTechString(EngineDevice);
  end;
end;

procedure TCustomPXLEngine.ControlResized;
begin
  InternalResize;
end;

function TCustomPXLEngine.GetBitmap(Const lX : Integer = 32; Const lY : Integer = 32) : TBitmap;
begin
  Result := TBitmap.Create(EngineDevice);
  Result.SetSize(lX,LY);
  Result.Clear;
end;

function TCustomPXLEngine.GetEngineDevice: TCustomDevice;
begin
  Result := TCustomDevice(EngineDevice);
end;

function TCustomPXLEngine.GetFPS: Integer;
begin
  Result := EngineTimer.FrameRate;
end;

function TCustomPXLEngine.GetMaxFPS: Integer;
begin
  if FMaxFPS = 0 then
  begin
    FMaxFPS := CST_MAX_FPS;
  end;

  if Assigned(EngineTimer) then
  begin
    FMaxFPS := EngineTimer.MaxFPS;
  end;

  Result := FMaxFPS;
end;

procedure TCustomPXLEngine.InternalEngineProcess(const Sender: TObject);
begin
  if Assigned(FOnProcess) then
  begin
    FOnProcess(Sender);
  end;
end;

procedure TCustomPXLEngine.InternalEnginerRender(const Sender: TObject);
begin
  if Not(FActive) then
    Exit;
  if EngineDevice.BeginScene then
  try
    EngineDevice.Clear([TClearType.Color], 0);

    if EngineCanvas.BeginScene then
    try
      if Assigned(FOnRender) then
      begin
        FOnRender(Sender);
      end;
    finally
      EngineCanvas.EndScene;
    end;

    EngineTimer.Process;
  finally
    EngineDevice.EndScene;
  end;
end;


end.

