unit PXL.Comps.VCL.View;

interface

Uses Windows,
     Classes,
     Graphics,
     Controls,
     ExtCtrls,
     Messages,
     Math,
     PXL.TypeDef,
     PXL.Types,
     PXL.Surfaces,
     PXL.Surfaces.GDI,
     PXL.Comps.Surfaces,
     PXL.Comps.VCL,
     Types;


Type
TPXLSurfaceView = Class(TCustomControl)
private
  FInternalViewSurface : TGDIPixelSurface;
  FDatasource: TCustomPXLSurface;
  FStretch: Boolean;
  fOnMouseEnter: TNotifyEvent;
  fOnMouseLeave: TNotifyEvent;
  FProportional: Boolean;
  FCenter: Boolean;
  FOffsetX: Integer;
  FOffsetY: Integer;
  FStandalone: Boolean;
  procedure SetDataSource(const Value: TCustomPXLSurface);
  procedure SetCenter(const Value: Boolean);
  procedure SetProportional(const Value: Boolean);
  procedure SetStretch(const Value: Boolean);
  procedure SetOffsetX(const Value: Integer);
  procedure SetOffsetY(const Value: Integer);
  procedure SetStandalone(const Value: Boolean);
  procedure InternalReset;
Protected
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;

  procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
  procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  Function DestRect : TRect;
Public
  constructor Create(AOwner: TComponent); override;
  Destructor Destroy; Override;

  Procedure Paint; Override;
Published
  Property StandaloneSurface : Boolean read FStandalone Write SetStandalone;
  Property Surface : TCustomPXLSurface read FDatasource Write SetDataSource;
  Property Stretch : Boolean read FStretch Write SetStretch;
  Property Proportional : Boolean read FProportional write SetProportional;
  Property Center : Boolean read FCenter Write SetCenter;
  Property OffsetX : Integer read FOffsetX Write SetOffsetX;
  Property OffsetY : Integer read FOffsetY Write SetOffsetY;

  property Width default 250;
  property Height default 250;
  Property Align;
  Property Anchors;
  Property Visible;

  property OnClick;
  property OnDblClick;
  property OnDragDrop;
  property OnDragOver;
  property OnEndDock;
  property OnEndDrag;
  property OnMouseDown;
  property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
  property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
  property OnStartDock;
  property OnStartDrag;
End;

Procedure DrawNotAssignedStyle(aControl : TControl; aCanvas : TCanvas; aTitle : String);

Procedure Register;

implementation

Procedure Register;
begin
  RegisterComponents('PXL Surfaces',[TPXLSurfaceView]);
end;


Procedure DrawNotAssignedStyle(aControl : TControl; aCanvas : TCanvas; aTitle : String);
begin
  aCanvas.Brush.Color := clWhite;
  aCanvas.Pen.Style := psDot;
  aCanvas.Rectangle(0,0,aControl.Width, aControl.Height);
  aCanvas.Pen.Style := psSolid;
  aCanvas.Brush.Style := bsCross;
  aCanvas.Brush.Color := clBlack;
  aCanvas.Rectangle(0,0,aControl.Width,aControl.height);
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.Color := clWhite;
  aCanvas.Font.Name := 'Arial';
  aCanvas.Font.Size := 12;
  aCanvas.Font.Style := [fsBold];
  aCanvas.TextOut( aControl.Width div 2 - (aCanvas.TextWidth(aTitle) div 2),
                   aControl.Height div 2 - (aCanvas.TextHeight(aTitle) div 2),
                   aTitle);
end;

{ TPXLSurfaceView }

procedure TPXLSurfaceView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(fOnMouseEnter) then
    fOnMouseEnter(Self);
end;

procedure TPXLSurfaceView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(fOnMouseLeave) then
    fOnMouseLeave(Self);
end;

constructor TPXLSurfaceView.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  FInternalViewSurface := TGDIPixelSurface.Create;
  InternalReset;
  FStandalone := False;
  FCenter := True;
  FProportional := True;
  FStretch := False;
end;

function TPXLSurfaceView.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  if Not assigned(FInternalViewSurface) then
    Exit;

  w := FInternalViewSurface.Width;
  h := FInternalViewSurface.Height;
  cw := ClientWidth;
  ch := ClientHeight;

  //Normal representation.
  Result.Left := FOffsetX;
  Result.Top := FOffsetY;
  Result.Right := w;
  Result.Bottom := h;

  if FStretch then
  begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := ClientWidth;
    Result.Bottom := ClientHeight;
  end
  Else
  begin
    if FProportional then
    begin
      if (w>0) and (h>0) then
      begin
        xyaspect := Min(cw/w,ch/h);
        Result.Left := 0;
        Result.Top := 0;
        Result.Right := Round(xyaspect * w);
        Result.Bottom := Round(xyaspect * h);
      end;
    end;
    if FCenter then
    begin
      if Proportional then
      begin
        w := Result.Right;
        h := Result.Bottom;
      end;
      Result.Left := round((cw-w)/2);
      Result.Top  := round((ch-h)/2);
      Result.Right := w - Result.Left;
      Result.Bottom := h - Result.top;
    end;
  end;

end;


destructor TPXLSurfaceView.Destroy;
begin
  FInternalViewSurface.Free;
  inherited;
end;

procedure TPXLSurfaceView.InternalReset;
begin
  FInternalViewSurface.SetSize(CST_STD_WIDTH,CST_STD_HEIGHT,TPixelFormat.A8R8G8B8);
  FInternalViewSurface.FillRect(IntRect(0,0,CST_STD_WIDTH,CST_STD_HEIGHT),$FF000000);
end;

procedure TPXLSurfaceView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDatasource) and (Operation = opRemove) then
  begin
    FDatasource := nil;
    Repaint;
  end;
end;

procedure TPXLSurfaceView.Paint;
var lR : TRect;
begin
  inherited;
  lR := DestRect;
  if FStandalone then
  begin
    //FInternalViewSurface.BitBlt(Canvas.Handle,Point2i(lR.Left,lR.Top),Point2i(lR.Left + lR.Right,lR.Top + lR.Bottom),Point2i(0,0));
    SetStretchBltMode(Canvas.handle,ColorOnColor); //For proper stretch colour treatement.
    StretchBlt( Canvas.Handle,
                lR.Left , lR.Top, lR.Left + lR.Right, lR.Top + lR.Bottom,
                FInternalViewSurface.Handle,0,0,FInternalViewSurface.Width,FInternalViewSurface.Height, SRCCOPY);
  end
  else
  begin
    if Assigned(FDatasource) then
    begin
      FInternalViewSurface.CopyFrom(FDatasource.Surface);
      //FInternalViewSurface.BitBlt(Canvas.Handle,Point2i(lR.Left,lR.Top),Point2i(lR.Left + lR.Right,lR.Top + lR.Bottom),Point2i(0,0));
      SetStretchBltMode(Canvas.handle,ColorOnColor); //For proper stretch colour treatement.
      StretchBlt( Canvas.Handle,
                  lR.Left , lR.Top, lR.Left + lR.Right, lR.Top + lR.Bottom,
                  FInternalViewSurface.Handle,0,0,FInternalViewSurface.Width,FInternalViewSurface.Height, SRCCOPY);
    end
    else
    begin
      DrawNotAssignedStyle(Self, Canvas, 'No Surface assigned for '+Name+' - ('+ClassName+')');
    end;
  end;
end;

procedure TPXLSurfaceView.SetCenter(const Value: Boolean);
begin
  FCenter := Value;
  Repaint;
end;

procedure TPXLSurfaceView.SetDataSource(const Value: TCustomPXLSurface);
begin
  FDatasource := Value;
  if Assigned(FDatasource) then
  begin
    FInternalViewSurface.SetSize(FDatasource.Surface.Size,TPixelFormat.A8R8G8B8);
  end
  else
  begin

  end;
  Repaint;
end;

procedure TPXLSurfaceView.SetOffsetX(const Value: Integer);
begin
  FOffsetX := Value;
  Repaint;
end;

procedure TPXLSurfaceView.SetOffsetY(const Value: Integer);
begin
  FOffsetY := Value;
  Repaint;
end;

procedure TPXLSurfaceView.SetProportional(const Value: Boolean);
begin
  FProportional := Value;
  Repaint;
end;

procedure TPXLSurfaceView.SetStandalone(const Value: Boolean);
begin
  FStandalone := Value;
  InternalReset;
  Repaint;
end;

procedure TPXLSurfaceView.SetStretch(const Value: Boolean);
begin
  FStretch := Value;
  Repaint;
end;


end.
