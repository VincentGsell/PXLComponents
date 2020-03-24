unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  PXL.TypeDef,
  PXL.Types,
  PXL.Canvas,
  PXL.Comps.Core,
  PXL.Comps.VCL,
  PXL.Comps.Surfaces,
  PXL.Comps.Assets,
  PXL.Comps.AtlasImage, PXL.Comps.Surfaces.Process, PXL.Comps.VCL.View;

type
  TForm1 = class(TForm)
    PXLAssets1: TPXLAssets;
    PXLEngine1: TPXLEngine;
    PXLAtlasImage1: TPXLAtlasImage;
    procedure PXLEngine1Render(Sender: TObject);
    procedure PXLEngine1Process(Sender: TObject);
    procedure PXLEngine1Init(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    EngineTicks: Integer;
    AssetDisplayIndex : integer;
    Fx, Fy : Single;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses PXL.Bitmaps, PXL.Surfaces;


procedure TForm1.FormClick(Sender: TObject);
begin
  //Roll throught the image registered in asset comps.
  Inc(AssetDisplayIndex);
  if AssetDisplayIndex>=PXLAssets1.Assets.Count then
  begin
    AssetDisplayIndex := 0;
  end;
  //setup AtlasImage with the asset.
  PXLAtlasImage1.AssetUsualName := TPXLAssetUnit(PXLAssets1.Assets.Items[AssetDisplayIndex]).UsageName;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Fx := X;
  Fy := Y;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  PXLEngine1.ControlResized;
  PXLEngine1.Render; //In order to redraw during resize.
end;

procedure TForm1.PXLEngine1Init(Sender: TObject);
begin
  //this is trigged if engine is successfully initialized : Occured only one time after Activate.
  Caption := PXLEngine1.DeviceTechDescription;
  AssetDisplayIndex := 0;
  EngineTicks := 0;
end;

procedure TForm1.PXLEngine1Process(Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TForm1.PXLEngine1Render(Sender: TObject);
var
  J, I: Integer;
  Omega, Kappa: VectorFloat;
begin
  // Draw gray background.
  for J := 0 to ClientHeight div 40 do
    for I := 0 to ClientWidth div 40 do
      PXLEngine1.Canvas.FillQuad(
        Quad(I * 40, J * 40, 40, 40),
        ColorRect($FF585858, $FF505050, $FF484848, $FF404040));


  for I := 0 to ClientWidth div 40 do
    PXLEngine1.Canvas.Line(
      Point2f(I * 40.0, 0.0),
      Point2f(I * 40.0, ClientHeight),
      $FF555555);

  for J := 0 to ClientHeight div 40 do
    PXLEngine1.Canvas.Line(
      Point2f(0.0, J * 40.0),
      Point2f(ClientWidth, J * 40.0),
      $FF555555);

  // Draw an animated hole.
  PXLEngine1.Canvas.QuadHole(
    Point2f(0.0, 0.0),
    Point2f(ClientWidth, ClientHeight),
    Point2f(
      ClientWidth * 0.5 + Cos(EngineTicks * 0.0073) * ClientWidth * 0.25,
      ClientHeight * 0.5 + Sin(EngineTicks * 0.00312) * ClientHeight * 0.25),
    Point2f(80.0, 100.0),
    $20FFFFFF, $80955BFF, 16);

  // Drawing Lenna and friends...
  if PXLAtlasImage1.IsAtlasReady then
  begin
    PXLEngine1.Canvas.UseImagePx(PXLAtlasImage1.GetAtlas, Quad(0, 0, 512, 512));
    PXLEngine1.Canvas.TexQuad(TQuad.Rotated(
      Point2f(ClientWidth, ClientHeight) * 0.5,
      Point2f(300.0, 300.0),
      EngineTicks * 0.01),
      IntColorAlpha(128));
  end;

  // Draw an animated Arc.
  Omega := EngineTicks * 0.0274;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01854) * 0.5 * Pi;

  PXLEngine1.Canvas.FillArc(
    Point2f(ClientWidth * 0.1, ClientHeight * 0.9),
    Point2f(75.0, 50.0),
    Omega, Omega + Kappa, 32,
    ColorRect($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  // Draw an animated Ribbon.
  Omega := EngineTicks * 0.02231;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.024751) * 0.5 * Pi;

  PXLEngine1.Canvas.FillRibbon(
    Point2f(ClientWidth * 0.9, ClientHeight * 0.85),
    Point2f(25.0, 20.0),
    Point2f(70.0, 80.0),
    Omega, Omega + Kappa, 32,
    ColorRect($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  PXLEngine1.SystemDrawText(4,4,'FPS: ' + IntToStr(PXLEngine1.FPS));
  PXLEngine1.SystemDrawText(4,20,'Technology: ' + PXLEngine1.DeviceTechDescription);
  PXLEngine1.SystemDrawText(fx,fy+15,'click !');
  PXLEngine1.SystemDrawText(fx,fy+30,'(' + IntToStr(Round(Fx))+','+IntToStr(Round(Fy))+')');

end;

end.
