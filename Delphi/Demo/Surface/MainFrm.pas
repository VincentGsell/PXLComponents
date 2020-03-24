unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PXL.Comps.Core, PXL.Comps.Surfaces,
  PXL.Comps.VCL, PXL.Comps.Assets, PXL.Comps.VCL.View,
  PXL.Comps.Surfaces.Process, Vcl.StdCtrls, Vcl.ExtCtrls,
  PXL.Comps.Surfaces.Process.Generators, PXL.Comps.AtlasImage;

type
  TForm2 = class(TForm)
    PXLSurface1: TPXLSurface;
    PXLSurfaceView1: TPXLSurfaceView;
    PXLProcessorImage1: TPXLProcessorImage;
    PXLAssets1: TPXLAssets;
    PXLProcessorImage2: TPXLProcessorImage;
    PXLSurfaceGeneratorGourault1: TPXLSurfaceGeneratorGourault;
    PXLSurfaceGeneratorPlasma1: TPXLSurfaceGeneratorPlasma;
    PXLProcessorDrawingLayer1: TPXLProcessorDrawingLayer;
    PXLSurfaceProcessorBackGround1: TPXLSurfaceProcessorBackGround;
    procedure PXLProcessorDrayingLayer1Render(Sender: TObject;
      Canvas: TPixelSurfaceCanvas);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.PXLProcessorDrayingLayer1Render(Sender: TObject;
  Canvas: TPixelSurfaceCanvas);
begin
  Canvas.Line(0,0,Canvas.Surface.Width,Canvas.Surface.Height,$FFFF0000);
end;

end.
