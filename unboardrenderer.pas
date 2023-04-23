unit unBoardRenderer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TBoardRenderer }

  TBoardRenderer = class
  private
    FBitmap: TBitmap;
    FiWidth: integer;
    FiHeight: integer;
    FBackColor: TColor;
  public
    constructor Create(const iWidth, iHeight: integer; backColor: TColor);
    destructor Destroy; override;
    procedure Render;
    property Bitmap: TBitmap read FBitmap;
  end;


implementation

uses
  uncirclephysicsconstants;

{ TBoardRenderer }

constructor TBoardRenderer.Create(const iWidth, iHeight: integer; backColor: TColor);
begin
  FiWidth := iWidth;
  FiHeight := iHeight;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := TPixelFormat.pf32bit;
  FBitmap.Width := FiWidth;
  FBitmap.Height := FiHeight;
  FBackColor := backColor;
end;

destructor TBoardRenderer.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TBoardRenderer.Render;
var
  ACanvas: TCanvas;
begin
  ACanvas := FBitmap.Canvas;
  ACanvas.Brush.Color := FBackColor;
  ACanvas.FillRect(0, 0, FiWidth, FiHeight);
  ACanvas.Pen.Color := clLtGray;
  ACanvas.MoveTo(FiWidth div 2, 0);
  ACanvas.LineTo(FiWidth div 2, FiHeight);
  ACanvas.MoveTo(0, FiHeight div 2);
  ACanvas.LineTo(FiHeight, FiWidth div 2);

  ACanvas.Brush.Color := clBlack;
  ACanvas.Ellipse(-POCKET_RADIUS, -POCKET_RADIUS,POCKET_RADIUS, POCKET_RADIUS);
  ACanvas.Ellipse(FiWidth - POCKET_RADIUS, -POCKET_RADIUS,FiWidth + POCKET_RADIUS, POCKET_RADIUS);
  ACanvas.Ellipse(-POCKET_RADIUS, FiHeight-POCKET_RADIUS,POCKET_RADIUS, FiHeight + POCKET_RADIUS);
    ACanvas.Ellipse(FiWidth-POCKET_RADIUS, FiHeight-POCKET_RADIUS, FiWidth + POCKET_RADIUS, FiHeight + POCKET_RADIUS);
end;

end.
