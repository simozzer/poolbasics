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
    procedure SetWidth(const iWidth: integer);
    procedure SetHeight(const iHeight: integer);
  public
    constructor Create(const iWidth, iHeight: integer; backColor: TColor);
    destructor Destroy; override;
    procedure Render;
    property Bitmap: TBitmap read FBitmap;
    property Width: integer read FiWidth write SetWidth;
    property Height: integer read FiHeight write SetHeight;
  end;


implementation

uses
  uncirclephysicsconstants;

{ TBoardRenderer }

procedure TBoardRenderer.SetWidth(const iWidth: integer);
begin
  if (iWidth = FiWidth) then exit;
  FiWidth := iWidth;
  FBitmap.Width := iWidth;
end;

procedure TBoardRenderer.SetHeight(const iHeight: integer);
begin
  if (iHeight = FiHeight) then exit;
  FiHeight := iHeight;
  FBitmap.Height := FiHeight;

end;

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
  ACanvas.Ellipse(263,263,338,338);

  ACanvas.MoveTo(FiWidth div 2, 0);
  ACanvas.LineTo(FiWidth div 2, FiHeight);
  ACanvas.MoveTo(0, FiHeight div 2);
  ACanvas.LineTo(FiHeight, FiWidth div 2);

  ACanvas.MoveTo(80,50);
  ACanvas.LineTo(520,50);
  ACanvas.MoveTo(80, 50+(2*PUCK_RADIUS));
  ACanvas.LineTo(520, 50+(2*PUCK_RADIUS));
  ACanvas.Ellipse(80-PUCK_RADIUS,50,80+PUCK_RADIUS,50+(2*PUCK_RADIUS));
  ACanvas.Ellipse(520-PUCK_RADIUS,50,520+PUCK_RADIUS, 50+(2*PUCK_RADIUS));

  ACanvas.MoveTo(80, 550);
  ACanvas.LineTo(520,550);
  ACanvas.MoveTo(80, 550-(2*PUCK_RADIUS));
  ACanvas.LineTo(520, 550-(2*PUCK_RADIUS));
  ACanvas.Ellipse(80-PUCK_RADIUS,550,80+PUCK_RADIUS,550-(2*PUCK_RADIUS));
  ACanvas.Ellipse(520-PUCK_RADIUS,550,520+PUCK_RADIUS, 550-(2*PUCK_RADIUS));

  ACanvas.MoveTo(50, 80);
  ACanvas.LineTo(50,520);
  ACanvas.MoveTo(50 +(2*PUCK_RADIUS), 80);
  ACanvas.LineTo(50 + (2*PUCK_RADIUS), 520);
  ACanvas.Ellipse(50,80-PUCK_RADIUS,50+(2*PUCK_RADIUS),80+PUCK_RADIUS);
  ACanvas.Ellipse(50, 520-PUCK_RADIUS,50+(2*PUCK_RADIUS),520+PUCK_RADIUS);


  ACanvas.MoveTo(550, 80);
  ACanvas.LineTo(550,520);
  ACanvas.MoveTo(550 -(2*PUCK_RADIUS), 80);
  ACanvas.LineTo(550 - (2*PUCK_RADIUS), 520);

    ACanvas.Ellipse(550,80-PUCK_RADIUS,550-(2*PUCK_RADIUS),80+PUCK_RADIUS);
  ACanvas.Ellipse(550, 520-PUCK_RADIUS,550-(2*PUCK_RADIUS),520+PUCK_RADIUS);





  ACanvas.Brush.Color := clBlack;
  ACanvas.Ellipse(-POCKET_RADIUS, -POCKET_RADIUS, POCKET_RADIUS, POCKET_RADIUS);
  ACanvas.Ellipse(FiWidth - POCKET_RADIUS, -POCKET_RADIUS, FiWidth +
    POCKET_RADIUS, POCKET_RADIUS);
  ACanvas.Ellipse(-POCKET_RADIUS, FiHeight - POCKET_RADIUS, POCKET_RADIUS,
    FiHeight + POCKET_RADIUS);
  ACanvas.Ellipse(FiWidth - POCKET_RADIUS, FiHeight - POCKET_RADIUS,
    FiWidth + POCKET_RADIUS, FiHeight + POCKET_RADIUS);


end;

end.
