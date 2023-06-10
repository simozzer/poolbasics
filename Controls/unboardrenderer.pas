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
    FBoardBackgroundBitmap: array[0..1000] of TBitmap;
    FiWidth: integer;
    FiHeight: integer;
    FBackColor: TColor;
    FiBackgroundIndex: Integer;
    procedure SetWidth(const iWidth: integer);
    procedure SetHeight(const iHeight: integer);
    procedure RenderBoardBackground;
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
  uncirclephysicsconstants, GraphType;

{ TBoardRenderer }

procedure TBoardRenderer.SetWidth(const iWidth: integer);
var
  i: integer;
begin
  if (iWidth = FiWidth) then exit;
  FiWidth := iWidth;
  FBitmap.Width := iWidth;
  for i := 0 to 99 do
  begin
    FreeAndNil(FBoardBackgroundBitmap[i]);
  end;
end;


procedure TBoardRenderer.SetHeight(const iHeight: integer);
var
  i: integer;
begin
  if (iHeight = FiHeight) then exit;
  FiHeight := iHeight;
  FBitmap.Height := FiHeight;
  for i := 0 to 99 do
  begin
    FreeAndNil(FBoardBackgroundBitmap[i]);
  end;

end;

procedure TBoardRenderer.RenderBoardBackground;
var
  ACanvas: TCanvas;
  dAngle: double;
  dRadius: double;
  i: integer;
begin

  if not assigned(FBoardBackgroundBitmap[0]) then
  begin
    for i := 0 to 999 do
    begin

      FBoardBackgroundBitmap[i] := TBitmap.Create;
      FBoardBackgroundBitmap[i].PixelFormat := TPixelFormat.pf32bit;
      FBoardBackgroundBitmap[i].Width := FiWidth;
      FBoardBackgroundBitmap[i].Height := FiHeight;

      ACanvas := FBoardBackgroundBitmap[i].Canvas;

      ACanvas := FBoardBackgroundBitmap[i].Canvas;
      ACanvas.Pen.Color := clLtGray;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.FillRect(0, 0, FiWidth, FiHeight);


      ACanvas.FillRect(50 + (2 * PUCK_RADIUS), 50 + (2 * PUCK_RADIUS),
        550 - (2 * PUCK_RADIUS), 550 - (2 * PUCK_RADIUS));

      ACanvas.MoveTo(FiWidth div 2, 0);
      ACanvas.LineTo(FiWidth div 2, FiHeight);
      ACanvas.MoveTo(0, FiHeight div 2);
      ACanvas.LineTo(FiHeight, FiWidth div 2);

      // Draw center circle
      ACanvas.Pen.Color := clLtGray;
      ACanvas.Brush.Color := clcream;
      ACanvas.Ellipse(Round(BOARD_WIDTH / 2 - (5.3 * TARGET_RADIUS)),
        Round(BOARD_HEIGHT / 2 - (5.3 * TARGET_RADIUS)),
        Round(BOARD_WIDTH / 2 + (5.3 * TARGET_RADIUS)),
        Round(BOARD_HEIGHT / 2 + (5.3 * TARGET_RADIUS)));

      // Draw center swirl
      ACanvas.moveTo(BOARD_WIDTH div 2, BOARD_HEIGHT div 2);
      dAngle := 0;
      dRadius := 0;
      while (dRadius < (5.3 * TARGET_RADIUS)) do
      begin
        ACanvas.LineTo(BOARD_WIDTH div 2 + Round(dRadius * cos(dAngle)),
          +BOARD_HEIGHT div 2 + Round(dRadius * sin(dAngle)));
        dRadius += 0.5;
        dAngle += 0.2;
        dAngle := dAngle + (i/500/(2*pi));
      end;

      ACanvas.moveTo(BOARD_WIDTH div 2, BOARD_HEIGHT div 2);
      dAngle := 0;
      dRadius := 0;
      while (dRadius < (5.3 * TARGET_RADIUS)) do
      begin
        ACanvas.LineTo(BOARD_WIDTH div 2 + Round(dRadius * cos(dAngle)),
          +BOARD_HEIGHT div 2 + Round(dRadius * sin(dAngle)));
        dRadius += 0.4;
        dAngle += 0.2;
        dAngle := dAngle + (i/1000/(2*pi));
      end;

      ACanvas.Brush.Color := clWhite;
      ACanvas.FloodFill(
        (BOARD_WIDTH div 2) + 1, (BOARD_WIDTH div 2) + 1, clLtGray, fsBorder);
      ACanvas.Brush.Color := TColor($321280);
      ACanvas.FloodFill(
        (BOARD_WIDTH div 2) + 2, (BOARD_WIDTH div 2) + 2, clLtGray, fsBorder);


      ACanvas.Brush.Color := $f0f0f0;
      ACanvas.Pen.Color := $f0f0f0;

      // draw circles at ends of launch zones;
      ACanvas.Ellipse(80 - PUCK_RADIUS, 50, 80 + PUCK_RADIUS, 50 + (2 * PUCK_RADIUS));
      ACanvas.Ellipse(520 - PUCK_RADIUS, 50, 520 + PUCK_RADIUS, 50 + (2 * PUCK_RADIUS));

      ACanvas.Ellipse(80 - PUCK_RADIUS, 550, 80 + PUCK_RADIUS, 550 - (2 * PUCK_RADIUS));
      ACanvas.Ellipse(520 - PUCK_RADIUS, 550, 520 + PUCK_RADIUS, 550 -
        (2 * PUCK_RADIUS));

      ACanvas.Ellipse(50, 80 - PUCK_RADIUS, 50 + (2 * PUCK_RADIUS), 80 + PUCK_RADIUS);
      ACanvas.Ellipse(50, 520 - PUCK_RADIUS, 50 + (2 * PUCK_RADIUS), 520 + PUCK_RADIUS);

      ACanvas.Ellipse(550, 80 - PUCK_RADIUS, 550 - (2 * PUCK_RADIUS), 80 + PUCK_RADIUS);
      ACanvas.Ellipse(550, 520 - PUCK_RADIUS, 550 - (2 * PUCK_RADIUS),
        520 + PUCK_RADIUS);

      // Draw launch zones
      ACanvas.Rectangle(550, 80, 550 - (2 * PUCK_RADIUS), 520);
      ACanvas.Rectangle(80, 50, 520, 50 + (2 * PUCK_RADIUS));
      ACanvas.Rectangle(80, 550, 520, 550 - (2 * PUCK_RADIUS));
      ACanvas.Rectangle(50, 80, 50 + (2 * PUCK_RADIUS), 520);


      // Draw the pockets
      ACanvas.Brush.Color := clBlack;
      ACanvas.Ellipse(-POCKET_RADIUS, -POCKET_RADIUS, POCKET_RADIUS, POCKET_RADIUS);
      ACanvas.Ellipse(FiWidth - POCKET_RADIUS, -POCKET_RADIUS, FiWidth +
        POCKET_RADIUS, POCKET_RADIUS);
      ACanvas.Ellipse(-POCKET_RADIUS, FiHeight - POCKET_RADIUS, POCKET_RADIUS,
        FiHeight + POCKET_RADIUS);
      ACanvas.Ellipse(FiWidth - POCKET_RADIUS, FiHeight - POCKET_RADIUS,
        FiWidth + POCKET_RADIUS, FiHeight + POCKET_RADIUS);

    end;
  end;
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
  FiBackgroundIndex:= 0;

end;

destructor TBoardRenderer.Destroy;
var
  i: Integer;
begin
  FBitmap.Free;
  for i:= 0 to 999 do
  begin
    FBoardBackgroundBitmap[i].Free;
  end;
  inherited Destroy;
end;

procedure TBoardRenderer.Render;
var
  ACanvas: TCanvas;
begin
  RenderBoardBackground;
  FBitmap.Canvas.Draw(0, 0, FBoardBackgroundBitmap[FiBackgroundIndex]);
  Inc(FiBackgroundIndex);
  if FiBackgroundIndex > 999 then
    FiBackgroundIndex :=0;
end;

end.
