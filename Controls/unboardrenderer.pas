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
    FBoardBackgroundBitmap: TBitmap;
    FiWidth: integer;
    FiHeight: integer;
    FBackColor: TColor;
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
  uncirclephysicsconstants;

{ TBoardRenderer }

procedure TBoardRenderer.SetWidth(const iWidth: integer);
begin
  if (iWidth = FiWidth) then exit;
  FiWidth := iWidth;
  FBitmap.Width := iWidth;
  FBoardBackgroundBitmap.Width := iWidth;
  if (FBoardBackgroundBitmap <> nil) then
    FreeAndNil(FBoardBackgroundBitmap);
end;

procedure TBoardRenderer.SetHeight(const iHeight: integer);
begin
  if (iHeight = FiHeight) then exit;
  FiHeight := iHeight;
  FBitmap.Height := FiHeight;
  FBoardBackgroundBitmap.Width := FiWidth;
  if (FBoardBackgroundBitmap <> nil) then
    FreeAndNil(FBoardBackgroundBitmap);

end;

procedure TBoardRenderer.RenderBoardBackground;
var
  ACanvas : TCanvas;
begin
  if not assigned(FBoardBackgroundBitmap) then
  begin
    FBoardBackgroundBitmap := TBitmap.Create;
    FBoardBackgroundBitmap.PixelFormat := TPixelFormat.pf32bit;
    FBoardBackgroundBitmap.Width := FiWidth;
    FBoardBackgroundBitmap.Height := FiHeight;

    ACanvas := FBoardBackgroundBitmap.Canvas;

    ACanvas := FBoardBackgroundBitmap.Canvas;
    ACanvas.Pen.Color := clLtGray;
    ACanvas.Brush.Color := clWhite; //FBackColor;
    ACanvas.FillRect(0, 0, FiWidth, FiHeight);


    ACanvas.FillRect(50 + (2 * PUCK_RADIUS), 50 + (2 * PUCK_RADIUS), 550 -
      (2 * PUCK_RADIUS), 550 - (2 * PUCK_RADIUS));

    ACanvas.MoveTo(FiWidth div 2, 0);
    ACanvas.LineTo(FiWidth div 2, FiHeight);
    ACanvas.MoveTo(0, FiHeight div 2);
    ACanvas.LineTo(FiHeight, FiWidth div 2);

    ACanvas.Pen.Color := clLtGray;
    ACanvas.Brush.Color := clcream;
    ACanvas.Ellipse(Round(BOARD_WIDTH / 2 - (5.3 * TARGET_RADIUS)),
      Round(BOARD_HEIGHT / 2 - (5.3 * TARGET_RADIUS)),
      Round(BOARD_WIDTH / 2 + (5.3 * TARGET_RADIUS)),
      Round(BOARD_HEIGHT / 2 + (5.3 * TARGET_RADIUS)));



    ACanvas.Brush.Color := $f0f0f0;
    ACanvas.Pen.Color := $f0f0f0;

    // draw circles at ends of launch zones;
    ACanvas.Ellipse(80 - PUCK_RADIUS, 50, 80 + PUCK_RADIUS, 50 + (2 * PUCK_RADIUS));
    ACanvas.Ellipse(520 - PUCK_RADIUS, 50, 520 + PUCK_RADIUS, 50 + (2 * PUCK_RADIUS));

    ACanvas.Ellipse(80 - PUCK_RADIUS, 550, 80 + PUCK_RADIUS, 550 - (2 * PUCK_RADIUS));
    ACanvas.Ellipse(520 - PUCK_RADIUS, 550, 520 + PUCK_RADIUS, 550 - (2 * PUCK_RADIUS));

    ACanvas.Ellipse(50, 80 - PUCK_RADIUS, 50 + (2 * PUCK_RADIUS), 80 + PUCK_RADIUS);
    ACanvas.Ellipse(50, 520 - PUCK_RADIUS, 50 + (2 * PUCK_RADIUS), 520 + PUCK_RADIUS);

    ACanvas.Ellipse(550, 80 - PUCK_RADIUS, 550 - (2 * PUCK_RADIUS), 80 + PUCK_RADIUS);
    ACanvas.Ellipse(550, 520 - PUCK_RADIUS, 550 - (2 * PUCK_RADIUS), 520 + PUCK_RADIUS);

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
  FBoardBackgroundBitmap.Free;
  inherited Destroy;
end;

procedure TBoardRenderer.Render;
var
  ACanvas: TCanvas;
begin
  RenderBoardBackground;
  FBitmap.Canvas.Draw(0,0, FBoardBackgroundBitmap);

end;

end.
