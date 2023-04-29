unit unAngleSelectorControl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

{ TAngleControl }

type
  TAngleControl = class(TCustomControl)
  private
    FbmpBack: TBitmap;
    FclrCenterCircleFillColor: TColor;
    FclrFill: TColor;
    FclrCircleFill: TColor;
    FclrAngleColor: TColor;
    FdAngle: double;
    FiHeight: Integer;
    FiWidth: Integer;
    FOnChange: TNotifyEvent;
    FbSettingAngle: boolean;
    procedure SetAngleColor(AValue: TColor);
    procedure SetCenterCircleFillColor(AValue: TColor);
    procedure SetFillColor(AValue: TColor);
    procedure SetCircleFillColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected

    procedure SetAngle(const dAngle: double);
    function GetBackCanvas: TCanvas;
    procedure FillBackground;
    procedure DrawBackGround;
    procedure DrawAngleIndicator;
    procedure DrawCircle;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property BackCanvas: TCanvas read GetBackCanvas;
    property FillColor: TColor read FclrFill write SetFillColor;
    property AngleColor: TColor read FclrAngleColor write SetAngleColor;
    property CenterCircleFillColor: TColor read FclrCenterCircleFillColor write SetCenterCircleFillColor;
    property Angle: double read FdAngle write SetAngle;
    property Width: Integer read FiWidth write SetWidth;
    property Height: Integer  read FiHeight write SetHeight;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

implementation

uses
  Math;

const
  CONTROL_SIZE = 100;
  HALF_SIZE = CONTROL_SIZE div 2;
  CENTER_CIRCLE_RADIUS= 10;


{ TAngleControl }

procedure TAngleControl.SetFillColor(AValue: TColor);
begin
  if FclrFill = AValue then Exit;
  FclrFill := AValue;
  Invalidate;
end;

procedure TAngleControl.SetAngleColor(AValue: TColor);
begin
  if FclrAngleColor = AValue then Exit;
  FclrAngleColor := AValue;
  Invalidate;
end;

procedure TAngleControl.SetCenterCircleFillColor(AValue: TColor);
begin
  if FclrCenterCircleFillColor=AValue then Exit;
  FclrCenterCircleFillColor:=AValue;
  Invalidate;
end;

procedure TAngleControl.SetCircleFillColor(AValue: TColor);
begin
  if (FclrCircleFill = AValue) then exit;
  FclrCircleFill := AValue;
  Invalidate;
end;

procedure TAngleControl.SetHeight(AValue: Integer);
begin
  if FiHeight=AValue then Exit;
  inherited Height := AValue;
  FiHeight:=AValue;
  FbmpBack.Height:=AValue;
  Invalidate;
end;

procedure TAngleControl.SetWidth(AValue: Integer);
begin
  if FiWidth=AValue then Exit;
  inherited Width := AValue;
  FbmpBack.Width := AValue;
  FiWidth:=AValue;
    Invalidate;
end;


procedure TAngleControl.SetAngle(const dAngle: double);
var
  d: double;
begin
  d := dAngle;
  if (d = FdAngle) then Exit;
  if (d > 2 * pi) then
    d := d - (2 * PI);
  if (d < 0) then
    d := d + (2 * PI);
  FdAngle := d;
  invalidate();
  if assigned(FOnChange) then FOnChange(Self);
end;

function TAngleControl.GetBackCanvas: TCanvas;
begin
  Result := FbmpBack.Canvas;
end;

procedure TAngleControl.FillBackground;
begin
  BackCanvas.Pen.Color := FclrFill;
  BackCanvas.Brush.Color := FclrFill;
  BackCanvas.Rectangle(0, 0, CONTROL_SIZE, CONTROL_SIZE);
end;

procedure TAngleControl.DrawBackGround;
begin
  FillBackground;
  DrawCircle;
  DrawAngleIndicator;
end;

procedure TAngleControl.DrawAngleIndicator;
begin
  BackCanvas.MoveTo(HALF_SIZE, HALF_SIZE);
  BackCanvas.Pen.Color := FclrAngleColor;
  BackCanvas.LineTo(HALF_SIZE + Round(HALF_SIZE * cos(-FdAngle)), HALF_SIZE + Round(HALF_SIZE * sin(FdAngle)));
  BackCanvas.Brush.Color := FclrCenterCircleFillColor;
  BackCanvas.Ellipse(HALF_SIZE - CENTER_CIRCLE_RADIUS,
    HALF_SIZE - CENTER_CIRCLE_RADIUS,
    HALF_SIZE + CENTER_CIRCLE_RADIUS,
    HALF_SIZE + CENTER_CIRCLE_RADIUS);
end;

procedure TAngleControl.DrawCircle;
begin
  BackCanvas.Brush.Color := FclrCircleFill;
  BackCanvas.Ellipse(0, 0, Width, Height);
end;

procedure TAngleControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  dAngle: double;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FbSettingAngle := True;
    dAngle := ArcTan2((Y - CONTROL_SIZE / 2) / CONTROL_SIZE,
      (X - CONTROL_SIZE / 2) / CONTROL_SIZE);
    SetAngle(dAngle);
  end;
end;

procedure TAngleControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FbSettingAngle := False;
end;

procedure TAngleControl.MouseMove(Shift: TShiftState; X, Y: integer);
var
  dAngle: double;
begin
  inherited MouseMove(Shift, X, Y);
  if FbSettingAngle then
  begin
    dAngle := ArcTan2((Y - CONTROL_SIZE / 2) / CONTROL_SIZE,
      (X - CONTROL_SIZE / 2) / CONTROL_SIZE);
    SetAngle(dAngle);
  end;
end;

procedure TAngleControl.Paint;
begin
  inherited Paint;
  DrawBackGround;
  Canvas.Draw(0, 0, FbmpBack);

end;

constructor TAngleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FbmpBack := TBitmap.Create;
  Width := CONTROL_SIZE;
  Height := CONTROL_SIZE;
  FclrFill := clBtnFace;
  FclrCircleFill := clLtGray;
  FclrAngleColor := clWhite;
  FclrCenterCircleFillColor := clBlue;
  FdAngle := 0;
  FOnChange := nil;
  FbSettingAngle := False;
  BorderStyle := bsNone;
  BorderWidth := 0;
end;

end.
